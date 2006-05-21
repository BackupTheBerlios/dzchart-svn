unit w_MainForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Registry,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  Menus,
  u_FeedDesc,
  u_FeedFrameList,
  wf_TileFrame;

// http://rss.slashdot.org/Slashdot/slashdot
// http://delphi.wikia.com/index.php?title=Special:Recentchanges&feed=atom
// http://del.icio.us/rss/tag/delphi
// http://www.heise.de/newsticker/heise-atom.xml
// http://news.borland.com/codecentral_delphi.xml

const
  REGISTRY_KEY = 'Software\dzFeedReader';

type
  Tf_MainForm = class(TForm)
    p_Description: TPanel;
    l_Description: TLabel;
    p_Title: TPanel;
    l_Title: TLabel;
    p_Link: TPanel;
    l_Link: TLabel;
    pm_Main: TPopupMenu;
    mi_AddFeed: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mi_ExitClick(Sender: TObject);
    procedure mi_AddFeedClick(Sender: TObject);
  private
    FFeedFrames: TFeedFrameList;
    procedure ShowDescriptionEvent(_Sender: TObject; const _Title, _Description, _Link: string);
    procedure HideFeedEvent(_Sender: TObject);
    procedure InitFeeds;
    function ReadFeed(_Reg: TRegistry; _Feed: TFeedDesc): boolean;
    procedure CreateFeedFrame(_Feed: TFeedDesc);
    procedure WriteFeed(_Feed: TFeedDesc);
  private
    FMoving: boolean;
    FMovingControl: Tfr_TileFrame;
    FMovingX: integer;
    FMovingY: integer;
    procedure EndMoveTile(_Sender: TObject; _MouseX, _MouseY: integer);
    procedure MoveTile(_Sender: TObject; _MouseX, _MouseY: integer);
    procedure StartMoveTile(_Sender: TObject; _MouseX, _MouseY: integer);
  public
  end;

var
  f_MainForm: Tf_MainForm;

implementation

{$R *.dfm}

uses
  StrUtils,
  wf_RssFrame,
  w_FeedEditForm;

procedure Tf_MainForm.EndMoveTile(_Sender: TObject; _MouseX, _MouseY: integer);
begin
  FMoving := false;
  Refresh;
end;

procedure Tf_MainForm.FormCreate(Sender: TObject);
begin
  FFeedFrames := TFeedFrameList.Create;
  l_Description.Top := p_Title.Height + 1;
  InitFeeds;
end;

procedure Tf_MainForm.FormDestroy(Sender: TObject);
begin
  FFeedFrames.Free;
end;

procedure Tf_MainForm.mi_AddFeedClick(Sender: TObject);
var
  frm: Tf_FeedEditForm;
begin
  frm := Tf_FeedEditForm.Create(self);
  try
    if mrOK = frm.ShowModal then begin
      WriteFeed(frm.Feed);
      CreateFeedFrame(frm.Feed);
    end;
  finally
    frm.Free;
  end;
end;

procedure Tf_MainForm.mi_ExitClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_MainForm.MoveTile(_Sender: TObject; _MouseX, _MouseY: integer);
var
  dx: integer;
  dy: integer;
  l: integer;
  t: integer;
begin
  if FMoving then begin
    dx := _MouseX - FMovingX;
    dy := _MouseY - FMovingY;
    l := FMovingControl.Left + dx;
    if l < 0 then
      l := 0;
    FMovingControl.Left := l;
    t := FMovingControl.Top + dy;
    if t < 0 then
      t := 0;
    FMovingControl.Top := t;
    FMovingX := _MouseX;
    FMovingY := _MouseY;
  end;
  Refresh;
end;

procedure Tf_MainForm.HideFeedEvent(_Sender: TObject);
var
  Idx: integer;
  Feed: TFeedDesc;
begin
  Feed := (_Sender as Tfr_RssFrame).Feed;
  if FFeedFrames.Search(Feed.FeedKey, Idx) then begin
    FFeedFrames.Extract(Idx);
  end;
end;

function Tf_MainForm.ReadFeed(_Reg: TRegistry; _Feed: TFeedDesc): boolean;
begin
  try
    _Feed.FeedKey := ExtractFileName(_Reg.CurrentPath);
    _Feed.FeedName := _Reg.ReadString('FeedName');
    _Feed.FeedUrl := _Reg.ReadString('FeedURL');
    Result := true;
  except
    Result := false;
  end;
end;

procedure Tf_MainForm.CreateFeedFrame(_Feed: TFeedDesc);
var
  fr: Tfr_RssFrame;
begin
  fr := Tfr_RssFrame.Create(Self);
  fr.Parent := self;
  fr.Name := '';
  fr.OnShowDescription := ShowDescriptionEvent;
  fr.OnRemoveFeed := HideFeedEvent;
  fr.OnEndMove := EndMoveTile;
  fr.OnMove := MoveTile;
  fr.OnStartMove := StartMoveTile;
  try
    fr.Feed := _Feed;
  except
    fr.Free;
    exit;
  end;
  fr.Left := FFeedFrames.Count * fr.Width + 1;
  FFeedFrames.Insert(fr);
end;

procedure Tf_MainForm.StartMoveTile(_Sender: TObject; _MouseX,  _MouseY: integer);
var
  i: integer;
  Frame: Tfr_TileFrame;
begin
  for i := 0 to FFeedFrames.Count - 1 do begin
    FFeedFrames[i].Deactivate;
  end;
  Frame := _Sender as Tfr_TileFrame;
  Frame.Activate;
  FMoving := true;
  FMovingX := _MouseX;
  FMovingY := _MouseY;
  FMovingControl := Frame;
end;

procedure Tf_MainForm.WriteFeed(_Feed: TFeedDesc);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey(REGISTRY_KEY + '\Feeds\' + _Feed.FeedKey, true);
    try
      Reg.WriteString('FeedName', _Feed.FeedName);
      Reg.WriteString('FeedURL', _Feed.FeedUrl);
    finally
      Reg.CloseKey
    end;
  finally
    Reg.Free;
  end;
end;

procedure Tf_MainForm.InitFeeds;
var
  Reg: TRegistry;
  Keys: TStringList;
  i: Integer;
  Feed: TFeedDesc;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if not Reg.OpenKey(REGISTRY_KEY + '\Feeds', False) then
      exit;
    try
      Keys := TStringList.Create;
      try
        Reg.GetKeyNames(Keys);
        Feed := TFeedDesc.Create;
        try
          for i := 0 to Keys.Count - 1 do begin
            Reg.CloseKey;
            if Reg.OpenKey(REGISTRY_KEY + '\Feeds\' + Keys[i], False) then begin
              if ReadFeed(Reg, Feed) then begin
                CreateFeedFrame(Feed);
              end;
            end;
          end;
        finally
          Feed.Free;
        end;
      finally
        Keys.Free;
      end;
    finally
      Reg.CloseKey;
    end;

//    frm := Tfr_RssFrame.Create(Self);
//    frm.Parent := self;
//    frm.Name := '';
//    frm.Left := frm.Width;
//    frm.Url := 'http://www.heise.de/newsticker/heise-atom.xml';
//    frm.OnShowDescription := ShowDescriptionEvent;
  finally
    Reg.Free;
  end;
end;

procedure Tf_MainForm.ShowDescriptionEvent(_Sender: TObject;
  const _Title, _Description, _Link: string);
const
  MIN_WIDTH = 100;
var
  b: boolean;
  MPos: TPoint;
  x: integer;
  y: integer;
  w: integer;
  h: integer;
  Size: integer;
begin
  MPos := ScreenToClient(Mouse.CursorPos);
  b := _Title <> '';
  if b then begin
    l_Title.Caption := _Title;
    l_Description.Caption := _Description;
    l_Link.Caption := _Link;
    w := MIN_WIDTH;
    Size := l_Title.Canvas.TextWidth(_Title);
    if Size > w then
      w := Size;
    Size := l_Link.Canvas.TextWidth(_Link);
    if Size > w then
      w := Size;
    p_Description.ClientWidth := w;
    l_Description.Width := w;
    h := p_Title.Height + l_Description.Height + p_Link.Height;
    p_Description.ClientHeight := h;
    x := MPos.X + 16;
    if x + w > ClientWidth then
      x := ClientWidth - w;
    p_Description.Left := x;
    y := MPos.Y + 16;
    if y + h > ClientHeight then
      y := ClientHeight - h;
    p_Description.Top := y;

    p_Description.BringToFront;
  end;
  p_Description.Visible := b;
end;

end.

