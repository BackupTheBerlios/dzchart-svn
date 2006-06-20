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
  wf_TileFrame,
  w_HintWindow;

// http://rss.slashdot.org/Slashdot/slashdot
// http://delphi.wikia.com/index.php?title=Special:Recentchanges&feed=atom
// http://del.icio.us/rss/tag/delphi
// http://www.heise.de/newsticker/heise-atom.xml
// http://news.borland.com/codecentral_delphi.xml

const
  REGISTRY_KEY = 'Software\dzFeedReader';

type
  Tf_MainForm = class(TForm)
    pm_Main: TPopupMenu;
    mi_AddFeed: TMenuItem;
    procedure mi_ExitClick(Sender: TObject);
    procedure mi_AddFeedClick(Sender: TObject);
  private
    FHintWindow: TMyHintWindow;
    FFeedFrames: TFeedFrameList;
    procedure CreateFeedFrame(_Feed: TFeedDesc);
    procedure InitFeeds;
    procedure EditFeed(_Sender: TObject);
    procedure HideFeedEvent(_Sender: TObject);
    function ReadFeed(_Reg: TRegistry; _Feed: TFeedDesc): boolean;
    procedure ShowDescriptionEvent(_Sender: TObject; const _Title, _Description, _Link: string);
    procedure WriteFeed(_Feed: TFeedDesc);
  private
    FMoving: boolean;
    FMovingControl: Tfr_TileFrame;
    FMovingX: integer;
    FMovingY: integer;
    procedure EndMoveTile(_Sender: TObject; _MouseX, _MouseY: integer);
    procedure MoveTile(_Sender: TObject; _MouseX, _MouseY: integer);
    procedure StartMoveTile(_Sender: TObject; _MouseX, _MouseY: integer);
    procedure EndResizeTile(_Sender: TObject);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

var
  f_MainForm: Tf_MainForm;

implementation

{$R *.dfm}

uses
  StrUtils,
  wf_RssFrame,
  w_FeedEditForm;

constructor Tf_MainForm.Create(_Owner: TComponent);
var
  Reg: TRegistry;
begin
  inherited;
  FHintWindow := TMyHintWindow.Create(nil);
  FFeedFrames := TFeedFrameList.Create;

  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey(REGISTRY_KEY + '\WindowPos', true);
    try
      try
        Left := Reg.ReadInteger('Left');
        Top := Reg.ReadInteger('Top');
        Width := Reg.ReadInteger('Width');
        Height := Reg.ReadInteger('Height');
      except
        // ignore
      end;
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;

  InitFeeds;
end;

destructor Tf_MainForm.Destroy;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    Reg.OpenKey(REGISTRY_KEY + '\WindowPos', true);
    try
      try
        Reg.WriteInteger('Left', Left);
        Reg.WriteInteger('Top', Top);
        Reg.WriteInteger('Width', Width);
        Reg.WriteInteger('Height', Height);
      except
        // ignore
      end;
    finally
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
  FreeAndNil(FHintWindow);
  FreeAndNil(FFeedFrames);
  inherited;
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
  fr.OnEditFeed := EditFeed;
  fr.OnEndMove := EndMoveTile;
  fr.OnMove := MoveTile;
  fr.OnStartMove := StartMoveTile;
  fr.OnResized := EndResizeTile;
  try
    fr.Feed := _Feed;
  except
    fr.Free;
    exit;
  end;
  if fr.Left = 0 then
    fr.Left := FFeedFrames.Count * fr.Width + 1;
  FFeedFrames.Insert(fr);
end;

procedure Tf_MainForm.EditFeed(_Sender: TObject);
var
  frm: Tf_FeedEditForm;
  FeedFrame: Tfr_RssFrame;
begin
  FeedFrame := _Sender as Tfr_RssFrame;
  frm := Tf_FeedEditForm.Create(self);
  try
    frm.Feed := FeedFrame.Feed;
    if mrOK = frm.ShowModal then begin
      WriteFeed(frm.Feed);
      FeedFrame.Feed := frm.Feed;
    end;
  finally
    frm.Free;
  end;
end;

procedure Tf_MainForm.EndMoveTile(_Sender: TObject; _MouseX, _MouseY: integer);
var
  Frame: Tfr_RssFrame;
  Feed: TFeedDesc;
begin
  FMoving := false;
  Refresh;
  Frame := _Sender as Tfr_RssFrame;
  Feed := Frame.Feed;
  Feed.Top := Frame.Top;
  Feed.Left := Frame.Left;
  Feed.Width := Frame.Width;
  Feed.Height := Frame.Height;
  WriteFeed(Feed);
end;

procedure Tf_MainForm.EndResizeTile(_Sender: TObject);
var
  Frame: Tfr_RssFrame;
  Feed: TFeedDesc;
begin
  FMoving := false;
  Refresh;
  Frame := _Sender as Tfr_RssFrame;
  Feed := Frame.Feed;
  Feed.Top := Frame.Top;
  Feed.Left := Frame.Left;
  Feed.Width := Frame.Width;
  Feed.Height := Frame.Height;
  WriteFeed(Feed);
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

function Tf_MainForm.ReadFeed(_Reg: TRegistry; _Feed: TFeedDesc): boolean;
begin
  Result := false;
  try
    _Feed.FeedKey := ExtractFileName(_Reg.CurrentPath);
    _Feed.FeedName := _Reg.ReadString('FeedName');
    _Feed.FeedUrl := _Reg.ReadString('FeedURL');
    Result := true;
    // those may be missing -> ignore
    _Feed.Left := _Reg.ReadInteger('Left');
    _Feed.Top := _Reg.ReadInteger('Top');
    _Feed.Width := _Reg.ReadInteger('Width');
    _Feed.Height := _Reg.ReadInteger('Height');
  except
    ; //ignore
  end;
end;

procedure Tf_MainForm.ShowDescriptionEvent(_Sender: TObject;
  const _Title, _Description, _Link: string);
var
  b: boolean;
begin
  if not Assigned(FHintWindow) then
    exit;
  b := _Title <> '';
  if b then
    FHintWindow.ShowHint(Mouse.CursorPos, _Title, _Description, _Link)
  else
    FHintWindow.HideHint;
end;

procedure Tf_MainForm.StartMoveTile(_Sender: TObject; _MouseX, _MouseY: integer);
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
      Reg.WriteInteger('Left', _Feed.Left);
      Reg.WriteInteger('Top', _Feed.Top);
      Reg.WriteInteger('Width', _Feed.Width);
      Reg.WriteInteger('Height', _Feed.Height);
    finally
      Reg.CloseKey
    end;
  finally
    Reg.Free;
  end;
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

end.

