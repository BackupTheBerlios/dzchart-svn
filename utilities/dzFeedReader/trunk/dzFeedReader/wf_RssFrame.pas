unit wf_RssFrame;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdHTTP,
  SimpleRSS,
  StdCtrls,
  ExtCtrls,
  Menus,
  u_FeedDesc,
  u_EventThread,
  wf_TileFrame;

type
  TOnShowDescription = procedure(_Sender: TObject; const _Title, _Description, _Link: string) of object;
  TRemoveFeed = procedure(_Sender: TObject) of object;

type
  Tfr_RssFrame = class(Tfr_TileFrame)
    SimpleRSS: TSimpleRSS;
    IdHTTP: TIdHTTP;
    pm_Title: TPopupMenu;
    mi_Properties: TMenuItem;
    mi_Saveas: TMenuItem;
    mi_Remove: TMenuItem;
    lb_RssFeed: TListBox;
    procedure lb_RssFeedMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure SimpleRSSParseXML(Sender: TObject);
    procedure lb_RssFeedMouseLeave(Sender: TObject);
    procedure mi_RemoveClick(Sender: TObject);
  private
    FThread: TEventThread;
    FCurrentLine: Integer;
    FOnShowDescription: TOnShowDescription;
    FFeed: TFeedDesc;
    FRemoveFeed: TRemoveFeed;
    FMessage: string;
    procedure doShowMessage(const _Message: string);
    procedure SyncShowMessage;
    procedure SyncUpdateFeed;
    procedure doShowDescription(const _Title, _Description, _Link: string);
    procedure doRemoveFeed;
    procedure SetFeed(const _Feed: TFeedDesc);
    procedure OnThreadExecute(_Sender: TObject);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToFile(const _Filename: string);
    property Feed: TFeedDesc read FFeed write SetFeed;
    property OnShowDescription: TOnShowDescription read FOnShowDescription write FOnShowDescription;
    property OnRemoveFeed: TRemoveFeed read FRemoveFeed write FRemoveFeed;
  end;

implementation

{$R *.dfm}

uses
  SimpleRSSTypes,
  ComObj,
  ActiveX;

{ Tfr_RssFrame }

constructor Tfr_RssFrame.Create(_Owner: TComponent);
begin
  inherited;
  FCurrentLine := -1;
  FFeed := TFeedDesc.Create;
  FThread := TEventThread.Create(OnThreadExecute);
end;

destructor Tfr_RssFrame.Destroy;
begin
  if Assigned(FThread) then begin
    FThread.Terminate;
  end;
  //SimpleRSS.SaveToFile('d:\' + SimpleRSS.Channel.Required.Title + '.xml');
  inherited;
end;

procedure Tfr_RssFrame.doRemoveFeed;
begin
  if Assigned(FRemoveFeed) then
    FRemoveFeed(Self);
end;

procedure Tfr_RssFrame.doShowDescription(const _Title, _Description, _Link: string);
begin
  if Assigned(FOnShowDescription) then
    FOnShowDescription(Self, _Title, _Description, _Link);
end;

procedure Tfr_RssFrame.doShowMessage(const _Message: string);
begin
  FMessage := _Message;
  TThread.Synchronize(FThread, SyncShowMessage);
end;

procedure Tfr_RssFrame.lb_RssFeedMouseLeave(Sender: TObject);
begin
  doShowDescription('', '', '');
  FCurrentLine := -1;
end;

procedure Tfr_RssFrame.lb_RssFeedMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Idx: Integer;
  Item: TRSSItem;
begin
  Idx := lb_RssFeed.ItemAtPos(Point(x, y), true);
  if FCurrentLine = Idx then
    exit;
  if (Idx = -1) or (Idx >= SimpleRSS.Items.Count) then begin
    doShowDescription('', '', '')
  end else begin
    Item := SimpleRSS.Items[Idx];
    doShowDescription(Item.Title, Item.Description, Item.Link);
  end;
  FCurrentLine := Idx;
end;

procedure Tfr_RssFrame.OnThreadExecute(_Sender: TObject);
var
  Thread: TEventThread;
  i: integer;
begin
  CoInitializeEx(nil, COINIT_MULTITHREADED);
  try
    Thread := _Sender as TEventThread;
    repeat
      doShowMessage('Updating ...');
      try
        if FFeed.IsFilebased then
          SimpleRSS.LoadFromFile(FFeed.FeedUrl)
        else
          SimpleRSS.LoadFromHTTP(FFeed.FeedUrl);
      except
        on e: Exception do begin
          doShowMessage(e.Message);
        end;
      end;
      for i := 0 to 60 * 60 - 1 do begin
        if Thread.Terminated then
          break;
        Sleep(1000);
      end;
//      sleep(60 * 60 * 1000);
    until Thread.Terminated;
    Thread.FreeOnTerminate := true;
  finally
    try
      CoUninitialize;
    except
      // ignore
    end;
  end;
end;

procedure Tfr_RssFrame.mi_RemoveClick(Sender: TObject);
begin
  doRemoveFeed;
  self.Free;
end;

procedure Tfr_RssFrame.SaveToFile(const _Filename: string);
begin
  SimpleRSS.SaveToFile(_Filename);
end;

procedure Tfr_RssFrame.SetFeed(const _Feed: TFeedDesc);
begin
  FFeed.Assign(_Feed);
  Caption := Feed.FeedName;
  FThread.Resume;
end;

procedure Tfr_RssFrame.SimpleRSSParseXML(Sender: TObject);
begin
  // This is called from the thread, so we need synchronize here
  TThread.Synchronize(FThread, SyncUpdateFeed);
end;

procedure Tfr_RssFrame.SyncShowMessage;
begin
  try
    lb_RssFeed.Clear;
    lb_RssFeed.Items.Add(FMessage);
  except
    // ignore
  end;
end;

procedure Tfr_RssFrame.SyncUpdateFeed;
var
  i: integer;
begin
  lb_RssFeed.Clear;
  // SimpleRSS.SaveToFile('d:\slashdot.xml');
  for i := 0 to SimpleRSS.Items.Count - 1 do begin
    lb_RssFeed.Items.Add(VarToStrDef(SimpleRSS.Items[i].Title, 'no title'));
  end;
  Caption := SimpleRSS.Channel.Required.Title;
end;

initialization
  CoInitializeEx(nil, COINIT_MULTITHREADED);
end.

