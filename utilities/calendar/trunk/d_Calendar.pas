unit d_Calendar;

interface

uses
  SysUtils,
  Classes,
{$ifdef linux}
  QTypes,
{$endif}
  IdBaseComponent,
  IdComponent,
{$ifndef linux}
  IdContext,
{$endif}
  IdTCPServer,
  IdCustomHTTPServer,
  IdHTTPServer;

type
  Tdm_Calendar = class(TDataModule)
    hs_HttpServer: TIdHTTPServer;
    procedure hs_HttpServerSessionStart(Sender: TIdHTTPSession);
    procedure hs_HttpServerSessionEnd(Sender: TIdHTTPSession);
  private
{$ifdef linux}
    procedure hs_HttpServerCommandGet(AThread: TIdPeerThread;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo);
{$else}
    procedure hs_HttpServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
{$endif}
  public
    constructor Create(_Owner: TComponent); override;
  end;

var
  dm_Calendar: Tdm_Calendar;

implementation

{$ifdef linux}
{$R *.xfm}
{$else}
{$R *.dfm}
{$endif}

uses
  u_Calendar,
  u_CalendarGlobals;

{$ifndef linux}
procedure WriteLn(const _s: string);
begin
  if IsConsole then
    try
      System.WriteLn(_s);
    except
      // ignore
    end;
end;
{$endif}

procedure Tdm_Calendar.hs_HttpServerSessionStart(Sender: TIdHTTPSession);
begin
  Sender.Content.AddObject('Calendar', TCalendar.Create(nil));
end;

procedure Tdm_Calendar.hs_HttpServerSessionEnd(Sender: TIdHTTPSession);
begin
  TCalendar(Sender.Content.Objects[0]).Free;
end;

{$ifdef linux}
procedure Tdm_Calendar.hs_HttpServerCommandGet(AThread: TIdPeerThread;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
{$else}
procedure Tdm_Calendar.hs_HttpServerCommandGet(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
{$endif}
var
  Calendar: TCalendar;
begin
  WriteLn(ARequestInfo.RemoteIP + ' (' + ARequestInfo.UserAgent + ') requested ' + ARequestInfo.Document);
  if (ARequestInfo.RemoteIP <> '127.0.0.1') then begin
    WriteLn('Request ignored');
    exit;
  end;
  if StartsWith(CALENDAR_URL, ARequestInfo.Document) then begin
    try
      Calendar := TCalendar(ARequestInfo.Session.Content.Objects[0]);
      Calendar.Execute(ARequestInfo, AResponseInfo);
    except
      on e: EInvalidUrl do begin
        AResponseInfo.ResponseNo := 404;
        AResponseInfo.ContentText := 'The requested URL ' + ARequestInfo.Document
          + ' was not found on this server.';
     end;
      on e: exception do
        AResponseInfo.ContentText := Format('%s: %s', [e.ClassName, e.Message]);
    end;
  end else begin
    AResponseInfo.ResponseNo := 404;
    AResponseInfo.ContentText := 'The requested URL ' + ARequestInfo.Document
     + ' was not found on this server.';
  end;
end;

constructor Tdm_Calendar.Create(_Owner: TComponent);
begin
  inherited;
  hs_HttpServer.OnCommandGet := hs_HttpServerCommandGet;
end;

end.
