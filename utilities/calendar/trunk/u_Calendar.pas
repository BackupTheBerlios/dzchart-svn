unit u_Calendar;

{$OPTIMIZATION off}

interface

uses
  SysUtils,
  Classes,
  IdCustomHTTPServer,
  u_CalendarGlobals,
  u_Appointments,
  u_CalendarParams,
  u_DetailsWriter,
  u_CalendarWriter,
  u_AptDelete,
  u_AptAdd;

type
  EInvalidUrl = class(ECalendarException);

type
  TCalendar = class(TComponent)
  private
    fRequestInfo: TIdHTTPRequestInfo;
    fResponseInfo: TIdHTTPResponseInfo;

    fHtmlTemplate: string;
    function IconUrl: string;
    function StyleUrl(_Params: TCalendarParams): string;
    function ReadFile(const _FileName: string): string;
    function GeneratePage(_Params: TCalendarParams; const _Title: string; const _Content: string): string;
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    procedure Execute(_RequestInfo: TIdHTTPRequestInfo; _ResponseInfo: TIdHTTPResponseInfo);
  end;

implementation

uses
  Math,
  DateUtils,
  StrUtils,
  u_ContentWriter, u_HtmlComponent;

constructor TCalendar.Create(_Owner: TComponent);
begin
  inherited Create(_Owner);
  fHtmlTemplate := ReadFile('/template.html');
end;

destructor TCalendar.Destroy;
begin
  inherited;
end;

function TCalendar.ReadFile(const _FileName: string): string;
var
  fs: TFileStream;
  ss: TStringStream;
begin
  ss := TStringStream.Create('');
  try
    { TODO : Make this safe: Do not allow .. in the filename (other problems?) }
    fs := TFileStream.Create(DOCUMENT_ROOT + _Filename, fmOpenRead + fmShareDenyWrite);
    try
      ss.CopyFrom(fs, fs.Size);
      Result := ss.DataString;
    finally
      fs.Free;
    end;
  finally
    ss.Free;
  end;
end;

procedure TCalendar.Execute(_RequestInfo: TIdHTTPRequestInfo; _ResponseInfo: TIdHTTPResponseInfo);
var
  s: string;
  Params: TCalendarParams;
  Content: string;
  Title: string;
  Writer: TContentWriter;
begin
  fRequestInfo := _RequestInfo;
  fResponseInfo := _ResponseInfo;

  s := fRequestInfo.Document;
  Assert(StartsWith(CALENDAR_URL, s));
  s := TailStr(s, Length(CALENDAR_URL) + 1);

  if StartsWith('/css/', s) then begin
    fResponseInfo.ContentType := 'text/css';
    fResponseInfo.ContentText := ReadFile(s);
    exit;
  end;

  if StartsWith('/lang/', s) then begin
    fResponseInfo.ContentType := 'text/css';
    fResponseInfo.ContentText := ReadFile(s);
    exit;
  end;

  if StartsWith('/img/', s) then begin
    fResponseInfo.ContentType := 'image/gif';
    fResponseInfo.ContentText := ReadFile(s);
    exit;
  end;

  fResponseInfo.ContentType := 'text/html';

  Params := TCalendarParams.Create(_RequestInfo);
  try
    if s = '/show' then begin
      Writer := TDetailsWriter.Create(Params);
    end else if s = '/delete' then begin
      Writer := TAptDelete.Create(Params);
    end else if s = '/add' then begin
      Writer := TAptAdd.Create(Params);
    end else if (s = '') or (s = '/') then begin
      Writer := TCalendarWriter.Create(Params);
    end else begin
      raise EInvalidUrl.Create('Calendar: Invalid document url');
    end;
    try
      Content := Writer.Html;
      Title := Writer.Title;
    finally
      Writer.Free;
    end;
    fResponseInfo.ContentText := GeneratePage(Params, Title, Content);
  finally
    Params.Free;
  end;
end;

function TCalendar.IconUrl: string;
begin
  Result := Format('%s/img/calendar_icon.gif', [CALENDAR_URL]);
end;

function TCalendar.StyleUrl(_Params: TCalendarParams): string;
begin
  Result := Format('%s/css/%s.css', [CALENDAR_URL, _Params.Style]);
end;

function TCalendar.GeneratePage(_Params: TCalendarParams; const _Title: string; const _Content: string): string;
begin
  Result := fHtmlTemplate;
  Result := StringReplace(Result, '${TITLE}', _Title, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '${ICONURL}', IconUrl, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '${STYLEURL}', StyleUrl(_Params),
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, '${CONTENT}', _Content, [rfReplaceAll, rfIgnoreCase]);
end;

end.


