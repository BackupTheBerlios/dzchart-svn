unit u_CalendarParams;

{$optimization off}

interface

uses
  SysUtils,
  Classes,
  IdCustomHTTPServer,
  u_Appointments,
  u_CalendarGlobals,
  u_HtmlContainer;

type
  TParameters = (
    cpStartDayOfWeek,
    cpStyle,
    cpLanguage,
    cpMonthsPast,
    cpMonthsFuture,
    cpCurrentDate);
  TParametersSet = set of TParameters;

type
  TCalendarParams = class
  protected
    fHolidays: TAptColl;
    fAppointments: TAptColl;
    fActiveApts: TAptColl;
    fInitializedYears: TStringList;

    // a global date object to speed things up a little, since
    // there are lots of places where we need today's date
    fToday: TDateTime;
    {: number of months to display in addition to the current month }
    fMonthsPast: integer;
    fMonthsFuture: integer;
    // language, I've used short three letter names for this
    fLanguage: string;
    // empty is default, to make something else default change to 'red', 'green' or whatever...
    fStyle: string;
    // fCurrentMonth and fCurrentYear are used for persistence between calls
    fCurrentDate: TDateTime;
    fStartDayOfWeek: integer;
    fDetailDate: string;
    fAptsToDelete: TStringList;
    // adding an appointment
    FAptAddYear: string;
    FAptAddMonth: string;
    FAptAddDay: string;
    FAptAddHour: string;
    FAptAddMinute: string;
    FAptAddDesc: string;
  public
    constructor Create(_RequestInfo: TIdHTTPRequestInfo); overload;
    constructor Craete(_Source: TCalendarparams); overload;
    destructor Destroy; override;
    procedure InitHolidays(_Year: integer);
    property Today: TDateTime read fToday write fToday;
    property MonthsPast: integer read fMonthsPast write fMonthsPast;
    property MonthsFuture: integer read fMonthsFuture write fMonthsFuture;
    property Language: string read fLanguage write fLanguage;
    property Style: string read fStyle write fStyle;
    property CurrentDate: TDateTime read fCurrentDate write fCurrentDate;
    property StartDayOfWeek: integer read fStartDayOfWeek write fStartDayOfWeek;
    property DetailDate: string read fDetailDate write fDetailDate;
    property AptsToDelete: TStringList read fAptsToDelete;
    property AptAddYear: string read FAptAddYear;
    property AptAddMonth: string read FAptAddMonth;
    property AptAddDay: string read FAptAddDay;
    property AptAddHour: string read FAptAddHour;
    property AptAddMinute: string read FAptAddMinute;
    property AptAddDesc: string read FAptAddDesc;
    function CurrentMonth: integer;
    function CurrentYear: integer;
    function TodaysDay: integer;
    procedure AddHiddenFields(_Container: THtmlContainer; _Exclude: TParametersSet);
    function BuildFatUrl(const _Function: string; _Exclude: TParametersSet): string;
    function HasDescription(const _Date: string; out _Holiday: TAppointment; _Apts: TAptColl): boolean; overload;
    function HasDescription(const _Date: TDateTime; out _Holiday: TAppointment; _Apts: TAptColl): boolean; overload;
    function IsHoliday(const _Date: TDateTime): boolean;
    function IsWeekend(const _Date: TDateTime): boolean;
  end;

implementation

uses
  DateUtils,
  u_HtmlInputField;

{ TCalendarParams }

constructor TCalendarParams.Craete(_Source: TCalendarparams);
begin
  inherited Create;
  fToday := Date;

  fCurrentDate := fToday;
  fCurrentDate := _Source.CurrentDate;

  fMonthsPast := _Source.MonthsPast;
  fMonthsFuture := _Source.MonthsFuture;

  fLanguage := _Source.Language;
  fStyle := _Source.Style;
  fStartDayOfWeek := _Source.StartDayOfWeek;

  fDetailDate := _Source.DetailDate;
end;

constructor TCalendarParams.Create(_RequestInfo: TIdHTTPRequestInfo);

  function ReadAddMonths(const _Direction: string; _Default: integer): integer;
  var
    s: string;
  begin
    Result := StrToIntDef(_RequestInfo.Params.Values['cal_' + _Direction], _Default);
    s := _RequestInfo.Params.Values['btn_' + _Direction];
    if s = '+' then
      Inc(Result)
    else if s = '-' then
      Dec(Result);
  end;

  function StrDef(const _Value, _Default: string): string;
  begin
    if _Value = '' then
      Result := _Default
    else
      Result := _Value;
  end;

var
  Year: integer;
  Month: integer;
  i: integer;
  s: string;
begin
  fHolidays := TAptColl.Create;
  fActiveApts := TAptColl.Create;
  fActiveApts.Duplicates := true;
  fInitializedYears := TStringList.Create;

  fAppointments := TAptColl.Create;
  fAppointments.Duplicates := true;
  fAppointments.ReadFromFile(APPOINTMENTS_DATA);

  fToday := Date;

  fCurrentDate := fToday;
  Month := StrToIntDef(_RequestInfo.Params.Values['month'], CurrentMonth);
  Year := StrToIntDef(_RequestInfo.Params.Values['year'], CurrentYear);
  fCurrentDate := EncodeDate(Year, Month, 1);

  fMonthsPast := ReadAddMonths('past', 1);
  fMonthsFuture := ReadAddMonths('future', 1);

  fLanguage := StrDef(_RequestInfo.Params.Values['lang'], 'eng');
  fStyle := StrDef(_RequestInfo.Params.Values['style'], 'default_calendar');
  fStartDayOfWeek := StrToIntDef(_RequestInfo.Params.Values['starton'], DayMonday) mod 7;

  fDetailDate := _RequestInfo.Params.Values['date'];

  fAptsToDelete := TStringList.Create;
  for i := 0 to _RequestInfo.Params.Count - 1 do begin
    s := _RequestInfo.Params[i];
    if StartsWith('appointment=', s) then begin
      fAptsToDelete.Add(TailStr(s, 13));
    end;
  end;

  FAptAddYear := _RequestInfo.Params.Values['addyear'];
  FAptAddMonth := _RequestInfo.Params.Values['addmonth'];
  FAptAddDay := _RequestInfo.Params.Values['addday'];
  FAptAddHour := _RequestInfo.Params.Values['addhour'];
  FAptAddMinute := _RequestInfo.Params.Values['addminute'];
  FAptAddDesc := _RequestInfo.Params.Values['adddesc'];
end;

function TCalendarParams.CurrentMonth: integer;
begin
  Result := MonthOf(fCurrentDate);
end;

function TCalendarParams.CurrentYear: integer;
begin
  Result := YearOf(fCurrentDate);
end;

function TCalendarParams.TodaysDay: integer;
begin
  Result := DayOfTheMonth(fToday);
end;

procedure TCalendarParams.AddHiddenFields(_Container: THtmlContainer; _Exclude: TParametersSet);
begin
  if not (cpStartDayOfWeek in _Exclude) then
    _Container.Add(THtmlInputField.Create('starton', 'hidden', IntToStr(StartDayOfWeek)));
  if not (cpStyle in _Exclude) then
    _Container.Add(THtmlInputField.Create('style', 'hidden', Style));
  if not (cpLanguage in _Exclude) then
    _Container.Add(THtmlInputField.Create('lang', 'hidden', Language));
  if not (cpMonthsPast in _Exclude) then
    _Container.Add(THtmlInputField.Create('cal_past', 'hidden', IntToStr(MonthsPast)));
  if not (cpMonthsFuture in _Exclude) then
    _Container.Add(THtmlInputField.Create('cal_future', 'hidden', IntToStr(MonthsFuture)));
  (*  if not (cpDisplayWeek in _Exclude) then
    _Container.Add(THtmlInputField.Create('displayweek', 'hidden', IntToStr(Ord(DisplayWeek)))); *)
  if not (cpCurrentDate in _Exclude) then begin
    _Container.Add(THtmlInputField.Create('month', 'hidden', IntToStr(CurrentMonth)));
    _Container.Add(THtmlInputField.Create('year', 'hidden', IntToStr(CurrentYear)));
  end;
end;

function TCalendarParams.BuildFatUrl(const _Function: string; _Exclude: TParametersSet): string;
var
  Delimiter: string;

  procedure AddSetting(const _Setting, _Value: string);
  begin
    Result := Result + Delimiter + _Setting + '=' + _Value;
    Delimiter := '&amp;';
  end;

begin
  Result := CALENDAR_URL;
  if _Function <> '' then
    Result := Result + '/' + _Function;

  Delimiter := '?';
  if not (cpStartDayOfWeek in _Exclude) then
    AddSetting('starton', IntToStr(StartDayOfWeek));
  if not (cpStyle in _Exclude) then
    AddSetting('style', Style);
  if not (cpLanguage in _Exclude) then
    AddSetting('lang', Language);
  if not (cpMonthsPast in _Exclude) then
    AddSetting('cal_past', IntToStr(MonthsPast));
  if not (cpMonthsFuture in _Exclude) then
    AddSetting('cal_future', IntToStr(MonthsFuture));
(*  if not (cpDisplayWeek in _Exclude) then
    AddSetting('displayweek', IntToStr(Ord(DisplayWeek))); *)
  if not (cpCurrentDate in _Exclude) then begin
    AddSetting('month', IntToStr(CurrentMonth));
    AddSetting('year', IntToStr(CurrentYear));
  end;
end;

procedure TCalendarParams.InitHolidays(_Year: integer);

  procedure AddOnceApt(_Apt: TAppointment);
  begin
    if YearOf(_Apt.Date) = _Year then
      fActiveApts.AddAppointment(_Apt.Date, _Apt.Time, _Apt.Desc);
  end;

  function GetEaster(Year: Integer): TDateTime;
  var
    y, m, d: Word;
    G, I, J, C, H, L: Integer;
    E: TDateTime;
  begin
    G := Year mod 19;
    C := year div 100;
    H := (C - C div 4 - (8 * C + 13) div 25 + 19 * G + 15) mod 30;
    I := H - (H div 28) * (1 - (H div 28) * (29 div (H + 1)) * ((21 - G) div 11));
    J := (Year + Year div 4 + I + 2 - C + C div 4) mod 7;
    L := I - J;
    m := 3 + (L + 40) div 44;
    d := L + 28 - 31 * (m div 4);
    y := Year;
    // E is the date of the full moon
    E := EncodeDate(y, m, d);
    // find next sunday
    while DayOfWeek(E) > 1 do
      E := E + 1;
    Result := E;
  end;

var
  Easter: TDateTime;
  YearStr: string;
  i: integer;
  Apt: TAppointment;
begin
  YearStr := Format('%.4d', [_Year]);
  if fInitializedYears.Find(YearStr, i) then
    exit;

  fHolidays.AddAppointment(EncodeDate(_Year, 01, 01), 'Neujahr');
  fHolidays.AddAppointment(EncodeDate(_Year, 05, 01), 'Maifeiertag');
  fHolidays.AddAppointment(EncodeDate(_Year, 10, 03), 'Tag d. Dt. Einheit');
  fHolidays.AddAppointment(EncodeDate(_Year, 10, 31), 'Reformationstag');
  fHolidays.AddAppointment(EncodeDate(_Year, 11, 01), 'Allerheiligen');
  fHolidays.AddAppointment(EncodeDate(_Year, 12, 25), '1. Weihnachtstag');
  fHolidays.AddAppointment(EncodeDate(_Year, 12, 26), '2. Weihnachtstag');

  Easter := GetEaster(CurrentYear);
  fHolidays.AddAppointment(IncDay(Easter, -2), 'Karfreitag');
  fHolidays.AddAppointment(Easter, 'Ostersonntag');
  fHolidays.AddAppointment(IncDay(Easter, 1), 'Ostermontag');
  fHolidays.AddAppointment(IncDay(Easter, 39), 'Christi Himmelfahrt');
  fHolidays.AddAppointment(IncDay(Easter, 49), 'Pfingstsonntag');
  fHolidays.AddAppointment(IncDay(Easter, 50), 'Pfingstmontag');

  for i := 0 to fAppointments.Count - 1 do begin
    Apt := fAppointments[i];
    case Apt.AptType of
      atOnce:
        AddOnceApt(Apt);
    end;
  end;

  fInitializedYears.Add(YearStr);
end;

function TCalendarParams.IsWeekend(const _Date: TDateTime): boolean;
begin
  Result := (DayOfTheWeek(_Date) in [DaySaturday, DaySunday]);
end;

function TCalendarParams.IsHoliday(const _Date: TDateTime): boolean;
var
  Idx: integer;
begin
  Result := fHolidays.SearchDate(_Date, Idx);
end;

function TCalendarParams.HasDescription(const _Date: string; out _Holiday: TAppointment; _Apts: TAptColl): boolean;
var
  Idx: integer;
  Apt: TAppointment;
begin
  Result := false;
  if fHolidays.Search(_Date, Idx) then begin
    Result := true;
    _Holiday := fHolidays[Idx];
  end else begin
    _Holiday := nil;
  end;

  fActiveApts.Search(_Date, Idx);
  if Idx < fActiveApts.Count then begin
    Apt := fActiveApts[Idx];
    while Apt.DateStr = _Date do begin
      Result := true;
      _Apts.Insert(Apt.Clone);
      Inc(Idx);
      if Idx >= fActiveApts.Count then
        exit;
      Apt := fActiveApts[Idx];
    end;
  end;
end;

function TCalendarParams.HasDescription(const _Date: TDateTime; out _Holiday: TAppointment; _Apts: TAptColl): boolean;
begin
  Result := HasDescription(IsoDate(_Date), _Holiday, _Apts);
end;

destructor TCalendarParams.Destroy;
begin
  FreeAndNil(fHolidays);
  FreeAndNil(fAppointments);
  FreeAndNil(fActiveApts);
  FreeAndNil(fInitializedYears);
  inherited;
end;

end.


