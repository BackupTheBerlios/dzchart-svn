unit u_ContentWriter;

{$optimization off}

interface

uses
  SysUtils,
  Classes,
  u_HtmlDocument,
  u_CalendarGlobals,
  u_Appointments,
  u_CalendarParams;

type
  TSectionWriter = procedure of object;

type
  TContentWriter = class(THtmlDocument)
  protected
    fParams: TCalendarParams;
    fHolidays: TAptColl;
    fAppointments: TAptColl;
    fActiveApts: TAptColl;
    fInitializedYears: TStringList;
    procedure InitHolidays(_Year: integer);
    function HasDescription(const _Date: string; out _Holiday: TAppointment; _Apts: TAptColl): boolean; overload;
    function HasDescription(const _Date: TDateTime; out _Holiday: TAppointment; _Apts: TAptColl): boolean; overload;
    function IsHoliday(const _Date: TDateTime): boolean;
    function IsWeekend(const _Date: TDateTime): boolean;
  protected
    procedure WriteSection(const _Name: string; _SectionWriter: TSectionWriter);
  public
    constructor Create(_Params: TCalendarParams);
    destructor Destroy; override;
    function Title: string; virtual;
  end;

implementation

uses
  DateUtils;

{ TContentWriter }

constructor TContentWriter.Create(_Params: TCalendarParams);
begin
  inherited Create;
  fParams := _Params;

  fHolidays := TAptColl.Create;
  fActiveApts := TAptColl.Create;
  fActiveApts.Duplicates := true;
  fInitializedYears := TStringList.Create;

  fAppointments := TAptColl.Create;
  fAppointments.Duplicates := true;
  fAppointments.ReadFromFile(APPOINTMENTS_DATA);
end;

destructor TContentWriter.Destroy;
begin
  fInitializedYears.Free;
  fActiveApts.Free;
  fAppointments.Free;
  fHolidays.Free;
  inherited;
end;

function TContentWriter.Title: string;
begin
  Result := 'Calendar';
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

procedure TContentWriter.InitHolidays(_Year: integer);

  procedure AddOnceApt(_Apt: TAppointment);
  begin
    if YearOf(_Apt.Date) = _Year then
      fActiveApts.AddAppointment(_Apt.Date, _Apt.Time, _Apt.Desc);
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

function TContentWriter.IsWeekend(const _Date: TDateTime): boolean;
begin
  Result := (DayOfTheWeek(_Date) in [DaySaturday, DaySunday]);
end;

function TContentWriter.IsHoliday(const _Date: TDateTime): boolean;
var
  Idx: integer;
begin
  Result := fHolidays.SearchDate(_Date, Idx);
end;

function TContentWriter.HasDescription(const _Date: string; out _Holiday: TAppointment; _Apts: TAptColl): boolean;
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

function TContentWriter.HasDescription(const _Date: TDateTime; out _Holiday: TAppointment; _Apts: TAptColl): boolean;
begin
  Result := HasDescription(IsoDate(_Date), _Holiday, _Apts);
end;

procedure TContentWriter.WriteSection(const _Name: string; _SectionWriter: TSectionWriter);
begin
  WriteFmt('<div id="%s">'#10, [_Name]);
  _SectionWriter;
  Write('</div>'#10);
end;

end.


