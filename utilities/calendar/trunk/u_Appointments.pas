unit u_Appointments;

{$optimization off}

interface

uses
  SysUtils,
  u_dzCollections;

type
  TAptTypes = (atOnce, atYearly);

type
  TAppointment = class
  private
  protected
    fAptType: TAptTypes;
    fDate: TDateTime;
    fTime: TDateTime;
    fDesc: string;
    fKey: string; // this must be stored because otherwise the string
    // can not be used as key of the collectin
  public
    constructor Create(const _Date, _Time: TDateTime; const _Desc: String; _AptType: TAptTypes = atOnce);
    class function DateTimestr(const _Date, _Time: TDateTime): string; overload;
    class function DateTimestr(const _Date: TDateTime): string; overload;
    function DateTimeStr: string; overload;
    function DateStr: string;
    function TimeStr: string;
    function TimeOrStar: string;
    function TimeDesc: string;
    function Clone: TAppointment;
    property AptType: TAptTypes read fAptType write fAptType;
    property Date: TDateTime read fDate write fDate;
    property Time: TDateTime read fTime write fTime;
    property Desc: string read fDesc write fDesc;
  end;

{$define TYPED_SORTED_COLLECTION_TEMPLATE}
type
  _COLLECTION_ITEM_ = TAppointment;
  _COLLECTION_KEY_  = string;
{$include 't_dzTypedSortedCollection.tpl'}

type
  TAptColl = class(_TYPED_SORTED_COLLECTION_)
  protected
    function KeyOf(_Item: TAppointment): string; override;
    function Compare(const _Key1, _Key2: string): integer; override;
  public
    constructor Create;
    procedure AddAppointment(_Date, _Time: TDateTime; const _Desc: String); overload;
    procedure AddAppointment(_Date: TDateTime; const _Desc: String); overload;
    function SearchDate(_Date: TDateTime; out _Idx: integer): boolean;
    function Clone: TAptColl;
    procedure AddAptColl(_Source: TAptColl);
    procedure ReadFromFile(const _Filename: string);
    procedure WriteToFile(const _Filename: string);
  end;

function IsoDate(const _Date: TDateTime): string;
function IsoTime(const _Time: TDateTime; _IncludeSeconds: boolean = true): string;
function IsoDateTime(const _DateTime: TDateTime; _IncludeSeconds: boolean = true): string; overload;
function IsoDateTime(const _Date, _Time: TDateTime; _IncludeSeconds: boolean = true): string; overload;
function TryIsoDateToDate(const _IsoDate: string; out _Date: TDateTime): boolean;
function TryIsoTimeToTime(const _IsoTime: string; out _Time: TDateTime): boolean;

implementation

uses
  StrUtils;

function IsoDate(const _Date: TDateTime): string;
begin
  Result := FormatDateTime('yyyy-mm-dd', _Date);
end;

function IsoTime(const _Time: TDateTime; _IncludeSeconds: boolean = true): string;
begin
  if _IncludeSeconds then
    Result := FormatDateTime('hh:nn:ss', _Time)
  else
    Result := FormatDateTime('hh:nn', _Time);
end;

function IsoDateTime(const _DateTime: TDateTime; _IncludeSeconds: boolean = true): string; overload;
begin
  if _IncludeSeconds then
    Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', _DateTime)
  else
    Result := FormatDateTime('yyyy-mm-dd hh:nn', _DateTime);
end;

function IsoDateTime(const _Date, _Time: TDateTime; _IncludeSeconds: boolean = true): string; overload;
begin
  Result := IsoDate(_Date) + ' ' + IsoTime(_Time, _IncludeSeconds);
end;

function TryIsoDateToDate(const _IsoDate: string; out _Date: TDateTime): boolean;
var
  y, m, d: integer;
begin
  y := StrToIntDef(LeftStr(_IsoDate, 4), 0);
  m := StrToIntDef(Copy(_IsoDate, 6, 2), 0);
  d := StrToIntDef(Copy(_IsoDate, 9, 2), 0);
  Result := (y > 0) or (m > 0) or (d > 0);
  if Result then
    Result := TryEncodeDate(y, m, d, _Date);
end;

function TryIsoTimeToTime(const _IsoTime: string; out _Time: TDateTime): boolean;
var
  h, m, s: integer;
begin
  h := StrToIntDef(LeftStr(_IsoTime, 2), -1);
  m := StrToIntDef(Copy(_IsoTime, 4, 2), -1);
  If Length(_IsoTime) > 5 then
    s := StrToIntDef(Copy(_IsoTime, 7, 2), -1)
  else
    s := 0;
  Result := (h >= 0) and (m >= 0) and (s >= 0);
  if Result then
    Result := TryEncodeTime(h, m, s, 0, _Time);
end;

{ TAppointment }

constructor TAppointment.Create(const _Date, _Time: TDateTime; const _Desc: String; _AptType: TAptTypes = atOnce);
begin
  inherited Create;
  fDate := _Date;
  fTime := _Time;
  fDesc := _Desc;
  fAptType := _AptType;
  fKey := DateTimestr;
end;

function TAppointment.DateStr: string;
begin
  Result := IsoDate(fDate);
end;

function TAppointment.TimeStr: string;
begin
  if fTime = -1 then
    Result := ''
  else
    Result := IsoTime(fTime, false);
end;

function TAppointment.TimeOrStar: string;
begin
  if fTime = -1 then
    Result := '*'
  else
    Result := IsoTime(fTime, false);
end;

function TAppointment.DateTimeStr: string;
begin
  Result := DateTimestr(fDate, fTime);
end;

class function TAppointment.DateTimestr(const _Date, _Time: TDateTime): string;
begin
  if _Time = -1 then
    Result := IsoDate(_Date)
  else
    Result := IsoDateTime(_Date, _Time, false);
end;

class function TAppointment.DateTimestr(const _Date: TDateTime): string;
begin
  Result := DateTimeStr(_Date, -1);
end;

function TAppointment.TimeDesc: string;
begin
  if fTime = -1 then
    Result := ''
  else
    Result := TimeStr + ' ';
  Result := Result + fDesc;
end;

function TAppointment.Clone: TAppointment;
begin
  Result := TAppointment.Create(fDate, fTime, fDesc, fAptType);
end;

{ TAptColl }

{$include 't_dzTypedSortedCollection.tpl'}

constructor TAptColl.Create;
begin
  inherited Create;
  Duplicates := true;
end;

procedure TAptColl.AddAppointment(_Date, _Time: TDateTime; const _Desc: String);
begin
  Insert(TAppointment.Create(_Date, _Time, _Desc));
end;

procedure TAptColl.AddAppointment(_Date: TDateTime; const _Desc: String);
begin
  Insert(TAppointment.Create(_Date, -1, _Desc));
end;

function TAptColl.Compare(const _Key1, _Key2: string): integer;
begin
  Result := CompareText(_Key1, _Key2);
end;

function TAptColl.KeyOf(_Item: TAppointment): string;
begin
  Result := _Item.fKey;
end;

function TAptColl.SearchDate(_Date: TDateTime; out _Idx: integer): boolean;
begin
  Result := Search(TAppointment.DateTimestr(_Date), _Idx);
end;

procedure TAptColl.ReadFromFile(const _Filename: string);

 // format of a line:
 // <apttype>#9<date>#9<time>#9<description>
  procedure HandleLine(const _Line: string);
  var
    Start: integer;
    p: integer;
    s: string;
    Date: TDateTime;
    Time: TDateTime;
    AptType: integer;
  begin
    p := Pos(#9, _Line);
    if p = 0 then
      exit;
    s := LeftStr(_Line, p - 1);
    if not TryStrToInt(s, AptType) then
      exit;

    Start := p + 1;
    p := PosEx(#9, _Line, Start);
    if p = 0 then
      exit;
    s := Copy(_Line, Start, p - Start);
    if not TryIsoDateToDate(s, Date) then
      exit;

    Start := p + 1;
    p := PosEx(#9, _Line, Start);
    if p = 0 then
      exit;
    s := Copy(_Line, Start, p - Start);
    if s = '*' then
      Time := -1
    else if not TryIsoTimeToTime(s, Time) then
      exit;

    s := Copy(_Line, p + 1, Length(_Line) - p);
    Insert(TAppointment.Create(Date, Time, s, TAptTypes(AptType)));
  end;

var
  Line: string;
  t: TextFile;
begin
  AssignFile(t, _Filename);
  try
    Reset(t);
    try
      while not Eof(t) do begin
        ReadLn(t, Line);
        HandleLine(Line);
      end;
    finally
      CloseFile(t);
    end;
  except
    // ignore non-existing files
  end;
end;

// <apttype>#9<date>#9<time>#9<description>

procedure TAptColl.WriteToFile(const _Filename: string);
var
  i: integer;
  t: TextFile;
begin
  AssignFile(t, _Filename);
  Rewrite(t);
  try
    for i := 0 to count - 1 do begin
      with Items[i] do begin
        WriteLn(t, Format('%d'#9'%s'#9'%s'#9'%s', [Ord(AptType), DateStr, TimeOrStar, Desc]));
      end;
    end;
  finally
    CloseFile(t);
  end;
end;

procedure TAptColl.AddAptColl(_Source: TAptColl);
var
  i: integer;
begin
  for i := 0 to _Source.Count - 1 do begin
    Insert(_Source[i].Clone);
  end;
end;

function TAptColl.Clone: TAptColl;
begin
  Result := TAptColl.Create;
  Result.AddAptColl(self);
end;

end.


