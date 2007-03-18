{GXFormatter.config=twm}
unit u_dzDateUtils;

{$i jedi.inc}

interface

function DateTime2Iso(_dt: TDateTime; _IncludeTime: boolean = false): string;
function Iso2Time(_s: string): TDateTime;
function Iso2Date(_s: string): TDateTime;

implementation

uses
  SysUtils,
  u_dzStringUtils;

function DateTime2Iso(_dt: TDateTime; _IncludeTime: boolean = false): string;
begin
  if _IncludeTime then
    DateTimeToString(Result, 'yyyy-mm-dd hh:nn:ss', _dt)
  else
    DateTimeToString(Result, 'yyyy-mm-dd', _dt);
end;

function Iso2Time(_s: string): TDateTime;
var
  s: string;
  h, m, sec: integer;
begin
  s := ExtractFirstWord(_s, ':');
  h := StrToInt(s);
  s := ExtractFirstWord(_s, ':');
  m := StrToInt(s);
  if _s = '' then
    sec := 0
  else
    sec := StrToInt(_s);
  Result := EncodeTime(h, m, sec, 0);
end;

function Iso2Date(_s: string): TDateTime;
var
  s: string;
  y, m, d: integer;
begin
  s := ExtractFirstWord(_s, '-');
  y := StrToInt(s);
  s := ExtractFirstWord(_s, '-');
  m := StrToInt(s);
  d := StrToInt(_s);
  Result := EncodeDate(y, m, d);
end;

function Iso2DateTime(_s: string): TDateTime;
var
  Date: string;
begin
  Date := ExtractFirstWord(_s, ' ');
  Result := Iso2Date(Date) + Iso2Time(_s)
end;

end.
