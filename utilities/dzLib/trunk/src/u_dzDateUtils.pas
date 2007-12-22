{GXFormatter.config=twm}
/// implements some utility functions for converting TDateTime to and from strings
/// in ISO 6801 format (note that these function do not implement the complete
/// standard but only the extended form without omitting date parts.
unit u_dzDateUtils;

{$i jedi.inc}

interface

/// <summary>
/// Converts a TDateTime value to a string in ISO 8601 format
/// @param dt is the TDateTime value to convert
/// @param IncludeTime is a boolean that determines whether the time should be
///                    included, defaults to false
/// @returns a string with the date (and optionally the time) in the format
///          'yyyy-mm-dd hh:mm:ss'
/// </summary>
function DateTime2Iso(_dt: TDateTime; _IncludeTime: boolean = false): string; inline;

/// <summary>
/// converts a string that contains a time in ISO 8601 format to a TDateTime value
/// @param s is the string to convert, it must be in the form 'hh:mm:ss' or 'hh:mm'
/// @returns a TDateTime value with the time
/// </summary>
function Iso2Time(_s: string): TDateTime;

/// <summary>
/// converts a string that contains a date in ISO 8601 format to a TDateTime value
/// @param s is the string to convert, it must be in the form 'yyyy-mm-dd', it must
///          not contain a time
/// @returns a TDateTime value with the date
/// </summary>
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
