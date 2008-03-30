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
function Time2Iso(_dt: TDateTime; _IncludeSeconds: boolean = true): string; inline;
/// <summary>
/// converts a string that contains a time in ISO 8601 format to a TDateTime value
/// @param s is the string to convert, it must be in the form 'hh:mm:ss' or 'hh:mm'
/// @returns a TDateTime value with the time
/// </summary>
function Iso2Time(_s: string): TDateTime;
function TryIso2Time(_s: string; out _Time: TDateTime): boolean;
/// <summary>
/// converts a string that contains a date in ISO 8601 format to a TDateTime value
/// @param s is the string to convert, it must be in the form 'yyyy-mm-dd', it must
///          not contain a time
/// @returns a TDateTime value with the date
/// </summary>
function Iso2Date(const _s: string): TDateTime;
function TryIso2Date(const _s: string; out _Date: TDateTime): boolean;

/// <summary>
/// converts a string that contains a date and time in ISO 8601 format to a TDateTime value
/// @param s is the string to convert, it must be in the form 'yyyy-mm-dd hh:mm[:ss]'
/// @returns a TDateTime value with the date
/// </summary>
function Iso2DateTime(const _s: string): TDateTime;
function TryIso2DateTime(const _s: string; out _DateTime: TDateTime): boolean;

function Date2ddmmyyyy(_Date: TDateTime): string;
function ddmmyyyy2Date(const _s: string): TDateTime;
function Tryddmmyyyy2Date(const _s: string; out _Date: TDateTime): boolean;

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

function Date2ddmmyyyy(_Date: TDateTime): string;
begin
  DateTimeToString(Result, 'dd.mm.yyyy', _Date);
end;

function Tryddmmyyyy2Date(const _s: string; out _Date: TDateTime): boolean;
var
  Settings: TFormatSettings;
begin
  Settings := GetUserDefaultLocaleSettings;
  Settings.DateSeparator := '.';
  Settings.ShortDateFormat := 'dd.mm.yyyy';
  Result := TryStrToDate(_s, _Date, Settings); 
end;

function ddmmyyyy2Date(const _s: string): TDateTime;
var
  Settings: TFormatSettings;
begin
  Settings := GetUserDefaultLocaleSettings;
  Settings.DateSeparator := '.';
  Settings.ShortDateFormat := 'dd.mm.yyyy';
  Result := StrToDate(_s, Settings);
end;

function Time2Iso(_dt: TDateTime; _IncludeSeconds: boolean = true): string;
var
  fmt: string;
begin
  fmt := 'hh:nn';
  if _IncludeSeconds then
    fmt := fmt + ':ss';
  DateTimeToString(Result, fmt, _dt);
end;

function TryIso2Time(_s: string; out _Time: TDateTime): boolean;
var
  Settings: TFormatSettings;
begin
  Settings := GetUserDefaultLocaleSettings;
  Settings.TimeSeparator := ':';
  Settings.ShortTimeFormat := 'hh:nn:ss';
  Result := TryStrToTime(_s, _Time, Settings);
end;

function Iso2Time(_s: string): TDateTime;
var
  Settings: TFormatSettings;
begin
  Settings := GetUserDefaultLocaleSettings;
  Settings.TimeSeparator := ':';
  Settings.ShortTimeFormat := 'hh:nn:ss';
  Result := StrToTime(_s, Settings);
end;

function TryIso2Date(const _s: string; out _Date: TDateTime): boolean;
var
  Settings: TFormatSettings;
begin
  Settings := GetUserDefaultLocaleSettings;
  Settings.DateSeparator := '-';
  Settings.ShortDateFormat := 'yyyy-mm-dd';
  Result := TryStrToDate(_s, _Date, Settings);
end;

function Iso2Date(const _s: string): TDateTime;
var
  Settings: TFormatSettings;
begin
  Settings := GetUserDefaultLocaleSettings;
  Settings.DateSeparator := '-';
  Settings.ShortDateFormat := 'yyyy-mm-dd';
  Result := StrToDate(_s, Settings);
end;

function TryIso2DateTime(const _s: string; out _DateTime: TDateTime): boolean;
var
  Settings: TFormatSettings;
begin
  Settings := GetUserDefaultLocaleSettings;
  Settings.DateSeparator := '-';
  Settings.ShortDateFormat := 'yyyy-mm-dd';
  Settings.TimeSeparator := ':';
  Settings.ShortTimeFormat := 'hh:nn:ss';
  Result := TryStrToDateTime(_s, _DateTime, Settings);
end;

function Iso2DateTime(const _s: string): TDateTime;
var
  Settings: TFormatSettings;
begin
  Settings := GetUserDefaultLocaleSettings;
  Settings.DateSeparator := '-';
  Settings.ShortDateFormat := 'yyyy-mm-dd';
  Settings.TimeSeparator := ':';
  Settings.ShortTimeFormat := 'hh:nn:ss';
  Result := StrToDateTime(_s, Settings);
end;

end.
