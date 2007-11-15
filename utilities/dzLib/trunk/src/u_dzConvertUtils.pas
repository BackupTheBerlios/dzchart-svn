{GXFormatter.config=twm}
{: Integer to string and string to integer conversion functions for decimal
   hexadecimal and custom number bases. This was taken from u_dzStringUtils
   which originally was a Delphi conversion of TwmStringFunc. }
unit u_dzConvertUtils;

{$I jedi.inc}

interface

uses
  SysUtils;

var
  {: contains the User's format setting, but with decimal separator = '.' and no thousands separator }
  DZ_FORMAT_DECIMAL_POINT: TFormatSettings;

type
  {: Raised by the number conversion functions if a digit is invalid for the
     given base. }
  EdzConvert = class(Exception);
  EDigitOutOfRange = class(EdzConvert);
  {: raised if there is a conversion error in one of the Str2XxxEx functions }
  EStringConvertError = class(EdzConvert);

type
  ULong = LongWord;

type
  TBaseN = 2..36;

const
  MinInt64 = $8000000000000000;
  MaxInt64 = $7FFFFFFFFFFFFFFF;

const
  {: String containing all characters that can be used as digits }
  DIGIT_CHARS: string[36] = '0123456789ABCDEFGHIJKlMNOPQRSTUVWXYZ';

// Str <-> Decimal conversion
{: Returns true if A is a valid decimal digit }
function isDecDigit(_a: char): boolean;
{: Returns true if S is a valid decimal number }
function isDec(const _s: string): boolean;
{: Converts a decimal digit to its number equivalent
   Raises EDigitOutOfRange if there is an invalid digit. }
function DecDigit2Long(_a: char): LongInt;
{: Converts a string representing a decimal number to a number
   Raises EDigitOutOfRange if there is an invalid digit. }
function Dec2Long(const _s: string): ULong;
{: Converts a number to its 2 digit decimal representation (left pads with '0') }
function Long2Dec2(_l: ULong): string;
{: Converts a number to its 4 digit decimal representation (left pads with '0') }
function Long2Dec4(_l: ULong): string;
{: Converts a number to its N digit decimal representation (left pads with '0') }
function Long2DecN(_l: ULong; _n: ULong): string;
{: Converts a number to its decimal representation }
function Long2Dec(_l: ULong): string;

// Str <-> Hex conversion
{: Returns true if A is a valid hexadecimal (base 16) digit }
function isHexDigit(_a: char): boolean;
{: Returns true if S is a valid hexadecimal (base 16) number }
function isHex(const _s: string): boolean;
{: Converts a hexadecimal digit to its number equivalent
   Raises EDigitOutOfRange if there is an invalid digit. }
function HexDigit2Long(_a: char): LongInt;
{: Converts a string representing a hexadecimal number to a number
   Raises EDigitOutOfRange if there is an invalid digit. }
function Hex2Long(const _s: string): ULong;
{: Converts a number to its hexadecimal representation }
function Long2Hex(_l: ULong): string;
{: converts a number to its hexadecimal representation left padding with 0 to a length of 2 }
function Long2Hex2(_l: ULong): string;

// Str <-> any numeric system conversion
{: Returns true if A is a valid digit in the given Base. }
function isDigit(_a: char; _Base: TBaseN): boolean;
{: Returns true if S is a valid number in the given Base. }
function isNumber(const _s: string; _Base: TBaseN): boolean;
{: Converts a Base digit to its number equivalent.
   Raises EDigitOutOfRange if there is an invalid digit. }
function Digit2Long(_a: char; _Base: TBaseN): LongInt;
{: Converts a string representing a number in Base to a number.
   Raises EDigitOutOfRange if there is an invalid digit. }
function Num2Long(const _s: string; _Base: TBaseN): ULong;
{: Converts a number to its Base representation. }
function Long2Num(_l: ULong; _Base: byte): string;
{: Returns the number of characters in S that are valid digits in the given Base. }
function isNumberN(const _s: string; _Base: TBaseN): integer;

function TimeStrToSeconds(const _Zeit: string): integer;
function SecondsToStr(_Seconds: integer): string; deprecated;
function SecondsToTimeStr(_Seconds: integer): string;
{$IFDEF Delphi7up}
function TimeToSeconds(_Zeit: TDateTime): integer; deprecated;
{$ENDIF}

{: Converts a string to an integer.
   If s can not be converted, it returns the Default.
   @param(s string to convert)
   @param(Default value to return if s can not be converted)
   @returns(the integer value of s or Default, if s can not be converted) }

function Str2Int(_s: string; _Default: integer): integer; overload;

{: Converts a string to an integer.
   If s can not be converted, it raises an exception EStringConvertError.
   @param(s string to convert)
   @param(Source string to include in the exception message)
   @returns(the integer value of s)
   @raises(EStringConvertError if s can not be converted) }

function Str2Int(_s: string; const _Source: string): integer; overload;

{: Converts a string to an int64.
   If s can not be converted, it returns the Default.
   @param(s string to convert)
   @param(Default value to return if s can not be converted)
   @returns(the int64 value of s or Default, if s can not be converted) }

function Str2Int64(_s: string; _Default: Int64): Int64; overload;

{: Converts a string to an int64.
   If s can not be converted, it raises an exception EStringConvertError.
   @param(s string to convert)
   @param(Source string to include in the exception message)
   @returns(the integer value of s)
   @raises(EStringConvertError if s can not be converted) }

function Str2Int64(_s: string; const _Source: string): Int64; overload;

{: tries to guess the decimal separator }
function GuessDecimalSeparator(const _s: string): char;

{: Converts a string to a float.
   If s can not be converted, it returns the Default.
   @param s string to convert
   @param Default value to return if s can not be converted
   @param DecSeparator is the decimal separator, defaults to '.'
          if passed as #0, GuessDecimalSeparator is called to guess it
   @returns the float value of s or Default, if s can not be converted }
function Str2Float(_s: string; _Default: extended; _DecSeparator: char = '.'): extended; overload;

{: Converts a string to a float.
   If s can not be converted, it raises an exception EStringConvertError.
   @param(s string to convert)
   @param(Source string to include in the exception message)
   @param DecSeparator is the decimal separator, defaults to '.'
          if passed as #0, GuessDecimalSeparator is called to guess it
   @returns(the float value of s)
   @raises(EStringConvertError if s can not be converted) }

function Str2Float(_s: string; const _Source: string; _DecSeparator: char = '.'): extended; overload;

{: tries to convert a string to a float, returns false if it fails
   @param s is the string to convert
   @param flt is the float, only valid if the function returns true
   @param DecSeparator is the decimal separator to use, defaults to '.',
          if passed as #0, GuessDecimalSeparator is called to guess it
   @returns true, if s could be converted, false otherwise }
function TryStr2Float(_s: string; out _flt: extended; _DecSeparator: char = '.'): boolean; overload;
function TryStr2Float(_s: string; out _flt: double; _DecSeparator: char = '.'): boolean; overload;

{: Converts a floating point number to a string using the given decimal separator
   in "General number format" with 15 significant digits
   @param(flt is an extended floating point value)
   @param(DecSeparator is the decimal separator to use)
   @returns(a string representation of the floating point value) }
function Float2Str(_flt: extended; _DecSeparator: char = '.'): string; overload;

{: Converts a floating point number to a string using the given with, number of decimals
   and a '.' as decimal separator, if width is too small the smallest representation possible
   will be used (eg. Float2Str(5.24, 3, 2, '.') = '5.24')
   @param flt is an extended floating point value
   @param Width is the total number of digits (including the decimal separator
   @param Decimals is the number of decimals to use
   @returns a string representation of the floating point value }
function Float2Str(_flt: extended; _Width, _Decimals: integer): string; overload;

{: Converts a floating point number to a string using the given number of decimals
   and a '.' as decimal separator.
   @param flt is an extended floating point value
   @param Decimals is the number of decimals to use
   @returns a string representation of the floating point value }
function Float2Str(_flt: extended; _Decimals: integer): string; overload;

function TryRound(_flt: extended; out _wrd: word): boolean;

// these contants refer to the "Xx binary byte" units as defined by the
// International Electronical Commission (IEC) and endorsed by the
// IEE and CiPM
const
  OneKibiByte = Int64(1024);
  OneMebiByte = Int64(1024) * OneKibiByte;
  OneGibiByte = Int64(1024) * OneMebiByte;
  OneTebiByte = Int64(1024) * OneGibiByte;
  OnePebiByte = Int64(1024) * OneTebiByte;
  OneExbiByte = Int64(1024) * OnePebiByte;

{: Converts a file size to a human readable string, e.g. 536870912000 = 5.00 GiB (gibibyte) }
function FileSizeToHumanReadableString(_FileSize: Int64): string;

const
  SecondsPerMinute = 60;
  MinutesPerHour = 60;
  SecondsPerHour = SecondsPerMinute * MinutesPerHour;
  HoursPerDay = 24;
  MinutesPerDay = HoursPerDay * MinutesPerHour;
  SecondsPerDay = MinutesPerDay * SecondsPerMinute;

{: returns a human readable string of the form '5d 23h' or '25h 15m' or '20m 21s' }
function SecondsToHumanReadableString(_Seconds: Int64): string;

implementation

uses
  DateUtils,
  StrUtils,
  u_dzTranslator,
  u_dzStringUtils;

resourcestring
  // "%s" ist kein gültiger Fließkomma Wert: %s
  STR_X_IS_NOT_A_VALID_FLOAT_VALUE_SS = '"%s" is not a valid floating point value: %s';
  // "%s" ist kein gültiger %s Wert: %s
  STR_X_IS_NOT_A_VALID_Y_VALUE_SSS = '"%s" is not a valid %s value: %s';

function isDigit(_a: char; _Base: TBaseN): boolean;
begin
  Result := (Pos(UpCase(_a), LeftStr(DIGIT_CHARS, _Base)) <> 0);
end;

function isNumber(const _s: string; _Base: TBaseN): boolean;
var
  i: integer;
begin
  Result := False;
  if Length(_s) = 0 then
    Exit;
  for i := 1 to Length(_s) do
    if not isDigit(_s[i], _Base) then
      Exit;
  Result := True;
end;

function isNumberN(const _s: string; _Base: TBaseN): integer;
begin
  Result := 0;
  while (Result < Length(_s)) and isDigit(_s[Result + 1], _Base) do
    Inc(Result);
end;

function Digit2Long(_a: char; _Base: TBaseN): LongInt;
begin
  Result := Pos(UpCase(_a), LeftStr(DIGIT_CHARS, _Base));
  if Result = 0 then
    raise EDigitOutOfRange.CreateFmt(_('Digit out of range %s'), [_a]);
  Dec(Result);
end;

function Num2Long(const _s: string; _Base: TBaseN): ULong;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(_s) do
    if isDigit(_s[i], _Base) then
      Result := (Result * _Base + ULong(Pos(UpCase(_s[i]), DIGIT_CHARS)) - 1)
    else
      raise EDigitOutOfRange.CreateFmt(_('Digit #%d (%s) out of range'), [i, _s[i]]);
end;

function Long2Num(_l: ULong; _Base: byte): string;
var
  m: byte;
begin
  Result := '';
  while _l > 0 do begin
    m := _l mod _Base;
    _l := _l div _Base;
    Result := DIGIT_CHARS[m + 1] + Result;
  end;
  if Result = '' then
    Result := '0';
end;

function isHexDigit(_a: char): boolean;
begin
  Result := isDigit(_a, 16);
end;

function isHex(const _s: string): boolean;
begin
  Result := isNumber(_s, 16);
end;

function HexDigit2Long(_a: char): LongInt;
begin
  Result := Digit2Long(_a, 16);
end;

function Hex2Long(const _s: string): ULong;
begin
  Result := Num2Long(_s, 16);
end;

function Long2Hex(_l: ULong): string;
begin
  Result := Long2Num(_l, 16);
end;

function Long2Hex2(_l: ULong): string;
begin
  Result := Long2Hex(_l);
  if Length(Result) < 2 then
    Result := '0' + Result;
end;

function isDecDigit(_a: char): boolean;
begin
  Result := isDigit(_a, 10);
end;

function isDec(const _s: string): boolean;
begin
  Result := isNumber(_s, 10);
end;

function DecDigit2Long(_a: char): LongInt;
begin
  Result := Digit2Long(_a, 10);
end;

function Dec2Long(const _s: string): ULong;
var
  c: integer;
  l: LongInt;
begin
  Val(_s, l, c);
  Result := l
end;

function Long2Dec(_l: ULong): string;
begin
  Str(_l, Result);
end;

function Long2Dec2(_l: ULong): string;
begin
  Result := Long2DecN(_l, 2);
end;

function Long2Dec4(_l: ULong): string;
begin
  Result := Long2DecN(_l, 4);
end;

function Long2DecN(_l: ULong; _n: ULong): string;
begin
  Result := Long2Dec(_l);
  if ULong(Length(Result)) < _n then
{$IFDEF delphi7up}
    Insert(DupeString('0', _n - ULong(Length(Result))), Result, 1);
{$ELSE}
    Insert(StringOf('0', _n - ULong(Length(Result))), Result, 1);
{$ENDIF}
end;

{$IFDEF Delphi7up}

function TimeToSeconds(_Zeit: TDateTime): integer;
begin
  Result := SecondOfTheDay(_Zeit);
end;
{$ENDIF}

function SecondsToTimeStr(_Seconds: integer): string;
begin
  if _Seconds < 0 then begin
    Result := '-';
    _Seconds := -_Seconds;
  end else
    Result := '';
  Result := Result + Format('%.2d:%.2d:%.2d', [_Seconds div 3600, (_Seconds div 60) mod 60, _Seconds mod 60]);
end;

function SecondsToStr(_Seconds: integer): string;
begin
  Result := SecondsToTimeStr(_Seconds);
end;

function TimeStrToSeconds(const _Zeit: string): integer;
var
  Zeit: string;
  s: string;
  Len: integer;
  Sign: integer;
begin
  Len := Length(_Zeit);
  if Len = 0 then begin
    Result := 0;
    exit;
  end;

  Sign := 1;
  case _Zeit[1] of
    '-': begin
        Zeit := TailStr(_Zeit, 2);
        Sign := -1;
      end;
    '+', ' ':
      Zeit := TailStr(_Zeit, 2);
  else
    Zeit := _Zeit;
  end;

  s := ExtractFirstWord(Zeit, [':']);
  if s = '' then
    Result := 0
  else
    Result := StrToInt(s);

  s := ExtractFirstWord(Zeit, [':']);
  if s <> '' then
    Result := Result * 60 + StrToInt(s);

  s := ExtractFirstWord(Zeit, [':']);
  if s <> '' then
    Result := Result * 60 + StrToInt(s);

  Result := Result * Sign;
end;

function Float2Str(_flt: extended; _DecSeparator: char = '.'): string;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings := DZ_FORMAT_DECIMAL_POINT;
  FormatSettings.DecimalSeparator := _DecSeparator;
  Result := SysUtils.FloatToStr(_Flt, FormatSettings);
end;

function Float2Str(_flt: extended; _Width, _Decimals: integer): string;
begin
  Str(_flt: _Width: _Decimals, Result);
end;

function Float2Str(_flt: extended; _Decimals: integer): string;
begin
  Str(_Flt: 0: _Decimals, Result);
end;

function TryRound(_flt: extended; out _wrd: word): boolean;
begin
  Result := (_flt >= 0) and (_flt <= $FFFF);
  if Result then
    try
      _wrd := Round(_flt);
    except
      Result := false;
    end;
end;

function Str2Int(_s: string; _Default: integer): integer;
var
  e: integer;
begin
  Val(_s, Result, e);
  if e <> 0 then
    Result := _Default
end;

function Str2Int(_s: string; const _Source: string): integer;
var
  e: integer;
begin
  Val(_s, Result, e);
  if e <> 0 then
    raise EStringConvertError.CreateFmt(STR_X_IS_NOT_A_VALID_Y_VALUE_SSS, [_s, 'Integer', _Source]);
end;

function Str2Int64(_s: string; _Default: Int64): Int64;
var
  e: integer;
begin
  Val(_s, Result, e);
  if e <> 0 then
    Result := _Default
end;

function Str2Int64(_s: string; const _Source: string): Int64;
var
  e: integer;
begin
  Val(_s, Result, e);
  if e <> 0 then
    raise EStringConvertError.CreateFmt(STR_X_IS_NOT_A_VALID_Y_VALUE_SSS, [_s, 'Int64', _Source]);
end;

function GuessDecimalSeparator(const _s: string): char;
var
  i: integer;
  //  DotCnt: integer;
  CommaCnt: integer;
begin
  //  DotCnt := 0;
  CommaCnt := 0;
  Result := '.';
  for i := 1 to length(_s) do begin
    case _s[i] of
      '.': begin
            //            Inc(DotCnt);
          Result := '.';
        end;
      ',': begin
          Inc(CommaCnt);
          Result := ',';
        end;
    end;
  end;
  if (Result = ',') and (CommaCnt = 1) then
    exit;
  Result := '.';
end;

function TryStr2Float(_s: string; out _flt: extended; _DecSeparator: char = '.'): boolean;
var
  TmpDecSeparator: char;
begin
  if _DecSeparator = #0 then
    _DecSeparator := GuessDecimalSeparator(_s);
  TmpDecSeparator := DecimalSeparator;
  DecimalSeparator := _DecSeparator;
  try
    Result := TextToFloat(PChar(_s), _flt, fvExtended);
  finally
    DecimalSeparator := TmpDecSeparator;
  end;
end;

function TryStr2Float(_s: string; out _flt: double; _DecSeparator: char = '.'): boolean; overload;
var
  flt: extended;
begin
  Result := TryStr2Float(_s, flt, _DecSeparator);
  if Result then
    _flt := flt;
end;

function Str2Float(_s: string; _Default: extended; _DecSeparator: char = '.'): extended; overload;
begin
  if not TryStr2Float(_s, Result, _DecSeparator) then
    Result := _Default
end;

function Str2Float(_s: string; const _Source: string; _DecSeparator: char = '.'): extended; overload;
begin
  if not TryStr2Float(_s, Result, _DecSeparator) then
    raise EStringConvertError.CreateFmt(STR_X_IS_NOT_A_VALID_FLOAT_VALUE_SS, [_s, _Source]);
end;

function FileSizeToHumanReadableString(_FileSize: Int64): string;
begin
  if _FileSize > 5 * OneExbiByte then
    Result := Format(_('%.2f EiB'), [_FileSize / OneExbiByte])
  else if _FileSize > 5 * OnePebiByte then
    Result := Format(_('%.2f PiB'), [_FileSize / OnePebiByte])
  else if _FileSize > 5 * OneTebiByte then
    Result := Format(_('%.2f TiB'), [_FileSize / OneTebiByte])
  else if _FileSize > 5 * OneGibiByte then
    Result := Format(_('%.2f GiB'), [_FileSize / OneGibiByte])
  else if _FileSize > 5 * OneMebiByte then
    Result := Format(_('%.2f MiB'), [_FileSize / OneMebiByte])
  else if _FileSize > 5 * OneKibiByte then
    Result := Format(_('%.2f KiB'), [_FileSize / OneKibiByte])
  else
    Result := Format(_('%d Bytes'), [_FileSize]);
end;

function SecondsToHumanReadableString(_Seconds: Int64): string;
begin
  if _Seconds > SecondsPerDay then
    Result := Format('%dd %dh', [_Seconds div SecondsPerDay, (_Seconds div SecondsPerHour) mod HoursPerDay])
  else if _Seconds > Round(1.5 * SecondsPerHour) then
    Result := Format('%dh %dm', [_Seconds div SecondsPerHour, (_Seconds div SecondsPerMinute) mod MinutesPerHour])
  else if _Seconds > Round(1.5 * SecondsPerMinute) then
    Result := Format('%dm %ds', [_Seconds div SecondsPerMinute, _Seconds mod SecondsPerMinute])
  else
    Result := Format('%ds', [_Seconds]);
end;

initialization
  DZ_FORMAT_DECIMAL_POINT := GetUserDefaultLocaleSettings;
  DZ_FORMAT_DECIMAL_POINT.DecimalSeparator := '.';
  DZ_FORMAT_DECIMAL_POINT.ThousandSeparator := #0;
end.

