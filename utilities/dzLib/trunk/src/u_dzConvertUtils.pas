{GXFormatter.config=twm}
{: Integer to string and string to integer conversion functions for decimal
   hexadecimal and custom number bases. This was taken from u_dzStringUtils
   which originally was a Delphi conversion of TwmStringFunc. }
unit u_dzConvertUtils;

{$i jedi.inc}

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

type
  ULong = LongWord;

type
  TBaseN = 2..36;

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

{: Converts a floating point number to a string using the given decimal separator
   in "General number format" with 15 significant digits
   @param(flt is an extended floating point value)
   @param(DecSeparator is the decimal separator to use)
   @returns(a string representation of the floating point value) }
function Float2Str(_flt: extended; _DecSeparator: char = '.'): string;

implementation

uses
  DateUtils,
  StrUtils,
  u_dzTranslator,
  u_dzStringUtils;

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

initialization
  DZ_FORMAT_DECIMAL_POINT := GetUserDefaultLocaleSettings;
  DZ_FORMAT_DECIMAL_POINT.DecimalSeparator := '.';
  DZ_FORMAT_DECIMAL_POINT.ThousandSeparator := #0;
end.

