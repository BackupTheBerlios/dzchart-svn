{GXFormatter.config=twm}
{: Implements a bunch of commonly used string functions. This is a copy of
   TwmString adapted to Delphi.
   @author   twm }
unit u_dzStringUtils;

{$i jedi.inc}

interface

uses
  Windows,
  Classes,
  StrUtils,
  SysUtils;

type
  {: Ancestor of all exceptions raised in u_dzStringUtils }
  EStringfunc = class(Exception);
  {: Raised by NthWord if the word index is out of range (1..word count)}
  EWordIndexOutOfRange = class(EStringfunc);
  {: Raised by CenterStr if the given string ist too long. }
  EStringTooLong = class(EStringfunc);
  {: Raised by NthCharOf if passed a char index of 0. }
  ECharIndexOutOfRange = class(EStringfunc);

type
  {: Do I really need to comment this? }
  TCharSet = set of char;

const
  {: Characters that are usually used as word delimiters.
     This can be passed when a function takes a TCharSet of delimiters.
     warning: These might not be complete. }
  STANDARD_DELIMITERS = [chr(0)..chr(255)] - ['a'..'z', 'A'..'Z',
    '0'..'9', 'ö', 'Ö', 'ä', 'Ä', 'ü', 'Ü', 'ß'];
  STANDARD_CONTROL_CHARS = [#0..' '];

function GetDelStr(var _Zeile: string; _Del: char): string;

{: extracts a substring from the start of Source up to the Delimiter, returns
   true, if a substring (even an empty one) was found. }
function ExtractStr(var _Source: string; _Delimiters: TCharSet; out _Substr: string): boolean; overload;
function ExtractStr(var _Source: string; _Delimiter: char; out _Substr: string): boolean; overload;
function ExtractStr(var _Source: string; _Delimiters: TCharSet; out _Substr: string; var _LastWasDelimiter: boolean): boolean; overload;
function ExtractStr(var _Source: string; _Delimiter: char; out _Substr: string; var _LastWasDelimiter: boolean): boolean; overload;

{: Converts A to lower case. }
function LoCase(_c: char): char;
// function UpStr(const _s: string): string; // use SysUtils.(Ansi)UpperCase instead
// function LoStr(const _s: string): string; // use SysUtils.(Ansi)LowerCase instead

{: Entfernt im Gegensatz zu Sysutils.Trim nur Spaces und keine Sonderzeichen }
function TrimSpaces(const _s: string): string;
{: Entfernt im Gegensatz zu SysUtils.TrimRight nur Spaces, keine Sonderzeichen }
function RTrimSpaces(const _s: string): string;
{: Entfernt im Gegensatz zu SysUtils.TrimLeft nur Spaces, keine Sonderzeichen }
function LTrimSpaces(const _s: string): string;

{: Creates a string with Anz spaces. }
function SpaceStr(_Anz: integer): string;

function StringOf(_c: char; _Anz: integer): string;
{$IFDEF Delphi7up}
deprecated; // use StrUtils.DupeString
{$ENDIF}

{: Prepend a backslash to the string if there isn't one already. }
function PrependBackslash(const _s: string): string;
{$IFDEF Delphi7up}
type
  TXxxBackslash = function(const _s: string): string;
const
  AddBackslash: TXxxBackslash = IncludeTrailingPathDelimiter deprecated;
  StripBackslash: TXxxBackslash = ExcludeTrailingPathDelimiter deprecated;
{$ELSE}
{: Append a backslash to the string if there isn't one already. }
function AddBackslash(const _s: string): string;
{: Remove a trailing backslash if there is one. Note that this does also
   remove a trailing backlash from the root directory ('c:\') which might
   not be what you want. }
function StripBackslash(const _s: string): string;
{$ENDIF}

{: Replaces an existing extension in Name with Ext or adds Ext to Name if
   it does not have an extension. }
function ForceExtension(const _Name, _Ext: string): string;
{: Returns only the filename (incl. extension) portion of Name. }
function JustFilename(const _Name: string): string;
{: removes an extension if it matches the given one
   @param Filename is the input filename
   @param Extension is the extension to remove, if the file has it (comparison is case insensitive)
   @returns the filename without the extension, if it matched, the unchanged filename otherwise }
function RemoveFileExtIfMatching(const _Filename: string; const _Extension: string): string;

{: Appends spaces to the string S to bring it to the given length. If S is
   too long it is truncated, thus the result is guaranteed to be Len characters
   long. }
function RPadStr(const _s: string; _Len: integer): string;
{: Prepends spaces to the string S to bring it to the given length. If S is
   too long it is truncated, thus the result is guaranteed to be Len characters
   long. }
function LPadStr(const _s: string; _Len: integer): string;

type
  TTrimStr = function(const _s: string): string;
const
  RTrimStr: TTrimStr = RTrimSpaces deprecated; // or possibily SysUtils.TrimRight
  LTrimStr: TTrimStr = LTrimSpaces deprecated; // or possibly SysUtils.TrimLeft
  TrimStr: TTrimStr = TrimSpaces deprecated; // or possibly SysUtils.Trim

  {: Returns true, if SubStr is found in Str and sets Head to the text before
     and Tail to the text after SubStr. Otherwise Head and Tail are undefined. }
function FindString(const _Substr, _Str: string; var _Head, _Tail: string): boolean;

{: Returns the rightmost position of Sub in S or 0 if Sub is not found in S. }
function RPosStr(_Sub, _s: string): integer;

{: Converts a PChar to as Pascal string. }
function Str2Pas(_p: PChar): string;
{: Allocates a new PChar and sets it to the contents of S, the length is set
   exactly to the length of S. }
function StrPNew(_s: string): PChar;
{: Returns a pointer to a temporary string containing S. Warning: This uses a
   global constant for ShortStrings respectively just typecasts AnsiStrings
   to PChar. Use with care! }
function Pas2Str(var _s: string): PChar;

{: Reads a line from the file F and returns it as PChar. P is allocated to
   the correct length. }
procedure StrReadLn(var _f: file; _p: PChar);
{: Reads a 0-terminated string from file F and returns it as PChar. P is
   allocated to the correct length. }
procedure StrReadZ(var _f: file; _p: PChar);

{: Returns true if Searched starts with the string Match. }
function MatchStr(const _Searched, _Match: string): boolean;
{: Returns true if Searched starts with the string Match ignoring case. }
function UMatchStr(const _Searched, _Match: string): boolean;

{: Creates a string of the form '...end of the string' with a maximum length. }
function LDotDotStr(const _s: string; _MaxLen: integer): string;
{: Creates a string of the form 'Start of string...' with a maximum length. }
function RDotDotStr(const _s: string; _MaxLen: integer): string;
{: Centers the given string, that is right and left pads it with spaces to
   MaxLenght characters. }
function CenterStr(const _s: string; _MaxLen: integer): string;

function TailStr(const _s: string; _Start: integer): string;
{: cuts off the rightmost part of a string }
function StrCutRight(const _s: string; _CutLen: integer): string;

{: Returns part of S left of C (or all of it if it does not contain C) }
function LeftStrOf(const _s: string; _c: char): string;
{: Returns part of S right of the last C (or all of it if it does not contain C) }
function RightStrOf(const _s: string; _c: char): string;
{: Returns part of S right of the last C (or all of it if it does not contain C) }
function TailStrOf(const _s: string; _c: char): string;

{: Returns the next postion of SubStr in S starting from Start. Start is
   1 based. Returns 0 if SubStr was not found.
   Note: Function is deprecated, use StrUtils.PosEx instead! }
function PosStr(const _SubStr, _s: string; _Start: integer): integer; deprecated;

{: Replaces all occurences of characters in Search in S with the corresponding
   character in Replace. }
function ReplaceChars(const _s, _Search, _Replace: string): string;

{: Replaces all control characters (ord(c) < ord(' ')) with ReplaceChar.
   If RemoveDuplicates is true, a sequence of control characters is replaced
   by a single ReplaceChar. }
function ReplaceCtrlChars(const _s: string; _ReplaceChar: char; _RemoveDuplicates: boolean = true): string;

{: Replaces all control characters (ord(c) < ord(' ')) with Spaces.
   If RemoveDuplicates is true, a sequence of control characters is replaced
   by a single space. }
function CtrlcharsToSpace(const _s: string; _RemoveDuplicates: boolean = true): string;

{: Replaces all control characters (ord(c) <= ord(' '), " and ') with Prefix<hexcode> }
function HexEncodeControlChars(_Prefix: char; const _s: string; _ControlChars: TCharSet = STANDARD_CONTROL_CHARS): string;
function HexDecodeControlChars(const _Prefix: char; const _s: string): string;

{: Replaces all control characters (ord(c) <= ord(' '), " and ') with %<hexcode> }
function UrlEncodeControlChars(const _s: string; _ControlChars: TCharSet = STANDARD_CONTROL_CHARS): string;
function UrlDecodeControlChars(const _s: string): string;

{: Returns the WordNo'th word, (counting from 1), using the given Delimiters.
   NOTE: duplicate delimiters are ignored, so 'abc  def' will be split
   into two words (which you would expect), but also 'abc'#9#9'def' is two words
   (which you might not expect) }
function nthWord(const _s: string; _WordNo: integer; const _Delimiter: string): string; overload;
{: Returns the WordNo'th word, (counting from 1), using the given Delimiters.
   NOTE: duplicate delimiters are ignored, so 'abc  def' will be split
   into two words (which you would expect), but also 'abc'#9#9'def' is two words
   (which you might not expect) }
function nthWord(const _s: string; _WordNo: integer; _Delimiter: TCharSet): string; overload;

{: Returns the Nth character of S or ' ' if S has less than N charaters. }
function nthCharOf(const _s: string; _n: integer): Char;

{: Extract the first word of S using the given delimiters. The word is deleted
   from S.
   NOTE: duplicate delimiters are ignored, so 'abc  def' will be split
   into two words (which you would expect), but also 'abc'#9#9'def' is two words
   (which you might not expect) }
function ExtractFirstWord(var _s: string; const _Delimiter: string): string; overload;
{: Extract the first word of S using the given delimiters. The word is deleted
   from S.
   NOTE: duplicate delimiters are ignored, so 'abc  def' will be split
   into two words (which you would expect), but also 'abc'#9#9'def' is two words
   (which you might not expect) }
function ExtractFirstWord(var _s: string; _Delimiter: TCharSet): string; overload;
{: Extract the first word of S using the given delimiters. The word is deleted
   from S.
   NOTE: duplicate delimiters are ignored, so 'abc  def' will be split
   into two words (which you would expect), but also 'abc'#9#9'def' is two words
   (which you might not expect)
   @returns true, if a word could be extracted, false otherwise }
function ExtractFirstWord(var _s: string; const _Delimiter: string; out _FirstWord: string): boolean; overload;
{: Extract the first word of S using the given delimiters. The word is deleted
   from S.
   NOTE: duplicate delimiters are ignored, so 'abc  def' will be split
   into two words (which you would expect), but also 'abc'#9#9'def' is two words
   (which you might not expect)
   @returns true, if a word could be extracted, false otherwise }
function ExtractFirstWord(var _s: string; _Delimiter: TCharSet; out _FirstWord: string): boolean; overload;

{: Split string s into the list of substrings delimited by delimter
   NOTE: duplicate delimiters are ignored, so 'abc  def' will be split
   into two words (which you would expect), but also 'abc'#9#9'def' is two words
   (which you might not expect)
   @param sl is the stringlist in which to return the result
   @param s is the string to split
   @param Delimiter is a string containing all delimiter characters
   @returns the sl parameter }
function SplitString(_sl: TStrings; _s: string; const _Delimiter: string): TStrings;

{: Converts Tab characters into SpcCount spaces }
function Tab2Spaces(const _s: string; _SpcCount: integer): string;
function StartsWith(const _Start, _s: string): boolean;
function EndsWith(const _End, _s: string): boolean;

function UStartsWith(const _Start, _s: string): boolean;
function UEndsWith(const _End, _s: string): boolean;

function UnquoteString(const _s: string; _Quote: char = '"'): string;

{: returns the string, if it isn't NIL or 'NULL' if it is. }
function StringOrNull(_P: PChar): string;

{: returns the default locale settings as read from the system's regional settings }
function GetUserDefaultLocaleSettings: TFormatSettings;
function GetSystemDefaultLocaleSettings: TFormatSettings;

implementation

uses
  u_dzConvertUtils;

resourcestring
  // nthWord: Nulltes Wort nicht verfügbar
  STR_NO_0TH_WORD = 'nthWord: 0th word not available';

  // Zu zentrierender String ist länger als die Maximallänge und kann nicht zentriert werden.
  STR_STRING_TOO_LONG_TO_CENTER = 'Cannot center string that is larger than the maximum length.';

  // Strings haben kein nulltes Zeichen.
  STR_CHAR_INDEX_OUT_OF_RANGE = 'Strings do not have a 0th character.';

function ForceExtension(const _Name, _Ext: string): string;
var
  p: integer;
begin
  p := RPosStr('.', _Name);
  if p = 0 then
    Result := _Name + '.' + _Ext
  else
    Result := LeftStr(_Name, p) + _Ext;
end;

function JustFilename(const _Name: string): string;
var
  p: integer;
begin
  p := RPosStr('\', _Name);
  if p = 0 then
    result := _Name
  else
    result := TailStr(_Name, p + 1);
end;

function RemoveFileExtIfMatching(const _Filename: string; const _Extension: string): string;
begin
  if UEndsWith(_Extension, _Filename) then
    Result := LeftStr(_Filename, Length(_Filename) - Length(_Extension))
  else
    Result := _Filename;
end;

function nthWordStartAndEnd(const _s: string; _WordNo: integer;
  const _Delimiter: TCharSet; var _Start, _Ende: integer): boolean; overload;
var
  i: integer;
begin
  if _WordNo = 0 then
    raise EWordIndexOutOfRange.Create(STR_NO_0TH_WORD);
  _Start := 0;
  _Ende := 0;
  i := 1;
  while i <= Length(_s) do begin
    while (i <= Length(_s)) and (NthCharOf(_s, i) in _Delimiter) do
      Inc(i);
    Dec(_WordNo);
    if _WordNo = 0 then
      _Start := i;
    while (i <= Length(_s)) and not (NthCharOf(_s, i) in _Delimiter) do
      Inc(i);
    if _WordNo = 0 then begin
      _Ende := i;
      Break;
    end;
  end;
  Result := (_Start <> 0) and (_Ende <> 0);
end;

function nthWordStartAndEnd(const _s: string; _WordNo: integer;
  const _Delimiter: string; var _Start, _Ende: integer): boolean; overload;
var
  i: integer;
  DelimiterSet: TCharSet;
begin
  DelimiterSet := [];
  for i := 1 to Length(_Delimiter) do
    Include(DelimiterSet, _Delimiter[i]);
  Result := nthWordStartAndEnd(_s, _WordNo, DelimiterSet, _Start, _Ende);
end;

function nthWord(const _s: string; _WordNo: integer; const _Delimiter: string): string;
var
  Start, Ende: integer;
begin
  if nthWordStartAndEnd(_s, _WordNo, _Delimiter, Start, Ende) then
    Result := Copy(_s, Start, Ende - Start)
  else
    Result := '';
end;

function nthWord(const _s: string; _WordNo: integer; _Delimiter: TCharSet): string;
var
  Start, Ende: integer;
begin
  if nthWordStartAndEnd(_s, _WordNo, _Delimiter, Start, Ende) then
    Result := Copy(_s, Start, Ende - Start)
  else
    Result := '';
end;

function ExtractFirstWord(var _s: string; _Delimiter: TCharSet): string; overload;
begin
  if not ExtractFirstWord(_s, _Delimiter, Result) then begin // s contained only Delimiters
    Result := '';
    _s := '';
  end;
end;

function ExtractFirstWord(var _s: string; const _Delimiter: string): string;
begin
  if not ExtractFirstWord(_s, _Delimiter, Result) then begin // s contained only Delimiters
    Result := '';
    _s := '';
  end;
end;

function ExtractFirstWord(var _s: string; const _Delimiter: string; out _FirstWord: string): boolean; overload;
var
  Start, Ende: integer;
begin
  Result := nthWordStartAndEnd(_s, 1, _Delimiter, Start, Ende);
  if Result then begin
    _FirstWord := Copy(_s, Start, Ende - Start);
    _s := TailStr(_s, Ende + 1);
  end;
end;

function ExtractFirstWord(var _s: string; _Delimiter: TCharSet; out _FirstWord: string): boolean; overload;
var
  Start, Ende: integer;
begin
  Result := nthWordStartAndEnd(_s, 1, _Delimiter, Start, Ende);
  if Result then begin
    _FirstWord := Copy(_s, Start, Ende - Start);
    _s := TailStr(_s, Ende + 1);
  end;
end;

function SplitString(_sl: TStrings; _s: string; const _Delimiter: string): TStrings;
var
  s: string;
begin
  Result := _sl;
  while _s <> '' do begin
    s := ExtractFirstWord(_s, _Delimiter);
    Result.Add(s);
  end;
end;

function ReplaceChars(const _s, _Search, _Replace: string): string;
var
  i, j: LongInt;
  p: integer;
begin
  SetLength(Result, Length(_s));
  j := 1;
  for i := 1 to Length(_s) do begin
    p := Pos(_s[i], _Search);
    if p <> 0 then begin
      if Length(_Replace) >= p then begin
        Result[j] := _Replace[p];
        Inc(j);
      end
    end else begin
      Result[j] := _s[i];
      Inc(j);
    end;
  end;
  SetLength(Result, j - 1);
end;

function ReplaceCtrlChars(const _s: string; _ReplaceChar: char; _RemoveDuplicates: boolean = true): string;
var
  i: integer;
  Dup: boolean;
begin
  Result := _s;
  Dup := false;
  for i := Length(Result) downto 1 do
    if Ord(Result[i]) <= Ord(' ') then begin
      if not Dup or not _RemoveDuplicates then begin
        Result[i] := _ReplaceChar;
        Dup := true;
      end else
        Delete(Result, i, 1);
    end else
      Dup := false;
end;

function CtrlcharsToSpace(const _s: string; _RemoveDuplicates: boolean = true): string;
begin
  Result := ReplaceCtrlChars(_s, ' ', _RemoveDuplicates);
end;

function HexEncodeControlChars(_Prefix: char; const _s: string; _ControlChars: TCharSet): string;
var
  i: integer;
begin
  Result := '';
  Include(_ControlChars, _Prefix);
  for i := 1 to Length(_s) do begin
    if _s[i] in _ControlChars then
      Result := Result + Format('%s%.2x', [_Prefix, Ord(_s[i])])
    else
      Result := Result + _s[i];
  end;
end;

function HexDecodeControlChars(const _Prefix: char; const _s: string): string;
var
  i: integer;
begin
  Result := '';
  i := 1;
  while i <= Length(_s) do begin
    if (_s[i] = _Prefix) and (i + 2 <= Length(_s)) and isHexDigit(_s[i + 1]) and isHexDigit(_s[i + 2]) then begin
      Result := Result + chr(Hex2Long(_s[i + 1] + _s[i + 2]));
      Inc(i, 2);
    end else
      Result := Result + _s[i];
    Inc(i);
  end;
end;

function UrlEncodeControlChars(const _s: string; _ControlChars: TCharSet = STANDARD_CONTROL_CHARS): string;
begin
  Result := HexEncodeControlChars('%', _s, _ControlChars);
end;

function UrlDecodeControlChars(const _s: string): string;
begin
  Result := HexDecodeControlChars('%', _s);
end;

function LDotDotStr(const _s: string; _MaxLen: integer): string;
begin
  if Length(_s) > _MaxLen then
    Result := '..' + Copy(_s, Length(_s) - _MaxLen + 3, _MaxLen - 2)
  else
    Result := _s;
end;

function RDotDotStr(const _s: string; _MaxLen: integer): string;
begin
  if Length(_s) > _MaxLen then
    Result := Copy(_s, 3, _MaxLen - 2) + '..'
  else
    Result := _s;
end;

function MatchStr(const _Searched, _Match: string): boolean;
begin
  Result := (LeftStr(_Searched, Length(_Match)) = _Match);
end;

function UMatchStr(const _Searched, _Match: string): boolean;
begin
  Result := (AnsiLowerCase(LeftStr(_Searched, Length(_Match))) = AnsiLowerCase(_Match));
end;

// this function is compatible with StrNew/StrDispose in *SysUtils*

function StrPNew(_s: string): PChar;
var
  Size: Cardinal;
begin
  Size := Length(_s);
  Result := StrAlloc(Size + 1);
  StrMove(Result, @_s[1], Size);
  (Result + Size)^ := #0;
end;

{$IFOPT h-}
var
  Pas2StrTempStr: string;
{$ENDIF}

function Pas2Str(var _s: string): PChar;
begin
{$IFOPT h+}
  result := PChar(_s);
{$ELSE}
  Pas2StrTempStr := _s + #0;
  result := @Pas2StrTempStr[1];
{$ENDIF}
end;

function Str2Pas(_p: PChar): string;
begin
  if _p = nil then
    Result := ''
  else
    Result := StrPas(_p);
end;

function RPadStr(const _s: string; _Len: integer): string;
begin
  if Length(_s) >= _Len then
    Result := LeftStr(_s, _Len)
  else
    Result := _s + SpaceStr(_Len - Length(_s));
end;

function LPadStr(const _s: string; _Len: integer): string;
begin
  if Length(_s) >= _Len then
    Result := RightStr(_s, _Len)
  else
    Result := SpaceStr(_Len - Length(_s)) + _s;
end;

function RTrimSpaces(const _s: string): string;
begin
  Result := _s;
  while NthCharOf(Result, Length(Result)) = ' ' do
    System.Delete(Result, Length(Result), 1);
end;

function LTrimSpaces(const _s: string): string;
begin
  Result := _s;
  while LeftStr(Result, 1) = ' ' do
    System.Delete(Result, 1, 1);
end;

// twm: There is probably a more efficient way to implement this.

function RPosStr(_Sub, _s: string): integer;
var
  p: integer;
begin
  Result := 0;
  p := Pos(_Sub, _s);
  while p > 0 do begin
    Inc(Result, p);
    _s := TailStr(_s, p + 1);
    p := Pos(_Sub, _s);
  end;
end;

function PrependBackslash(const _s: string): string;
begin
  if LeftStr(_s, 1) = '\' then
    Result := _s
  else
    Result := '\' + _s;
end;

{$IFNDEF delphi7up}

function AddBackslash(const _s: string): string;
begin
  if RightStr(_s, 1) = '\' then
    Result := _s
  else
    Result := _s + '\';
end;

function StripBackslash(const _s: string): string;
begin
  if RightStr(_s, 1) = '\' then
    Result := LeftStr(_s, Length(_s) - 1)
  else
    Result := _s;
end;
{$ENDIF}

function StringOf(_c: char; _Anz: integer): string;
var
  i: integer;
begin
  //  SetLength(Result, _Anz);
  //  FillChar(Result[1], _Anz, _c);
  Result := '';
  for i := 1 to _Anz do
    Result := Result + _c;
end;

function SpaceStr(_Anz: integer): string;
begin
{$IFDEF Delphi7up}
  Result := DupeString(' ', _Anz);
{$ELSE}
  Result := StringOf(' ', _Anz);
{$ENDIF}
end;

function TrimSpaces(const _s: string): string;
var
  I, L: Integer;
begin
  L := Length(_s);
  I := 1;
  while (I <= L) and (_s[I] = ' ') do
    Inc(I);
  if I > L then
    Result := ''
  else begin
    while _s[L] = ' ' do
      Dec(L);
    Result := Copy(_s, I, L - I + 1);
  end;
end;

//procedure Error(_Desc: string);
//begin
//  WriteLn(_Desc);
//  Halt(1);
//end;

function LoCase(_c: char): char;
begin
  if _c in ['A'..'Z'] then
    Result := Chr(Ord(_c) + 32)
  else
    Result := _c;
end;

{function UpStr(const _s: string): string;
  var
    i: integer;
  begin
  SetLength(Result, Length(_s));
  for i:=1 to Length(_s) do begin
    Result[i]:=UpCase(_s[i]);
  end;
end;}

{function LoStr(const _s : string) : string;
  var
    i: integer;
  begin
  SetLength(result, Length(_s));
  for i:=1 to Length(_s) do
    Result[i]:=LoCase(_s[i]);
end;}

procedure StrReadZ(var _f: file; _p: PChar);
begin
  BlockRead(_f, _p^, SizeOf(_p^));
  while _p^ <> #0 do begin
    Inc(_p);
    BlockRead(_f, _p^, SizeOf(_p^));
  end;
end;

procedure StrReadLn(var _f: file; _p: PChar);
begin
  BlockRead(_f, _p^, SizeOf(_p^));
  while _p^ <> #13 do begin
    Inc(_p);
    BlockRead(_f, _p^, SizeOf(_p^));
  end;
  _p^ := #0
end;

function ExtractStr(var _Source: string; _Delimiters: TCharSet; out _Substr: string; var _LastWasDelimiter: boolean): boolean;
var
  p: integer;
begin
  if _LastWasDelimiter then begin
    _LastWasDelimiter := false;
    Result := true;
    _Source := '';
    _SubStr := '';
    exit;
  end;

  p := 1;
  while p <= Length(_Source) do begin
    if _Source[p] in _Delimiters then begin
      _Substr := LeftStr(_Source, p - 1);
      _Source := TailStr(_Source, p + 1);
      if _Source = '' then
        _LastWasDelimiter := true;
      Result := true;
      exit;
    end;
    inc(p);
  end;
  Result := _Source <> '';
  if Result then begin
    _SubStr := LeftStr(_Source, p - 1);
    _Source := '';
  end;
end;

function ExtractStr(var _Source: string; _Delimiters: TCharSet; out _Substr: string): boolean;
var
  b: boolean;
begin
  b := false;
  Result := ExtractStr(_Source, _Delimiters, _Substr, b);
end;

function ExtractStr(var _Source: string; _Delimiter: char; out _Substr: string; var _LastWasDelimiter: boolean): boolean; overload;
begin
  Result := ExtractStr(_Source, [_Delimiter], _SubStr, _LastWasDelimiter);
end;

function ExtractStr(var _Source: string; _Delimiter: char; out _Substr: string): boolean;
var
  b: boolean;
begin
  b := false;
  Result := ExtractStr(_Source, [_Delimiter], _SubStr, b);
end;

function GetDelStr(var _Zeile: string; _Del: char): string;
var
  p: integer;
begin
  p := Pos(_Del, _Zeile);
  if p = 0 then begin
    Result := _Zeile;
    _Zeile := '';
  end else begin
    Result := LeftStr(_Zeile, p - 1);
    _Zeile := TailStr(_Zeile, p + 1);
  end;
end;

function CenterStr(const _s: string; _MaxLen: integer): string;
var
  l: integer;
begin
  if Length(_s) > _MaxLen then
    raise EStringTooLong.Create(STR_STRING_TOO_LONG_TO_CENTER);
  l := (_MaxLen - Length(_s)) div 2;
  Result := SpaceStr(l) + _s + SpaceStr(l);
  if Odd(Length(_s)) then
    Result := Result + ' ';
end;

function LeftStr(const _s: string; _Len: integer): string;
begin
  Result := Copy(_s, 1, _Len);
end;

function RightStr(const _s: string; _Len: integer): string;
begin
  if Length(_s) > _Len then
    Result := TailStr(_s, Length(_s) - _Len + 1)
  else
    Result := _s;
end;

function TailStr(const _s: string; _Start: integer): string;
begin
  if _Start > Length(_s) then
    Result := ''
  else
    Result := Copy(_s, _Start, Length(_s) - _Start + 1);
end;

function StrCutRight(const _s: string; _CutLen: integer): string;
begin
  Result := LeftStr(_s, Length(_s) - _CutLen);
end;

function LeftStrOf(const _s: string; _c: char): string;
var
  p: integer;
begin
  p := Pos(_c, _s);
  if p = 0 then
    Result := _s
  else
    Result := LeftStr(_s, p - 1);
end;

function RightStrOf(const _s: string; _c: char): string;
var
  p: integer;
begin
  p := RPosStr(_c, _s);
  if p = 0 then
    Result := _s
  else
    Result := TailStr(_s, p + 1);
end;

function TailStrOf(const _s: string; _c: char): string;
var
  p: integer;
begin
  p := Pos(_c, _s);
  if p = 0 then
    Result := _s
  else
    Result := TailStr(_s, p + 1);
end;

function PosStr(const _SubStr, _s: string; _Start: integer): integer;
begin
  Result := PosEx(_SubStr, _s, _Start);
  //  Result := Pos(_SubStr, TailStr(_s, _Start));
  //  if Result > 0 then
  //    Result := Result + _Start - 1;
end;

function FindString(const _Substr, _Str: string; var _Head, _Tail: string): boolean;
var
  P: integer;
begin
  p := Pos(_SubStr, _Str);
  Result := (p <> 0);
  if Result then begin
    _Head := LeftStr(_Str, p - 1);
    _Tail := TailStr(_Str, p + Length(_Substr));
  end;
end;

function NthCharOf(const _s: string; _n: integer): Char;
begin
  if _n = 0 then
    raise ECharIndexOutOfRange.Create(STR_CHAR_INDEX_OUT_OF_RANGE);
  if _n <= Length(_s) then
    Result := _s[_n]
  else
    Result := ' ';
end;

function Tab2Spaces(const _s: string; _SpcCount: integer): string;
var
  i: integer;
  Spaces: string;
begin
  // twm: This is not particularly efficient, just don't use it on large strings. ;-)
  Result := '';
  Spaces := SpaceStr(_SpcCount);
  for i := 1 to Length(_s) do begin
    if _s[i] = #9 then
      Result := Result + Spaces
    else
      Result := Result + _s[i];
  end;
end;

function StartsWith(const _Start, _s: string): boolean;
begin
  Result := AnsiSameStr(_Start, LeftStr(_s, Length(_Start)));
end;

function UStartsWith(const _Start, _s: string): boolean;
begin
  Result := AnsiSameText(_Start, LeftStr(_s, Length(_Start)));
end;

function EndsWith(const _End, _s: string): boolean;
begin
  Result := AnsiSameStr(_End, RightStr(_s, Length(_End)));
end;

function UEndsWith(const _End, _s: string): boolean;
begin
  Result := AnsiSameText(_End, RightStr(_s, Length(_End)));
end;

function UnquoteString(const _s: string; _Quote: char): string;
var
  Len: integer;
begin
  Len := Length(_s);
  if (Len > 1) and (_s[1] = _Quote) and (_s[Len] = _Quote) then
    Result := Copy(_s, 2, Len - 2)
  else
    Result := _s;
end;

function StringOrNull(_P: PChar): string;
begin
  if Assigned(_P) then
    Result := '"' + _P + '"'
  else
    Result := 'NULL';
end;

function GetSystemDefaultLocaleSettings: TFormatSettings;
begin
  GetLocaleFormatSettings(GetSystemDefaultLCID, Result);
end;

function GetUserDefaultLocaleSettings: TFormatSettings;
begin
  GetLocaleFormatSettings(GetUserDefaultLCID, Result);
end;

end.

