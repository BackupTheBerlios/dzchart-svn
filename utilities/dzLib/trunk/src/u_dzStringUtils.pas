{.GXFormatter.config=twm}
///<summary> Implements a bunch of commonly used string functions. This started out
///          as a copy of my Virtual Pascal Unit TwmString adapted to Delphi.
///          @author   twm </summary>
unit u_dzStringUtils;

{$I jedi.inc}

interface

uses
  Windows,
  Classes,
  StrUtils,
  SysUtils,
{$IFDEF SUPPORTS_UNICODE_STRING}
  AnsiStrings,
{$ENDIF SUPPORTS_UNICODE_STRING}
  u_dzTranslator;

type
///<summary> Ancestor of all exceptions raised in u_dzStringUtils </summary>
  EStringfunc = class(Exception);
///<summary> Raised by NthWord if the word index is out of range (1..word count) </summary>
  EWordIndexOutOfRange = class(EStringfunc);
///<summary> Raised by CenterStr if the given string ist too long. </summary>
  EStringTooLong = class(EStringfunc);
///<summary> Raised by NthCharOf if passed a char index of 0. </summary>
  ECharIndexOutOfRange = class(EStringfunc);

type
  TCharSet = set of ansichar;

const
  ///<summary> Characters that are usually used as word delimiters.
  ///          This can be passed when a function takes a TCharSet of delimiters.
  ///          warning: These might not be complete. </summary>
  STANDARD_DELIMITERS = [chr(0)..chr(255)] - ['a'..'z', 'A'..'Z',
    '0'..'9', '�', '�', '�', '�', '�', '�', '�'];
  STANDARD_CONTROL_CHARS = [#0..' '];

{$IFNDEF SUPPORTS_UNICODE_STRING}
function CharInSet(_C: Char; const _CharSet: TSysCharSet): Boolean;
{$ENDIF SUPPORTS_UNICODE_STRING}

/// <summary>
/// function is deprecated, use ExtractStr instead
/// </summary>
function GetDelStr(var _Zeile: string; _Del: char): string; deprecated;

/// <summary>
/// extracts the substring from the start of Source up to the Delimiter
/// </summary>
function ExtractStr(var _Source: string; _Delimiter: char): string; overload;
function ExtractStr(var _Source: string; _Delimiters: TCharSet): string; overload;
/// <summary>
/// extracts a substring from the start of Source up to the Delimiter
/// @returns true, if a substring (even an empty one) was found.
/// </summary>
function ExtractStr(var _Source: string; _Delimiters: TCharSet; out _Substr: string): boolean; overload;
/// <summary>
/// extracts a substring from the start of Source up to the Delimiter
/// @returns true, if a substring (even an empty one) was found.
/// </summary>
function ExtractStr(var _Source: string; _Delimiter: char; out _Substr: string): boolean; overload;
/// <summary>
/// extracts a substring from the start of Source up to the delimiter
/// @param LastWasDelimiter is a boolean that is to be used in repeated calls
///                         to the function to tell the next call that the
///                         last call returned an empty string because there
///                         was a double delimiter.
/// Use like this:
/// b := false;
/// while ExtractStr(Source, [' ', #9], s, b) do
///   HandleSubstring(s);
/// </summary>
function ExtractStr(var _Source: string; _Delimiters: TCharSet; out _Substr: string; var _LastWasDelimiter: boolean): boolean; overload;
/// <summary>
/// extracts a substring from the start of Source up to the delimiter
/// @param LastWasDelimiter is a boolean that is to be used in repeated calls
///                         to the function to tell the next call that the
///                         last call returned an empty string because there
///                         was a double delimiter.
/// Use like this:
/// b := false;
/// while ExtractStr(Source, [' ', #9], s, b) do
///   HandleSubstring(s);
/// </summary>
function ExtractStr(var _Source: string; _Delimiter: char; out _Substr: string; var _LastWasDelimiter: boolean): boolean; overload;

/// <summary>
/// Converts a char to lower case.
/// </summary>
function LoCase(_c: WideChar): WideChar; overload;
function LoCase(_c: AnsiChar): AnsiChar; overload;

// function UpStr(const _s: string): string; // use SysUtils.(Ansi)UpperCase instead
// function LoStr(const _s: string): string; // use SysUtils.(Ansi)LowerCase instead

/// <summary>
/// like SysUtils.Trim but only removes spaces, no special characters
/// </summary>
function TrimSpaces(const _s: string): string;
/// <summary>
/// like SysUtils.TrimRight but only removes spaces, no special characters
/// </summary>
function RTrimSpaces(const _s: string): string;
/// <summary>
/// like SysUtils.TrimLeft but only removes spaces, no special characters
/// </summary>
function LTrimSpaces(const _s: string): string;
type
  TTrimStr = function(const _s: string): string;
const
/// <summary>
/// function is deprcated, use RTrimSpaces instead, or possibily SysUtils.TrimRight
/// </summary>
  RTrimStr: TTrimStr = RTrimSpaces deprecated;
/// <summary>
/// function is deprcated, use LTrimSpaces instead, or possibly SysUtils.TrimLeft
/// </summary>
  LTrimStr: TTrimStr = LTrimSpaces deprecated;
/// <summary>
/// function is deprcated, use TrimSpaces instead, or possibly SysUtils.Trim
/// </summary>
  TrimStr: TTrimStr = TrimSpaces deprecated;

/// <summary>
/// Creates a string with Cnt spaces.
/// </summary>
function SpaceStr(_Cnt: integer): string;

/// <summary>
/// function is deprecated, use StrUtils.DupeString instead
/// </summary>
function StringOf(_c: char; _Cnt: integer): string; deprecated;

/// <summary>
/// Prepend a backslash to the string if there isn't one already.
/// </summary>
function PrependBackslash(const _s: string): string;
type
  TXxxBackslash = function(const _s: string): string;
const
/// <summary>
/// function is deprecated, use IncludeTrailingPathDelimiter instead
/// </summary>
  AddBackslash: TXxxBackslash = IncludeTrailingPathDelimiter deprecated;
/// <summary>
/// function is deprecated, use ExcludeTrailingPathDelimiter instead
/// </summary>
  StripBackslash: TXxxBackslash = ExcludeTrailingPathDelimiter deprecated;

/// <summary>
/// Replaces an existing extension in Name with Ext or adds Ext to Name if
/// it does not have an extension.
/// </summary>
function ForceExtension(const _Name, _Ext: string): string;
{: Returns only the filename (incl. extension) portion of Name. }
function JustFilename(const _Name: string): string;
/// <summary>
/// function is deprecated (because it did a different thing but what the name said)
/// use RemoveSuffixIfMatching instead
/// </summary>
function RemoveFileExtIfMatching(const _s, _Suffix: string): string; deprecated;
/// <summary>
/// removes a suffix (can be a file extension, but can also be any arbitrary string)
/// from a string if it matches the given one
/// @param s is the input string
/// @param Suffix is the suffix to remove, if the string has it (comparison is case insensitive)
/// @returns the string without the suffix, if it matched, the unchanged string otherwise }
/// </summary>
function RemoveSuffixIfMatching(const _s, _Suffix: string): string;

///<summary> Appends spaces to the string S to bring it to the given length. If S is
///          too long and AllowTruncate is true (default) it is truncated and thus the result is
///          guaranteed to be Len characters long. If AllowTruncate is false, no truncation
///          is done and the string is only guaranteed to be at least Len characters long. </summary>
function RPadStr(const _s: string; _Len: integer; _AllowTruncate: boolean = true): string;
///<summary> Prepends spaces to the string S to bring it to the given length. If S is
///          too long it is truncated, thus the result is guaranteed to be Len characters
///          long. </summary>
///<summary> Prepends spaces to the string S to bring it to the given length. If S is
///          too long and AllowTruncate is true (default) it is truncated (at the start) and thus the
///          result is guaranteed to be Len characters long. If AllowTruncate is false, no truncation
///          is done and the string is only guaranteed to be at least Len characters long. </summary>
function LPadStr(const _s: string; _Len: integer; _AllowTruncate: boolean = true): string;

///<summary> Returns true, if SubStr is found in Str and sets Head to the text before
///          and Tail to the text after SubStr. Otherwise Head and Tail are undefined. </summary>
function FindString(const _Substr, _Str: string; var _Head, _Tail: string): boolean;

///<summary> Returns the rightmost position of Sub in S or 0 if Sub is not found in S. </summary>
function RPosStr(_Sub, _s: string): integer;

///<summary> Converts a PChar to as Pascal string. Treats NIL as an empty string. </summary>
function Str2Pas(_p: PChar): string;
///<summary> Allocates a new PChar and sets it to the contents of S, the length is set
///          exactly to the length of S. </summary>
function StrPNew(_s: string): PChar;
///<summary> Returns a pointer to a temporary string containing S. Warning: This uses a
///          global constant for ShortStrings respectively just typecasts AnsiStrings
///          to PChar. Use with care! </summary>
function Pas2Str(var _s: string): PChar; deprecated; // just typecast AnsiStrings to PChar

///<summary> Reads a line from the file F and returns it as PChar. P is allocated to
///          the correct length. </summary>
procedure StrReadLn(var _f: file; _p: PChar);
///<summary> Reads a 0-terminated string from file F and returns it as PChar. P is
///          allocated to the correct length. </summary>
procedure StrReadZ(var _f: file; _p: PChar);

///</summary> Returns true if Searched starts with the string Match. </summary>
function MatchStr(const _Searched, _Match: string): boolean;
///<summary> Returns true if Searched starts with the string Match ignoring case. </summary>
function UMatchStr(const _Searched, _Match: string): boolean;

///<summary> Creates a string of the form '...end of the string' with a maximum length. </summary>
function LDotDotStr(const _s: string; _MaxLen: integer): string;
///<summary> Creates a string of the form 'Start of string...' with a maximum length. </summary>
function RDotDotStr(const _s: string; _MaxLen: integer): string;

function ZeroPadLeft(_Value: Integer; _Len: Integer): string;

///<summary> Centers the given string, that is right and left pads it with spaces to
///          MaxLenght characters. </summary>
function CenterStr(const _s: string; _MaxLen: integer): string;

function TailStr(const _s: string; _Start: integer): string;
///<summary> cuts off the rightmost part of a string </summary>
function StrCutRight(const _s: string; _CutLen: integer): string;

///<summary> Returns part of S left of C (or all of it if it does not contain C) </summary>
function LeftStrOf(const _s: string; _c: char): string;
///<summary> Returns part of S right of the last C (or all of it if it does not contain C) </summary>
function RightStrOf(const _s: string; _c: char): string;
///<summary> Returns part of S right of the first C (or all of it if it does not contain C) </summary>
function TailStrOf(const _s: string; _c: char): string;

///<summary> Returns the next postion of SubStr in S starting from Start. Start is
///          1 based. Returns 0 if SubStr was not found.
///          Note: Function is deprecated, use StrUtils.PosEx instead! </summary>
function PosStr(const _SubStr, _s: string; _Start: integer): integer; deprecated;

///<summary> Replaces all occurences of characters in Search in S with the corresponding
///          character in Replace. If there is no matching character in Replace,
///          the character will be omitted. </summary>
function ReplaceChars(const _s, _Search, _Replace: string): string; overload;

///<summary> Replaces all occurences of characters in Search in S with the Replace string.
///          If Replace is an empty string, the characters will be omitted. </summary>
function ReplaceChars(const _s: string; _Search: TCharSet; const _Replace: string; _RemoveDuplicates: boolean = true): string; overload;

///<summary> Replaces all control characters (ord(c) < ord(' ')) with ReplaceChar.
///          If RemoveDuplicates is true, a sequence of control characters is replaced
///          by a single ReplaceChar. </summary>
function ReplaceCtrlChars(const _s: string; _ReplaceChar: char; _RemoveDuplicates: boolean = true): string;

///<summary> Replaces all control characters (ord(c) < ord(' ')) with Spaces.
///          If RemoveDuplicates is true, a sequence of control characters is replaced
///          by a single space. </summary>
function CtrlcharsToSpace(const _s: string; _RemoveDuplicates: boolean = true): string;

///<summary> Checks whether a string contains only given chars </summary>
function ContainsOnlyCharsFrom(const _s: string; _ValidChars: TCharSet): boolean;

///<summary> Replaces all control characters (ord(c) <= ord(' '), " and ') with Prefix<hexcode> </summary>
function HexEncodeControlChars(_Prefix: char; const _s: string; _ControlChars: TCharSet = STANDARD_CONTROL_CHARS): string;
function HexDecodeControlChars(const _Prefix: char; const _s: string): string;

///<summary> Replaces all control characters (ord(c) <= ord(' '), " and ') with %<hexcode> </summary>
function UrlEncodeControlChars(const _s: string; _ControlChars: TCharSet = STANDARD_CONTROL_CHARS): string;
function UrlDecodeControlChars(const _s: string): string;

///<summary> Returns the WordNo'th word, (counting from 1), using the given Delimiters.
///          NOTE: duplicate delimiters are ignored, so 'abc  def' will be split
///         into two words (which you would expect), but also 'abc'#9#9'def' is two words
///         (which you might not expect) </summary>
function nthWord(const _s: string; _WordNo: integer; const _Delimiter: string): string; overload;
///<summary> Returns the WordNo'th word, (counting from 1), using the given Delimiters.
///          NOTE: duplicate delimiters are ignored, so 'abc  def' will be split
///          into two words (which you would expect), but also 'abc'#9#9'def' is two words
///          (which you might not expect) </summary>
function nthWord(const _s: string; _WordNo: integer; _Delimiter: TCharSet): string; overload;

///<summary> Returns the Nth character of S or ' ' if S has less than N charaters. </summary>
function nthCharOf(const _s: string; _n: integer): Char;

///<summary> Extract the first word of S using the given delimiters. The word is deleted
///          from S. See also ExtractStr.
///          NOTE: duplicate delimiters are ignored, so 'abc  def' will be split
///          into two words (which you would expect), but also 'abc'#9#9'def' is two words
///          (which you might not expect) </summary>
function ExtractFirstWord(var _s: string; const _Delimiter: string): string; overload;
///<summary> Extract the first word of S using the given delimiters. The word is deleted
///          from S. See also ExtractStr.
///          NOTE: duplicate delimiters are ignored, so 'abc  def' will be split
///          into two words (which you would expect), but also 'abc'#9#9'def' is two words
///          (which you might not expect) </summary>
function ExtractFirstWord(var _s: string; _Delimiter: TCharSet): string; overload;
///<summary> Extract the first word of S using the given delimiters. The word is deleted
///          from S.
///          NOTE: duplicate delimiters are ignored, so 'abc  def' will be split
///          into two words (which you would expect), but also 'abc'#9#9'def' is two words
///          (which you might not expect)
///          @returns true, if a word could be extracted, false otherwise </summary>
function ExtractFirstWord(var _s: string; const _Delimiter: string; out _FirstWord: string): boolean; overload;
///<summary> Extract the first word of S using the given delimiters. The word is deleted
///          from S. See also ExtractStr.
///          NOTE: duplicate delimiters are ignored, so 'abc  def' will be split
///          into two words (which you would expect), but also 'abc'#9#9'def' is two words
///          (which you might not expect)
///          @returns true, if a word could be extracted, false otherwise </summary>
function ExtractFirstWord(var _s: string; _Delimiter: TCharSet; out _FirstWord: string): boolean; overload;

///<summary> extracts the first N characters of a string </summary>
function ExtractFirstN(var _s: string; _N: integer): string;

///<summary> Split string s into the list of substrings delimited by delimter
///          NOTE: duplicate delimiters are ignored, so 'abc  def' will be split
///          into two words (which you would expect), but also 'abc'#9#9'def' is two words
///          (which you might not expect)
///          @param sl is the stringlist in which to return the result
///          @param s is the string to split
///          @param Delimiter is a string containing all delimiter characters
///          @returns the sl parameter </summary>
function SplitString(_sl: TStrings; _s: string; const _Delimiter: string): TStrings;

{$IFDEF SUPPORTS_UNICODE_STRING}
function Copy(const _s: AnsiString; _Pos, _Len: integer): AnsiString; overload;
function Copy(const _s: AnsiString; _Pos: integer): AnsiString; overload;
function Copy(const _s: string; _Pos, _Len: integer): string; overload;
function Copy(const _s: string; _Pos: integer): string; overload;
{$ENDIF SUPPORTS_UNICODE_STRING}

///<summary> Converts Tab characters into SpcCount spaces </summary>
function Tab2Spaces(const _s: string; _SpcCount: integer): string;
function StartsWith(const _Start, _s: string): boolean;
function EndsWith(const _End, _s: string): boolean;

function UStartsWith(const _Start, _s: string): boolean;
function UEndsWith(const _End, _s: string): boolean;

function UnquoteString(const _s: string; _Quote: char = '"'): string;

///<summary> returns the string, if it isn't NIL or 'NULL' if it is. </summary>
function StringOrNull(_P: PChar): string;

///<summary> returns the default locale settings as read from the system's regional settings </summary>
function GetUserDefaultLocaleSettings: TFormatSettings;
function GetSystemDefaultLocaleSettings: TFormatSettings;

///<summary> Read the content of the file into a string and return it </summary>
function LoadStringFromFile(const _Filename: string): string;
///<summary> Write the content of the string to a file </summary>
procedure SaveStringToFile(const _Filename: string; const _Content: string);

type
  ///<summary> Helper class for building a text line </summary>
  TLineBuilder = class
  private
    FListSeparator: string;
    FContent: string;
    FFormatSettings: TFormatSettings;
    FQuoteChar: char;
  public
    ///<summary> Creates a TLineBuilder instance with the given separator
    ///          @param ListSeparator is the separator string to use, defaults to TAB (#9)
    ///          @param DecimalSeparator is the decimal separator to use for floating point
    ///                                  values, defaults to a dot (.). </summary>
    constructor Create(const _ListSeparator: string = #9; const _DecimalSeparator: char = '.');
    ///<summary> Assigns the contents of another TLineBuilder instance </summary>
    procedure Assign(_Source: TLineBuilder);
    ///<summary> Adds a string column </summary>
    procedure Add(const _Column: string); overload;
    ///<summary> Adds a string column, putting it in quotes </summary>
    procedure AddQuoted(const _Column: string);
    ///<summary> Adds an integer value column </summary>
    procedure Add(_IntValue: integer); overload;
    ///<summary> Adds a floating point value column with the given number of decimals </summary>
    procedure Add(_FloatValue: extended; _Decimals: integer); overload;
    ///<summary> Adds a floating point value column with the given number of integer digits
    ///          and the given number of fractional digits </summary>
    procedure Add(_FloatValue: extended; _IntDigits, _FracDigits: integer); overload;
    ///<summary> Adds a column with a time in hh:mm:ss format </summary>
    procedure Add(_Hours, _Minutes, _Seconds: integer); overload;
    ///<summary> Adds a column with a time in hh:mm:ss:tt format </summary>
    procedure Add(_Hours, _Minutes, _Seconds, _Hundredth: integer); overload;
    ///<summary> Adds a boolean column, with 'Y' for true and 'N' for false </summary>
    procedure Add(_b: Boolean); overload;
    ///<summary> Clears the line </summary>
    procedure Clear;
    ///<summary> Appends the contents of the given line </summary>
    procedure Append(_Line: TLineBuilder);
    ///<summary> Prepends the contents of the given line </summary>
    procedure Prepend(_Line: TLineBuilder);
    ///<summary> Extracts the first column from the line, returns false when empty </summary>
    function ExtractFirst(out _Column: string): boolean;
    ///<summary> allows read access to the content that has been built </summary>
    property Content: string read FContent;
    property DecimalSeparator: char read FFormatSettings.DecimalSeparator write FFormatSettings.DecimalSeparator default '.';
    property ListSeparator: string read FListSeparator write FListSeparator;
    property QuoteChar: char read FQuoteChar write FQuoteChar;
    property FormatSettings: TFormatSettings read FFormatSettings;
  end;

implementation

uses
  u_dzConvertUtils;

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzlib');
end;

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

function RemoveFileExtIfMatching(const _s, _Suffix: string): string;
begin
  Result := RemoveSuffixIfMatching(_s, _Suffix);
end;

function RemoveSuffixIfMatching(const _s, _Suffix: string): string;
begin
  if UEndsWith(_Suffix, _s) then
    Result := LeftStr(_s, Length(_s) - Length(_Suffix))
  else
    Result := _s;
end;

function nthWordStartAndEnd(const _s: string; _WordNo: integer;
  const _Delimiter: TCharSet; var _Start, _Ende: integer): boolean; overload;
var
  i: integer;
begin
  if _WordNo = 0 then
    raise EWordIndexOutOfRange.Create(_('nthWord: 0th word not available'));
  _Start := 0;
  _Ende := 0;
  i := 1;
  while i <= Length(_s) do begin
    while (i <= Length(_s)) and CharInSet(NthCharOf(_s, i), _Delimiter) do
      Inc(i);
    Dec(_WordNo);
    if _WordNo = 0 then
      _Start := i;
    while (i <= Length(_s)) and not CharInSet(NthCharOf(_s, i), _Delimiter) do
      Inc(i);
    if _WordNo = 0 then begin
      _Ende := i;
      Break;
    end;
  end;
  Result := (_Start <> 0) and (_Ende <> 0);
end;

function nthWordStartAndEnd(const _s: string; _WordNo: integer;
  const _Delimiter: AnsiString; var _Start, _Ende: integer): boolean; overload;
var
  i: integer;
  DelimiterSet: TCharSet;
begin
  DelimiterSet := [];
  for i := 1 to Length(_Delimiter) do
    Include(DelimiterSet, _Delimiter[i]);
  Result := nthWordStartAndEnd(_s, _WordNo, DelimiterSet, _Start, _Ende);
end;

{$IFDEF SUPPORTS_UNICODE_STRING}

function nthWordStartAndEnd(const _s: string; _WordNo: integer;
  const _Delimiter: string; var _Start, _Ende: integer): boolean; overload;
begin
  Result := nthWordStartAndEnd(_s, _WordNo, AnsiString(_Delimiter), _Start, _Ende);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

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

function ExtractFirstWord(var _s: string; _Delimiter: TCharSet): string;
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

function ExtractFirstWord(var _s: string; const _Delimiter: string; out _FirstWord: string): boolean;
var
  Start, Ende: integer;
begin
  Result := nthWordStartAndEnd(_s, 1, _Delimiter, Start, Ende);
  if Result then begin
    _FirstWord := Copy(_s, Start, Ende - Start);
    _s := TailStr(_s, Ende + 1);
  end;
end;

function ExtractFirstWord(var _s: string; _Delimiter: TCharSet; out _FirstWord: string): boolean;
var
  Start, Ende: integer;
begin
  Result := nthWordStartAndEnd(_s, 1, _Delimiter, Start, Ende);
  if Result then begin
    _FirstWord := Copy(_s, Start, Ende - Start);
    _s := TailStr(_s, Ende + 1);
  end;
end;

function ExtractFirstN(var _s: string; _N: integer): string;
begin
  Result := Copy(_s, 1, _N);
  _s := Copy(_s, _N + 1);
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

function ReplaceChars(const _s: string; _Search: TCharSet; const _Replace: string; _RemoveDuplicates: boolean = true): string;
var
  i: LongInt;
  Dup: boolean;
begin
  Result := '';
  Dup := false;
  for i := 1 to Length(_s) do begin
    if CharInSet(_s[i], _Search) then begin
      if not Dup or not _RemoveDuplicates then begin
        Result := Result + _Replace;
        Dup := true;
      end;
    end else begin
      Result := Result + _s[i];
      Dup := false;
    end;
  end;
end;

function ReplaceCtrlChars(const _s: string; _ReplaceChar: char; _RemoveDuplicates: boolean = true): string;
var
  i: integer;
  Dup: boolean;
begin
  Result := _s;
  Dup := false;
  for i := Length(Result) downto 1 do
    if Ord(Result[i]) < Ord(' ') then begin
      if not Dup or not _RemoveDuplicates then begin
        Result[i] := _ReplaceChar;
        Dup := true;
      end else
        Delete(Result, i, 1);
    end else
      Dup := false;
end;

function ContainsOnlyCharsFrom(const _s: string; _ValidChars: TCharSet): boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 1 to Length(_s) do
    if not CharInSet(_s[i], _ValidChars) then
      exit;
  Result := True;
end;

function CtrlcharsToSpace(const _s: string; _RemoveDuplicates: boolean = true): string;
begin
  Result := ReplaceCtrlChars(_s, ' ', _RemoveDuplicates);
end;

function HexEncodeControlChars(_Prefix: Char; const _s: string; _ControlChars: TCharSet): string;
var
  i: integer;
begin
  Result := '';
  Include(_ControlChars, AnsiChar(_Prefix));
  for i := 1 to Length(_s) do begin
    if CharInSet(_s[i], _ControlChars) then
      Result := Result + Format('%s%.2x', [_Prefix, Ord(_s[i])]) // do not translate
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
  Result := SameStr(LeftStr(_Searched, Length(_Match)), _Match);
end;

function UMatchStr(const _Searched, _Match: string): boolean;
begin
  Result := SameText(LeftStr(_Searched, Length(_Match)), _Match);
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

function Pas2Str(var _s: string): PChar;
begin
  Result := PChar(_s);
end;

function Str2Pas(_p: PChar): string;
begin
  if _p = nil then
    Result := ''
  else
    Result := StrPas(_p);
end;

function RPadStr(const _s: string; _Len: integer; _AllowTruncate: boolean = true): string;
begin
  if Length(_s) >= _Len then begin
    if _AllowTruncate then
      Result := LeftStr(_s, _Len)
    else
      Result := _s;
  end else
    Result := _s + SpaceStr(_Len - Length(_s));
end;

function LPadStr(const _s: string; _Len: integer; _AllowTruncate: boolean = true): string;
begin
  if Length(_s) >= _Len then begin
    if _AllowTruncate then
      Result := RightStr(_s, _Len)
    else
      Result := _s;
  end else
    Result := SpaceStr(_Len - Length(_s)) + _s;
end;

function RTrimSpaces(const _s: string): string;
begin
  Result := _s;
  while (Length(Result) > 0) and (NthCharOf(Result, Length(Result)) = ' ') do
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

function StringOf(_c: char; _Cnt: integer): string;
begin
  Result := StrUtils.DupeString(_c, _cnt);
end;

function SpaceStr(_Cnt: integer): string;
begin
  Result := DupeString(' ', _Cnt);
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

function LoCase(_c: WideChar): WideChar;
begin
  Result := _c;
  case _c of
    'A'..'Z':
      Result := WideChar(Word(_c) or $0020);
  end;
end;

function LoCase(_c: AnsiChar): AnsiChar;
begin
  Result := _c;
  case _c of
    'A'..'Z':
      Result := AnsiChar(Byte(_c) or $20);
  end;
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

function ExtractStr(var _Source: string; _Delimiter: char): string;
var
  p: integer;
begin
  p := Pos(_Delimiter, _Source);
  if p = 0 then begin
    Result := _Source;
    _Source := '';
  end else begin
    Result := LeftStr(_Source, p - 1);
    _Source := TailStr(_Source, p + 1);
  end;
end;

function ExtractStr(var _Source: string; _Delimiters: TCharSet): string;
begin
  if not ExtractStr(_Source, _Delimiters, Result) then
    Result := '';
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
    if CharInSet(_Source[p], _Delimiters) then begin
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

function ExtractStr(var _Source: string; _Delimiter: char; out _Substr: string; var _LastWasDelimiter: boolean): boolean;
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
begin
  Result := ExtractStr(_Zeile, _Del);
end;

function CenterStr(const _s: string; _MaxLen: integer): string;
var
  l: integer;
begin
  if Length(_s) > _MaxLen then
    raise EStringTooLong.Create(_('Cannot center string that is larger than the maximum length.'));
  l := (_MaxLen - Length(_s)) div 2;
  Result := SpaceStr(l) + _s + SpaceStr(l);
  if Odd(Length(_s)) then
    Result := Result + ' ';
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
    raise ECharIndexOutOfRange.Create(_('Strings do not have a 0th character.'));
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
    Result := 'NULL'; // do not translate
end;

function GetSystemDefaultLocaleSettings: TFormatSettings;
begin
{$IFDEF RTL220_UP}
  Result := TFormatSettings.Create(GetSystemDefaultLCID);
{$ELSE}
  GetLocaleFormatSettings(GetSystemDefaultLCID, Result);
{$ENDIF}
end;

function GetUserDefaultLocaleSettings: TFormatSettings;
begin
{$IFDEF RTL220_UP}
  Result := TFormatSettings.Create(GetUserDefaultLCID);
{$ELSE}
  GetLocaleFormatSettings(GetUserDefaultLCID, Result);
{$ENDIF}
end;

{ TLineBuilder }

constructor TLineBuilder.Create(const _ListSeparator: string = #9; const _DecimalSeparator: char = '.');
begin
  inherited Create;
  FListSeparator := _ListSeparator;
  FFormatSettings := GetUserDefaultLocaleSettings;
  FFormatSettings.DecimalSeparator := _DecimalSeparator;
  FFormatSettings.ThousandSeparator := #0;
  FQuoteChar := '"';
end;

procedure TLineBuilder.Add(_IntValue: integer);
begin
  Add(IntToStr(_IntValue));
end;

procedure TLineBuilder.Add(_FloatValue: extended; _Decimals: integer);
begin
  Add(FloatToStrF(_FloatValue, ffFixed, 18, _Decimals, FFormatSettings));
end;

procedure TLineBuilder.Add(_FloatValue: extended; _IntDigits, _FracDigits: integer);
begin
  Add(Format('%*.*f', [_IntDigits, _FracDigits, _FloatValue], FFormatSettings));
end;

procedure TLineBuilder.Add(const _Column: string);
begin
  if FContent <> '' then
    FContent := FContent + FListSeparator;
  FContent := FContent + _Column;
end;

function ZeroPadLeft(_Value: Integer; _Len: Integer): string;
var
  s: AnsiString;
begin
  Str(_Value, s);
  Result := string(s);
  while Length(Result) < _Len do
    Result := '0' + Result;
end;

procedure TLineBuilder.Add(_Hours, _Minutes, _Seconds: integer);
begin
  Add(ZeroPadLeft(_Hours, 2) + ':' + ZeroPadLeft(_Minutes, 2) + ':' + ZeroPadLeft(_Seconds, 2));
end;

procedure TLineBuilder.Add(_Hours, _Minutes, _Seconds, _Hundredth: integer);
begin
  Add(ZeroPadLeft(_Hours, 2) + ':' + ZeroPadLeft(_Minutes, 2) + ':' + ZeroPadLeft(_Seconds, 2)
    + ':' + ZeroPadLeft(_Hundredth, 2));
end;

procedure TLineBuilder.Add(_b: Boolean);
begin
  Add(IfThen(_B, 'Y', 'N'));
end;

procedure TLineBuilder.AddQuoted(const _Column: string);
begin
  Add(FQuoteChar + _Column + FQuoteChar);
end;

procedure TLineBuilder.Append(_Line: TLineBuilder);
var
  s: string;
begin
  s := _Line.Content;
  if s = '' then
    exit;
  if FContent <> '' then
    FContent := FContent + FListSeparator + s
  else
    FContent := s;
end;

procedure TLineBuilder.Assign(_Source: TLineBuilder);
begin
  FContent := _Source.Content;
end;

procedure TLineBuilder.Clear;
begin
  FContent := '';
end;

function TLineBuilder.ExtractFirst(out _Column: string): boolean;
var
  p: Integer;
begin
  p := Pos(FListSeparator, FContent);
  Result := p <> 0;
  if Result then begin
    _Column := LeftStr(FContent, p - 1);
    FContent := Copy(FContent, p + 1);
  end;
end;

procedure TLineBuilder.Prepend(_Line: TLineBuilder);
var
  s: string;
begin
  s := _Line.Content;
  if s = '' then
    exit;
  if FContent <> '' then
    FContent := s + FListSeparator + FContent
  else
    FContent := s;
end;

function LoadStringFromFile(const _Filename: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(_Filename);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

procedure SaveStringToFile(const _Filename: string; const _Content: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := _Content;
    sl.SaveToFile(_Filename);
  finally
    sl.Free;
  end;
end;

{$IFNDEF SUPPORTS_UNICODE_STRING}

function CharInSet(_C: Char; const _CharSet: TSysCharSet): Boolean;
begin
  Result := _c in _CharSet;
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

{$IFDEF SUPPORTS_UNICODE_STRING}

function Copy(const _s: AnsiString; _Pos, _Len: integer): AnsiString;
begin
  SetLength(Result, _Len);
  Move(_s[_Pos], Result[1], _Len);
end;

function Copy(const _s: AnsiString; _Pos: integer): AnsiString;
begin
  Result := Copy(_s, _Pos, Length(_s) - _Pos);
end;

function Copy(const _s: string; _Pos, _Len: integer): string; inline;
begin
  Result := System.Copy(_s, _Pos, _Len);
end;

function Copy(const _s: string; _Pos: integer): string; inline;
begin
  Result := System.Copy(_s, _Pos);
end;
{$ENDIF SUPPORTS_UNICODE_STRING}

end.

