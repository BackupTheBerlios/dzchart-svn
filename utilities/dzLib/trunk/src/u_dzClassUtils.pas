{GXFormatter.config=twm}
{: Implements functions which work on classes but are not methods.
   @autor(twm) }
unit u_dzClassUtils;

interface

uses
  SysUtils,
  Classes;

// NOTE: The naming convention is <extended-class>_<Methodname>

type
  {: raised by StringByObj if no matching entry was found }
  EObjectNotFound = class(Exception);

  {: Removes trailing spaces from all lines in Strings as well as empty lines
     from the end of Strings, returns true if at least one string was shortened
     or an empty string was removed.
     @param Strings is the TStrings class to work on.
     @returns true, if something was changed, false otherwise }
function TStrings_RemoveTrailingSpaces(_Strings: TStrings): boolean;

{: Free a TStrings object including all it's Object[n]s }
procedure TStrings_FreeWithObjects(_Strings: TStrings);

{: Frees all objects stored in the TStrings intance and returns the instance,
   meant to be called like
   @code( TStrings_FreeAllObjects(sl).Free; ) or
   @code( TStrings_FreeAllObjects(sl).Clear; ) }
function TStrings_FreeAllObjects(_Strings: TStrings): TStrings;

{: frees the object and delets the entry from the list }
procedure TStrings_DeleteAndFreeObject(_Strings: TStrings; _Idx: integer);

{: Free a TList object an all TObjects it contains
   NOTE: this function is obsolete, use contnrs.TObjectList instead! }
procedure TList_FreeWithItems(var _List: TList); deprecated; // use contnrs.TObjectList

{: Write a string to the stream
   @param Stream is the TStream to write to.
   @param s is the string to write
   @returns the number of bytes written. }
function TStream_WriteString(_Stream: TStream; const _s: string): integer;

{: Write a string to the stream appending CRLF
   @param Stream is the TStream to write to.
   @param s is the string to write
   @returns the number of bytes written. }
function TStream_WriteStringLn(_Stream: TStream; const _s: string): integer;

{: Read a line from a stream, that is, a string ending in CRLF
   @param Stream is the TStream to read from.
   @param s returns the read string.
   @returns the number of bytes read. }
function TStream_ReadStringLn(_Stream: TStream; out _s: string): integer;

{: Write formatted data to the stream appending CRLF
   @param Stream is the TStream to write to.
   @param Format is a format string as used in sysutils.format
   @param Args is an array of const as used in sysutils.format
   @returns the number of bytes written. }
function TStream_WriteFmtLn(_Stream: TStream; const _Format: string; _Args: array of const): integer;

{: returns the string which has the given value as Object
   @param Strings is the TStrings to search
   @param Obj is a pointer to match
   @param RaiseException is a boolean that controls whether an exception should
          be raised (if true) if the Obj cannot be found or an empty strin should
          be returned (if false), Default = true
   @returns the string whose object matches Obj or an empty string, if none
            was found and RaiseExeption was false
   @raises EObjectNotFound if a matching object was not found and RaiseException
                           is true. }
function TStrings_StringByObj(_Strings: TStrings; _Obj: pointer; _RaiseException: boolean = true): string;

{: determines the string which has the given value as Object
   @param Strings is the TStrings to search
   @param Obj is a pointer to match
   @param Value is the string whose object matches Obj, only valid if result is true
   @returns true, if a matching object was found, false otherwise }
function TStrings_TryStringByObj(_Strings: TStrings; _Obj: pointer; out _Value: string): boolean;

implementation

uses
  StrUtils,
  u_dzStringUtils;

procedure TList_FreeWithItems(var _List: TList);
var
  i: integer;
begin
  if Assigned(_List) then begin
    for i := 0 to _List.Count - 1 do
      TObject(_List[i]).Free;
    _List.Free;
    _List := nil;
  end;
end;

function TStrings_RemoveTrailingSpaces(_Strings: tStrings): boolean;
var
  i: integer;
  s: string;
  Trailing: boolean;
  p: integer;
  Len: integer;
begin
  Result := false;
  Trailing := true;
  for i := _Strings.Count - 1 downto 0 do begin
    s := _Strings[i];
    Len := Length(s);
    p := Len;
    while (p > 0) and (s[p] = ' ') do
      Dec(p);
    if Len <> p then begin
      Result := true;
      s := LeftStr(s, p);
    end;
    if Trailing and (s = '') then begin
      Result := true;
      _Strings.Delete(i);
    end else begin
      Trailing := false;
      _Strings[i] := s;
    end;
  end;
end;

function TStrings_FreeAllObjects(_Strings: TStrings): TStrings;
var
  i: Integer;
begin
  for i := 0 to _Strings.Count - 1 do
    _Strings.Objects[i].Free;
  Result := _Strings;
end;

procedure TStrings_FreeWithObjects(_Strings: TStrings);
begin
  TStrings_FreeAllObjects(_Strings).Free;
end;

procedure TStrings_DeleteAndFreeObject(_Strings: TStrings; _Idx: integer);
begin
  _Strings.Objects[_Idx].Free;
  _Strings.Delete(_Idx);
end;

function TStream_WriteString(_Stream: TStream; const _s: string): integer;
begin
  Result := _Stream.Write(pChar(_s)^, Length(_s));
end;

function TStream_WriteStringLn(_Stream: TStream; const _s: string): integer;
begin
  Result := TStream_WriteString(_Stream, _s);
  Result := Result + TStream_WriteString(_Stream, #13#10);
end;

function TStream_WriteFmtLn(_Stream: TStream; const _Format: string; _Args: array of const): integer;
begin
  Result := TStream_WriteStringLn(_Stream, Format(_Format, _Args));
end;

function TStream_ReadStringLn(_Stream: TStream; out _s: string): integer;
var
  OldPos: integer;
  EndString: integer;
  NewPos: integer;
  c: char;
begin
  // twm: this is not really efficient, because it reads single bytes, if it becomes a problem, optimize it ;-)
  OldPos := _Stream.Position;
  Assert(SizeOf(c) = 1, 'This works only with one byte characters!');
  EndString := OldPos;
  NewPos := OldPos;
  while true do begin
    if _Stream.Read(c, 1) = 0 then begin // end of file
      EndString := _Stream.Position;
      NewPos := EndString;
      break;
    end else if c = #13 then begin
      EndString := _Stream.Position - 1;
      if _Stream.Read(c, 1) = 0 then
        NewPos := _Stream.Position
      else if c = #10 then
        NewPos := _Stream.Position
      else
        NewPos := _Stream.Position - 1;
      break;
    end;
  end;
  Result := EndString - OldPos;
  SetLength(_s, Result);
  if Result <> 0 then begin
    _Stream.Position := OldPos;
    _Stream.Read(_s[1], Length(_s));
  end;
  _Stream.Position := NewPos;
end;

function TStrings_TryStringByObj(_Strings: TStrings; _Obj: pointer; out _Value: string): boolean;
var
  i: integer;
begin
  for i := 0 to _Strings.Count - 1 do
    if _Strings.Objects[i] = _Obj then begin
      _Value := _Strings[i];
      Result := true;
      exit;
    end;
  Result := false;
end;

function TStrings_StringByObj(_Strings: TStrings; _Obj: pointer; _RaiseException: boolean = true): string;
begin
  if not TStrings_TryStringByObj(_Strings, _Obj, Result) then begin
    if _RaiseException then
      raise EObjectNotFound.Create('no matching object found');
    Result := '';
  end;
end;

end.

