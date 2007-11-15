{GXFormatter.config=twm}
{: Implements commonly used functions.
   This unit implements some commonly used functions.<br>
   There is also a NotImplemend procedure which should be called
   whereever some features are left out to be implemented "later"
   This procedure will not be available when we compile the
   shipping code (no DEBUG symbol), so the compiler should
   complain if it is still used by then.<br>
   <strong>note</strong>: String functions have been moved to dzStringfunc
   @author twm
}

unit u_dzMiscUtils;

{$I jedi.inc}

{ TODO -otwm : Move variant functions to u_dzVariants }

{$WARN SYMBOL_PLATFORM off}

interface

uses
  Variants,
  SysUtils,
  Windows,
  Registry;

type
  {: raised if there is a conversion error in one of the Xxx2YyyEx functions }
  EConvertError = class(Exception);

  {: raised if there is a conversion error in one of the Var2XxxEx functions }
  EVariantConvertError = class(EConvertError);

  {: raised if the variant passed to one of the Var2XxxEx functions is null }
  EVarIsNull = class(EVariantConvertError);

  {: raised if the variant passed to one of the Var2XxxEx functions is empty }
  EVarIsEmpty = class(EVariantConvertError);

  {: raised by Max([array of const]) and Min([array of const]) if the passed
     paramter is empty }
  EEmptyArray = class(Exception);

  EPathTooLong = class(Exception);

{$IFDEF debug}
  // do not remove the ifdef!!!
  ENotImplemented = class(exception);
{$ENDIF}

type
  TBooleanNames = array[boolean] of string;
const
  BOOLEAN_NAMES: TBooleanNames = ('false', 'true');

{: Emulates this infamous Visual Basic function of which nobody actually knows
   what it does.}
function TwipsPerPixelX(_Handle: hdc): Extended;

{: Emulates this infamous Visual Basic function of which nobody actually knows
   what it does.}
function TwipsPerPixelY(_Handle: hdc): Extended;

{: Returns the name for the HKey constant. }
function HKeyToString(_HKey: HKey): string;

{: Returns the name for the TRegDataType Value. }
function RegDataTypeToString(_DataType: TRegDataType): string;

{: returns a hex dump of the buffer (no spaces added)
   @param(Buffer is the memory block to dump)
   @param(Len is the length of the block)
   @returns(a string containing the hex dump of the buffer) }
function HexDump(const _Buffer; _Len: integer): string;

{: hex dumps a double value }
function HexDumpDouble(const _dbl: Double): string;

{: hex dumps an extended value }
function HexDumpExtended(const _ext: Extended): string;

{: returns a hex dump of the zero terminated string s }
function HexDumpString(const _s: string): string;

{: converts a hexdump of a double back to a double value }
procedure HexDumpToDbl(const _s: string; var _Value: double);

{: converts a hexdump of an extended back to an extended value }
procedure HexDumpToExtended(const _s: string; var _Value: Extended);

{: Converts an integer to a boolean.
   @param(Int is the integer to convert)
   @returns(false, if the integer is 0, true otherwise) }
function IntToBool(_Int: integer): boolean;

{: Converts a boolean to an integer.
   @param(B is the boolean to convert)
   @returns(0 if the boolean is false, 1 if it is true) }
function BoolToInt(_B: boolean): integer;

// Variant to other type conversion functions
// Var2XxxConditional converts from variant to type Xxx, returns false, if
// the variant is NULL.
// Var2Xxx converts from variant to type Xxx and returns the Default if the
// variant is NULL.
// Var2XxxEx converts from variant to type Xxx, but raises an exception if
// variant is NULL, using the Source for the message.

{: Converts a variant to an integer.
   If v is null or empty, it returns false
   @param(v Variant value to convert)
   @param(Value is the variants integer value, only valid if the function
          returns true.)
   @returns(true, if the variant could be converted to integer, false if not.) }
function Var2IntConditional(const _v: variant; out _Value: integer): boolean;

{: Converts a variant to an integer.
   If v is null or empty, it returns the Default.
   @param(v Variant value to convert)
   @param(Default Value to return if v is empty or null)
   @returns(the integer value of v or the Default if v can not be converted) }
function Var2Int(const _v: variant; _Default: integer): integer;

{: Converts a variant to an integer.
   Raises an exception if v can not be converted.
   @param(v Variant value to convert)
   @param(Source string to include in the exception message)
   @returns(the integer value of v)
   @raises(EVarIsNull if v is null)
   @raises(EVarIsEmpty if v is empty)
   @raises(EVariantConvertError if there is some other conversion error) }
function Var2IntEx(const _v: variant; const _Source: string): integer;

{: Converts a variant to the string representation of an integer.
   If v is null or empty, it returns the NullValue.
   @param(v Variant value to convert)
   @param(NullValue String value to return if v is empty or null)
   @returns(the string representation of the integer value of v or the
            NullValue if v can not be converted) }
function Var2IntStr(const _v: variant; const _NullValue: string = '*NULL*'): string;

{: Converts a variant to a double.
   If v is null or empty, it returns false.
   @param(v Variant value to convert)
   @param(Value is the variant's double value, only valid if the function
                returns true.)
   @returns(true, if the varian could be converted to double, false if not) }
function TryVar2Dbl(const _v: variant; out _Value: double): boolean;
function Var2DblConditional(const _v: variant; out _Value: double): boolean; deprecated; // use TryVar2Dbl

{: Converts a variant to a double.
   If v is null or empty, it returns the Default.
   @param(v Variant value to convert)
   @param(Default Value to return if v is empty or null)
   @returns(the double value of v or the Default if v can not be converted) }
function Var2Dbl(const _v: variant; const _Default: double): double;

{: Converts a variant to a double.
   Raises an exception if v can not be converted.
   @param(v Variant value to convert)
   @param(Source string to include in the exception message)
   @returns(the double value of v)
   @raises(EVarIsNull if v is null)
   @raises(EVarIsEmpty if v is empty)
   @raises(EVariantConvertError if there is some other conversion error) }
function Var2DblEx(const _v: variant; const _Source: string): double;

{: Converts a variant to the string representation of a double.
   If v is null or empty, it returns the Default.
   It uses Float2Str (not FloatToStr) with a '.' as decimal separator.
   @param(v Variant value to convert)
   @param(NullValue String value to return if v is empty or null)
   @returns(the string representation of the double value of v or the
            NullValue if v can not be converted) }
function Var2DblStr(const _v: variant; const _NullValue: string = '*NULL*'): string;

{: Converts a variant to a TDateTime.
   Raises an exception if v can not be converted.
   @param(v Variant value to convert)
   @param(Source string to include in the exception message)
   @returns(the TDateTime value of v)
   @raises(EVarIsNull if v is null)
   @raises(EVarIsEmpty if v is empty)
   @raises(EVariantConvertError if there is some other conversion error) }
function Var2DateTimeEx(const _v: variant; const _Source: string): TDateTime;

{: Converts a variant to an ISO format DateTime string (yyyy-mm-dd hh:mm:ss)
   @param(v Variant value to convert)
   @param(NullValue String value to return if v is empty or null)
   @returns(an ISO format DateTime string of v or NullValue if v can not be converted) }
function Var2DateTimeStr(const _v: variant; const _NullValue: string = '*NULL*'): string;

{: Converts a variant to a string.
   If v is null or empty, it returns the Default.
   @param(v Variant value to convert)
   @param(Default Value to return if v is empty or null)
   @returns(the string value of v or the Default if v can not be converted) }
function Var2Str(const _v: variant; const _Default: string = '*NULL*'): string;

{: Converts a variant to a string.
   Raises an exception if v can not be converted.
   @param(v Variant value to convert)
   @param(Source string to include in the exception message)
   @returns(the string value of v)
   @raises(EVarIsNull if v is null)
   @raises(EVarIsEmpty if v is empty)
   @raises(EVariantConvertError if there is some other conversion error) }
function Var2StrEx(_v: variant; const _Source: string): string;

{: Uses GetLastError to get the last WinAPI error code, then
   calls SysErrorMessage to get the corresponding error string,
   optionally takes a format string.
   @param(Error is the error string, only valid if error code <> 0)
   @param(Format The Format string to use. It must have %d and %s in it, to
                 change the order, use %0:d and %1:s, e.g. 'Error %1:s (%0:d)'
                 %d is replaced by the error code and %s is replaced by the
                 error message string.
                 If no format string is given Error will just contain the
                 Windows error message.)
   @returns(the error code) }
function GetLastOsError(out _Error: string; const _Format: string = ''): DWORD;

{: Similar to SysUtils.Win32Check, but does not raise an exception. Instead
   it returns the error message. The function optionally takes a format string.
   @param RetVal is the return value of a WinAPI function
   @param ErrorCode is the error code returned by GetLastError
   @param Error is the error message corresponding to the error code (only valid if result <> 0)
   @param Format The Format string to use. It must have %d and %s in it, to
                 change the order, use %0:d and %1:s, e.g. 'Error %1:s (%0:d)'
                 %d is replaced by the error code and %s is replaced by the
                 error message string.
                 If no format string is given Error will just contain the
                 Windows error message.
   @Returns the error code }
function Win32CheckEx(_RetVal: BOOL; out _ErrorCode: DWORD; out _Error: string; const _Format: string = ''): BOOL;

{: Same as VCL RaiseLastWin32Error but can specify a format.
   This procedure does the same as the VCL RaiseLastWin32Error but you can
   specify a format string to use. With this string you can provide some
   additional information about where the error occured.
   It calls GetLastError to get the result code of the last Win32 api function.
   If it returns non 0 the function uses SysErrorMessage to retrieve an error
   message for the error code and raises raises an EWin32Error exception
   (to be compatible with the VCL function) with the Error message.
   NOTE: Do not pass a resource string as format parameter, since loading this
         string will reset the error code returned by GetLastError, so
         you always get 0. Use the overloaded Version that takes the error code
         as parameter and get it before using the resource string if you want that.
   @param Format The Format string to use. It must have %d and %s in it, to
                 change the order, use %0:d and %1:s, e.g. 'Error %1:s (%0:d)'
                 %d is replaced by the error code and %s is replaced by the
                 error message string. }
procedure RaiseLastOsErrorEx(const _Format: string); overload;

{: Same as VCL RaiseLastWin32Error but can specify a format.
   This procedure does the same as the VCL RaiseLastWin32Error but you can
   specify a format string to use. With this string you can provide some
   additional information about where the error occured.
   If it returns non 0 the function uses SysErrorMessage to retrieve an error
   message for the error code and raises raises an EWin32Error exception
   (to be compatible with the VCL function) with the Error message.
   NOTE: If you pass a resource string as format parameter make sure you
         call GetLastError before referencing the resource string, otherwise
         loading the string will reset the error code returned by GetLastError, so
         you always get 0.
   @param ErrorCode is an error code returned from GetLastWin32Error
   @param Format The Format string to use. It must have %d and %s in it, to
                 change the order, use %0:d and %1:s, e.g. 'Error %1:s (%0:d)'
                 %d is replaced by the error code and %s is replaced by the
                 error message string. }
procedure RaiseLastOsErrorEx(_ErrorCode: integer; _Format: string); overload;

//{: Determines the greater of two integer values.
//   @Returns the greater of A and B. }
//function Max(_a, _b: integer): integer; overload;
//
//{: Determines the greater of two double values.
//   @Returns the greater of A and B. }
//function Max(_a, _b: double): double; overload;
//
//{: Determines the greater of two int64 values.
//   @Returns the greater of A and B. }
//function Max(_a, _b: int64): int64; overload;

//{: Determines the lesser of two integer values.
//   @Returns(the lesser of A and B.) }
//
//function Min(_a, _b: integer): integer; overload;
//
//{: Determines the lesser of two double values.
//   @Returns(the lesser of A and B.) }
//
//function Min(_a, _b: double): double; overload;
//
//{: Determines the lesser of two int64 values.
//   @Returns(the lesser of A and B.) }
//
//function Min(_a, _b: int64): int64; overload;

{: Combines WriteLn with Format
   @param(FormatStr string describing the format)
   @param(Args constant array with the arguments) }
procedure WriteFmtLn(const _FormatStr: string; _Args: array of const);

{: splits a wildcard into its components: The path and the filemask
   @param(Wildcard is a string specifying the wildcard)
   @param(Path is a string returning the path part of the wildcard)
   @param(Mask is a string returning the Mask part of the wildcard)
   @returns(true, if the Path exists, false otherwise) }
function SplitWildcard(_Wildcard: string; out _Path, _Mask: string): boolean;

{: returns the string's reference counter, pass a string by typecasting it
   to a pointer to avoid an additional increment of the reference counter }
function GetStringRefCount(_s: pointer): integer;

{$IFDEF debug}
// do NOT remove, this is a sanity check so we don't ship anything where there are
// missing features.
procedure NotImplemented;
{$ENDIF debug}

implementation

uses
{$IFDEF debug}
  Controls,
  Dialogs,
{$ENDIF}
  FileCtrl,
  StrUtils,
  u_dzStringUtils,
  u_dzConvertUtils;

resourcestring
  // Variant ist Null, sollte %s sein: %s
  STR_VARIANT_IS_NULL_SHOULD_BE_SS = 'Variant is Null, should be %s: %s';
  // Variant ist Empty, sollte %s sein: %s
  STR_VARIANT_IS_EMPTY_SHOULD_BE_SS = 'Variant is Empty, should be %s: %s';
  // Variant kann nicht nach %s konvertiert werden: %s
  STR_VARAINT_CAN_NOT_BE_CONVERTED_TO_SS = 'Variant can not be converted to %s: %s';
{$IFDEF delphi7up}
  // Unbekannter Betriebssystem Fehler
  STR_UNKNOWN_WIN32_ERROR = 'unknown OS error';
{$ELSE}
  // Unbekannter Win32 Fehler
  STR_UNKNOWN_WIN32_ERROR = 'unknown Win32 error';
{$ENDIF delphi7up}

{$IFDEF debug}

procedure NotImplemented;
begin
  if mrAbort = MessageDlg('Function not implemented!', mtWarning, [mbAbort, mbIgnore], 0) then
    raise ENotImplemented.Create('Function not implemented');
end;
{$ENDIF debug}

function TwipsPerPixelX(_Handle: hdc): Extended;
var
  Pixels: integer;
begin
  Pixels := GetDeviceCaps(_Handle, LOGPIXELSX);
  if Pixels = 0 then
    Result := 0
  else
    result := 1440 / Pixels;
end;

function TwipsPerPixelY(_Handle: hdc): Extended;
var
  Pixels: integer;
begin
  Pixels := GetDeviceCaps(_Handle, LOGPIXELSY);
  if Pixels = 0 then
    Result := 0
  else
    result := 1440 / Pixels;
end;

function HKeyToString(_HKey: HKey): string;
begin
  case _HKey of
    HKEY_CLASSES_ROOT: result := 'HKEY_CLASSES_ROOT';
    HKEY_CURRENT_USER: result := 'HKEY_CURRENT_USER';
    HKEY_LOCAL_MACHINE: result := 'HKEY_LOCAL_MACHINE';
    HKEY_USERS: result := 'HKEY_USERS';
    HKEY_PERFORMANCE_DATA: result := 'HKEY_PERFORMANCE_DATA';
    HKEY_CURRENT_CONFIG: result := 'HKEY_CURRENT_CONFIG';
    HKEY_DYN_DATA: result := 'HKEY_DYN_DATA';
  else
    Result := 'unknown Registry Root Key';
  end;
end;

function RegDataTypeToString(_DataType: TRegDataType): string;
begin
  case _DataType of
    rdUnknown: Result := 'Unknown';
    rdString: Result := 'String';
    rdExpandString: Result := 'ExpandString';
    rdInteger: Result := 'Integer';
    rdBinary: Result := 'Binary';
  else
    Result := 'unknown RegDataType';
  end;
end;

function IntToBool(_Int: integer): boolean;
begin
  Result := (_Int <> 0);
end;

function BoolToInt(_B: boolean): integer;
begin
  if _B then
    Result := 1
  else
    Result := 0;
end;

function Var2IntConditional(const _v: variant; out _Value: integer): boolean;
begin
  Result := not VarIsNull(_v) and not VarIsEmpty(_v);
  if Result then
    _Value := _v;
end;

function Var2Int(const _v: variant; _Default: integer): integer;
begin
  if not Var2IntConditional(_v, Result) then
    Result := _Default;
end;

function Var2IntEx(const _v: variant; const _Source: string): integer;
begin
  if VarIsNull(_v) then
    raise EVarIsNull.CreateFmt(STR_VARIANT_IS_NULL_SHOULD_BE_SS, ['Integer', _Source]);
  if VarIsEmpty(_v) then
    raise EVarIsEmpty.CreateFmt(STR_VARIANT_IS_EMPTY_SHOULD_BE_SS, ['Integer', _Source]);
  try
    Result := _v;
  except
    on e: EVariantError do
      raise EVariantConvertError.CreateFmt(STR_VARAINT_CAN_NOT_BE_CONVERTED_TO_SS, ['Integer', _Source]);
  end;
end;

function Var2IntStr(const _v: variant; const _NullValue: string = '*NULL*'): string;
var
  Value: integer;
begin
  if Var2IntConditional(_v, Value) then
    Result := IntToStr(Value)
  else
    Result := _NullValue;
end;

function Var2DateTimeEx(const _v: variant; const _Source: string): TDateTime;
begin
  if VarIsNull(_v) then
    raise EVarIsNull.CreateFmt(STR_VARIANT_IS_NULL_SHOULD_BE_SS, ['Date', _Source]);
  if VarIsEmpty(_v) then
    raise EVarIsEmpty.CreateFmt(STR_VARIANT_IS_EMPTY_SHOULD_BE_SS, ['Date', _Source]);
  try
    Result := _v;
  except
    on e: EVariantError do
      raise EVariantConvertError.CreateFmt(STR_VARAINT_CAN_NOT_BE_CONVERTED_TO_SS, ['Date', _Source]);
  end;
end;

function Var2DateTimeStr(const _v: variant; const _NullValue: string = '*NULL*'): string;
var
  Value: TDateTime;
begin
  if VarIsNull(_v) or VarIsEmpty(_v) then
    Result := _NullValue
  else
    try
      Value := _v;
      Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', Value);
    except
      Result := _NullValue;
    end;
end;

function TryVar2Dbl(const _v: variant; out _Value: double): boolean;
begin
  Result := not VarIsNull(_v) and not VarIsEmpty(_v);
  if Result then
    _Value := _v;
end;

function Var2DblConditional(const _v: variant; out _Value: double): boolean;
begin
  Result := TryVar2Dbl(_v, _Value);
end;

function Var2Dbl(const _v: variant; const _Default: double): double;
begin
  if not TryVar2Dbl(_v, Result) then
    Result := _Default
end;

function Var2DblEx(const _v: variant; const _Source: string): double;
begin
  if VarIsNull(_v) then
    raise EVarIsNull.CreateFmt(STR_VARIANT_IS_NULL_SHOULD_BE_SS, ['Double', _Source]);
  if VarIsEmpty(_v) then
    raise EVarIsEmpty.CreateFmt(STR_VARIANT_IS_EMPTY_SHOULD_BE_SS, ['Double', _Source]);
  try
    Result := _v;
  except
    on e: EVariantError do
      raise EVariantConvertError.CreateFmt(STR_VARAINT_CAN_NOT_BE_CONVERTED_TO_SS, ['Double', _Source]);
  end;
end;

function Var2DblStr(const _v: variant; const _NullValue: string = '*NULL*'): string;
var
  Value: double;
begin
  if TryVar2Dbl(_v, Value) then
    Result := Float2Str(Value)
  else
    Result := _NullValue;
end;

function Var2Str(const _v: variant; const _Default: string): string;
begin
  if VarIsNull(_v) or VarIsEmpty(_v) then
    Result := _Default
  else
    Result := _v;
end;

function Var2StrEx(_v: variant; const _Source: string): string;
begin
  if VarIsNull(_v) then
    raise EVarIsNull.CreateFmt(STR_VARIANT_IS_NULL_SHOULD_BE_SS, ['String', _Source]);
  if VarIsEmpty(_v) then
    raise EVarIsEmpty.CreateFmt(STR_VARIANT_IS_EMPTY_SHOULD_BE_SS, ['String', _Source]);
  try
    Result := _v;
  except
    on e: EVariantError do
      raise EVariantConvertError.CreateFmt(STR_VARAINT_CAN_NOT_BE_CONVERTED_TO_SS, ['String', _Source]);
  end;
end;

procedure RaiseLastOsErrorEx(const _Format: string);
begin
  RaiseLastOsErrorEx(GetLastError, _Format);
end;

procedure RaiseLastOsErrorEx(_ErrorCode: integer; _Format: string); overload;
var
  Error: EOSError;
begin
  if _ErrorCode <> ERROR_SUCCESS then
    Error := EOSError.CreateFmt(_Format, [_ErrorCode, SysErrorMessage(_ErrorCode)])
  else
    Error := EOsError.CreateFmt(_Format, [_ErrorCode, STR_UNKNOWN_WIN32_ERROR]);
  Error.ErrorCode := _ErrorCode;
  raise Error;
end;

function GetLastOsError(out _Error: string; const _Format: string = ''): DWORD;
var
  s: string;
begin
  Result := GetLastError;
  if Result <> ERROR_SUCCESS then
    s := SysErrorMessage(Result)
  else
    s := STR_UNKNOWN_WIN32_ERROR;
  if _Format <> '' then
    try
      _Error := Format(_Format, [Result, s])
    except
      _Error := s;
    end else
    _Error := s;
end;

function Win32CheckEx(_RetVal: BOOL; out _ErrorCode: DWORD; out _Error: string;
  const _Format: string = ''): BOOL;
begin
  Result := _RetVal;
  if not Result then
    _ErrorCode := GetLastOsError(_Error, _Format);
end;

// Irgendwie doof, dass man das dreimal implementieren muss, nur weil der
// Typ verschieden ist. Habe ich mal erwaehnt, dass C macros doch ganz nett
// sein koennen? ;-)

//function Max(_a, _b: integer): integer;
//begin
//  if _a > _b then
//    Result := _a
//  else
//    Result := _b;
//end;
//
//function Max(_a, _b: double): double;
//begin
//  if _a > _b then
//    Result := _a
//  else
//    Result := _b;
//end;
//
//function Max(_a, _b: int64): int64;
//begin
//  if _a > _b then
//    Result := _a
//  else
//    Result := _b;
//end;

// und jetzt dasselbe nochmal fuer Min ...

//function Min(_a, _b: integer): integer;
//begin
//  if _a < _b then
//    Result := _a
//  else
//    Result := _b;
//end;
//
//function Min(_a, _b: double): double;
//begin
//  if _a < _b then
//    Result := _a
//  else
//    Result := _b;
//end;
//
//function Min(_a, _b: int64): int64;
//begin
//  if _a < _b then
//    Result := _a
//  else
//    Result := _b;
//end;

procedure WriteFmtLn(const _FormatStr: string; _Args: array of const);
begin
  WriteLn(Format(_FormatStr, _Args));
end;

function SplitWildcard(_Wildcard: string; out _Path, _Mask: string): boolean;
var
  i: integer;
  MaskFound: boolean;
begin
  if _Wildcard = '' then begin
    _Path := '.';
    _Mask := '';
    Result := true;
    exit;
  end;
  MaskFound := false;
  i := Length(_Wildcard);
  while i > 0 do begin
    if _Wildcard[i] in ['*', '?'] then
      MaskFound := true;
    if _Wildcard[i] = '\' then begin
      if MaskFound or not DirectoryExists(_Wildcard) then begin
              // if we had a mask, this is easy, just split the wildcard at position i
              // if there was no mask, and the whole thing is not directory,
              // split at position i
        _Mask := TailStr(_Wildcard, i + 1);
        _Path := LeftStr(_Wildcard, i - 1);
        Result := DirectoryExists(_Path);
      end else begin
              // there was no mask and the whole thing is a directory
        Result := true;
        _Path := _Wildcard;
        _Mask := '';
      end;
      exit;
    end;
    Dec(i);
  end;

  // we found no backslash in the whole thing, so this could either
  // be a file or a subdirectory in the current directory.

  // if there was a mask, or the thing is not a directory, it is a file in
  // the current directory
  if MaskFound or not DirectoryExists(_Wildcard) then begin
    _Path := '.';
    _Mask := _Wildcard;
    Result := true;
  end else begin
      // otherwise it is a subdirectory
    _Path := _Wildcard;
    _Mask := '';
    Result := true;
  end;
end;

function iif(_Cond: boolean; _IfTrue: integer; _IfFalse: integer): integer; overload;
begin
  if _Cond then
    Result := _IfTrue
  else
    Result := _IfFalse;
end;

function iif(_Cond: boolean; const _IfTrue: string; const _IfFalse: string): string; overload;
begin
  if _Cond then
    Result := _IfTrue
  else
    Result := _IfFalse;
end;

function iif(_Cond: boolean; const _IfTrue: double; const _IfFalse: double): double; overload;
begin
  if _Cond then
    Result := _IfTrue
  else
    Result := _IfFalse;
end;

function iif(_Cond: boolean; const _IfTrue: char; const _IfFalse: char): char; overload;
begin
  if _Cond then
    Result := _IfTrue
  else
    Result := _IfFalse;
end;

function HexDumpString(const _s: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to Length(_s) do begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + Long2Hex2(Ord(_s[i]));
  end;
end;

function HexDump(const _Buffer; _Len: integer): string;
type
  PByte = ^Byte;
var
  i: integer;
  p: PByte;
begin
  p := @_Buffer;
  Result := '';
  for i := 0 to _Len - 1 do begin
    Result := Result + Long2Hex2(p^);
    Inc(p);
  end;
end;

function HexDumpDouble(const _dbl: Double): string;
begin
  Result := HexDump(_dbl, SizeOf(Double));
end;

function HexDumpExtended(const _ext: Extended): string;
begin
  Result := HexDump(_Ext, SizeOf(Extended));
end;

procedure HexDumpToDbl(const _s: string; var _Value: double);
type
  TBuffer = array[0..SizeOf(_Value)] of byte;
var
  i: integer;
  dec: LongWord;
  p: ^TBuffer;
begin
  Assert(Length(_s) = SizeOf(_Value) * 2);
  p := @_Value;
  for i := 0 to SizeOf(_Value) - 1 do begin
    Dec := Hex2Long(Copy(_s, i * 2 + 1, 2));
    p^[i] := Dec;
  end;
end;

procedure HexDumpToExtended(const _s: string; var _Value: Extended);
type
  TBuffer = array[0..SizeOf(_Value)] of byte;
var
  i: integer;
  dec: LongWord;
  p: ^TBuffer;
begin
  Assert(Length(_s) = SizeOf(_Value) * 2);
  p := @_Value;
  for i := 0 to SizeOf(_Value) - 1 do begin
    Dec := Hex2Long(Copy(_s, i * 2 + 1, 2));
    p^[i] := Dec;
  end;
end;

type
  PStringDescriptor = ^TStringDescriptor;
  TStringDescriptor = record
    RefCount: integer;
    Size: integer;
  end;

function GetStringRefCount(_s: pointer): integer;
var
  desc: PStringDescriptor;
begin
  if _s <> nil then begin
    desc := pointer(integer(_s) - 8);
    Result := desc.RefCount;
  end else
    Result := 0;
end;

{$IFNDEF delphi7up}

function ExcludeTrailingPathDelimiter(const _s: string): string;
begin
  Result := ExcludeTrailingBackslash(_s);
end;

function IncludeTrailingPathDelimiter(const _s: string): string;
begin
  Result := IncludeTrailingBackslash(_s);
end;
{$ENDIF}

end.

