{: several utilty functions for Variants }
unit u_dzVariantUtils;

interface

uses
  SysUtils,
  Variants;

type
  {: raised if there is a conversion error in one of the Var2XxxEx functions }
  EVariantConvertError = class(Exception);
  {: raised if the variant passed to one of the Var2XxxEx functions is null }
  EVarIsNull = class(EVariantConvertError);
  {: raised if the variant passed to one of the Var2XxxEx functions is empty }
  EVarIsEmpty = class(EVariantConvertError);

{: converts a variant to its string representation (for debugging) }
function toString(_v: OleVariant): string; overload;

{: Checks whether a variant is a type that can be assigned to an integer (signed 32 bit),
   Note: Excludes longword and Int64, even if the value may be <= MaxLongInt }
function VarIsInteger(_v: variant): boolean;

{: Checks whether a variant is of a type that can be assigned to a longword (unsigned 32 bit),
   Note: Excludes signed integers, even if the value may be positive }
function VarIsLongWord(_v: variant): boolean;

{: Checks whether a variant is of a type that can be assigned to an Int64 (signed 64 bit) }
function VarIsInt64(_v: variant): boolean;

// Variant to other type conversion functions
// TryVar2Xxx converts from variant to type Xxx, returns false, if
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
function TryVar2Int(const _v: variant; out _Value: integer): boolean;

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
   @returns(true, if the variant could be converted to double, false if not) }
function TryVar2Dbl(const _v: variant; out _Value: double): boolean;

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

{: Converts a variant to a string
   If v is null or empty, it returns false.
   @param(v Variant value to convert)
   @param(Value is the variant's string value, only valid if the function
                returns true.)
   @returns(true, if the variant could be converted to double, false if not) }
function TryVar2Str(const _v: variant; out _Value: string): boolean;

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

implementation

uses
  u_dzConvertUtils;

resourcestring
  // Variant ist Null, sollte %s sein: %s
  STR_VARIANT_IS_NULL_SHOULD_BE_SS = 'Variant is Null, should be %s: %s';
  // Variant ist Empty, sollte %s sein: %s
  STR_VARIANT_IS_EMPTY_SHOULD_BE_SS = 'Variant is Empty, should be %s: %s';
  // Variant kann nicht nach %s konvertiert werden: %s
  STR_VARIANT_CAN_NOT_BE_CONVERTED_TO_S = 'Variant can not be converted to %s';
  STR_VARIANT_CAN_NOT_BE_CONVERTED_TO_SS = 'Variant can not be converted to %s: %s';

function toString(_v: OleVariant): string;
var
  i, j: Integer;
begin
  try
    case VarType(_v) of
      varEmpty: result := '<Empty>';
      varNull: result := '<Null>';
      varSmallint: result := VarToStr(_v);
      varInteger: result := VarToStr(_v);
      varSingle: result := VarToStr(_v);
      varDouble: result := VarToStr(_v);
      varCurrency: result := VarToStr(_v);
      varDate: result := VarToStr(_v);
      varOleStr: result := VarToStr(_v);
      varDispatch: result := VarToStr(_v);
      varString: result := VarToStr(_v);
      varArray: begin
          if VarArrayDimCount(_v) = 1 then begin
            for i := VarArrayLowBound(_v, 1) to VarArrayHighBound(_v, 1) do
              result := result + toString(_v[i]);
          end else if VarArrayDimCount(_v) = 2 then begin
            for i := VarArrayLowBound(_v, 1) to VarArrayHighBound(_v, 1) do
              for j := VarArrayLowBound(_v, 1) to VarArrayHighBound(_v, 1) do
                result := result + toString(_v[i, j]);
          end else
            result := '3dim-array not supported';
        end;
    else
      result := '<Unknown Type>';
    end;
    result := result + ' (' + VarTypeAsText(VarType(_v)) + ')';
  except
    on ex: Exception do
      result := result + '#ERROR: ' + ex.Message;
  end;
end;

function VarIsInteger(_v: variant): boolean;
begin
  Result := FindVarData(_V)^.VType in [varSmallInt, varInteger, varShortInt,
    varByte, varWord];
end;

function VarIsLongWord(_v: variant): boolean;
begin
  Result := FindVarData(_V)^.VType in [varByte, varWord, varLongWord];
end;

function VarIsInt64(_v: variant): boolean;
begin
  Result := FindVarData(_V)^.VType in [varSmallInt, varInteger, varShortInt,
    varByte, varWord, varLongWord, varInt64];
end;

function TryVar2Int(const _v: variant; out _Value: integer): boolean;
begin
  Result := not VarIsNull(_v) and not VarIsEmpty(_v);
  if Result then
    try
      _Value := _v;
    except
      on e: EVariantError do
        raise EVariantConvertError.CreateFmt(STR_VARIANT_CAN_NOT_BE_CONVERTED_TO_S, ['Integer']);
    end;
end;

function Var2Int(const _v: variant; _Default: integer): integer;
begin
  if not TryVar2Int(_v, Result) then
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
      raise EVariantConvertError.CreateFmt(STR_VARIANT_CAN_NOT_BE_CONVERTED_TO_SS, ['Integer', _Source]);
  end;
end;

function Var2IntStr(const _v: variant; const _NullValue: string = '*NULL*'): string;
var
  Value: integer;
begin
  if TryVar2Int(_v, Value) then
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
      raise EVariantConvertError.CreateFmt(STR_VARIANT_CAN_NOT_BE_CONVERTED_TO_SS, ['Date', _Source]);
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
    try
      _Value := _v;
    except
      on e: EVariantError do
        raise EVariantConvertError.CreateFmt(STR_VARIANT_CAN_NOT_BE_CONVERTED_TO_S, ['Double']);
    end;
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
      raise EVariantConvertError.CreateFmt(STR_VARIANT_CAN_NOT_BE_CONVERTED_TO_SS, ['Double', _Source]);
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

function TryVar2Str(const _v: variant; out _Value: string): boolean;
begin
  Result := not VarIsNull(_v) and not VarIsEmpty(_v);
  if Result then
    try
      _Value := _v;
    except
      on e: EVariantError do
        raise EVariantConvertError.CreateFmt(STR_VARIANT_CAN_NOT_BE_CONVERTED_TO_S, ['String']);
    end;
end;

function Var2Str(const _v: variant; const _Default: string): string;
begin
  if not TryVar2Str(_v, Result) then
    Result := _Default
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
      raise EVariantConvertError.CreateFmt(STR_VARIANT_CAN_NOT_BE_CONVERTED_TO_SS, ['String', _Source]);
  end;
end;

end.
