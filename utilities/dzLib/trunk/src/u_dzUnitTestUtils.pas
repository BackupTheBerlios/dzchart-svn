unit u_dzUnitTestUtils;

interface

uses
  SysUtils,
  Classes,
  TestFramework;

{$M+}
type
  {: implements some additional CheckXxx procedures, for now mainly for checking variants }
  TdzTestCase = class(TTestcase)
  protected
  public
    {: Checks whether the value is a variant of a floating point type (includes integer) }
    procedure CheckVariantIsFloat(_Value: variant; _Msg: string);
    {: Checks whether the value is a variant of an (signed) integer type (excludes LongWord and Int64) }
    procedure CheckVariantIsInteger(_Value: variant; _Msg: string);
    {: Checks whether the value is a variant of an (signed) int64 type (includes LongWord and Int64) }
    procedure CheckVariantIsInt64(_Value: variant; _Msg: string);
    {: Checks whether the value is a variant of an (unsigned) integer type (excludes all signed integer types) }
    procedure CheckVariantIsLongWord(_Value: variant; _Msg: string);
    {: Checks whether the value is a non Null variant }
    procedure CheckVariantIsNotNull(_Value: variant; _Msg: string); overload;
    {: Checks whether the value is a Null variant }
    procedure CheckVariantIsNull(_Value: variant; _Msg: string); overload;
    {: Checks whether the value is a variant of a string type }
    procedure CheckVariantIsString(_Value: variant; _Msg: string); overload;
    {: Called by the CheckVAriantIsXxxx functions to show errors }
    procedure FailNotVarType(const _Expected, _Actual, _Msg: string; _ErrorAddr: pointer);
    {: Called by the FailNotVarType function to generate the error message }
    function NotVarTypeErrorMessage(const _Expected: string; const _Actual: string; _Msg: string): string;
    {: Checks whether the date part of two TDateTime values is equal }
    procedure CheckEqualsDate(_Expected, _Actual: TDateTime; const _Message: string);
    {: Checks whether two TDateTime values are equal }
    procedure CheckEqualsDateTime(_Expected, _Actual: TDateTime; const _Message: string);
    {: Checks whether the time part of two TDateTime values is equal }
    procedure CheckEqualsTime(_Expected, _Actual: TDateTime; const _Message: string);
    {: Checks multiline strings for equality }
    procedure CheckEqualsMultiline(_Expected, _Actual: string; const _Message: string = '');
    {: Checks a multiline string against the content of a file }
    procedure CheckEqualsFile(const _Filename: string; _Actual: string; const _Message: string = '');
    {: checks whether two files have got the same contents, content is treated as a multiline string }
    procedure CheckEqualsFiles(const _ExpectedFile, _ActualFile: string; const _Message: string = '');
  end;

implementation

uses
  Variants,
  DateUtils,
  u_dzVariantUtils;

resourcestring
  StrSexpectedVariantT = '%sexpected variant type: <%s> but was: <%s>';

  { TdzTestCase }

procedure TdzTestCase.CheckVariantIsInt64(_Value: variant; _Msg: string);
begin
  if not VarIsInt64(_Value) then
    FailNotVarType('int64', VarTypeAsText(VarType(_Value)), _Msg, CallerAddr);
end;

procedure TdzTestCase.CheckVariantIsInteger(_Value: variant; _Msg: string);
begin
  if not VarIsInteger(_Value) then
    FailNotVarType('integer', VarTypeAsText(VarType(_Value)), _Msg, CallerAddr);
end;

procedure TdzTestCase.CheckVariantIsLongWord(_Value: variant; _Msg: string);
begin
  if not VarIsLongWord(_Value) then
    FailNotVarType('longword', VarTypeAsText(VarType(_Value)), _Msg, CallerAddr);
end;

procedure TdzTestCase.CheckEqualsDate(_Expected, _Actual: TDateTime; const _Message: string);
begin
  if not DateUtils.SameDate(_Expected, _Actual) then
    FailNotEquals(FloatToStr(_Expected), FloatToStr(_Actual), _Message, CallerAddr);
end;

procedure TdzTestCase.CheckEqualsDateTime(_Expected, _Actual: TDateTime;
  const _Message: string);
begin
  if not DateUtils.SameDateTime(_Expected, _Actual) then
    FailNotEquals(FloatToStr(_Expected), FloatToStr(_Actual), _Message, CallerAddr);
end;

procedure TdzTestCase.CheckEqualsFile(const _Filename: string; _Actual: string; const _Message: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(_Filename);
    CheckEqualsMultiline(sl.Text, _Actual, _Message);
  finally
    sl.Free;
  end;
end;

procedure TdzTestCase.CheckEqualsFiles(const _ExpectedFile, _ActualFile, _Message: string);
var
  sl: TStringList;
  Soll: string;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(_ExpectedFile);
    Soll := sl.Text;
    sl.Clear;
    sl.LoadFromFile(_ActualFile);
    CheckEqualsMultiline(Soll, sl.Text);
  finally
    sl.Free;
  end;
end;

procedure TdzTestCase.CheckEqualsMultiline(_Expected, _Actual: string; const _Message: string = '');
var
  ExpStrings: TStringList;
  ActStrings: TStringList;
  i: Integer;
begin
  ActStrings := nil;
  ExpStrings := TStringList.Create;
  try
    ActStrings := TStringList.Create;
    ExpStrings.Text := _Expected;
    ActStrings.Text := _Actual;
    CheckEquals(ExpStrings.Count, ActStrings.Count, _Message + ' (no of lines does not match)');
    for i := 0 to ExpStrings.Count - 1 do
      CheckEquals(ExpStrings[i], ActStrings[i], _Message + Format(' (line %d does not match)', [i]));
  finally
    ActStrings.Free;
    ExpStrings.Free;
  end;
end;

procedure TdzTestCase.CheckEqualsTime(_Expected, _Actual: TDateTime;
  const _Message: string);
begin
  if not DateUtils.SameTime(_Expected, _Actual) then
    FailNotEquals(FloatToStr(_Expected), FloatToStr(_Actual), _Message, CallerAddr);
end;

procedure TdzTestCase.CheckVariantIsFloat(_Value: variant; _Msg: string);
begin
  if not VarIsFloat(_Value) then
    FailNotVarType('float', VarTypeAsText(VarType(_Value)), _Msg, CallerAddr);
end;

procedure TdzTestCase.CheckVariantIsNotNull(_Value: variant; _Msg: string);
begin
  if VarIsNull(_Value) then
    FailNotVarType('not null', 'null', _Msg, CallerAddr);
end;

procedure TdzTestCase.CheckVariantIsNull(_Value: variant; _Msg: string);
begin
  if not VarIsNull(_Value) then
    FailNotVarType('null', VarTypeAsText(VarType(_Value)), _Msg, CallerAddr);
end;

procedure TdzTestCase.CheckVariantIsString(_Value: variant; _Msg: string);
begin
  if not VarIsStr(_Value) then
    FailNotVarType('string', VarTypeAsText(VarType(_Value)), _Msg, CallerAddr);
end;

procedure TdzTestCase.FailNotVarType(const _Expected, _Actual, _Msg: string; _ErrorAddr: pointer);
begin
  Fail(NotVarTypeErrorMessage(_Expected, _Actual, _Msg), _ErrorAddr);
end;

function TdzTestCase.NotVarTypeErrorMessage(const _Expected: string; const _Actual: string; _Msg: string): string;
begin
  if (_Msg <> '') then
    _Msg := _Msg + ', ';
  Result := Format(StrSexpectedVariantT, [_Msg, _Expected, _Actual])
end;

end.
