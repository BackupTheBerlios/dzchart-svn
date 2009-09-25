unit u_dzGuidUtils;

interface

uses
  SysUtils;

type
  TNullableGuid = record
  private
    FValue: TGuid;
    FIsValid: IInterface;
  public
    ///<summary> Generates a new GUID using WinAPI calls </summary>
    procedure GenerateNew;
    ///<summary> convert to a variant for using for database fields / parameters) </summary>
    function ToVariant: Variant;
    ///<summary> convert from a variant for assigning a database field </summary>
    function AssignVariant(_v: Variant): boolean;
    ///<summary> explicit cast to string "string(GUID)" converts to standard string form </summary>
    class operator Explicit(_a: TNullableGuid): string;
    ///<summary> compares two NullableGuids, returns true, if the are equal, raises exception if one
    ///          is not valid </summary>
    class operator Equal(_a, _b: TNullableGuid): boolean;
    ///<summary> compares two NullableGuids, returns truw if they are different or at least one is invalid </summary>
    class operator NotEqual(_a, _b: TNullableGuid): boolean;
    ///<summary> implicit conversion from TGUID </summary>
    class operator Implicit(_a: TGUID): TNullableGuid;
    ///<summary> returns the GUID, if valid, raises an exception otherwise </summary>
    function Value: TGuid;
    ///<summary> returns true, if valid, false otherwise </summary>
    function IsValid: boolean;
  end;

///<summary> Tries to convert a string to a GUID, returns true if successfull </summary>
function TryStr2GUID(const _s: string; out _GUID: TGUID): boolean;
///<summary> Tries to convert a variant to a GUID, returns true if successfull </summary>
function TryVar2GUID(const _v: variant; out _GUID: TGUID): boolean;

implementation

uses
  u_dzVariantUtils,
  ActiveX;

// this is a fake interfaced object that only exists as the VMT
// It can still be used to trick the compiler into believing an interface pointer is assigned

function NopAddref(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopRelease(inst: Pointer): Integer; stdcall;
begin
  Result := -1;
end;

function NopQueryInterface(inst: Pointer; const IID: TGUID; out Obj): HResult; stdcall;
begin
  Result := E_NOINTERFACE;
end;

const
  FlagInterfaceVTable: array[0..2] of Pointer =
    (
    @NopQueryInterface,
    @NopAddref,
    @NopRelease
    );
const
  FlagInterfaceInstance: Pointer = @FlagInterfaceVTable;

function GetNullableTypesFlagInterface: IInterface;
begin
  Result := IInterface(@FlagInterfaceInstance);
end;

{ TGuidEx }

function TryStr2GUID(const _s: string; out _GUID: TGUID): boolean;
begin
  Result := Succeeded(CLSIDFromString(PWideChar(WideString(_s)), _GUID));
end;

function TryVar2GUID(const _v: variant; out _GUID: TGUID): boolean;
var
  s: string;
begin
  Result := TryVar2Str(_v, s);
  if Result then
    Result := TryStr2GUID(s, _GUID);
end;

function TNullableGuid.AssignVariant(_v: Variant): boolean;
begin
  Result := TryVar2GUID(_v, FValue);
  if Result then
    FIsValid := GetNullableTypesFlagInterface
  else
    FIsValid := nil;
end;

function TNullableGuid.ToVariant: Variant;
begin
  Result := string(Self);
end;

function TNullableGuid.Value: TGuid;
begin
  if not Assigned(FIsValid) then
    raise Exception.Create('TNullableGuid is not valid');
  Result := FValue;
end;

class operator TNullableGuid.Equal(_a, _b: TNullableGuid): boolean;
begin
  Result := IsEqualGUID(_a.Value, _b.Value);
end;

class operator TNullableGuid.NotEqual(_a, _b: TNullableGuid): boolean;
begin
  if _a.IsValid and _b.IsValid then
    Result := not IsEqualGUID(_a.Value, _b.Value)
  else
    Result := _a.IsValid or _b.IsValid;
end;

class operator TNullableGuid.Explicit(_a: TNullableGuid): string;
begin
  if _a.IsValid then
    Result := GUIDToString(_a.FValue)
  else
    Result := '';
end;

procedure TNullableGuid.GenerateNew;
begin
  if Succeeded(CreateGUID(FValue)) then
    FIsValid := GetNullableTypesFlagInterface
  else
    FIsValid := nil;
end;

class operator TNullableGuid.Implicit(_a: TGUID): TNullableGuid;
begin
  Result.FValue := _a;
  Result.FIsValid := GetNullableTypesFlagInterface;
end;

function TNullableGuid.IsValid: boolean;
begin
  Result := Assigned(FIsValid);
end;

end.

