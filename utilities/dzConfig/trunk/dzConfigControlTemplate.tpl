{$IFNDEF DZ_CONFIG_CONTROL_TEMPLATE}
unit dzConfigControlTemplate;

interface

uses
  Classes,
  u_dzAutoConfigCommon;

type
  _DZ_CONFIG_CONTROL_ANCESTOR_ = TControl;
const
  _DZ_CONFIG_CONTROL_STORAGE_TYPE_ = csInteger;
  _DZ_CONFIG_CONTROL_STORAGE_TYPES_: TdzConfigStorageTypes = [csInteger];
  _DZ_CONFIG_CONTROL_PREFIX_ = 'grmpf_';

{$ENDIF DZ_CONFIG_CONTROL_TEMPLATE}

{$IFNDEF DZ_CONFIG_CONTROL_TEMPLATE_SECOND_PASS}
                                        
type
  _DZ_CONFIG_CONTROL_ = class(_DZ_CONFIG_CONTROL_ANCESTOR_, IdzConfigControl)
  protected
    fConfigStorageType: TdzConfigStorageType;
    fConfigStorageName: string;
    procedure ReadSetting(const _Path: string; const _Reader: IdzConfigReader); virtual; abstract;
    procedure WriteSetting(const _Path: string; const _Writer: IdzConfigWriter); virtual; abstract;
    {: setter method for ConfigStorageType property, allows only csNone and the storage
       types available for the control }
    procedure SetConfigStorageType(const _ConfigStorageType: TdzConfigStorageType); virtual;
    {: getter method for ConfigStorageType property }
    function GetConfigStorageType: TdzConfigStorageType; virtual;
    procedure SetConfigStorageName(const _ConfigStorageName: string); virtual;
    function GetConfigStorageName: string; virtual;
    function SupportedStorageTypes: TdzConfigStorageTypes; virtual;
    {: Only csNone and the storage types available for the control are allowed }
    property ConfigStorageType: TdzConfigStorageType read GetConfigStorageType write SetConfigStorageType;
    {: if not set, the component's name will be used }
    property ConfigStorageName: string read GetConfigStorageName write SetConfigStorageName;
  public
    constructor Create(_Owner: TComponent); override;
  end;

{$ENDIF DZ_CONFIG_CONTROL_TEMPLATE_SECOND_PASS}

{$IFNDEF DZ_CONFIG_CONTROL_TEMPLATE}
implementation
{$DEFINE DZ_CONFIG_CONTROL_TEMPLATE_SECOND_PASS}
{$ENDIF DZ_CONFIG_CONTROL_TEMPLATE}

{$IFDEF DZ_CONFIG_CONTROL_TEMPLATE_SECOND_PASS}

constructor _DZ_CONFIG_CONTROL_.Create(_Owner: TComponent);
begin
  inherited;
  fConfigStorageType := _DZ_CONFIG_CONTROL_STORAGE_TYPE_;
end;

function _DZ_CONFIG_CONTROL_.GetConfigStorageName: string;
var
  Len: integer;
begin
  Result := fConfigStorageName;
  if Result = '' then
    begin
      Result := Name;
      Len := Length(_DZ_CONFIG_CONTROL_PREFIX_);
      if Copy(Result, 1, Len) = _DZ_CONFIG_CONTROL_PREFIX_ then
        Result := Copy(Result, Len + 1, 255);
    end;
end;

procedure _DZ_CONFIG_CONTROL_.SetConfigStorageName(const _ConfigStorageName: string);
begin
  fConfigStorageName := _ConfigStorageName;
end;

function _DZ_CONFIG_CONTROL_.GetConfigStorageType: TdzConfigStorageType;
begin
  Result := fConfigStorageType;
end;

procedure _DZ_CONFIG_CONTROL_.SetConfigStorageType(const _ConfigStorageType: TdzConfigStorageType);
begin
  if _ConfigStorageType in SupportedStorageTypes + [csNone] then
    fConfigStorageType := _ConfigStorageType;
end;

function _DZ_CONFIG_CONTROL_.SupportedStorageTypes: TdzConfigStorageTypes;
begin
  Result := _DZ_CONFIG_CONTROL_STORAGE_TYPES_;
end;


{$WARNINGS off}
{$IFNDEF DZ_CONFIG_CONTROL_TEMPLATE}
end.
{$ENDIF DZ_CONFIG_CONTROL_TEMPLATE}
{$ENDIF DZ_CONFIG_CONTROL_TEMPLATE_SECOND_PASS}
{$DEFINE DZ_CONFIG_CONTROL_TEMPLATE_SECOND_PASS}
