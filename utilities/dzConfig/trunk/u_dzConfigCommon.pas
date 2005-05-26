{: declares several interfaces central to the dzConfig package and
   a few utility functions }
unit u_dzConfigCommon;

interface

uses
  SysUtils,
  Classes,
{$IFDEF linux}
  QForms;
{$ENDIF linux}
{$IFDEF mswindows}
  Forms;
{$ENDIF mswindows}


const
  {: Prefix assumed for TRadioGroup control names }
  RADIOGROUP_PREFIX = 'rg_';
  {: Prefix assumed for TListBox control names }
  LISTBOX_PREFIX = 'lb_';
  {: Prefix assumed for TComboBox control names }
  COMBOBOX_PREFIX = 'cmb_';
  {: Prefix assumed for TCheckBox control names }
  CHECKBOX_PREFIX = 'chk_';
  {: Prefix assumed for TEdit control names }
  EDIT_PREFIX = 'ed_';
  {: Prefix assumed for TTabSheet control names }
  TABSHEET_PREFIX = 'ts_';
  {: Prefix assumed for TGroupBox control names }
  GROUPBOX_PREFIX = 'grp_';
  {: Prefix assumed for TPanel control names }
  PANEL_PREFIX = 'p_';

type
  {: Declares the storage type for a control
     csNone = isn't stored at all
     csString = content is stored as string
     csBoolean = content is stored as boolean
     csInteger = content is stored as integer }
  TdzConfigStorageType = (csNone, csString, csBoolean, csInteger);
  {: Declares a set of storage types for a control (used in
     IdzConfigControl.SupportedStorageTypes }
  TdzConfigStorageTypes = set of TdzConfigStorageType;

type
  {: Declares the interface for reading a configuration setting from permanent
     storage (e.g. an INI file, a database ...) }
  IdzConfigReader = interface ['{50E0AC5E-D3FC-D811-9297-000854097AB5}']
    {: Reads a boolean setting from the permanent storage, returns true if there was a value.
       @param(Path) is the setting's path
       @param(Name) is the setting's name
       @param(Value) returns the setting's value, if the function returns true,
                     undefined otherwise
       @returns true, if the setting has a value, false otherwise }
    function GetSetting(const _Path, _Name: string; var _Value: boolean): boolean; overload;
    {: Reads an integer setting from the permanent storage, returns true if there was a value.
       @param(Path) is the setting's path
       @param(Name) is the setting's name
       @param(Value) returns the setting's value, if the function returns true,
                     undefined otherwise
       @returns true, if the setting has a value, false otherwise }
    function GetSetting(const _Path, _Name: string; var _Value: integer): boolean; overload;
    {: Reads a string setting from the permanent storage, returns true if there was a value.
       @param(Path) is the setting's path
       @param(Name) is the setting's name
       @param(Value) returns the setting's value, if the function returns true,
                     undefined otherwise
       @returns true, if the setting has a value, false otherwise }
    function GetSetting(const _Path, _Name: string; var _Value: string): boolean; overload;
  end;

type
  {: Declares the interface for writing a configuration setting to permanent storage }
  IdzConfigWriter = interface ['{6C180A62-D3FC-D811-9297-000854097AB5}']
    {: Stores a boolean setting in the permanent storage
       @param(Path) is the path of the setting
       @param(Name) is the name of the setting
       @param(Value) is the value of the setting }
    procedure StoreSetting(const _Path, _Name: string; const _Value: boolean); overload;
    {: Stores an integer setting in the permanent storage
       @param(Path) is the path of the setting
       @param(Name) is the name of the setting
       @param(Value) is the value of the setting }
    procedure StoreSetting(const _Path, _Name: string; const _Value: integer); overload;
    {: Stores a string setting in the permanent storage
       @param(Path) is the path of the setting
       @param(Name) is the name of the setting
       @param(Value) is the value of the setting }
    procedure StoreSetting(const _Path, _Name: string; const _Value: string); overload;
  end;

type
  {: Declares the interface all dzConfig enabled controls must support }
  IdzConfigControl = interface ['{D2332877-D3FC-D811-9297-000854097AB5}']
    {: Reads the corresponding setting from the config reader.
       @param(Path) is the path to be used for the setting
       @param(Reader) is a IdzConfigReader interface for doing the actual
                      reading. }
    procedure ReadSetting(const _Path: string; const _Reader: IdzConfigReader);
    {: Writes the corresponding setting to the config writer.
       @param(Path) is the path to be used for the setting
       @param(Reader) is a IdzConfigReader interface for doing the actual
                      reading. }
    procedure WriteSetting(const _Path: string; const _Writer: IdzConfigWriter);
    {: setter method for ConfigStorageType property, allows only csNone and the storage
       types available for the control }
    procedure SetConfigStorageType(const _ConfigStorageType: TdzConfigStorageType);
    {: getter method for ConfigStorageType property }
    function GetConfigStorageType: TdzConfigStorageType;
    procedure SetConfigStorageName(const _ConfigStorageName: string);
    function GetConfigStorageName: string;
    function SupportedStorageTypes: TdzConfigStorageTypes;
    {: Only csNone and the storage types available for the control are allowed }
    property ConfigStorageType: TdzConfigStorageType read GetConfigStorageType write SetConfigStorageType;
    {: if not set, the component's name will be used }
    property ConfigStorageName: string read GetConfigStorageName write SetConfigStorageName;
  end;

type
  IdzConfigFormHandler = interface ['{B8163D67-D3FC-D811-9297-000854097AB5}']
    procedure SettingsToForm(const _Reader: IdzConfigReader; const _RootPath: string; _Form: TForm);
    procedure FormToSettings(const _Writer: IdzConfigWriter; const _RootPath: string; _Form: TForm);
  end;

  {: Checks whether a string starts with another string.
     @param(Start) is the string with which s should start
     @param(s) is the checked string
     @returns true, if s starts with Start, false otherwise }
function StartsWith(const _Start, _s: string): boolean;
{: Returns the substring from a given position to the end
   @param(s) is the whole string
   @param(Start) is the start position for the string to extract
   @returns the substring of s starting at Start }
function TailStr(const _s: string; _Start: integer): string;
{: Returns the substring up to a given position
   @param(s) is the whole string
   @param(Len) is the length of the substring to extract
   @returns the substring of s starting from the beginning up to the Len's character }
function LeftStr(const _s: string; _Len: integer): string;
{: Returns a string with the prefix removed
   @param(Prefix) is a string to remove from the front if it exists
   @param(CtrlName) is a control's name
   @returns CtrlName, if it does not start with Prefix, otherwise CtrlName
            with the Prefix removed. }
function RemoveCtrlPrefix(const _Prefix: string; _CtrlName: string): string;

{: Returns the current user's home directory $(HOME) }
function GetHomeDir: string;

implementation

{$IFDEF linux}
uses
  LibC;
{$ENDIF}

function LeftStr(const _s: string; _Len: integer): string;
begin
  Result := Copy(_s, 1, _Len);
end;

function StartsWith(const _Start, _s: string): boolean;
begin
  Result := AnsiSameStr(_Start, LeftStr(_s, Length(_Start)));
end;

function TailStr(const _s: string; _Start: integer): string;
begin
  if _Start > Length(_s) then
    Result := ''
  else
    Result := Copy(_s, _Start, Length(_s) - _Start + 1);
end;

function RemoveCtrlPrefix(const _Prefix: string; _CtrlName: string): string;
begin
  if StartsWith(_Prefix, _CtrlName) then
    Result := TailStr(_CtrlName, Length(_Prefix) + 1)
  else
    Result := _CtrlName;
end;

function GetHomeDir: string;
{$IFDEF linux}
var
  PwRec: PPasswordRecord;
begin
  PwRec := getpwuid(geteuid);
  Result := PwRec^.pw_dir;
end;
{$ENDIF}
{$IFDEF mswindows}
begin
  Result := GetEnvironmentVariable('HOME');
  if Result = '' then
    Result := GetEnvironmentVariable('HOMEDRIVE') + GetEnvironmentVariable('HOMEPATH');
end;
{$ENDIF}

end.

