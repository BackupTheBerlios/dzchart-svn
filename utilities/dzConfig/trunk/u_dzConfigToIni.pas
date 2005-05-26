unit u_dzConfigToIni;

interface

uses
  SysUtils,
  Classes,
  IniFiles,
  u_dzConfigCommon;

type
  {: Implements the IdzConfigWriter and IdzConfigReader interface
     for writing and reading configuration to and from an INI file }
  TConfigToIni = class(TInterfacedObject, IdzConfigWriter, IdzConfigReader)
  protected
    {: Stores the TIniFile object used for reading and writing }
    fIniFile: TIniFile;
  protected // IdzConfigWriter interface
    {: Stores a boolean setting in the INI file
       @param(Path) is the path of the setting
       @param(Name) is the name of the setting
       @param(Value) is the value of the setting }
    procedure StoreSetting(const _Path, _Name: string; const _Value: boolean); overload;
    {: Stores an integer setting in the INI file
       @param(Path) is the path of the setting
       @param(Name) is the name of the setting
       @param(Value) is the value of the setting }
    procedure StoreSetting(const _Path, _Name: string; const _Value: integer); overload;
    {: Stores a string setting in the INI file
       @param(Path) is the path of the setting
       @param(Name) is the name of the setting
       @param(Value) is the value of the setting }
    procedure StoreSetting(const _Path, _Name: string; const _Value: string); overload;
  protected // IdzConfigReader interface
    {: Reads a boolean setting from the INI file, returns true if there was a value.
       @param(Path) is the setting's path
       @param(Name) is the setting's name
       @param(Value) returns the setting's value, if the function returns true,
                     undefined otherwise
       @returns true, if the setting has a value, false otherwise }
    function GetSetting(const _Path, _Name: string; var _Value: boolean): boolean; overload;
    {: Reads an integer setting from the INI file, returns true if there was a value.
       @param(Path) is the setting's path
       @param(Name) is the setting's name
       @param(Value) returns the setting's value, if the function returns true,
                     undefined otherwise
       @returns true, if the setting has a value, false otherwise }
    function GetSetting(const _Path, _Name: string; var _Value: integer): boolean; overload;
    {: Reads a string setting from the INI file, returns true if there was a value.
       @param(Path) is the setting's path
       @param(Name) is the setting's name
       @param(Value) returns the setting's value, if the function returns true,
                     undefined otherwise
       @returns true, if the setting has a value, false otherwise }
    function GetSetting(const _Path, _Name: string; var _Value: string): boolean; overload;
  public
    {: Creates a TConfigToIni object reading and writing to the given file
       @param(Filename) is the name of the INI file to use, if an empty
                        string (default) is passed, the application's
                        filename with the extension .ini will be used.
                        Under Linux, any filename that is not absolute
                        (starts with a '/') is assumed to be in the users's
                        home directory and and additional '.' will be added
                        to the front (e.g. 'myini.ini' -> '~/.myini.ini') }
    constructor Create(_Filename: string = '');
    {: Destroys a TConfigToIni object }
    destructor Destroy; override;
  end;

implementation

uses
{$IFDEF linux}
  QForms;
{$ENDIF linux}
{$IFDEF mswindows}
  Forms;
{$ENDIF mswindows}

{ TConfigToIni }

constructor TConfigToIni.Create(_Filename: string = '');
begin
  inherited Create;
  if _Filename = '' then
    _Filename := ChangeFileExt(ExtractFileName(Application.ExeName), '.ini');
{$IFDEF linux}
  if not StartsWith('/', _Filename) then
    begin
      _Filename := IncludeTrailingPathDelimiter(GetHomeDir) + '.' + _Filename;
    end;
{$ENDIF linux}
{$IFDEF mswindows}
  if ExtractFileExt(_Filename) = '' then
    _Filename := ChangeFileExt(_Filename, '.ini');
{$ENDIF}
  fIniFile := TIniFile.Create(_Filename);
end;

destructor TConfigToIni.Destroy;
begin
  fIniFile.Destroy;
  inherited;
end;

procedure TConfigToIni.StoreSetting(const _Path, _Name: string; const _Value: boolean);
begin
  fIniFile.WriteBool(_Path, _Name, _Value);
end;

procedure TConfigToIni.StoreSetting(const _Path, _Name: string; const _Value: integer);
begin
  fIniFile.WriteInteger(_Path, _Name, _Value);
end;

procedure TConfigToIni.StoreSetting(const _Path, _Name, _Value: string);
begin
  fIniFile.WriteString(_Path, _Name, _Value);
end;

function TConfigToIni.GetSetting(const _Path, _Name: string; var _Value: boolean): boolean;
begin
  Result := fIniFile.ValueExists(_Path, _Name);
  if Result then
    _Value := fIniFile.ReadBool(_Path, _Name, false);
end;

function TConfigToIni.GetSetting(const _Path, _Name: string; var _Value: integer): boolean;
begin
  Result := fIniFile.ValueExists(_Path, _Name);
  if Result then
    _Value := fIniFile.ReadInteger(_Path, _Name, -1);
end;

function TConfigToIni.GetSetting(const _Path, _Name: string; var _Value: string): boolean;
begin
  Result := fIniFile.ValueExists(_Path, _Name);
  if Result then
    _Value := fIniFile.ReadString(_Path, _Name, '');
end;

end.

