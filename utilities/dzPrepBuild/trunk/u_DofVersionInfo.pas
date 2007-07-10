unit u_DofVersionInfo;

interface

uses
  SysUtils,
  IniFiles,
  i_VersionInfo,
  u_IniVersionInfo;

type
  {: Tries to read a <projectname>.dof file, succeeds, if it exists and IncludeVerInfo is <> 0
     @param Project is the project name (*.dpr file without extension)
     @param VersionInfo is a TVersionInfoRec record which will be filled with the version info }
  TDofVersionInfo = class(TIniVersionInfo, IVersionInfo)
  public
    constructor Create(const _ProjectName: string);
  end;

implementation

const
  VERSION_INFO_SECTION = 'Version Info';
  VERSION_INFO_KEYS_SECTION = 'Version Info Keys';

{ TDofVersionInfo }

constructor TDofVersionInfo.Create(const _ProjectName: string);
begin
  inherited Create(_ProjectName + '.dof', VERSION_INFO_SECTION, VERSION_INFO_KEYS_SECTION);
  if FIniFile.ReadInteger(VERSION_INFO_SECTION, 'IncludeVerInfo', 0) <> 1 then
    raise ENoVersionInfo.Create('.dof file does not contain version info');
end;

end.

