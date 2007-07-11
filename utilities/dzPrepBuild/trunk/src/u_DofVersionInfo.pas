unit u_DofVersionInfo;

interface

uses
  SysUtils,
  IniFiles,
  i_VersionInfo,
  u_IniVersionInfo;

type
  {: This is a specialized versoin of TIniVersionInfo which readds a
     <projectname>.dof file, that was used by Delphi up to version 7. }
  TDofVersionInfo = class(TIniVersionInfo, IVersionInfo)
  private
    FProjectName: string;
  protected
    function VerInfoFilename: string;
  public
    {: Creates a TDofVersionInfo instance. Succeeds, if the file exists
       and IncludeVerInfo is <> 0
       @param _Projectname is the project name (*.dpr file without extension)
       @raises ENoVersionInfo if the file does not exist or
                              the value of [Version Info] IncludeVerInfo is not 1 }
    constructor Create(const _ProjectName: string);
    class function FilenameFor(const _ProjectName: string): string;
  end;

implementation

const
  VERSION_INFO_SECTION = 'Version Info';
  VERSION_INFO_KEYS_SECTION = 'Version Info Keys';

{ TDofVersionInfo }

constructor TDofVersionInfo.Create(const _ProjectName: string);
begin
  FProjectName := _ProjectName;
  inherited Create(VerInfoFilename, VERSION_INFO_SECTION, VERSION_INFO_KEYS_SECTION);
  if FIniFile.ReadInteger(VERSION_INFO_SECTION, 'IncludeVerInfo', 0) <> 1 then
    raise ENoVersionInfo.Create('.dof file does not contain version info');
end;

class function TDofVersionInfo.FilenameFor(const _ProjectName: string): string;
begin
  Result := _ProjectName + '.dof';
end;

function TDofVersionInfo.VerInfoFilename: string;
begin
  Result := FilenameFor(FProjectName);
end;

end.
