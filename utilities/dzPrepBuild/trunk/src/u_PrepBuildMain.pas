unit u_PrepBuildMain;

interface

uses
  u_dzDefaultMain,
  u_dzGetOpt;

type
  TPrepBuildMain = class(TDefaultMain)
  private
  protected
    procedure InitCmdLineParser; override;
    function doExecute: integer; override;
  public

  end;

implementation

{ TPrepBuildMain }

function TPrepBuildMain.doExecute: integer;
begin
  Result := inherited doExecute;
end;

procedure TPrepBuildMain.InitCmdLineParser;
begin
  inherited;
  FGetOpt.RegisterOption('ReadDof', 'read a .dof file to get the version information', true);
  FGetOpt.RegisterOption('ReadBdsproj', 'read a .bdsproj file to get the version information', true);
  FGetOpt.RegisterOption('ReadIni', 'read a .ini file to get the version information', true);
  FGetOpt.RegisterOption('Exec', 'execute the given program or script with extended environment', true);
  FGetOpt.RegisterOption('UpdateDof', 'update a .dof file with the version information', true);
  FGetOpt.RegisterOption('UpdateBdsproj', 'update a .bdsproj file with the version information', true);
  FGetOpt.RegisterOption('UpdateIni', 'update a .ini file with the version information', true);
  FGetOpt.RegisterOption('IncBuild', 'increment the build number', false);
  FGetOpt.RegisterOption('MajorVer', 'set the major version number', true);
  FGetOpt.RegisterOption('MinorVer', 'set the minor version number', true);
  FGetOpt.RegisterOption('Revision', 'set the revision number', true);
  FGetOpt.RegisterOption('Build', 'set the build number', true);
  FGetOpt.RegisterOption('FileDesc', 'set the file description', true);
  FGetOpt.RegisterOption('InternalName', 'set the internal name', true);
  FGetOpt.RegisterOption('OriginalName', 'set the original file name', true);
  FGetOpt.RegisterOption('Product', 'set the product name', true);
  FGetOpt.RegisterOption('ProductVersion', 'set the product version', true);
  FGetOpt.RegisterOption('Company', 'set the company name', true);
  FGetOpt.RegisterOption('Copyright', 'set the legal copyright', true);
  FGetOpt.RegisterOption('Trademark', 'set the legal trademark', true);
  FGetOpt.RegisterOption('Comments', 'set the comments', true);
end;

initialization
  MainClass := TPrepBuildMain;
end.

