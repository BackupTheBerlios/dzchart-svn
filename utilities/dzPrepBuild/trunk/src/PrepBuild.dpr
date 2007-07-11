program PrepBuild;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  oxmldom,
  u_IncBuildNoMain in 'u_IncBuildNoMain.pas',
  d_BdsProjVersionInfo in 'd_BdsProjVersionInfo.pas' {dm_BdsProjVersionInfo: TDataModule},
  i_VersionInfo in 'i_VersionInfo.pas',
  u_DofVersionInfo in 'u_DofVersionInfo.pas',
  u_IniVersionInfo in 'u_IniVersionInfo.pas',
  u_CentralIniVersionInfo in 'u_CentralIniVersionInfo.pas';

{$R *.res}

begin
  Main;
end.
