program PrepBuild;




{$APPTYPE CONSOLE}

uses
  SysUtils,
  Forms,
  oxmldom,
  d_BdsProjVersionInfo in 'd_BdsProjVersionInfo.pas' {dm_BdsProjVersionInfo: TDataModule},
  i_VersionInfo in 'i_VersionInfo.pas',
  u_DofVersionInfo in 'u_DofVersionInfo.pas',
  u_IniVersionInfo in 'u_IniVersionInfo.pas',
  u_CentralIniVersionInfo in 'u_CentralIniVersionInfo.pas',
  u_dzDefaultMain in '..\libs\dzlib\src\u_dzDefaultMain.pas',
  u_PrepBuildMain in 'u_PrepBuildMain.pas',
  u_DummyVersionInfo in 'u_DummyVersionInfo.pas',
  u_AbstractVersionInfo in 'u_AbstractVersionInfo.pas';

{$R *_icon.res}
{$R *_version.res}

begin
  Application.Initialize;
  Application.Title := 'PrepBuild';
  MainClass := TPrepBuildMain;
  System.ExitCode := Main;
end.

