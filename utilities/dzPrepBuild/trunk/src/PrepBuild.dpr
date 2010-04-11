program PrepBuild;




{$APPTYPE CONSOLE}

uses
  SysUtils,
  Forms,
  oxmldom,
  d_XmlVersionInfo in 'd_XmlVersionInfo.pas' {dm_XmlVersionInfo: TDataModule},
  d_DprojVersionInfo in 'd_DprojVersionInfo.pas' {dm_DprojVersionInfo: TDataModule},
  d_BdsProjVersionInfo in 'd_BdsProjVersionInfo.pas' {dm_BdsProjVersionInfo: TDataModule},
  i_VersionInfoAccess in 'i_VersionInfoAccess.pas',
  u_DofVersionInfo in 'u_DofVersionInfo.pas',
  u_IniVersionInfo in 'u_IniVersionInfo.pas',
  u_CentralIniVersionInfo in 'u_CentralIniVersionInfo.pas',
  u_PrepBuildMain in 'u_PrepBuildMain.pas',
  u_VersionInfo in 'u_VersionInfo.pas',
  u_dzGetOpt in '..\libs\dzCmdLineParsersrc\u_dzGetOpt.pas',
  u_dzDefaultMain in '..\libs\dzCmdLineParsersrc\u_dzDefaultMain.pas',
  u_dzParamDescList in '..\libs\dzCmdLineParsersrc\u_dzParamDescList.pas',
  u_dzParamFoundList in '..\libs\dzCmdLineParsersrc\u_dzParamFoundList.pas',
  u_dzOptionDescList in '..\libs\dzCmdLineParsersrc\u_dzOptionDescList.pas',
  u_dzOptionNameList in '..\libs\dzCmdLineParsersrc\u_dzOptionNameList.pas',
  u_dzOptionFoundList in '..\libs\dzCmdLineParsersrc\u_dzOptionFoundList.pas',
  w_dzDialog in '..\libs\dzlib\forms\w_dzDialog.pas' {f_dzDialog},
  u_dzJclUtils in '..\libs\dzlib\src\u_dzJclUtils.pas';

{$R *_icon.res}
{$R *_version.res}

begin
  Application.Initialize;
  Application.Title := 'PrepBuild';
  MainClass := TPrepBuildMain;
  System.ExitCode := Main;
end.

