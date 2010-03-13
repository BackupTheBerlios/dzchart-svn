program IniFileFormatter;

uses
  Forms,
  w_IniFileFormatter in 'w_IniFileFormatter.pas' {f_IniFileFormatter},
  u_IniFileFormatterMain in 'u_IniFileFormatterMain.pas',
  w_dzDialog in '..\libs\dzLib\forms\w_dzDialog.pas' {f_dzDialog},
  u_dzIniFileFormatter in '..\libs\dzLib\IniFileFormatter\u_dzIniFileFormatter.pas',
  u_dzIniSections in '..\libs\dzLib\IniFileFormatter\u_dzIniSections.pas',
  u_dzIniEntryList in '..\libs\dzLib\IniFileFormatter\u_dzIniEntryList.pas',
  u_dzDefaultMain in '..\libs\dzCmdLineParser\src\u_dzDefaultMain.pas',
  u_dzCmdLineParser in '..\libs\dzCmdLineParser\src\u_dzCmdLineParser.pas',
  u_dzCmdLineParserStates in '..\libs\dzCmdLineParser\src\u_dzCmdLineParserStates.pas',
  u_dzGetOpt in '..\libs\dzCmdLineParser\src\u_dzGetOpt.pas',
  u_dzOptionDescList in '..\libs\dzCmdLineParser\src\u_dzOptionDescList.pas',
  u_dzOptionFoundList in '..\libs\dzCmdLineParser\src\u_dzOptionFoundList.pas',
  u_dzOptionNameList in '..\libs\dzCmdLineParser\src\u_dzOptionNameList.pas',
  u_dzParamDescList in '..\libs\dzCmdLineParser\src\u_dzParamDescList.pas',
  u_dzParamFoundList in '..\libs\dzCmdLineParser\src\u_dzParamFoundList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Ini File Formatter';
  MainClass := TIniFileFormatterMain;
  System.ExitCode := Main;
end.

