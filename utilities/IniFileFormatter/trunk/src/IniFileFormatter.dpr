program IniFileFormatter;

uses
  Forms,
  w_IniFileFormatter in 'w_IniFileFormatter.pas' {f_IniFileFormatter},
  u_dzIniFileFormatter in '..\..\..\IniFileFormatter\u_dzIniFileFormatter.pas',
  u_dzIniSections in '..\..\..\IniFileFormatter\u_dzIniSections.pas',
  u_dzIniEntryList in '..\..\..\IniFileFormatter\u_dzIniEntryList.pas',
  u_IniFileFormatterMain in 'u_IniFileFormatterMain.pas',
  u_dzDefaultMain in '..\..\..\..\dzcmdlineparser\src\u_dzDefaultMain.pas',
  u_dzGetOpt in '..\..\..\..\dzcmdlineparser\src\u_dzGetOpt.pas',
  u_dzOptionDescList in '..\..\..\..\dzcmdlineparser\src\u_dzOptionDescList.pas',
  u_dzOptionFoundList in '..\..\..\..\dzcmdlineparser\src\u_dzOptionFoundList.pas',
  u_dzOptionNameList in '..\..\..\..\dzcmdlineparser\src\u_dzOptionNameList.pas',
  u_dzParamDescList in '..\..\..\..\dzcmdlineparser\src\u_dzParamDescList.pas',
  u_dzParamFoundList in '..\..\..\..\dzcmdlineparser\src\u_dzParamFoundList.pas',
  w_dzDialog in '..\..\..\forms\w_dzDialog.pas' {f_dzDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Ini File Formatter';
  MainClass := TIniFileFormatterMain;
  System.ExitCode := Main;
end.

