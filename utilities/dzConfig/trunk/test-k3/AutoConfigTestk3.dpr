program AutoConfigTestk3;

uses
  QForms,
  w_AutoConfigTest in 'w_AutoConfigTest.pas' {f_AutoConfigTest},
  u_ConfigToConsole in '../u_ConfigToConsole.pas',
  u_ConfigToIni in '../u_ConfigToIni.pas',
  u_ConfigFormHandler in '../u_ConfigFormHandler.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_AutoConfigTest, f_AutoConfigTest);
  Application.Run;
end.
