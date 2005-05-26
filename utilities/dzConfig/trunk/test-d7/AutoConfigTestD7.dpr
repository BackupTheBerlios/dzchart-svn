program AutoConfigTestD7;

uses
  Forms,
  w_AutoConfigTest in 'w_AutoConfigTest.pas',
  u_ConfigFormHandler in '..\u_ConfigFormHandler.pas',
  u_ConfigToConsole in '..\u_ConfigToConsole.pas',
  u_ConfigToIni in '..\u_ConfigToIni.pas',
  u_dzAutoConfigCommon in '..\u_dzAutoConfigCommon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_AutoConfigTest, f_AutoConfigTest);
  Application.Run;
end.
