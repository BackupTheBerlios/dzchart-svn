program AutoConfigTestD9;

uses
  Forms,
  w_AutoConfigTest in 'w_AutoConfigTest.pas',
  u_dzConfigFormHandler in '..\u_dzConfigFormHandler.pas',
  u_dzConfigToConsole in '..\u_dzConfigToConsole.pas',
  u_dzConfigToIni in '..\u_dzConfigToIni.pas',
  u_dzConfigCommon in '..\u_dzConfigCommon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_AutoConfigTest, f_AutoConfigTest);
  Application.Run;
end.
