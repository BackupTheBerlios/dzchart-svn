program dzConfigTestD7;

uses
  Forms,
  w_AutoConfigTest in 'w_dzConfigTest.pas',
  u_ConfigFormHandler in '..\u_ConfigFormHandler.pas',
  u_ConfigToConsole in '..\u_ConfigToConsole.pas',
  u_ConfigToIni in '..\u_ConfigToIni.pas',
  u_dzAutoConfigCommon in '..\u_dzConfigCommon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_dzConfigTest, f_dzConfigTest);
  Application.Run;
end.
