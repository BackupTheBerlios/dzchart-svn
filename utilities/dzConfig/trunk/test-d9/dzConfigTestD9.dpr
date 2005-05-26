program dzConfigTestD9;

uses
  Forms,
  w_dzConfigTest in 'w_dzConfigTest.pas' {f_dzConfigTest},
  u_dzConfigFormHandler in '..\u_dzConfigFormHandler.pas',
  u_dzConfigToConsole in '..\u_dzConfigToConsole.pas',
  u_dzConfigToIni in '..\u_dzConfigToIni.pas',
  u_dzConfigCommon in '..\u_dzConfigCommon.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_dzConfigTest, f_dzConfigTest);
  Application.Run;
end.
