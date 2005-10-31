program StateEngineTest;

{%File '..\dzTemplates\Templates\t_dzIntegerSortedObjectListTemplate.tpl'}

uses
  SysUtils,
  Forms,
  u_dzStateEngineStates in 'u_dzStateEngineStates.pas',
  u_dzQuicksort in 'libs\dzTemplates\Units\u_dzQuicksort.pas',
  u_dzStateEngineTransitions in 'u_dzStateEngineTransitions.pas',
  u_dzStateEngineActions in 'u_dzStateEngineActions.pas',
  u_dzException in 'u_dzException.pas',
  u_dzStateEngine in 'u_dzStateEngine.pas',
  w_TrafficLight in 'w_TrafficLight.pas' {f_TrafficLight};

begin
  Application.Initialize;
  Application.Title := 'Traffic Light';
  Application.CreateForm(Tf_TrafficLight, f_TrafficLight);
  Application.Run;
end.
