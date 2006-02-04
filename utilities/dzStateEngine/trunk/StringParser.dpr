program StringParser;

{%File 'libs\dztemplates\Templates\t_dzSortedObjectListTemplate.tpl'}

uses
  Forms,
  w_StringParser in 'w_StringParser.pas' {f_StringParser},
  u_StringParser in 'u_StringParser.pas',
  u_dzException in 'u_dzException.pas',
  u_CharSortedListOfActionLists in 'u_CharSortedListOfActionLists.pas',
  u_dzStateEngine in 'u_dzStateEngine.pas',
  u_dzStateEngineActions in 'u_dzStateEngineActions.pas',
  u_dzStateEngineStates in 'u_dzStateEngineStates.pas',
  u_dzStateEngineTransitions in 'u_dzStateEngineTransitions.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_StringParser, f_StringParser);
  Application.Run;
end.
