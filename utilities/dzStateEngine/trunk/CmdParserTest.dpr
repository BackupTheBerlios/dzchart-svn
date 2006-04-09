program CmdParserTest;

{%TogetherDiagram 'ModelSupport_CmdParserTest\default.txaPackage'}

uses
  Forms,
  u_CmdParser in 'u_CmdParser.pas',
  u_dzStateEngineTransitions in 'u_dzStateEngineTransitions.pas',
  u_dzException in 'u_dzException.pas',
  u_dzStateEngine in 'u_dzStateEngine.pas',
  u_dzStateEngineActions in 'u_dzStateEngineActions.pas',
  u_dzStateEngineStates in 'u_dzStateEngineStates.pas',
  u_dzQuicksort in 'libs\dztemplates\Units\u_dzQuicksort.pas',
  u_CharSortedListOfActionLists in 'u_CharSortedListOfActionLists.pas',
  u_CmdOptionList in 'u_CmdOptionList.pas',
  w_CmdParser in 'w_CmdParser.pas' {f_CmdParser};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_CmdParser, f_CmdParser);
  Application.Run;
end.
