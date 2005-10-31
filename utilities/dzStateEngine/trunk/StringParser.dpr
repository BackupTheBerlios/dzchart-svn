program StringParser;

{%File 'libs\dztemplates\Templates\t_dzSortedObjectListTemplate.tpl'}

uses
  Forms,
  w_StringParser in 'w_StringParser.pas' {f_StringParser},
  u_CharSortedListOfActionLists in 'u_CharSortedListOfActionLists.pas',
  u_StringParser in 'u_StringParser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_StringParser, f_StringParser);
  Application.Run;
end.
