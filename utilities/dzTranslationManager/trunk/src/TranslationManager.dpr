program TranslationManager;

uses
  Forms,
  w_TranslationManager in 'w_TranslationManager.pas' {f_TranslationManager},
  u_TranslationMerger in 'u_TranslationMerger.pas',
  poparser in '..\libs\dxgettext\dxgettext\dxgettext\poparser.pas',
  gnugettext in '..\libs\dxgettext\lib\gnugettext.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_TranslationManager, f_TranslationManager);
  Application.Run;
end.
