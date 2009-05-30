program GtdNotes;

uses
  Forms,
  w_gtdNotes in 'w_gtdNotes.pas' {f_gtdNotes};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tf_gtdNotes, f_gtdNotes);
  Application.Run;
end.
