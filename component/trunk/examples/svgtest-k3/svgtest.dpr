program svgtest;

uses
  QForms,
  w_Shadow in 'w_Shadow.pas' {Form1},
  u_dzSvgOutput in '../u_dzSvgOutput.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
