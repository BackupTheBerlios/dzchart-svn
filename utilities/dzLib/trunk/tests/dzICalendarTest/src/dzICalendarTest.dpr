program dzICalendarTest;

uses
  Forms,
  w_dzICalendarTest in 'w_dzICalendarTest.pas' {Form1},
  u_dzICalendar in '..\..\..\src\u_dzICalendar.pas',
  u_dzICalDuration in '..\..\..\src\u_dzICalDuration.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
