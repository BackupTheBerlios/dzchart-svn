program ChartTest;

uses
  Forms,
  w_ChartTest in 'w_ChartTest.pas' {f_ChartTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_ChartTest, f_ChartTest);
  Application.Run;
end.
