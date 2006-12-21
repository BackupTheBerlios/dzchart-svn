program ChartTest;

uses
  QForms,
  w_ChartTest in 'w_ChartTest.pas' {f_ChartTest},
  w_Bitmap in 'w_Bitmap.pas' {f_Bitmap};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_ChartTest, f_ChartTest);
  Application.Run;
end.
