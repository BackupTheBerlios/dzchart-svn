unit w_ChartTest;

interface

uses
  SysUtils,
  Variants,
  Classes,
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QStdCtrls,
  QExtCtrls,
  u_dzCustomCharts,
  u_dzXYChart,
  u_dzPolynomialDataseries;

type
  Tf_ChartTest = class(TForm)
    XYChart: TdzXYChart;
    Button1: TButton;
    pd_squared2: TdzPolynomialDataseries;
    pd_Cubic: TdzPolynomialDataseries;
    pd_Squared1: TdzPolynomialDataseries;
    pd_Squared4: TdzPolynomialDataseries;
    pd_Sqared3: TdzPolynomialDataseries;
    procedure FormCreate(Sender: TObject);
    procedure XYChart1GetNextXAxisLabel(_Sender: TObject; _Idx: integer;
      var _Value: Double; var _Stop: Boolean);
    procedure XYChart1FormatXAxisLabel(_Sender: TObject; _Value: Double;
      var _Text: string);
    procedure Button1Click(Sender: TObject);
    procedure XYChartGetPointHint(_Sender: TObject; const _x, _y: Double;
      var _Hint: string);
  private
    procedure CreateHighLowOpenClose;
    procedure CreateCandle;
  public
  end;

var
  f_ChartTest: Tf_ChartTest;

implementation

{$R *.xfm}

uses
  Math,
  u_dzDataSeries;

procedure Tf_Charttest.CreateHighLowOpenClose;
var
  x: integer;
  y: integer;
  MvSeries: TdzMultiValueDataSeries;
begin
  pd_squared2.Free;
  pd_Cubic.Free;
  pd_Squared1.Free;
  pd_Squared4.Free;
  pd_Sqared3.Free;

  MvSeries:= TdzMultiValueDataSeries.Create;
  for x := 0 to 3 do
    begin
      y := sqr(x * 10);
      MvSeries.AddDataPoint(x * 60 * 60, [y / 2 - 1000, y / 2 - 800, y/2, y/2 + 300, y/2 + 1000]);
    end;
  MvSeries.Caption := 'HighLowOpenClose';
  MvSeries.ChartType := ctHighLowOpenClose;
  MvSeries.LineColor := clBlack;
  MvSeries.FillColor := clWhite;
  MvSeries.PointStyle := psNone;
  MvSeries.BearishColor := clBlue;
  MvSeries.BullishColor := clRed;
  MvSeries.PointColor := clBlack;
  XYChart.AddDataSeries(MvSeries);
end;

procedure Tf_Charttest.CreateCandle;
var
  x: integer;
  y: integer;
  MvSeries: TdzMultiValueDataSeries;
begin
  pd_squared2.Free;
  pd_Cubic.Free;
  pd_Squared1.Free;
  pd_Squared4.Free;
  pd_Sqared3.Free;

  MvSeries:= TdzMultiValueDataSeries.Create;
  for x := 0 to 3 do
    begin
      y := sqr(x * 10);
      if Odd(x) then
        MvSeries.AddDataPoint(x * 60 * 60, [y / 2 - 1000, y/2 + 300, y/2, y / 2 - 800, y/2 + 1000])
      else
        MvSeries.AddDataPoint(x * 60 * 60, [y / 2 - 1000, y / 2 - 800, y/2, y/2 + 300, y/2 + 1000]);
    end;
  MvSeries.Caption := 'Candle';
  MvSeries.ChartType := ctCandle;
  MvSeries.LineColor := clBlack;
  MvSeries.FillColor := clWhite;
  MvSeries.PointStyle := psNone;
  MvSeries.BearishColor := clBlue;
  MvSeries.BullishColor := clRed;
  MvSeries.PointColor := clBlack;
  XYChart.AddDataSeries(MvSeries);
end;

procedure Tf_ChartTest.FormCreate(Sender: TObject);
label
  lbl;
var
  x: integer;
  y: integer;
  SvSeries: TdzPointDataSeries;
  MvSeries: TdzMultiValueDataSeries;
begin
goto lbl;         // Have you ever used goto in Delphi? This is how it is done. ;-)
  SvSeries := tdzPointDataSeries.Create;
  SvSeries.Caption := 'horz. line';
  svSeries.LineColor := clRed;
  svSeries.LineStyle := psSolid;
  SvSeries.PointStyle := psCircle;
  SvSeries.ChartType := ctAlternatingLine;
  SvSeries.AddDataPoint(NegInfinity, 0);
  SvSeries.AddDataPoint(0, 0);
  SvSeries.AddDataPoint(0, 1);
  SvSeries.AddDataPoint(Infinity, 1);
//  SvSeries.AddDataPoint(Infinity, 12);
  XYChart.AddDataSeries(SvSeries);
exit;

lbl:
  CreateCandle;
exit;

  SvSeries := TdzPointDataSeries.Create;
  SvSeries.Caption := 'x Square';
  SvSeries.LineColor := clBlack;
  SvSeries.LineStyle := psDot;
  SvSeries.PointColor := clYellow;
  SvSeries.PointStyle := psRectangle;
  SvSeries.ChartType := ctXY;
  for x := 0 to 20 do
    SvSeries.AddDataPoint(x * 60 * 60, sqr(x * 10));
  XYChart.AddDataSeries(SvSeries);

  SvSeries := TdzPointDataSeries.Create;
  SvSeries.Caption := 'Linear';
  SvSeries.LineColor := clRed;
  SvSeries.PointColor := clBlue;
  SvSeries.PointStyle := psTriangle;
  SvSeries.ChartType := ctXY;
  for x := 12 to 24 do
    SvSeries.AddDataPoint(x * 60 * 60, x * 30);
  XYChart.AddDataSeries(SvSeries);
end;

type
  TdzXYChartHack = class(TdzXYChart);

procedure Tf_ChartTest.XYChart1GetNextXAxisLabel(_Sender: TObject;
  _Idx: integer; var _Value: Double; var _Stop: Boolean);
begin
  if _Idx = 0 then
    _Value := 0
  else
    _Value := _Value + 60 * 60;
  _Stop := (_Value > 24 * 60 * 60);
end;

procedure Tf_ChartTest.XYChart1FormatXAxisLabel(_Sender: TObject; _Value: Double; var _Text: string);
var
  Sign: string;
begin                               exit;
  if _Value < 0 then
    begin
      Sign := '-';
      _Value := -_Value;
    end
  else
    Sign := '';

  _Text := Sign + Format('%.2d:%.2d:%.2d',
    [Round(_Value) div 3600,
    (Round(_Value) div 60) mod 60,
      Round(_Value) mod 60]);
end;

procedure Tf_ChartTest.Button1Click(Sender: TObject);
begin
  XYChart.BeginUpdate;
  try
    if XYChart.BottomAxis.LabelOrientation = loVertical then
      begin
        XYChart.BottomAxis.LabelOrientation := loHorizontal;
//        XYChart.LeftAxis.Scale.ScaleType := stLinear;
      end
    else
      begin
        XYChart.BottomAxis.LabelOrientation := loVertical;
//        XYChart.LeftAxis.Scale.ScaleType := stLogarithmic;
      end;
  finally
    XYChart.EndUpdate;
  end;
end;

procedure Tf_ChartTest.XYChartGetPointHint(_Sender: TObject; const _x,
  _y: Double; var _Hint: string);
begin
  _Hint := 'some hint'#13#10 + _Hint;
end;

end.

