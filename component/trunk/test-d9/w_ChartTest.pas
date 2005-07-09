unit w_ChartTest;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  u_dzDataSeries,
  u_dzCustomCharts,
  u_dzXYChart,
  u_dzPolynomialDataseries;

type
  Tf_ChartTest = class(TForm)XYChart: TdzXYChart;
    pd_squared2: TdzPolynomialDataseries;
    pd_Cubic: TdzPolynomialDataseries;
    pd_Squared1: TdzPolynomialDataseries;
    pd_Squared4: TdzPolynomialDataseries;
    pd_Sqared3: TdzPolynomialDataseries;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure XYChart1GetNextXAxisLabel(_Sender: TObject; _Idx: integer; var _Value: Double; var _Stop: Boolean);
    procedure XYChart1FormatXAxisLabel(_Sender: TObject; _Value: Double; var _Text: string);
    procedure XYChartGetNextLimitArrow(_Sender: TObject; _Idx: Integer; var _X, _Y: Double; var _IsMinimum, _Stop: Boolean);
    procedure XYChartGetPointHint(_Sender: TObject; const _x, _y: Double; var _Hint: string);
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  f_ChartTest: Tf_ChartTest;

implementation

{$R *.dfm}

uses
  Math,
  u_dzGraphics,
  u_dzSvgOutput;

procedure Tf_ChartTest.FormCreate(Sender: TObject);
label
  lbl;
var
  x: integer;
  y: integer;
  SvSeries: TdzPointDataSeries;
  MvSeries: TdzMultiValueDataSeries;
begin
  goto lbl; // Have you ever used goto in Delphi? This is how it is done. ;-)
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
  MvSeries := TdzMultiValueDataSeries.Create;
  for x := 0 to 3 do
    begin
      y := sqr(x * 10);
      MvSeries.AddDataPoint(x * 60 * 60, [y / 2 - 1000, y / 2 - 800, y / 2, y / 2 + 300, y / 2 + 1000]);
    end;
  MvSeries.Caption := 'test';
  MvSeries.ChartType := ctHighLowOpenClose;
  MvSeries.LineColor := clRed;
  MvSeries.FillColor := clWhite;
  MvSeries.PointStyle := psNone;
  MvSeries.PointColor := clRed;
  XYChart.AddDataSeries(MvSeries);
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

procedure Tf_ChartTest.XYChart1GetNextXAxisLabel(_Sender: TObject; _Idx: integer; var _Value: Double; var _Stop: Boolean);
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
begin
  if _Value < 0 then
    begin
      Sign := '-';
      _Value := -_Value;
    end
  else
    Sign := '';
  _Text := Sign + Format('%.2d:%.2d:%.2d', [Round(_Value) div 3600, (Round(_Value) div 60) mod 60, Round(_Value) mod 60]);
end;

procedure Tf_ChartTest.XYChartGetNextLimitArrow(_Sender: TObject; _Idx: Integer; var _X, _Y: Double; var _IsMinimum, _Stop: Boolean);
begin
  if _Idx = 0 then
    begin
      _X := 3600;
      _Y := 5000;
      _IsMinimum := false;
    end
  else
    _Stop := true;
end;

procedure Tf_ChartTest.XYChartGetPointHint(_Sender: TObject; const _x, _y: Double; var _Hint: string);
begin
  _Hint := 'some hint'#13#10 + _Hint;
end;

procedure Tf_ChartTest.Button1Click(Sender: TObject);
var
  wmf: TMetafile;
var
  svg: IdzSvgGraphics;
begin
  svg := TdzSvgGraphics.Create;
  XYChart.PaintTo(svg);
  svg.WriteToFile('c:\dzcharttext.svg');

  exit;
  if XYChart.BottomAxis.LabelOrientation = loVertical then
    XYChart.BottomAxis.LabelOrientation := loHorizontal
  else
    XYChart.BottomAxis.LabelOrientation := loVertical;

  //  XYChart.BottomAxis.LabelFont.Size := 20;
  exit;
  { TODO -otwm : this does not work yet }
  wmf := TMetaFile.Create;
  try
    wmf.Width := XYChart.ClientWidth;
    wmf.Height := XYChart.ClientHeight;
    wmf.Enhanced := true;
    XYChart.PaintTo(wmf.Handle, 0, 0);
    wmf.SaveToFile('c:\screenshot.wmf');
  finally
    wmf.Free;
  end;
end;

end.

