(*****************************************************************************
 *           TdzPolynomialDataseries Component
 *
 *        This unit is part of dummzeuch.de Charts
 *
 *                (c) 2003 Thomas Mueller
 *                 http://www.dummzeuch.de
 *
 *  Based on Paul Warren's homegrown chart components
 *        http://users.uniserve.com/~hg_soft
 *
 *****************************************************************************
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is u_dzPolynomialDataseries.
 *
 * The Initial Developer of the Original Code is
 * Thomas Mueller.
 * Portions created by the Initial Developer are Copyright (C) 2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * * Paul Warren
 *****************************************************************************
 * TdzPolynomialDataseries is dataseries for TdzXYChart for drawing
 * data based on a polynom.
 *****************************************************************************)

unit u_dzPolynomialDataseries;

{.$define consoledebug}

interface

uses
  SysUtils,
  Classes,
{$IFDEF MSWINDOWS}
  Graphics,
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  QGraphics,
{$ENDIF LINUX}
  u_dzDataseries,
  u_dzXYChart;

type
  TdzPolynomialDataseries = class;

  TdzPolynomDescriptor = class(TComponent)
  protected
    fSeries: TdzPolynomialDataseries;
    fOrder: integer;
    fCoefficients: array of double;
    procedure DefineProperties(Filer: TFiler); override;
    function GetCoefficients(_Idx: integer): double;
    procedure SetCoefficients(_Idx: integer; const _Coefficient: double);
    procedure SetOrder(const _Order: integer);
    procedure ReadData(Reader: TReader);
    procedure WriteData(Writer: TWriter);
  public
    constructor Create(_Owner: TComponent); override;
    function GetFunction: string;
    property Coefficients[_Idx: integer]: double read GetCoefficients write SetCoefficients; default;
  published
    property Order: integer read fOrder write SetOrder;
  end;

  TdzPolynomialDataseries = class(TComponent, IdzDataSeries, IdzSingleValueDataSeries)
  protected
    fMinX: double;
    fMaxX: double;
    fLineColor: TColor;
    fLineStyle: TPenStyle;
    fForceLineColor: boolean;
    fPointColor: TColor;
    fPointStyle: TPointStyle;
    fFillColor: TColor;
    fCaption: string;
    fChartType: TSingleValueChartType;
    fChart: TdzXYChart;
    fPolynom: TdzPolynomDescriptor;
    fPointCount: integer;
    procedure SetPointCount(const _PointCount: integer);
    procedure SetPolynom(const _Polynom: TdzPolynomDescriptor);
    procedure SetChart(const _Chart: TdzXYChart);
    procedure doChanged;
    procedure SetLineColor(const _LineColor: TColor);
    procedure SetLineStyle(const _LineStyle: TPenStyle);
    procedure SetMaxX(const _MaxX: double);
    procedure SetMinX(const _MinX: double);
    procedure SetPointColor(const _PointColor: TColor);
    procedure SetPointStyle(const _PointStyle: TPointStyle);
    procedure SetFillColor(const _FillColor: TColor);
    procedure SetCaption(const _Caption: string);
    procedure SetChartType(const _ChartType: TSingleValueChartType);

    function GetPointCount: integer;
    function GetDataPoint(_Idx: integer): IdzDataPoint;
    function GetLineColor: TColor;
    function GetLineStyle: TPenStyle;
    function GetPointColor: TColor;
    function GetPointStyle: TPointStyle;
    function GetFillColor: TColor;
    function GetChartType: TSingleValueChartType;
    function GetCaption: string;
    procedure Notification(_Component: TComponent; _Operation: TOperation); override;
    {: returns true if this is a IdzMultiValueDataSeries }
    function IsMulti: boolean;
    {: returns the minimum and maximum X / Y values of the data series
       (only valid if result is true)
       @param(MinX is the minium X value)
       @param(MaxX is the maximum X value)
       @param(MinY is the minimum Y value)
       @param(MaxX is the maximum Y value)
       @returns(true, if there is at least one data point in the series) }
    function GetMinAndMax(out _MinX, _MaxX, _MinY, _MaxY: Double): boolean;
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  published
    property ChartType: TSingleValueChartType read GetChartType write SetChartType;
    property MinX: double read fMinX write SetMinX;
    property MaxX: double read fMaxX write SetMaxX;
    property PointCount: integer read GetPointCount write SetPointCount;
    property LineColor: TColor read GetLineColor write SetLineColor;
    property LineStyle: TPenStyle read GetLineStyle write SetLineStyle;
    property ForceLineColor: boolean read fForceLineColor write fForceLineColor;
    property PointColor: TColor read GetPointColor write SetPointColor;
    property PointStyle: TPointStyle read GetPointStyle write SetPointStyle;
    property FillColor: TColor read GetFillColor write SetFillColor;
    property Caption: string read GetCaption write SetCaption;
    property Chart: TdzXYChart read fChart write SetChart;
    property Polynom: TdzPolynomDescriptor read fPolynom write SetPolynom stored true;
  end;

implementation

{ TdzPolynomialDataseries }

constructor TdzPolynomialDataseries.Create(_Owner: TComponent);
begin
  inherited;
  fMinX := -1;
  fMaxX := 1;
  fPointCount := 11;
  fLineColor := clBlack;
  fLineStyle := psSolid;
  fPointColor := clBlack;
  fPointStyle := psCircle;
  fFillColor := clBlack;
  fCaption := 'New Polynomial Dataseries';
  fChartType := ctXY;
  fPolynom := TdzPolynomDescriptor.Create(Self);
end;

destructor TdzPolynomialDataseries.Destroy;
begin
  SetChart(nil);
  fPolynom.Free;
  fPolynom := nil;
  inherited;
end;

function TdzPolynomialDataseries.GetFillColor: TColor;
begin
  if fForceLineColor then
    Result := fLineColor
  else
    Result := fFillColor;
end;

function TdzPolynomialDataseries.GetChartType: TSingleValueChartType;
begin
  Result := fChartType;
end;

function TdzPolynomialDataseries.GetLineColor: TColor;
begin
  Result := fLineColor;
end;

function TdzPolynomialDataseries.GetLineStyle: TPenStyle;
begin
  Result := fLineStyle;
end;

function TdzPolynomialDataseries.GetPointColor: TColor;
begin
  if fForceLineColor then
    Result := fLineColor
  else
    Result := fPointColor;
end;

function TdzPolynomialDataseries.GetPointStyle: TPointStyle;
begin
  Result := fPointStyle;
end;

function TdzPolynomialDataseries.GetCaption: string;
begin
  Result := fCaption;
end;

procedure TdzPolynomialDataseries.doChanged;
begin
  if Assigned(fChart) then
    fChart.Refresh;
end;

procedure TdzPolynomialDataseries.SetChart(const _Chart: TdzXYChart);
begin
  if Assigned(fChart) then
    fChart.RemoveDataSeries(self);
  fChart := _Chart;
  if Assigned(fChart) then
    fChart.AddDataSeries(Self);
end;

procedure TdzPolynomialDataseries.Notification(_Component: TComponent;
  _Operation: TOperation);
begin
  inherited;
  if (_Component = fChart) and (_Operation = opRemove) then
    fChart := nil;
end;

procedure TdzPolynomialDataseries.SetLineColor(const _LineColor: TColor);
begin
  fLineColor := _LineColor;
  doChanged;
end;

procedure TdzPolynomialDataseries.SetLineStyle(const _LineStyle: TPenStyle);
begin
  fLineStyle := _LineStyle;
  doChanged;
end;

procedure TdzPolynomialDataseries.SetMaxX(const _MaxX: double);
begin
  fMaxX := _MaxX;
  doChanged;
end;

procedure TdzPolynomialDataseries.SetMinX(const _MinX: double);
begin
  fMinX := _MinX;
  doChanged;
end;

procedure TdzPolynomialDataseries.SetPointColor(const _PointColor: TColor);
begin
  fPointColor := _PointColor;
  doChanged;
end;

procedure TdzPolynomialDataseries.SetPointStyle(const _PointStyle: TPointStyle);
begin
  fPointStyle := _PointStyle;
  doChanged;
end;

procedure TdzPolynomialDataseries.SetCaption(const _Caption: string);
begin
  fCaption := _Caption;
  doChanged;
end;

procedure TdzPolynomialDataseries.SetFillColor(const _FillColor: TColor);
begin
  fFillColor := _FillColor;
  doChanged;
end;

procedure TdzPolynomialDataseries.SetChartType(const _ChartType: TSingleValueChartType);
begin
  fChartType := _ChartType;
  doChanged;
end;

procedure TdzPolynomialDataseries.SetPolynom(const _Polynom: TdzPolynomDescriptor);
begin
  fPolynom := _Polynom;
end;

function TdzPolynomialDataseries.GetDataPoint(_Idx: integer): IdzDataPoint;
var
  x: double;
  y: Extended;
  i: integer;
  xn: extended;
begin
  if fPointCount = 1 then
    x := fMinX
  else
    x := fMinX + _Idx * ((fMaxX - fMinX) / (fPointCount - 1));
  y := 0;
  xn := 1;
  for i := 0 to fPolynom.fOrder do
    begin
      y := y + xn * fPolynom.Coefficients[i];
{$IFDEF consoledebug}
      if not (csDesigning in ComponentState) then
        if IsConsole then
          WriteLn('X=', x: 2, '; XN=', xn: 2, '; C=', fPolynom.Coefficients[i]: 2, '; Y=', y: 2);
{$ENDIF consoledebug}
      xn := xn * x;
    end;
  Result := TdzDataPoint.Create(x, y);
end;

function TdzPolynomialDataseries.GetPointCount: integer;
begin
  Result := fPointCount;
end;

procedure TdzPolynomialDataseries.SetPointCount(const _PointCount: integer);
begin
  fPointCount := _PointCount;
  doChanged;
end;

function TdzPolynomialDataseries.GetMinAndMax(out _MinX, _MaxX, _MinY, _MaxY: Double): boolean;
var
  i: integer;
  x: double;
  y: double;
  Point: IdzDataPoint;
begin
  Result := GetPointCount > 0;
  if Result then
    begin
      Point := GetDataPoint(0);
      _MinX := Point.GetX;
      _MaxX := _MinX;
      _MinY := Point.GetY;
      _MaxY := _MinY;
      for i := 1 to GetPointCount - 1 do
        begin
          Point := GetDataPoint(i);
          x := Point.GetX;
          y := Point.GetY;
          if x > _MaxX then
            _MaxX := x;
          if x < _MinX then
            _MinX := x;
          if y > _MaxY then
            _MaxY := y;
          if y < _MinY then
            _MinY := y;
        end;
    end;
end;

function TdzPolynomialDataseries.IsMulti: boolean;
begin
  Result := false;
end;

{ TdzPolynomDescriptor }

constructor TdzPolynomDescriptor.Create(_Owner: TComponent);
begin
  inherited Create(_Owner);
  SetSubComponent(true);
  fSeries := _Owner as TdzPolynomialDataseries;
  fOrder := 2;
  SetLength(fCoefficients, 11);
  fCoefficients[0] := 0;
  fCoefficients[1] := 0;
  fCoefficients[2] := 1;
end;

procedure TdzPolynomDescriptor.SetOrder(const _Order: integer);
begin
  if (_Order > -1) and (fOrder <> _Order) then
    begin
      fOrder := _Order;
      fSeries.doChanged;
    end;
end;

function TdzPolynomDescriptor.GetCoefficients(_Idx: integer): double;
begin
  if _Idx <= fOrder then
    Result := fCoefficients[_Idx]
  else
    Result := 0;
end;

procedure TdzPolynomDescriptor.SetCoefficients(_Idx: integer; const _Coefficient: double);
begin
  if _Idx <= 10 then
    if fCoefficients[_Idx] <> _Coefficient then
      begin
        fCoefficients[_Idx] := _Coefficient;
        if _Idx <= fOrder then
          fSeries.doChanged;
      end;
end;

function TdzPolynomDescriptor.GetFunction: string;
var
  i: integer;
  s: string;
  c: double;
begin
  s := '';
  for i := fOrder downto 0 do
    begin
      c := fCoefficients[i];
      if (c <> 0) or ((i = 0) and (s = '')) then
        begin
          if s <> '' then
            s := s + ' + ';
          if (i = 0) or (c - 1 <> 0) then
            begin
              s := s + FloatToStr(c);
            end;
          if i > 0 then
            s := s + 'x';
          case i of
            0, 1: ;
            2: s := s + '²';
            3: s := s + '³';
          else
            s := s + '^' + IntToStr(i);
          end;
        end;
    end;
  Result := 'y = ' + s;
end;

procedure TdzPolynomDescriptor.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty('Coefficients', ReadData, WriteData, true);
end;

procedure TdzPolynomDescriptor.WriteData(Writer: TWriter);
var
  i: integer;
begin
  Writer.WriteListBegin;
  for i := 0 to 10 do
    Writer.WriteFloat(fCoefficients[i]);
  Writer.WriteListEnd;
end;

procedure TdzPolynomDescriptor.ReadData(Reader: TReader);
var
  i: integer;
begin
  Reader.ReadListBegin;
  i := 0;
  while not Reader.EndOfList do
    begin
      fCoefficients[i] := Reader.ReadFloat;
      Inc(i);
    end;
  while i <= 10 do
    begin
      fCoefficients[i] := 0;
      Inc(i);
    end;
  Reader.ReadListEnd;
end;

end.

