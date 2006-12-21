(*****************************************************************************
 *                 TdzPieChart Component
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
 * The Original Code is u_dzPieChart.
 *
 * The Initial Developer of the Original Code is
 * Thomas Mueller.
 * Portions created by the Initial Developer are Copyright (C) 2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * * Paul Warren
 *****************************************************************************
 *
 * TdzPieChart is a Pie Chart component based on TdzCustomChart
 * NOTE: This component is not quite finished!
 *****************************************************************************)

unit u_dzPieChart;

interface

uses
  SysUtils,
  Types,
  Classes,
  Math,
{$ifdef MSWINDOWS}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
{$endif MSWINDOWS}
{$ifdef LINUX}
  Qt,
  u_dzQWindows,
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QStdCtrls,
{$endif LINUX}
  u_dzCanvas,
  u_dzGraphics,
  u_dzDataSeries,
  u_dzCustomCharts;

type
  TDrawPie = procedure(Sender: TObject; var Color: TColor) of object;
  TDrawLabel = procedure(Sender: TObject; Data, DataSum: Double; var DataText: string) of object;

  TdzPieChart = class(TdzCustomChart)
  private
    fDataSeries: IdzSingleValueDataSeries;
    FDataSum: Double;
    FUseLabels: boolean;
    FOnDrawPie: TDrawPie;
    FOnDrawLabel: TDrawLabel;
    function GetLegendRect(const _Target: IdzGraphics; var Offs: integer): TRect;
  protected
    function GetDrawRect(const _Target: IdzGraphics): TRect;
    procedure DrawGrid(const _Target: IdzGraphics; Rect: TRect); override;
    procedure DrawBottomScale(const _Target: IdzGraphics); override;
    procedure DrawLeftScale(const _Target: IdzGraphics); override;
    procedure DrawDataPoints(const _Target: IdzGraphics); override;
    procedure ScaleChart; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
//    procedure AddData(X: Double; Y: string);
//    procedure ClearData;
  published
    property UseLabels: boolean read FUseLabels write FUseLabels default true;
    property OnDrawPie: TDrawPie read FOnDrawPie write FOnDrawPie;
    property OnDrawLabel: TDrawLabel read FOnDrawLabel write FOnDrawLabel;
    property Align;
    property BackGround;
    property ChartColor;
    property Font;
    property ChartTitle;
  end;

implementation

{ TdzPieChart }

constructor TdzPieChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // create the data list
  fDataSeries := TdzPointDataSeries.Create;
  // set default values
  Width := 200;
  Height := 150;
  FDataSum := 0;
  FUseLabels := true;
end;

destructor TdzPieChart.Destroy;
begin
  fDataSeries := nil;
  inherited Destroy;
end;

function TdzPieChart.GetLegendRect(const _Target: IdzGraphics; var Offs: integer): TRect;
var
  Cnt: integer;
begin
  // get chart rect
  Result := CalcChartRect(_Target);

  // size it
  Result.Left := Result.Right - _Target.Canvas.TextWidth('Abcdefghijklmno') - 19;

  // calc space for legends
  Cnt := fDataSeries.GetPointCount;
  if Cnt <> 0 then
    Offs := (Result.Bottom - Result.Top) div Cnt
  else
    Offs := (Result.Bottom - Result.Top);
end;

{ GetDrawRect - get area for drawing }

function TdzPieChart.GetDrawRect(const _Target: IdzGraphics): TRect;
var
  Offs: integer;
  CR, LR: TRect;
  dx, dy: integer;
begin
  // get chart rect
  CR := CalcChartRect(_Target);
  // get legend rect
  LR := GetLegendRect(_Target, Offs);

  // calculate difference
  SubtractRect(Result, CR, LR);
  // offset for appearance
  Offs := ((5 * Height) div 100);

  // calculate drawing rect for pie
  if Result.Right >= Result.Bottom then // if wider than high
    begin
      Result.Top := Result.Top + Offs;
      Result.Bottom := Result.Bottom - Offs;
      Result.Left := Result.Left + Offs;
      Result.Right := Result.Left + (Result.Bottom - Result.Top);
      dx := ((LR.Left - CR.Left) div 2) - (((Result.Right - Result.Left) + Offs) div 2);
      OffsetRect(Result, dx, 0);
    end
  else // else higher than wide
    begin
      Result.Top := Result.Top + Offs;
      Result.Left := Result.Left + Offs;
      Result.Right := Result.Right - Offs;
      Result.Bottom := Result.Top + (Result.Right - Result.Left);
      dy := (CR.Bottom div 2) - ((Result.Bottom + Offs) div 2);
      OffsetRect(Result, 0, dy);
    end;
end;

//procedure TdzPieChart.AddData(X: Double; Y: string);
//var
//  P: TDataPoint;
//begin
// // keep track of data sum
//  FDataSum := FDataSum + X;
//  // create data object
//  P := TDataPoint.Create(X, 0, Y);
////  P.X := X;
////  P.Y := 0;
////  P.ALabel := Y;
//
//  // add it to the list
//  fDataSeries.AddDataPoint(P);
//  // don't need to free - the list does that
//end;

//procedure TdzPieChart.ClearData;
//begin
//  if fDataSeries.Count <> 0 then
//    fDataSeries.Clear; // clear data
//  FDataSum := 0; // re-set datasum
//end;

procedure TdzPieChart.DrawGrid(const _Target: IdzGraphics; Rect: TRect);
begin
  // no grids
end;

procedure TdzPieChart.DrawBottomScale;
begin
  // do nothing - no scales
end;

procedure TdzPieChart.DrawLeftScale;
begin
  // do nothing - no scales
end;

procedure TdzPieChart.ScaleChart;
begin
  // do nothing - no extrema
end;

procedure TdzPieChart.DrawDataPoints;
const
  Margin: integer = 5;
  AColor: array[0..16] of TColor = (clAqua, clBlue, clFuchsia, clRed, clLime,
    clYellow, clLtGray, clNavy, clMaroon, clGray, clOlive, clPurple, clSilver,
    clTeal, clDkGray, clWhite, clGreen);
var
  LegendRect: TRect;
  midX, midY: integer;
  SAngle, EAngle: integer;
  i: integer;
  LegendOffs: integer;
  Color: TColor;
  DataText: string;

  // get point from angle and length

  function GetPoint(Angle, Length: integer): TPoint;
  var
    sX, sY: double;
  begin
    sX := Cos((Angle / 180.0) * pi);
    sY := Sin((Angle / 180.0) * pi); // in radians
    Result.X := Round(sX * Length);
    Result.Y := Round(sY * Length);
  end;

  // get angle form data/datasum

  function GetAngle(Value: Double): integer;
  var
    Tmp: Double;
  begin
    Tmp := (Value / FDataSum) * 360;
    Result := Round(Tmp);
  end;

  // calculate radius in pixels

  function Radius: integer;
  var
    Rect: TRect;
  begin
    Rect := GetDrawRect(_Target);
    Result := (Rect.Bottom - Rect.Top) div 2;
  end;

  // draw data labels in pie slice only if they fit in the slice

  procedure DrawValues(SAngle, EAngle: integer; P: TPoint; S: string);
  var
    Points: array[0..2] of TPoint;
    Rgn, TmpRgn: hRgn;
    SA, EA: integer;
  begin
    TmpRgn := hRgn(0);
    with _Target.Canvas do
      begin
      // offset text starting point so
      // center of text at center of pie
        P.X := P.X - (TextWidth(S) div 2);
        P.Y := P.Y - (TextHeight(S) div 2);

      // compute region similar to pie
        SA := SAngle;
        EA := SA + 20;
        if EA > EAngle then
          EA := EAngle;
        Points[0] := Point(MidX, MidY);
        Points[1] := Point(MidX + GetPoint(SA, Radius).X, MidY - GetPoint(SA, Radius).Y);
        Points[2] := Point(midX + GetPoint(EA, Radius).X, midY - GetPoint(EA, Radius).Y);
        Rgn := CreatePolygonRgn(Points, 3, ALTERNATE); // first slice
        try
          SA := EA;
          while SA <> EAngle do
            begin
              EA := SA + 20;
              if EA > EAngle then
                EA := EAngle;
              Points[0] := Point(MidX, MidY);
              Points[1] := Points[2];
              Points[2] := Point(midX + GetPoint(EA, Radius).X, midY - GetPoint(EA, Radius).Y);
              try
                TmpRgn := CreatePolygonRgn(Points, 3, ALTERNATE); // next slice
                CombineRgn(Rgn, Rgn, TmpRgn, RGN_OR); // combine slices
              finally
                DeleteObject(TmpRgn); // free resources
              end;
              SA := EA;
            end;

        // Only output values if the text fits the region
          if PtInRegion(Rgn, P.X, P.Y) and PtInRegion(Rgn, P.X + TextWidth(S), P.Y) and
            PtInRegion(Rgn, P.X, P.Y + TextHeight(S)) and
            PtInRegion(Rgn, P.X + TextWidth(S), P.Y + TextHeight(S)) then
            TextOut(P.X, P.Y, S);
        finally
          DeleteObject(Rgn); // free resources
        end;
      end;
  end;

var
  DrawRect: TRect;
begin
  // get rect for drawing legend
  LegendRect := GetLegendRect(_Target, LegendOffs);

  // draw fOffScreen for speed
  with _Target.Canvas do
    begin
    // calculate center
      DrawRect := GetDrawRect(_Target);
      midX := ((DrawRect.Right + DrawRect.Left) div 2);
      midY := ((DrawRect.Bottom + DrawRect.Top) div 2);

      SAngle := 0; // default value

    // draw and fill circle
      Brush.Color := AColor[0];
      Ellipse(DrawRect);

      for i := 0 to fDataSeries.GetPointCount - 1 do
        begin
        // calculate end angle from start angle and data
          EAngle := SAngle + GetAngle(fDataSeries.GetDataPoint(i).GetX);

          Color := AColor[i]; // set color for slice
          if Assigned(FOnDrawPie) then
            FOnDrawPie(Self, Color); // trigger event
          Brush.Color := Color; // re-set color in case modified

          if EAngle <> SAngle then // don't bother drawing pie if angles are equal
            Pie(DrawRect.Left, DrawRect.Top, DrawRect.Right, DrawRect.Bottom,
              MidX + GetPoint(SAngle, Radius).X, MidY - GetPoint(SAngle, Radius).Y,
              midX + GetPoint(EAngle, Radius).X, midY - GetPoint(EAngle, Radius).Y);

        // draw data labels
          DataText := FloatToStr(fDataSeries.GetDataPoint(i).GetX);
          if Assigned(FOnDrawLabel) then
            FOnDrawLabel(Self, fDataSeries.GetDataPoint(i).GetX, FDataSum, DataText);
          if FUseLabels then
            DrawValues(SAngle, EAngle, Point(MidX + GetPoint((EAngle + SAngle) div 2, Radius div 2).X,
              MidY - GetPoint((EAngle + SAngle) div 2, Radius div 2).Y), DataText);

        // draw legend
          Rectangle(LegendRect.Left + Margin, LegendRect.Top + ((i + 1) * LegendOffs) - (LegendOffs div 2),
            LegendRect.Left + Margin + 14, LegendRect.Top + ((i + 1) * LegendOffs - (LegendOffs div 2) + 14));
          Brush.Color := ChartColor;

//          TextRect(Rect(LegendRect.Left + Margin + 20, LegendRect.Top + ((i + 1) * LegendOffs - (LegendOffs div 2)),
//            LegendRect.Right - 5, LegendRect.Bottom - 1), LegendRect.Left + Margin + 20,
//            LegendRect.Top + ((i + 1) * LegendOffs) - (LegendOffs div 2), fDataSeries.GetDataPoint(i).GetPointLabel);

        // set end angle equal to start angle for next iteration
          SAngle := EAngle;
        end;
    end;
end;

end.

