(*****************************************************************************
 *                 TdzXYChart Component
 *
 *        This unit is part of dummzeuch.de Charts
 *
 *              (c) 2003-2005 Thomas Mueller
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
 * The Original Code is u_dzXYChart.
 *
 * The Initial Developer of the Original Code is
 * Thomas Mueller.
 * Portions created by the Initial Developer are Copyright (C) 2003-2004
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * * Paul Warren
 * * Romeo Lefter
 *****************************************************************************)

 {: @abstract(Implements TdzXYChart, capable of drawing XY, scatter, line and bar charts.)
    @author(Thomas Mueller)
    @author(Paul Warren)
    @author(Romeo Lefter)
 }

unit u_dzXYChart;

interface

uses
  SysUtils,
  Types,
  Classes,
  Math,
  Contnrs,
{$IFDEF MSWINDOWS}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  Qt,
  u_dzQWindows,
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QStdCtrls,
{$ENDIF LINUX}
  u_dzDataSeries,
  u_dzCustomCharts,
  u_dzCanvas,
  u_dzGraphics;

type
  EInvalidDataSeries = class(EdzCharts);

const
  {: Default title string for chart legend }
  DEFAULT_LEGEND_TITLE = 'Legend';
  {: Default title string for X axis }
  DEFAULT_X_AXIS_NAME = 'X-Axis';
  {: Default title string for Y axis }
  DEFAULT_Y_AXIS_NAME = 'Y-Axis';

type
  TScaleType = (stLinear{, stLogarithmic});

type
  {: Defnies the orientation of the axis labels }
  TAxisLabelOrientation = (loHorizontal, loVertical);
  {: Event procedure type used in the @link(TdzXYChart.OnScaling) event
     @param(Sender is the sending chart)
     @param(MinX is the minimum X value to display in the chart)
     @param(MaxX is the maximum X value to display in the chart)
     @param(MinY is the minimum Y value to display in the chart)
     @param(MaxY is the maximum Y value to display in the chart) }
  TOnScaling = procedure(_Sender: TObject; var _MinX, _MaxX, _MinY, _MaxY: Double) of object;
  {: Event procecure used in the @link(TdzChartAxis.OnFormatLabel) event.
     @param(Sender is the sending axis)
     @param(Value is the label's value to be formatted)
     @param(Text is the actual label text, defaults to FloatToStr(Value) ) }
  TOnFormatAxisLabel = procedure(_Sender: TObject; _Value: Double; var _Text: string) of object;
  {: Event procedure used in the @link(TdzChartAxis.OnGetNextLabel) event.
     @param(Sender is the sending axis)
     @param(LabelIndex is the current label's index, starting with 0 and
            by 1 after every call to this event)
     @param(Value is the position of the next label)
     @param(Stop is a boolean indicating whether the call returned a label for
            the axis, it defaults to true so it must be set to false as long
            as there are labels to draw, the last call must set Stop to true
            the value of Value will be ignored.) }
  TOnGetNextAxisLabel = procedure(_Sender: TObject; _LabelIndex: integer; var _Value: double; var _Stop: boolean) of object;

type
  TdzCustomXYChart = class;

  {: Describes an axis scale }
  TdzCustomAxisScale = class(TComponent)
  protected
    {: Stores the Automatic property }
    fAutomatic: boolean;
    {: Stores the Min property }
    fMin: double;
    {: Stores the Max property }
    fMax: double;
    {: Stores the Divisions property }
    fDivisions: double;
    {: Stores the ScaleType property }
    fScaleType: TScaleType;
    {: Calls the chart's refresh method }
    procedure RefreshChart;
    {: Setter method for Automatic property, calls RefreshChart if value changed }
    procedure SetAutomatic(const _Automatic: boolean);
    {: Setter method for Divisions property, calls RefreshChart if value changed }
    procedure SetDivisions(const _Divisions: double);
    {: Setter method for Max property, calls RefreshChart if value changed }
    procedure SetMax(const _Max: double);
    {: Setter method for Min property, calls RefreshChart if value changed }
    procedure SetMin(const _Min: double);
    {: Setter method for ScaleType property, calls RefreshChart if value changed }
    procedure SetScaleType(const _ScaleType: TScaleType);
    {: If set to true, automatic scaling is enabled, so the values of the
       Min, Max and Divisions will be ignored. }
    property Automatic: boolean read fAutomatic write SetAutomatic stored true;
    {: Minimum value of the axis scale (Only valid if Automatich is set to false)
       See @link(Divisions) for an example. }
    property Min: double read fMin write SetMin;
    {: Maximum value of the axis scale (Only valid if Automatich is set to false)
       See @link(Divisions) for an example. }
    property Max: double read fMax write SetMax;
    {: Division value of the axis scale (Only valid if Automatic is set to false)
       this is the number of divisions the axis scale will be divided into to
       place axis labels.
       Example:
       @longcode(#
         MyChart.BeginUpdate;
         try
           MyChart.LeftAxis.Scale.Automatic := false;
           MyChart.LeftAxis.Scale.Min := 0;
           MyChart.LeftAxis.Scale.Max := 10;
           MyChart.LeftAxis.Scale.Divisions := 2;
         finally
           MyChart.EndUpdate;
         end;
       #)
       Will make the chart start at Y = 0 and end at Y = 10 and divide this
       into two parts, resulting in labels at Y = 0, 5 and 10. }
    property Divisions: double read fDivisions write SetDivisions;
    {: determines whether the scale is a linear or logarithmic scale }
    property ScaleType: TScaleType read fScaleType write SetScaleType;
  public
    {: Constructor for @classname }
    constructor Create(_Owner: TComponent); override;
  end;

  {: @classname extends @inherited by publishing the properties Automatic,
     Min, Max and Divisions }
  TdzAxisScale = class(TdzCustomAxisScale)
  published
    {: see @inherited }
    property Automatic;
    {: see @inherited }
    property Min;
    {: see @inherited }
    property Max;
    {: see @inherited }
    property Divisions;
    {: see @inherited }
    property ScaleType;
  end;

  {: Describes a chart axis for a @link(TdzXYChart) }
  TdzCustomChartAxis = class(TdzCustomTitle)
  protected
    {: Used internally to store the "natural" label orientation, that is
       horizontal for the left and vertical for the bottom axis }
    fNaturalLabelOrientation: TAxisLabelOrientation;
    {: Stores the LabelFont property }
    fLabelFont: TFont;
    {: Stores the LabelOrientation property }
    fLabelOrientation: TAxisLabelOrientation;
    {: Stores the Scale subcomponent }
    fScale: TdzAxisScale;
    {: Stores the OnGetNextLabel event }
    fOnGetNextLabel: TOnGetNextAxisLabel;
    {: Stores the OnFormatLabel event }
    FOnFormatLabel: TOnFormatAxisLabel;
    {: Setter method for the LabelFont property }
    procedure SetLabelFont(_LabelFont: TFont);
    {: Setter method for the LabelOrientation property }
    procedure SetLabelOrientation(_LabelOrientation: TAxisLabelOrientation);
    {: Initializes the default labels for the axis
       @param(Size is the current axis size in pixels)
       @param(Min is the minimum value of the axis as determined by the chart component)
       @param(Max is the maximum value of the axis as determined by the chart component)
       @param(Canvas is the canvas used to draw the axis) }
    procedure InitDefaultLabels(_Size: integer; const _Min, _Max: double; const _Canvas: IdzCanvas);
    {: Calls the OnGetNextLabel event if assigned, otherwise uses the values of
       the Scale subcomponent to calculate the axis labels.
       @param(Idx is the label index, starting with 0 and incremented by 1 after
              each call)
       @param(Value is the value of the next label)
       @returns(true, if there is no (more) label, false otherwise) }
    function GetNextLabel(_Idx: integer; var _Value: double): boolean;
    {: Calls the OnFormatLabel event if assigned, otherwise the label will
       be formatted using FloatToStr(Value).
       @param(Value is the value to be formatted)
       @returns(A formatted label string) }
    function FormatLabel(const _Value: double): string;
    {: Calculates the space in pixels needed for drawing the axis' labels
       @param(Canvas is the canvas used to calculate the space) }
    function CalcLabelingSpace(const _Canvas: IdzCanvas): integer; virtual;

    {: @name is a sub component @link(TdzAxisScale) describing how the axis is to be scaled }
    property Scale: TdzAxisScale read fScale;
    {: @name is the font to be used for drawing the axis labels }
    property LabelFont: TFont read fLabelFont write SetLabelFont;
    {: @name is the orientation of the label text }
    property LabelOrientation: TAxisLabelOrientation read fLabelOrientation write SetLabelOrientation;

    {: @name is called to get the axis label positions, if not assigned, the
       label positions will be determined by the @link(Scale) property }
    property OnGetNextLabel: TOnGetNextAxisLabel read fOnGetNextLabel write fOnGetNextLabel;
    {: @name is called to format an axis label, if not assigned the label will
       be formatted as @code(FloatToStr(Value)) }
    property OnFormatLabel: TOnFormatAxisLabel read FOnFormatLabel write FOnFormatLabel;
  public
    {: Creates a new @classname object }
    constructor Create(_Owner: TComponent); override;
    {: Destroys a @classname object }
    destructor Destroy; override;
  end;

  {: @classname extends @inherited by publishing the properties
    Title, TitleFont, LabelFont, LabelOrientation and Scale and the
    events OnGetNextLabel, OnFormatLabel and OnNeedLines. See @inherited
    for a description of these. }
  TdzChartAxis = class(TdzCustomChartAxis)
  published
    {: see @inherited }
    property Title;
    {: see @inherited }
    property TitleFont;
    {: see @inherited }
    property LabelFont;
    {: see @inherited }
    property LabelOrientation;
    {: see @inherited }
    property OnGetNextLabel;
    {: see @inherited }
    property OnFormatLabel;
    {: see @inherited }
    property Scale;
  end;

  {: @name initializes a @inherited for a bottom axis, that is, it sets
     the fNaturalLabelOrientation to vertical. }
  TdzBottomChartAxis = class(TdzChartAxis)
  public
    {: @name creates a new @classname}
    constructor Create(_Owner: TComponent); override;
  end;

  {: @name initializes a @inherited for a left axis, that is, it sets
     the fNaturalLabelOrientation to horizontal. }
  TdzLeftChartAxis = class(TdzChartAxis)
  public
    {: @name creates a new @classname}
    constructor Create(_Owner: TComponent); override;
  end;

  {: Determines the position of the chart's legend or turns it off. }
  TLegendPosition = (lpNoLegend, lpLeft, lpRight, lpTop, lpBottom);

  {: @name describes a legend for a @link(TdzXYChart). }
  TdzXYChartLegend = class(TdzCustomTitle)
  protected
    {: @Name stores the @link(Position) property }
    fPosition: TLegendPosition;
    {: @Name stores the @link(LegendFont) property }
    fLegendFont: TFont;
    {: @Name stores the @link(Frame) sub component }
    fFrame: TdzChartFrame;
    {: @Name stores the @link(BackGround) property }
    fBackGround: TColor;
    {: @Name stores the @link(Columns) property }
    fColumns: integer;
    {: @name is the setter method for the @link(Background) property }
    procedure SetBackGround(const _Background: TColor);
    {: @name is the setter method for the @link(Columns) property }
    procedure SetColumns(_Columns: integer);
    {: @name is the setter method for the @link(LegendFont) property }
    procedure SetLegendFont(const _LegendFont: TFont);
    {: @name is the setter method for the @link(Position) property }
    procedure SetPosition(const _Position: TLegendPosition);
  public
    {: @name creates a new @classname object}
    constructor Create(_Owner: TComponent); override;
    {: @name destroys a @classname object }
    destructor Destroy; override;
  published
    {: @name describes the font used to draw the legend entries }
    property LegendFont: TFont read fLegendFont write SetLegendFont;
    {: @name describes the position of the legend on the chart, if no legend
       is desired, set to lpNoLegend }
    property Position: TLegendPosition read fPosition write SetPosition;
    {: @name describes the legend's frame, that is color, size etc. }
    property Frame: TdzChartFrame read fFrame;
    {: @name describes the background color used for the legend }
    property BackGround: TColor read fBackGround write SetBackGround;
    {: @name describes the legend's title. If set to an empty string,
       no space is alloceted for the title. }
    property Title stored true;
    {: @name describes the font used to draw the legend's title }
    property TitleFont;
    {: @name is the number of columns in the legend }
    property Columns: integer read fColumns write SetColumns;
  end;

  {: @name is internally used for storing point descriptions }
  TdzPointDesc = class
  protected
    {: stores the point's X coordinate }
    fX: double;
    {: stores the point's Y coordinate }
    fY: double;
    {: stores the point's X position in pixels }
    fXPos: integer;
    {: stores the point's Y position in pixels }
    fYPos: integer;
  public
    {: @Name creats a new @classname object }
    constructor Create(const _x, _y: double; _XPos, _YPos: integer);
  end;

  {: Event procedure used in the @link(TdzXYChart.OnGetPointHint) event.
     @param(Sender is the sending chart)
     @param(X is the X coordinate of the point)
     @param(Y is the Y coordinate of the point)
     @param(Hint is the hint text, initialized to
            @code(x = <xvalue>
                  y = <yvalue>)
            where the values are formatted calling the FormatLabel method
            of the respective axis.
            If set to an empty string, no hint will be displayed.) }
  TOnGetPointHint = procedure(_Sender: TObject; const _x, _y: double; var _Hint: string) of object;

  {: @name is the custom ancestor of TdzXYChart for drawing XY, scatter, line and bar charts }
  TdzCustomXYChart = class(TdzCustomChart)
  private
  protected
    {: @name stores the @link(BottomAxis) property }
    FBottomAxis: TdzChartAxis;
    {: @name stores the @link(LeftAxis) property }
    FLeftAxis: TdzChartAxis;
    {: @name stores the @link(Legend) property }
    FLegend: TdzXYChartLegend;
    {: @name stores the @link(MinX) property }
    fMinX: Double;
    {: @name stores the @link(MinY) property }
    fMinY: Double;
    {: @name stores the @link(MaxX) property }
    fMaxX: Double;
    {: @name stores the @link(MaxY) property }
    fMaxY: Double;
    {: @name stores an internally used list of @link(TdzPointDesc) objects }
    fPoints: TObjectList;
    {: @name stores an internally used list of IdzSingleValueDataSeries and
       IdzMultiValueDataSeries interfaces }
    fDataSeriesList: TInterfaceList;
    {: @name stores the OnScaling event }
    FOnScaling: TOnScaling;
    {: @name stores the OnGetPointHint event }
    FOnGetPointHint: TOnGetPointHint;
    {: @name calculates the width of the bars in a bar chart }
    function CalcBarWidth(const _Target: IdzGraphics): integer;
    {: @name gets the maximum point count for all data series }
    function GetDataSeriesMaxPointCount: integer;
    {: @name draws a single point in a XY or Scatter chart
       @param(Series is the data series for this point)
       @param(AbsX is the absolute X position of the point in pixels (after scaling))
       @param(AbsY is the absolute Y position of the point in pixels (after scaling))
       @param(ForLegend is a boolean determining whether the point is drawn for
              the legend or for the actual chart. This will switch the meaning of
              psHorizontalLine and psVerticalLine for the legend. }
    procedure DrawPoint(const _Target: IdzGraphics; const _Series: IdzDataSeries; _AbsX, _AbsY: integer; _ForLegend: boolean = false);
    {: @name draws a line between the given coordnates for a XY or line chart
       @param(Series is the data series for this line)
       @param(X1 is the X coordinate of the starting point in pixels (after scaling)
       @param(Y1 is the Y coordinate of the starting point in pixels (after scaling)
       @param(X2 is the X coordinate of the end point in pixels (after scaling)
       @param(Y2 is the Y coordinate of the end point in pixels (after scaling) }
    procedure DrawLine(const _Target: IdzGraphics; const _Series: IdzDataSeries; _X1, _Y1, _X2, _Y2: integer);
    {: @name calculates space for the chart's area by first calling the inherited method and
       then substracting the space for the legend and the axis descriptions. }
    function CalcChartRect(const _Target: IdzGraphics): TRect; override;
    {: @name calculates space for the chart's drawing area (used for values) }
    function CalcDrawRect(const _Target: IdzGraphics): TRect;
    {: @name calculates the space used for the chart's legend }
    function CalcLegendRect(const _Target: IdzGraphics; var _ChartRect: TRect): TRect;
    {: @name scales the chart to the available drawing space }
    procedure ScaleChart; override;
    {: @name transform point's data to chart dimension for a given axis
       (used internally by @link(NormalizePtX) and @link(NormalizeptY))
       @param(Point is the point's position in regard to an axis)
       @param(Min is the minimum of the axis)
       @param(Max is the maximum of the axis)
       @param(Size is the length of the axis in pixels) }
    function NormalizePt(const _Point, _Min, _Max: double; _Size: integer): double;
    {: @name transforms X-axis data to chart dimensions
       @param(Point is the X-axis position of a point) }
    function NormalizePtX(const _Target: IdzGraphics; const _Point: Double): Double;
    {: @name transforms Y-axis data to chart dimensions
       @param(Point is the Y-axis position of a point) }
    function NormalizePtY(const _Target: IdzGraphics; const _Point: Double): Double;
    {: @name draws the grid at the label positions of both axis
       @param(Rect is the space used for the grid) }
    procedure DrawGrid(const _Target: IdzGraphics; _Rect: TRect); override;
    {: @name drawas the chart's bottom scale including ticks and labels }
    procedure DrawBottomScale(const _Target: IdzGraphics); override;
    {: @name drawas the chart's left scale including ticks and labels }
    procedure DrawLeftScale(const _Target: IdzGraphics); override;
    {: @name draws the chart's data points, lines and bars }
    procedure DrawDataPoints(const _Target: IdzGraphics); override;
    {: @name draws the frame items of the chart, that is the title, the legend and the
       axis scales }
    procedure DrawFrameItems(const _Target: IdzGraphics); override;
    {: @name draws a legend entry for a data series
       @param(Canvas is the canvas used for drawing)
       @param(Series is the data series for which to draw the legend entry)
       @param(X is the X coordinate in pixels of the entry)
       @param(Y is the Y coordinate in pixels of the entry) }
    procedure DrawLegendEntry(const _Target: IdzGraphics; const _Series: IdzDataSeries; _X, _Y: integer);
    {: @name draws the chart's legend
       @param(Canvas is the canvas used for drawing)
       @param(LegendRect is the space for the legend) }
    procedure DrawLegend(const _Target: IdzGraphics; _LegendRect: TRect);
    {: @name overrides the inherited method to conditionally display point hints
       @param(Shift describes the special keys pressed)
       @param(X is the X coordinate in pixels of the mouse pointer)
       @param(Y is the Y coordinate in pixels of the mouse pointer) }
    procedure MouseMove(_Shift: TShiftState; _X, _Y: Integer); override;
    {: @name calls the OnGetPointHint event if assigned }
    procedure doGetPointHint(const _X, _Y: double; var _Hint: string);

    property MinX: Double read fMinX;
    property MaxX: Double read fMaxX;
    property MinY: Double read fMinY;
    property MaxY: Double read fMaxY;
    property BottomAxis: TdzChartAxis read FBottomAxis;
    property LeftAxis: TdzChartAxis read FLeftAxis;
    property Legend: TdzXYChartLegend read FLegend;
    property OnScaling: TOnScaling read FOnScaling write FOnScaling;
    property OnGetPointHint: TOnGetPointHint read FOnGetPointHint write FOnGetPointHint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {: Add a data series to the chart. (Note: the parameter is not 'const' because
       this could run havoc with reference counting)
       @param(DataSeries is a IdzSingleValueDataSeries or IdzMultiValueDataSeries
              interface for a data series to add) }
    function AddDataSeries(_DataSeries: IdzDataSeries): integer;
    function RemoveDataSeries(const _DataSeries: IdzDataSeries): integer;
  published
  end;

  TdzXYChart = class(TdzCustomXYChart)
  published
    property OnScaling;
    property OnGetPointHint;
    property Align;
    property BackGround;
    property ChartColor;
    property Font;
    property Grid;
    property ChartTitle;
    property BottomAxis;
    property LeftAxis;
    property Legend;
    property ChartFrame;
  end;

implementation

type
  TLineDescription = class
  protected
    fColor: TColor;
    fDescription: string;
  public
    constructor Create(_Color: TColor; const _Description: string);
    property Color: TColor read fColor;
    property Description: string read fDescription;
  end;

  { TLineDescription }

constructor TLineDescription.Create(_Color: TColor; const _Description: string);
begin
  inherited Create;
  fColor := _Color;
  fDescription := _Description;
end;

{ TdzCustomXYChart }

constructor TdzCustomXYChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fPoints := TObjectList.Create;

  FBottomAxis := TdzBottomChartAxis.Create(Self);
  FBottomAxis.SetSubComponent(true);
  FBottomAxis.Name := self.Name + 'BottomAxis';
  FBottomAxis.Title := DEFAULT_X_AXIS_NAME;
  FBottomAxis.TitleFont.Name := 'Arial';
  FBottomAxis.TitleFont.Size := 8;
  FBottomAxis.LabelFont.Name := 'Arial';
  FBottomAxis.LabelFont.Size := 8;

  FLeftAxis := TdzLeftChartAxis.Create(self);
  FLeftAxis.SetSubComponent(true);
  FLeftAxis.Name := self.Name + 'LeftAxis';
  FLeftAxis.Title := DEFAULT_Y_AXIS_NAME;
  FLeftAxis.TitleFont.Name := 'Arial';
  FLeftAxis.TitleFont.Size := 8;

  FLegend := TdzXYChartLegend.Create(Self);
  FLegend.SetSubComponent(true);
  FLegend.Name := self.Name + 'Legend';
  FLegend.Position := lpBottom;
  FLegend.Title := DEFAULT_LEGEND_TITLE;
  FLegend.TitleFont.Name := 'Arial';
  FLegend.TitleFont.Size := 8;
  FLegend.TitleFont.Style := [fsBold];
  FLegend.LegendFont.Name := 'Arial';
  FLegend.LegendFont.Size := 8;
  FLegend.BackGround := clWhite;
  FLegend.Columns := 1;

  fDataSeriesList := TInterfaceList.Create;

  fMinX := -1;
  fMaxX := 1;
  fMinY := -1;
  fMaxY := 1;
end;

destructor TdzCustomXYChart.Destroy;
var
  i: integer;
begin
  FreeAndNil(fPoints);

  for i := 0 to fDataSeriesList.Count - 1 do
    (fDataSeriesList[i] as IdzDataSeries).Notification(Self, opRemove);
  FLegend.Free;
  FBottomAxis.Free;
  FLeftAxis.Free;
  fDataSeriesList := nil;
  inherited Destroy;
end;

function TdzCustomXYChart.CalcLegendRect(const _Target: IdzGraphics; var _ChartRect: TRect): TRect;
var
  Cnvs: IdzCanvas;

  function CalcLongestSeriesText: integer;
  var
    i: integer;
    w: integer;
    Series: IdzDataSeries;
  begin
    Result := 0;
    for i := 0 to fDataSeriesList.Count - 1 do
      begin
        Series := fDataSeriesList[i] as IdzDataSeries;
        w := Cnvs.TextWidth(Series.GetCaption);
        if w > Result then
          Result := w;
      end;
  end;

var
  i: integer;
  Cnt: integer;
  h: integer;
  MaxW: integer;
  TitleWidth: integer;
begin
  Result := _ChartRect;

  if fLegend.Position = lpNoLegend then
    begin
      // no legend -> size 0
      Result.Right := Result.Left;
      Result.Bottom := Result.Top;
      exit;
    end;

  Cnvs := _Target.Canvas;
  if fLegend.Title <> '' then
    begin
      Cnvs.Font := fLegend.TitleFont;
      h := Round(Cnvs.TextHeight('Aj') * 1.5);
      TitleWidth := Cnvs.TextWidth(fLegend.Title);
    end
  else
    begin
      TitleWidth := 0;
      h := 0;
    end;
  Cnvs.Font := fLegend.LegendFont;
  Cnt := 0;
  for i := 0 to fDataSeriesList.Count - 1 do
    if (fDataSeriesList[i] as IdzDataSeries).GetCaption <> '' then
      Inc(Cnt);
  MaxW := (CalcLongestSeriesText + 30 + 8) * fLegend.Columns;
  h := h + Ceil(Cnt / fLegend.Columns) * Round(Cnvs.TextHeight('Aj') * 1.5);
  if TitleWidth > MaxW then
    MaxW := TitleWidth;
  h := h + 8;
  MaxW := MaxW;
  case fLegend.Position of
    lpLeft:
      begin
        Result.Right := Result.Left + MaxW;
        Result.Top := (Result.Bottom - Result.Top - h) div 2;
        Result.Bottom := Result.Top + h;
        _ChartRect.Left := _ChartRect.Left + MaxW + 8;
      end;
    lpRight:
      begin
        Result.Left := Result.Right - MaxW;
        Result.Top := (Result.Bottom - Result.Top - h) div 2;
        Result.Bottom := Result.Top + h;
        _ChartRect.Right := _ChartRect.Right - MaxW - 8;
      end;
    lpTop:
      begin
        Result.Bottom := Result.Top + h;
        Result.Left := (Result.Right - Result.Left - MaxW) div 2;
        Result.Right := Result.Left + MaxW;
        _ChartRect.Top := _ChartRect.Top + h + 8;
      end;
    lpBottom:
      begin
        Result.Top := Result.Bottom - h;
        Result.Left := (Result.Right - Result.Left - MaxW) div 2;
        Result.Right := Result.Left + MaxW;
        _ChartRect.Bottom := _ChartRect.Bottom - h - 8;
      end;
  end;
end;

function TdzCustomXYChart.CalcChartRect(const _Target: IdzGraphics): TRect;
var
  Cnvs: IdzCanvas;
  LegendRect: TRect;
begin
  Result := inherited CalcChartRect(_Target);

  // in addition leave some space for the Axis labels and the legend

  LegendRect := CalcLegendRect(_Target, Result);

  Cnvs := _Target.Canvas;

  // leave 1.5 times the axis caption height for the axis captions
  fLeftAxis.InitDefaultLabels(0, MinY, MaxY, Cnvs);
  Result.Left := Result.Left + LeftAxis.CalcLabelingSpace(Cnvs);

  fBottomAxis.InitDefaultLabels(0, MinX, MaxX, Cnvs);
  Result.Bottom := Result.Bottom - BottomAxis.CalcLabelingSpace(Cnvs);
end;

{ CalcDrawRect - get area for drawing }

function TdzCustomXYChart.CalcDrawRect(const _Target: IdzGraphics): TRect;
begin
  Result := CalcChartRect(_Target);
  { offset inwards 3 and 4 percent }
  Result.Left := Result.Left + ((4 * Width) div 100);
  Result.Top := Result.Top + ((3 * Width) div 100);
  Result.Right := Result.Right - ((4 * Width) div 100);
  Result.Bottom := Result.Bottom - ((3 * Width) div 100);
end;

function TdzCustomXYChart.GetDataSeriesMaxPointCount: integer;
var
  i: integer;
  Series: IdzDataSeries;
begin
  Result := 0;
  for i := 0 to fDataSeriesList.Count - 1 do
    begin
      Series := fDataSeriesList[i] as IdzDataSeries;
      if Series.GetPointCount > Result then
        Result := Series.GetPointCount;
    end;
end;

{ GetBarWidth - get width of bars for bar chart }

function TdzCustomXYChart.CalcBarWidth(const _Target: IdzGraphics): integer;
const
  MaxBarWidth: integer = 15;
var
  DrawRect: TRect;
  PointCount: integer;
begin
  DrawRect := CalcDrawRect(_Target);
  PointCount := GetDataSeriesMaxPointCount;
  if PointCount > 0 then
    Result := (((DrawRect.Right - DrawRect.Left) div PointCount) div 2)
  else
    Result := DrawRect.Right - DrawRect.Left;
  if Result > MaxBarWidth then
    Result := MaxBarWidth;
end;

{ ScaleChart - calculate extrema }

procedure TdzCustomXYChart.ScaleChart;
var
  i: integer;
  MinX, MaxX: double;
  MinY, MaxY: double;
  dbl: double;
  SumHalf: double;
  AbsDiffHalf: double;
  Series: IdzDataSeries;
  First: boolean;
begin
  // determine min and max values from all data series
  if BottomAxis.Scale.Automatic then
    begin
      fMinX := Infinity;
      fMaxX := NegInfinity;
    end
  else
    begin
      fMinX := BottomAxis.Scale.Min;
      fMaxX := BottomAxis.Scale.Max;
    end;
  if LeftAxis.Scale.Automatic then
    begin
      fMinY := Infinity;
      fMaxY := NegInfinity;
    end
  else
    begin
      fMinY := LeftAxis.Scale.Min;
      fMaxY := LeftAxis.Scale.Max;
    end;

  First := True;
  for i := 0 to fDataSeriesList.Count - 1 do
    begin
      Series := fDataSeriesList[i] as IdzDataSeries;
      if Series.GetMinAndMax(MinX, MaxX, MinY, MaxY) then
        begin
          if BottomAxis.Scale.Automatic then
            begin
              if not IsInfinite(MaxX) and not IsInfinite(MinX) then
                begin
                  SumHalf := (MaxX + MinX) / 2;
                  AbsDiffHalf := abs(MaxX - MinX) / 2;

                  dbl := SumHalf - AbsDiffHalf;
                  if First or (dbl < fMinX) then
                    fMinX := dbl;

                  dbl := SumHalf + AbsDiffHalf;
                  if First or (dbl > fMaxX) then
                    fMaxX := dbl;
                end;
            end;

          if LeftAxis.Scale.Automatic then
            begin
              if not IsInfinite(MaxY) and not IsInfinite(MinY) then
                begin
                  SumHalf := (MaxY + MinY) / 2;
                  AbsDiffHalf := abs(MaxY - MinY) / 2;

                  dbl := SumHalf - AbsDiffHalf;
                  if First or (dbl < fMinY) then
                    fMinY := dbl;

                  dbl := SumHalf + AbsDiffHalf;
                  if First or (dbl > fMaxY) then
                    fMaxY := dbl;
                end;
            end;
          First := False;
        end;
    end;

  if IsInfinite(fMinX) then
    fMinX := 0;
  if IsInfinite(fMaxX) then
    fMaxX := 0;
  if IsInfinite(fMinY) then
    fMinY := 0;
  if IsInfinite(fMaxY) then
    fMaxY := 0;

  if Assigned(FOnScaling) then
    FOnScaling(Self, fMinX, fMaxX, fMinY, fMaxY);

  // make sure minima and maxima are not equal or in wrong order
  if fMaxX - fMinX <= 0 then
    begin
      fMinX := fMaxX - 0.1;
      fMaxX := fMaxX + 0.1;
    end;
  if fMaxY - fMinY <= 0 then
    begin
      fMinY := fMaxY - 0.1;
      fMaxY := fMaxY + 0.1;
    end;
end;

{ procedure to draw grids if desired }

procedure TdzCustomXYChart.DrawGrid(const _Target: IdzGraphics; _Rect: TRect);
var
  Cnvs: IdzCanvas;
  GraphWd: integer;
  GraphHt: integer;
  RangeX: double;
  RangeY: double;
  DrawRect: TRect;
  ChartRect: TRect;

  procedure DoDrawGridX(_Pos: double);
  var
    XPos: integer;
  begin
    XPos := Round(DrawRect.Left + ((_Pos - fMinX) * GraphWd) / RangeX);
    Cnvs.Line(XPos, ChartRect.Top + FChartFrame.Width, XPos, ChartRect.Bottom - FChartFrame.Width);
  end;

  procedure DoDrawGridY(_Pos: double);
  var
    YPos: integer;
  begin
    YPos := Round(DrawRect.Bottom - ((_Pos - fMinY) * GraphHt) / RangeY);
    Cnvs.Line(ChartRect.Left + FChartFrame.Width, YPos, ChartRect.Right - FChartFrame.Width, YPos);
  end;

var
  Idx: integer;
  Value: double;
begin
  if FGrid.GridType = gtNone then
    Exit;
  DrawRect := CalcDrawRect(_Target);
  ChartRect := CalcChartRect(_Target);
  Cnvs := _Target.Canvas;
  Cnvs.Pen.Width := 1;
  GraphWd := (DrawRect.Right - DrawRect.Left);
  GraphHt := (DrawRect.Bottom - DrawRect.Top);

  RangeX := fMaxX - fMinX;
  RangeY := fMaxY - fMinY;

  Cnvs.Pen.Color := FGrid.Color;
  Cnvs.Pen.Style := FGrid.LineStyle;

  if Grid.GridType in [gtVert, gtBoth] then
    begin
      BottomAxis.InitDefaultLabels(GraphWd, fMinX, fMaxX, Cnvs);
      Value := fMinX;
      Idx := 0;
      while not BottomAxis.GetNextLabel(Idx, Value) do
        begin
          DoDrawGridX(Value);
          Inc(Idx);
        end;
    end;

  if Grid.GridType in [gtBoth, gtHorz] then
    begin
      LeftAxis.InitDefaultLabels(GraphHt, fMinY, fMaxY, Cnvs);
      Value := fMinY;
      Idx := 0;
      while not LeftAxis.GetNextLabel(Idx, Value) do
        begin
          DoDrawGridY(Value);
          Inc(Idx);
        end;
    end;
end;

procedure TdzCustomXYChart.DrawBottomScale;
var
  DrawRect: TRect;
  ChartRect: TRect;
  Cnvs: IdzCanvas;
  TextHeight: integer;

  procedure DoDrawScale(_Offs: integer; _Value: Double);
  var
    s: string;
    x: integer;
    y: integer;
    w: integer;
  begin
    s := BottomAxis.FormatLabel(_Value);
    x := DrawRect.Left + _Offs;
    y := ChartRect.Bottom;
    if BottomAxis.LabelOrientation = loVertical then
      w := Cnvs.TextWidthAngle(90, s)
    else
      w := Cnvs.TextWidth(s);

    Cnvs.Line(x, y, x, y - FChartFrame.Width - 5);

    if FBottomAxis.LabelOrientation = loVertical then
      begin
        x := x + 1;
{$IFDEF MSWINDOWS}
        // x is used differently in Windows and QT
        // in Windows x is the left point of the text
        // in QT it is the base line
        // this should really be handled in TextOutAngle, but ...
        x := x - TextHeight div 2;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX} // maybe this should really be "ifdef QT" or something
        x := x + TextHeight div 4;
{$ENDIF LINUX}
        Cnvs.TextOutAngle(90, x, y + 3 + w, s)
      end
    else
      Cnvs.TextOut(x + 1 - w div 2, y + 3, s);
  end;

var
  Idx: integer;
  Value: double;
  GraphWid: integer;
  Range: Double;
begin
  ChartRect := CalcChartRect(_Target);
  DrawRect := CalcDrawRect(_Target);
  GraphWid := (DrawRect.Right - DrawRect.Left);
  Range := fMaxX - fMinX;
  Cnvs := _Target.Canvas;
  Cnvs.Pen.Color := clBlack;
  Cnvs.Brush.Color := FBackGround;
  Cnvs.Font := fBottomAxis.LabelFont;
  FBottomAxis.InitDefaultLabels(GraphWid, fMinX, fMaxX, Cnvs);
  TextHeight := Cnvs.TextHeight('Aj');
  Value := fMinX;
  Idx := 0;
  while not FBottomAxis.GetNextLabel(Idx, Value) do
    begin
      DoDrawScale(Round(((Value - fMinX) * GraphWid) / Range), Value);
      Inc(Idx);
    end;
end;

procedure TdzCustomXYChart.DrawLeftScale;
var
  GraphHt: integer;
  DrawRect: TRect;
  ChartRect: TRect;
  Cnvs: IdzCanvas;
  TextHeight: integer;

  procedure DoDrawScale(_Offs: integer; _Value: Double);
  var
    S: string;
    x: integer;
    y: integer;
    w: integer;
  begin
    S := FLeftAxis.FormatLabel(_Value);
    x := ChartRect.Left;
    y := DrawRect.Bottom - _Offs;
    w := Cnvs.TextWidth(s);

    Cnvs.Line(x, y, x + FChartFrame.Width + 5, y);

    if FLeftAxis.LabelOrientation = loVertical then
      begin
        x := x - 3;
{$IFDEF MSWINDOWS}
        // x is used differently in Windows and QT
        // in Windows x is the left point of the text
        // in QT it is the base line
        // this should really be handled in TextOutAngle, but ...
        x := x - TextHeight;
{$ENDIF MSWINDOWS}
        Cnvs.TextOutAngle(90, x, y + (w div 2), S);
      end
    else
      Cnvs.TextOut(x - 3 - w, y - (TextHeight div 2), S);
  end;

  function Log10Z(const _Value: double): extended;
  begin
    if _Value = 0 then
      Result := 0
    else
      Result := Log10(_Value);
  end;

var
  Idx: integer;
  Value: double;
  Range: Double;
begin
  ChartRect := CalcChartRect(_Target);
  DrawRect := CalcDrawRect(_Target);
  GraphHt := (DrawRect.Bottom - DrawRect.Top);
  Range := fMaxY - fMinY;
  Cnvs := _Target.Canvas;
  Cnvs.Pen.Color := clBlack;
  Cnvs.Brush.Color := FBackGround;
  Cnvs.Font := FLeftAxis.LabelFont;
  TextHeight := Cnvs.TextHeight('Aj');
  FLeftAxis.InitDefaultLabels(GraphHt, fMinY, fMaxY, Cnvs);
  Value := fMinY;
  Idx := 0;
  while not FLeftAxis.GetNextLabel(Idx, Value) do
    begin
//      case LeftAxis.Scale.ScaleType of
//        stLogarithmic:
//          DoDrawScale(Round(Log10Z(Value) * GraphHt), Value);
//      else
        DoDrawScale(Round(((Value - fMinY) * GraphHt) / Range), Value);
//      end;
      Inc(Idx);
    end;
end;

function TdzCustomXYChart.NormalizePt(const _Point, _Min, _Max: double; _Size: integer): double;
var
  Range: double;
begin
  Result := _Point;
  Range := _Max - _Min;
  if Range > 0 then
    Result := ((Result - _Min) * _Size) / Range;
end;

{ NormalizePtX - transform  X-axis data to chart dimensions }

function TdzCustomXYChart.NormalizePtX(const _Target: IdzGraphics; const _Point: Double): Double;
var
  DrawRect: TRect;
begin
  DrawRect := CalcDrawRect(_Target);
  Result := NormalizePt(_Point, fMinX, fMaxX, DrawRect.Right - DrawRect.Left);
end;

{ NormalizePtY - transform  Y-axis data to chart dimensions }

function TdzCustomXYChart.NormalizePtY(const _Target: IdzGraphics; const _Point: Double): Double;
var
  DrawRect: TRect;
begin
  DrawRect := CalcDrawRect(_Target);
  Result := NormalizePt(_Point, fMinY, fMaxY, DrawRect.Bottom - DrawRect.Top);
end;

procedure TdzCustomXYChart.DrawPoint(const _Target: IdzGraphics; const _Series: IdzDataSeries; _AbsX, _AbsY: integer; _ForLegend: boolean);
const // for a equilateral triangle
  a = 9;
  e = a / 1.7320508075689; // sqrt(3)
  d = e / 2;
const
  ARROW_LENGTH = 7;
  ARROW_WIDTH = 4;
var
  Cnvs: IdzCanvas;
begin
  Cnvs := _Target.Canvas;
  Cnvs.Pen.Color := _Series.GetPointColor;
  Cnvs.Pen.Style := psSolid;
  Cnvs.Pen.Width := 1;
  Cnvs.Brush.Color := _Series.GetFillColor;
  Cnvs.Brush.Style := bsSolid;
  case _Series.GetPointStyle of
    psRectangle:
      begin
        Cnvs.Rectangle(_AbsX - 3, _AbsY - 3, _AbsX + 4, _AbsY + 4);
      end;
    psCircle:
      begin
        Cnvs.Ellipse(_AbsX - 3, _AbsY - 3, _AbsX + 4, _AbsY + 4);
      end;
    psTriangle:
      begin
        Cnvs.DrawPolygon([Point(_AbsX, _AbsY - round(e)),
          Point(_AbsX - Round(a / 2), _AbsY + round(d)),
            Point(_AbsX + Round(a / 2), _AbsY + round(d)),
            Point(_AbsX, _AbsY - round(e))]);
      end;
    psDownTriangle:
      begin
        Cnvs.DrawPolygon([Point(_AbsX, _AbsY + round(e)),
          Point(_AbsX + Round(a / 2), _AbsY - round(d)),
            Point(_AbsX - Round(a / 2), _AbsY - round(d)),
            Point(_AbsX, _AbsY + round(e))]);
      end;
    psCross:
      begin
        Cnvs.Line(_AbsX + 3, _AbsY, _AbsX - 3, _AbsY);
        Cnvs.Line(_AbsX, _AbsY + 3, _AbsX, _AbsY - 3);
      end;
    psDiagCross:
      begin
        Cnvs.Line(_AbsX - 3, _AbsY - 3, _AbsX + 3, _AbsY + 3);
        Cnvs.Line(_AbsX - 3, _AbsY + 3, _AbsX + 3, _AbsY - 3);
      end;
    psStar:
      begin
        Cnvs.Line(_AbsX + 3, _AbsY + 3, _AbsX - 3, _AbsY - 3);
        Cnvs.Line(_AbsX + 3, _AbsY - 3, _AbsX - 3, _AbsY + 3);
        Cnvs.Line(_AbsX - 3, _AbsY, _AbsX + 3, _AbsY);
        Cnvs.Line(_AbsX, _AbsY - 3, _AbsX, _AbsY + 3);
      end;
    psDiamond:
      begin
        Cnvs.DrawPolygon([
          Point(_AbsX, _AbsY + 4),
            Point(_AbsX + 4, _AbsY),
            Point(_AbsX, _AbsY - 4),
            Point(_AbsX - 4, _AbsY),
            Point(_AbsX, _AbsY + 4)]);
      end;
    psSmallDot:
      Cnvs.Ellipse(_AbsX - 1, _AbsY - 1, _AbsX + 2, _AbsY + 2);
    psDownArrow:
      begin
        Cnvs.DrawPolygon([
          Point(_AbsX, _AbsY),
            Point(_AbsX - ARROW_WIDTH, _AbsY - ARROW_WIDTH),
            Point(_AbsX - 1, _AbsY - ARROW_WIDTH),
            Point(_AbsX - 1, _AbsY - ARROW_LENGTH),
            Point(_AbsX + 1, _AbsY - ARROW_LENGTH),
            Point(_AbsX + 1, _AbsY - ARROW_WIDTH),
            Point(_AbsX + ARROW_WIDTH, _AbsY - ARROW_WIDTH),
            Point(_AbsX, _AbsY)]);
      end;
    psUpArrow:
      begin
        Cnvs.DrawPolygon([
          Point(_AbsX, _AbsY),
            Point(_AbsX - ARROW_WIDTH, _AbsY + ARROW_WIDTH),
            Point(_AbsX - 1, _AbsY + ARROW_WIDTH),
            Point(_AbsX - 1, _AbsY + ARROW_LENGTH),
            Point(_AbsX + 1, _AbsY + ARROW_LENGTH),
            Point(_AbsX + 1, _AbsY + ARROW_WIDTH),
            Point(_AbsX + ARROW_WIDTH, _AbsY + ARROW_WIDTH),
            Point(_AbsX, _AbsY)]);
      end;
    psHorizontalLine:
      begin
        if _ForLegend then
          Cnvs.Line(_AbsX, _AbsY + 3, _AbsX, _AbsY - 3)
        else
          Cnvs.Line(_AbsX + 3, _AbsY, _AbsX - 3, _AbsY);
      end;
    psVerticalLine:
      begin
        if _ForLegend then
          Cnvs.Line(_AbsX + 3, _AbsY, _AbsX - 3, _AbsY)
        else
          Cnvs.Line(_AbsX, _AbsY + 3, _AbsX, _AbsY - 3);
      end;
  else // psNone
    // no points
  end;
end;

procedure TdzCustomXYChart.DrawLine(const _Target: IdzGraphics; const _Series: IdzDataSeries; _X1, _Y1, _X2, _Y2: integer);
var
  Cnvs: IdzCanvas;
begin
  Cnvs := _Target.Canvas;
  Cnvs.Pen.Color := _Series.GetLineColor;
  Cnvs.Pen.Style := _Series.GetLineStyle;
  Cnvs.Pen.Width := 1;
  Cnvs.Line(_X1, _Y1, _X2, _Y2);
end;

function ComparePoints(_Item1, _Item2: pointer): integer;
var
  Pd1: TdzPointDesc;
  pd2: TdzPointDesc;
begin
  pd1 := _Item1;
  pd2 := _Item2;
  Result := Pd2.fXPos - Pd1.fXPos;
  if Result = 0 then
    Result := Pd2.fYPos - pd1.fYPos;
end;

procedure TdzCustomXYChart.DrawDataPoints(const _Target: IdzGraphics);
var
  Cnvs: IdzCanvas;
  DrawRect: TRect;
  ChartRect: TRect;
  BarWidth: integer;

  procedure DrawBar(const _Series: IdzDataSeries; const _Point: IdzDataPoint);
  begin
    Cnvs.Pen.Color := _Series.GetLineColor;
    Cnvs.Pen.Width := 1;
    Cnvs.Brush.Style := bsSolid;
    Cnvs.Brush.Color := _Series.GetFillColor;
    Cnvs.Rectangle(DrawRect.Left + Ceil(NormalizePtX(_Target, _Point.GetX)) - BarWidth,
      DrawRect.Bottom - Round(NormalizePtY(_Target, _Point.GetY)),
      DrawRect.Left + Floor(NormalizePtX(_Target, _Point.GetX)) + BarWidth,
      DrawRect.Bottom);
  end;

  procedure DrawSeriesLines(const _Series: IdzSingleValueDataSeries); overload;

    function CalcPointX(const _Value: double): integer;
    begin
      if _Series.GetChartType = ctAlternatingLine then
        begin
          if IsInfinite(_Value) then
            begin
              if Sign(_Value) < 0 then
                Result := ChartRect.Left
              else
                Result := ChartRect.Right;
            end
          else
            begin
               Result := DrawRect.Left + Round(NormalizePtX(_Target, _Value));
               if Result < ChartRect.Left then
                 Result := ChartRect.Left
               else if Result > ChartRect.Right then
                 Result := ChartRect.Right;
            end;
        end
      else
        Result := DrawRect.Left + Round(NormalizePtX(_Target, _Value));
    end;

    function CalcPointY(const _Value: double): integer;
    begin
      if _Series.GetChartType = ctAlternatingLine then
        begin
          if IsInfinite(_Value) then
            begin
              if Sign(_Value) < 0 then
                Result := ChartRect.Bottom
              else
                Result := ChartRect.Top;
            end
          else
            begin
              Result := DrawRect.Bottom - Round(NormalizePtY(_Target, _Value));
              if Result > ChartRect.Bottom then
                Result := ChartRect.Bottom
              else if Result < ChartRect.Top then
                Result := ChartRect.Top;
            end;
        end
      else
        Result := DrawRect.Bottom - Round(NormalizePtY(_Target, _Value));
    end;

  var
    Point: IdzDataPoint;
    i: integer;
    x, y: integer;
    OldX, OldY: integer;
    LineIsOn: boolean;
  begin
    if not (_Series.GetChartType in [ctLine, ctXY, ctAlternatingLine]) then
      exit; // no lines for other types

    Point := _Series.GetDataPoint(0);
    OldX := CalcPointX(Point.GetX);
    OldY := CalcPointY(Point.GetY);
    LineIsOn := true;
    for i := 1 to _Series.GetPointCount - 1 do
      begin
        Point := _Series.GetDataPoint(i);
        x := CalcPointX(Point.GetX);
        y := CalcPointY(Point.GetY);
        case _Series.GetChartType of
          ctLine:
            begin
              DrawLine(_Target, _Series, OldX, OldY, x, y);
            end;
          ctAlternatingLine:
            begin
              if LineIsOn then
                DrawLine(_Target, _Series, OldX, OldY, x, y);
              LineIsOn := not LineIsOn;
            end;
          ctXY:
            begin
              DrawLine(_Target, _Series, OldX, OldY, x, y);
              fPoints.Add(TdzPointDesc.Create(Point.GetX, Point.GetY, x, y));
              DrawPoint(_Target, _Series, x, y);
            end;
        end;
        Oldx := x;
        OldY := y;
      end;
    if LineIsOn and (_Series.GetChartType = ctAlternatingLine) then
      DrawLine(_Target, _Series, OldX, OldY, CalcPointX(Infinity), OldY);
  end;

  procedure DrawSeriesLines(const _Series: IdzMultiValueDataSeries); overload;
  begin
  end;

  procedure DrawSeriesPoints(const _Series: IdzSingleValueDataSeries); overload;
  var
    Point: IdzDataPoint;
    i: integer;
    x, y: integer;
    Value: double;
  begin
    if not (_Series.GetChartType in [ctBar, ctScatter, ctXY, ctAlternatingLine]) then
      exit; // no point markers for other types

    for i := 0 to _Series.GetPointCount - 1 do
      begin
        Point := _Series.GetDataPoint(i);

        Value := Point.GetX;
        if IsInfinite(Value) then
          Continue; // infinite points can not be drawn
        x := DrawRect.Left + Round(NormalizePtX(_Target, Value));

        Value := Point.GetY;
        if IsInfinite(Value) then
          Continue; // infinite points can not be drawn
        y := DrawRect.Bottom - Round(NormalizePtY(_Target, Value));

        case _Series.GetChartType of
          ctBar:
            DrawBar(_Series, Point);
          ctScatter,
            ctAlternatingLine,
            ctXY:
            begin
              fPoints.Add(TdzPointDesc.Create(Point.GetX, Point.GetY, x, y));
              DrawPoint(_Target, _Series, x, y);
            end;
        end;
      end;
  end;

  procedure DrawSeriesPoints(const _Series: IdzMultiValueDataSeries); overload;
  var
    Point: IdzMultiValueDataPoint;
    PtIdx: integer;
    ValIdx: integer;
    x, YMin, YMax: integer;
    YOpen, YClose: integer;
    y: integer;
  begin
    for PtIdx := 0 to _Series.GetPointCount - 1 do
      begin
        Point := _Series.GetDataPoint(PtIdx);
        if Point.GetValueCount = 0 then
          Continue;
        x := DrawRect.Left + Round(NormalizePtX(_Target, Point.GetX));
        YMin := DrawRect.Bottom - Round(NormalizePtY(_Target, Point.GetY(0)));
        YMax := DrawRect.Bottom - Round(NormalizePtY(_Target, Point.GetY(Point.GetValueCount - 1)));
        if Point.GetValueCount > 1 then
          begin
            YOpen := DrawRect.Bottom - Round(NormalizePtY(_Target, Point.GetY(1)));
            YClose := DrawRect.Bottom - Round(NormalizePtY(_Target, Point.GetY(Point.GetValueCount - 2)));
          end
        else
          begin
            YOpen := YMin;
            YClose := YMax;
          end;
        case _Series.GetChartType of
          ctHighLow:
            DrawLine(_Target, _Series, x, YMin, x, YMax);
          ctHighLowOpenClose:
            begin
              DrawLine(_Target, _Series, x, YMin, x, YMax);
              if Point.GetValueCount > 1 then
                begin
                  DrawLine(_Target, _Series, x - 5, YOpen, x, YOpen);
                  DrawLine(_Target, _Series, x, YClose, x + 5, YClose);
                end;
            end;
          ctCandle:
            begin
              DrawLine(_Target, _Series, x, YMin, x, YMax);
              if Point.GetValueCount > 1 then
                begin
                  Cnvs.Pen.Color := _Series.GetLineColor;
                  Cnvs.Pen.Width := 1;
                  Cnvs.Brush.Style := bsSolid;
                  if YOpen > YClose then
                    Cnvs.Brush.Color := _Series.GetBullishColor
                  else
                    Cnvs.Brush.Color := _Series.GetBearishColor;
                  Cnvs.Rectangle(X - 3, YOpen, X + 4, YClose);
                end;
            end;
        end;
        for ValIdx := 0 to Point.GetValueCount - 1 do
          begin
            y := DrawRect.Bottom - Round(NormalizePtY(_Target, Point.GetY(ValIdx)));
            DrawPoint(_Target, _Series, x, y);
          end;
      end;
  end;

var
  SeriesIdx: integer;
  Series: IdzDataSeries;
  Rgn: hRgn;
begin
  fPoints.Clear;
  ChartRect := CalcChartRect(_Target);
  DrawRect := CalcDrawRect(_Target);
  Cnvs := _Target.Canvas;
  BarWidth := CalcBarWidth(_Target);

  Rgn := CreateRectRgn(ChartRect.Left + 1, ChartRect.Top + 1, ChartRect.Right - 1, ChartRect.Bottom - 1);
//  Rgn := CreateRectRgn(DrawRect.Left - 5, DrawRect.Top - 5, DrawRect.Right + 5, DrawRect.Bottom + 5);
  try
    Cnvs.SetClipRgn(Rgn);
    for SeriesIdx := 0 to fDataSeriesList.Count - 1 do
      begin
        Series := fDataSeriesList[SeriesIdx] as IdzDataSeries;
        if Series.GetPointCount <> 0 then
          begin
            if Series.IsMulti then
              DrawSeriesLines(Series as IdzMultiValueDataSeries)
            else
              DrawSeriesLines(Series as IdzSingleValueDataSeries);
          end;
      end;

    for SeriesIdx := 0 to fDataSeriesList.Count - 1 do
      begin
        Series := fDataSeriesList[SeriesIdx] as IdzDataSeries;
        if Series.GetPointCount <> 0 then
          begin
            if Series.IsMulti then
              DrawSeriesPoints(Series as IdzMultiValueDataSeries)
            else
              DrawSeriesPoints(Series as IdzSingleValueDataSeries);
          end;
      end;
  finally
    DeleteObject(Rgn);
  end;

  Cnvs.Pen.Color := clBlack;

  fPoints.Sort(ComparePoints);
end;

function TdzCustomXYChart.AddDataSeries(_DataSeries: IdzDataSeries): integer;
begin
  if not Supports(_DataSeries, IdzSingleValueDataSeries) and not Supports(_DataSeries, IdzMultiValueDataSeries) then
    raise EInvalidDataSeries.CreateFmt('%s must implement either IdzSingleValueDataSeries or IdzMultiValueDataSeries',
      [_DataSeries.GetCaption]);
  Result := fDataSeriesList.Add(_DataSeries);
  Refresh;
end;

function TdzCustomXYChart.RemoveDataSeries(const _DataSeries: IdzDataSeries): integer;
begin
  Result := fDataSeriesList.Remove(_DataSeries);
  Refresh;
end;

procedure TdzCustomXYChart.DrawLegendEntry(const _Target: IdzGraphics; const _Series: IdzDataSeries; _X, _Y: integer);
var
  h: integer;
  s: string;
  Cnvs: IdzCanvas;
begin
  Cnvs := _Target.Canvas;
  s := _Series.GetCaption;
  h := Cnvs.TextHeight(s) div 2;
  DrawLine(_Target, _Series, _X + 2, _Y + h, _X + 25, _Y + h);
  DrawPoint(_Target, _Series, _X + 13, _Y + h, true);
  Cnvs.Brush.Color := FLegend.BackGround;
  Cnvs.TextOut(_x + 30, _y, s);
end;

procedure TdzCustomXYChart.DrawLegend(const _Target: IdzGraphics; _LegendRect: TRect);

  procedure DrawLineEntry(const _Canvas: IdzCanvas; const _LineDesc: TLineDescription;
    _X, _Y: integer);
  var
    h: integer;
    s: string;
  begin
    s := _LineDesc.Description;
    h := _Canvas.TextHeight(s) div 2;

    _Canvas.Pen.Color := _LineDesc.Color;
    _Canvas.Pen.Style := psSolid;
    _Canvas.Pen.Width := 1;
    _Canvas.Line(_X + 2, _Y + h, _X + 25, _Y + h);

    _Canvas.TextOut(_x + 30, _y, s);
  end;

var
  Column: integer;
  Row: integer;

  procedure NextColumn;
  begin
    Inc(Column);
    if Column >= FLegend.Columns then
      begin
        Column := 0;
        Inc(Row);
      end;
  end;

var
  s: string;
  Offs: integer;
  h: integer;
  w: integer;
  i: integer;
  Cnvs: IdzCanvas;
  Series: IdzDataSeries;
begin
  if fLegend.Position = lpNoLegend then
    exit;

  Cnvs := _Target.Canvas;
  Cnvs.Pen := fLegend.Frame.Pen;
  Cnvs.Brush.Color := fLegend.BackGround;
  Cnvs.Brush.Style := bsSolid;
  if FLegend.Frame.Rounded then
    Cnvs.RoundRect(_LegendRect.Left, _LegendRect.Top,
      _LegendRect.Right, _LegendRect.Bottom,
      15, 15)
  else
    Cnvs.Rectangle(_LegendRect);

  Cnvs.Font := FLegend.TitleFont;
  s := FLegend.Title;
  h := Cnvs.TextWidth(s);
  Cnvs.TextOut((_LegendRect.Left + _LegendRect.Right - h) div 2, _LegendRect.Top + 4, s);
  Offs := Round(Cnvs.TextHeight(s) * 1.5);

  Cnvs.Font := FLegend.LegendFont;
  w := (_LegendRect.Right - _LegendRect.Left) div FLegend.Columns;
  h := Round(Cnvs.TextHeight('Aj') * 1.5);

  Column := 0;
  Row := 0;

  for i := 0 to fDataSeriesList.Count - 1 do
    begin
      Series := fDataSeriesList[i] as IdzDataSeries;
      if Series.GetCaption <> '' then
        begin
          DrawLegendEntry(_Target, Series,
            _LegendRect.Left + 4 + Column * w,
            _LegendRect.Top + 4 + Offs + Row * h);
          NextColumn;
        end;
    end;
end;

procedure TdzCustomXYChart.DrawFrameItems(const _Target: IdzGraphics);
var
  Cnvs: IdzCanvas;
  LegendRect: TRect;
  FrameRect: TRect;
  ChartRect: TRect;
  s: string;
begin
  inherited;
  FrameRect := inherited CalcChartRect(_Target);
  LegendRect := CalcLegendRect(_Target, FrameRect);

  ChartRect := CalcChartRect(_Target);

  Cnvs := _Target.Canvas;

  DrawLegend(_Target, LegendRect);

  Cnvs.Font := FBottomAxis.TitleFont;
  Cnvs.Brush.Color := fBackGround;
  s := FBottomAxis.Title;
  Cnvs.TextOut((ChartRect.Right + ChartRect.Left - Cnvs.TextWidth(s)) div 2,
    FrameRect.Bottom - Cnvs.TextHeight(s), s);

  Cnvs.Font := FLeftAxis.TitleFont;
  s := FLeftAxis.Title;
  Cnvs.TextOutAngle(90, FrameRect.Left,
    (ChartRect.Bottom + ChartRect.Top + Cnvs.TextWidth(s)) div 2, s);
end;

procedure TdzCustomXYChart.MouseMove(_Shift: TShiftState; _X, _Y: Integer);
var
  i: integer;
  PointDesc: TdzPointDesc;
  BestGuess: TdzPointDesc;
  MinDistance: double;
  Distance: double;
  x, y: double;
  s: string;
begin
  inherited;
  MinDistance := 10;
  BestGuess := nil;
  for i := 0 to fPoints.Count - 1 do
    begin
      PointDesc := fPoints[i] as TdzPointDesc;
      if (Abs(PointDesc.fXPos - _X) < 10) and (Abs(PointDesc.fYPos - _Y) < 10) then
        begin
          Distance := sqrt(sqr(PointDesc.fXPos - _X) + sqr(PointDesc.fYPos - _Y));
          if Distance < MinDistance then
            begin
              BestGuess := PointDesc;
              MinDistance := Distance;
              if MinDistance = 0 then
                break;
            end;
        end;
    end;
  if Assigned(BestGuess) then
    begin
      x := BestGuess.fX;
      y := BestGuess.fY;

      s := format('x = %s'#13#10'y = %s',
        [FBottomAxis.FormatLabel(X),
        FLeftAxis.FormatLabel(Y)]);
      doGetPointHint(x, y, s);
      self.Hint := s;
      self.ShowHint := s <> '';
    end
  else
    self.ShowHint := false;
end;

procedure TdzCustomXYChart.doGetPointHint(const _X, _Y: double; var _Hint: string);
begin
  if Assigned(fOnGetPointHint) then
    fOnGetPointHint(self, _X, _Y, _Hint);
end;

{ TdzCustomChartAxis }

constructor TdzCustomChartAxis.Create(_Owner: TComponent);
begin
  inherited Create(_Owner);
  fNaturalLabelOrientation := loHorizontal;
  fLabelFont := TFont.Create;
  fLabelFont.OnChange := self.FontChanged;
  fLabelOrientation := loHorizontal;
  fScale := TdzAxisScale.Create(self);
  fScale.Name := self.Name + 'Scale';
  fScale.SetSubComponent(true);
end;

destructor TdzCustomChartAxis.Destroy;
begin
  fScale.Free;
  fLabelFont.Free;
  inherited;
end;

function TdzCustomChartAxis.FormatLabel(const _Value: double): string;
begin
  Result := FloatToStr(_Value);
  if Assigned(fOnFormatLabel) then
    FOnFormatLabel(fChart, _Value, Result);
end;

function TdzCustomChartAxis.CalcLabelingSpace(const _Canvas: IdzCanvas): integer;
var
  i: integer;
  dbl: double;
  Max: integer;
  w: integer;
begin
  _Canvas.Font := TitleFont;
  Result := Round(_Canvas.TextHeight('Aj') * 1.5);

  _Canvas.Font := LabelFont;
  if LabelOrientation = fNaturalLabelOrientation then
    begin
      Max := 0;
      i := 0;
      while not Self.GetNextLabel(i, dbl) do
        begin
          w := _Canvas.TextWidth(FormatLabel(dbl));
          if w > Max then
            Max := w;
          Inc(i);
        end;
      Result := Result + Max;
    end
  else
    Result := Result + _Canvas.TextHeight('Aj');
end;

function TdzCustomChartAxis.GetNextLabel(_Idx: integer; var _Value: double): boolean;
begin
  if Assigned(fOnGetNextLabel) then
    begin
      Result := true;
      fOnGetNextLabel(fChart, _Idx, _Value, Result);
      exit;
    end;

  Result := _Idx > fScale.Divisions;
  if not Result then
//    case fScale.ScaleType of
//      stLogarithmic:
//        begin
//          _Value := fScale.Min + exp((_Idx * (ln(fScale.Max - fScale.Min) / ln(10)) / fScale.Divisions) * ln(10));
//        end;
//    else // stLinear
      _Value := fScale.Min + _Idx * (fScale.Max - fScale.Min) / fScale.Divisions;
//    end;
end;

procedure TdzCustomChartAxis.SetLabelFont(_LabelFont: TFont);
begin
  fLabelFont.Assign(_LabelFont);
end;

procedure TdzCustomChartAxis.SetLabelOrientation(_LabelOrientation: TAxisLabelOrientation);
begin
  if _LabelOrientation <> fLabelOrientation then
    begin
      fLabelOrientation := _LabelOrientation;
      RefreshChart;
    end;
end;

procedure TdzCustomChartAxis.InitDefaultLabels(_Size: integer; const _Min, _Max: double; const _Canvas: IdzCanvas);
begin
  if fScale.Automatic then
    begin
      fScale.fMin := _Min;
      fScale.fMax := _Max;

      if _Size = 0 then
        fScale.fDivisions := 10
      else
        begin
          _Canvas.Font := LabelFont;
          if LabelOrientation = fNaturalLabelOrientation then
            fScale.fDivisions := _Size div (2 * _Canvas.TextHeight('Aj'))
          else
            fScale.fDivisions := _Size div (2 * _Canvas.TextWidth(FormatLabel(_Max)));
          if fScale.fDivisions < 1 then
            fScale.fDivisions := 1
          else if fScale.fDivisions > 5 then
            fScale.fDivisions := 10;
        end;
    end;
end;

{ TdzBottomChartAxis }

constructor TdzBottomChartAxis.Create(_Owner: TComponent);
begin
  inherited;
  fNaturalLabelOrientation := loVertical;
end;

{ TdzLeftChartAxis }

constructor TdzLeftChartAxis.Create(_Owner: TComponent);
begin
  inherited;
  fNaturalLabelOrientation := loHorizontal;
end;

{ TdzXYChartLegend }

constructor TdzXYChartLegend.Create(_Owner: TComponent);
begin
  inherited;
  fLegendFont := TFont.Create;
  fLegendFont.OnChange := self.FontChanged;
  fFrame := TdzChartFrame.Create(Self, _Owner as TdzCustomChart);
  fFrame.SetSubComponent(true);
  FFrame.Name := Self.Name + 'Frame';
  fFrame.Color := clBlack;
  fFrame.Width := 1;
  fFrame.Rounded := false;
  fColumns := 1;
end;

destructor TdzXYChartLegend.Destroy;
begin
  fFrame.Free;
  fLegendFont.Free;
  inherited;
end;

procedure TdzXYChartLegend.SetBackGround(const _Background: TColor);
begin
  if fBackGround <> _Background then
    begin
      fBackGround := _Background;
      RefreshChart;
    end;
end;

procedure TdzXYChartLegend.SetColumns(_Columns: integer);
begin
  if _Columns < 1 then
    _Columns := 1;
  if fColumns <> _Columns then
    begin
      fColumns := _Columns;
      RefreshChart;
    end;
end;

procedure TdzXYChartLegend.SetLegendFont(const _LegendFont: TFont);
begin
  fLegendFont.Assign(_LegendFont);
end;

procedure TdzXYChartLegend.SetPosition(const _Position: TLegendPosition);
begin
  if fPosition <> _Position then
    begin
      fPosition := _Position;
      RefreshChart;
    end;
end;

{ TdzPointDesc }

constructor TdzPointDesc.Create(const _x, _y: double; _XPos, _YPos: integer);
begin
  inherited Create;
  fX := _x;
  fY := _y;
  fXPos := _XPos;
  fYPos := _YPos;
end;

{ TdzCustomAxisScale }

constructor TdzCustomAxisScale.Create(_Owner: TComponent);
begin
  inherited;
  fAutomatic := true;
  fMin := -1;
  fMax := 1;
  fDivisions := 1;
end;

procedure TdzCustomAxisScale.RefreshChart;
begin
  if Assigned(Owner) and (Owner is TdzCustomChartAxis) then
    (Owner as TdzCustomChartAxis).RefreshChart;
end;

procedure TdzCustomAxisScale.SetAutomatic(const _Automatic: boolean);
begin
  if fAutomatic <> _Automatic then
    begin
      fAutomatic := _Automatic;
      RefreshChart;
    end;
end;

procedure TdzCustomAxisScale.SetDivisions(const _Divisions: double);
begin
  if not fAutomatic and (fDivisions <> _Divisions) and (_Divisions >= 1) then
    begin
      fDivisions := _Divisions;
      RefreshChart;
    end;
end;

procedure TdzCustomAxisScale.SetMax(const _Max: double);
begin
  if not fAutomatic and (fMax <> _Max) then
    begin
      fMax := _Max;
      RefreshChart;
    end;
end;

procedure TdzCustomAxisScale.SetMin(const _Min: double);
begin
  if not fAutomatic and (fMin <> _Min) then
    begin
      fMin := _Min;
      RefreshChart;
    end;
end;

procedure TdzCustomAxisScale.SetScaleType(const _ScaleType: TScaleType);
begin
  if fScaleType <> _ScaleType then
    begin
      fScaleType := _ScaleType;
      RefreshChart;
    end;
end;

end.

