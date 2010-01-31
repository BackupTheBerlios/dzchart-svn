(*****************************************************************************
*           IdzDataSeries, IdzDataPoint interfaces
*           TdzDataseries, TdzDataPoint classes
*
*        This unit is part of dummzeuch.de Charts
*
*                (c) 2003-2005 Thomas Mueller
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
* The Original Code is u_dzDataSeries.
*
* The Initial Developer of the Original Code is
* Thomas Mueller.
* Portions created by Thomas Mueller are Copyright (C) 2003-2004
* by Thomas Mueller. All Rights Reserved.
*
* Contributor(s):
* * Paul Warren
* * Romeo Lefter
*****************************************************************************)

{: @abstract(Interface for dataseries and datapoint, used by u_dzXYChart)
  This unit declares the interfaces @link(IdzSingleValueDataSeries),
  @link(IdzMultiValueDataSeries) and @link(IdzDatapoint) which ar used
  in @link(TdzXYChart).
  An example implementtion @link(TdzPointDataSeries) for
  IdzSingleValueDataSeries and @link(TdzDataPoint) for IdzDataPoint
  is also provided.
  For a more complex implementation of IdzSingleValueDataSeries see
  @link(u_dzPolynomialDataSeries).
  @author(Thomas Mueller)
  @author(Paul Warren)
  @author(Romeo Lefter)
}

unit u_dzDataSeries;

interface

uses
  SysUtils,
  Classes,
{$IFDEF MSWINDOWS}
  Graphics,
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  QGraphics,
{$ENDIF}
  Math,
  Contnrs;

type
  {: Types of single value charts.
     Maybe ctScatter and ctLine should be removed since ctScatter can be
     implemented as ctXY with LineStyle = psClear and ctLine as ctXY with
     PointStyle = psNone.
     ctAlternatingLine is special: The chart will draw a line from
     the first data point to the second then another from the third data
     point to the fourth and so on. The special values Infinity and
     NegInfinity (from Math unit) can be used to draw a horizontal line over
     the whole chart. If there is no endpoint given for the last starting point
     (that is the number of points in the series is not even), Infinity will be
     assumed, so you can draw a horizontal line by just having a data series
     with one point at (NegInfinity, Y-Value).
     The idea behind this is to have control charts with limits that change
     over time. In addition the start
     and endpoints can have markers like with all the other series types. }
  TSingleValueChartType = (ctScatter, ctLine, ctBar, ctXY, ctAlternatingLine);

  {: Types of point style for ctScatter and ctXY style charts,
     All types but psUpArrow and psDownArrow draw the point marker
     around the actual point. psUpArrow and psDownArrow draw it so the
     arrow points at the point. }
  TPointStyle = (psNone, psRectangle, psCircle, psTriangle, psDownTriangle,
    psCross, psDiagCross, psStar, psDiamond, psSmallDot, psUpArrow, psDownArrow,
    psHorizontalLine, psVerticalLine);

type
  {: Types of multi value charts }
  TMultiValueChartType = (ctHighLow, ctHighLowOpenClose, ctCandle);

type
  IdzDataSeries = interface ['{007BD1C9-62E8-D811-A461-000854097AB5}']
    {: Get the number of data points in the data series
       @returns the number of data points }
    function GetPointCount: integer;
    {: Get the Idx'th data point of the data series
       @param(Idx is the index of the data point, must be >=0 and < GetPointCount)
       @returns(a IdzDataPoint interface for the Idx'th data point) }
    {: Get the line color for the data series }
    function GetLineColor: TColor;
    {: Get the line style for the data series }
    function GetLineStyle: TPenStyle;
    {: Get the point color for the data series }
    function GetPointColor: TColor;
    {: Get the point style for the data series }
    function GetPointStyle: TPointStyle;
    {: Get the fill color for the data series (for ctBar and to fill the point markers) }
    function GetFillColor: TColor;
    {: Get the caption of the data series used in the legend. }
    function GetCaption: string;
    {: This procedure is called to notify the class of some operations.
       @param(Component is the component causing the notification)
       @param(Operation is the operation causing the notification, that is one of
              opInsert and opRemove) }
    procedure Notification(_Component: TComponent; _Operation: TOperation);
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
  end;

type
  {: @classname describes a single data point of a chart. }
  IdzDataPoint = interface ['{08B26745-F831-4F36-9DFD-16884D3E8D6E}']
    {: @returns the X value of the point }
    function GetX: double;
    {: @returns the Y value of the point }
    function GetY: double;
    //    function GetLabel: string;
  end;

type
  {: @classname describes a single value data series, that is a data series where there is
     only one Y value for a given X value (see also @link(IdzMultiValueDataSeries)) }
  IdzSingleValueDataSeries = interface(IdzDataSeries)['{703E9533-F610-4354-939E-373E4BBAE78A}']
    function GetDataPoint(_Idx: integer): IdzDataPoint;
    {: Get the type of the data series }
    function GetChartType: TSingleValueChartType;
  end;

type
  IdzMultiValueDataPoint = interface ['{3E579B6B-55E8-D811-A461-000854097AB5}']
    {: Returns the X value of the data point }
    function GetX: double;
    {: Returns the number of Y values of the data point }
    function GetValueCount: integer;
    {: Returns the Idx'th Y value of the data point.
       @param(Idx is the value's index, must be >=0 and <GetValueCount)
       Note that these points need not be in any order but low to high is recommended
       otherwise the result might not be as expected, depending on the chart type. }
    function GetY(_Idx: integer): double;
  end;

type
  {: @classname describes a multi value data series, that is a data series where there are
     several Y value for a given X value (see also @link(IdzSingleValueDataSeries)).
     There is currently no implementation of this interface in TdzChart. }
  IdzMultiValueDataSeries = interface(IdzDataSeries)['{02A14F76-F1D4-D811-B814-000854097AB5}']
    {: Returns the Idx'th data point of the data series
       @param(Idx is the index of the data point, must be >=0 and < GetPointCount)
       @returns(a IdzMultiValueDataPoint interface for retrieving the values) }
    function GetDataPoint(_Idx: integer): IdzMultiValueDataPoint;
    {: Get the chart type for this data series. The following types are available:
       <ul><li>ctHighLow is a vertical line described by GetLineColor and GetLineStyle
               from the first to the last Y value. In addition the data points
               might be drawn as described by GetPointColor and GetPointStyle</li>
           <li>ctHighLowOpenClose is a vertical line from the first to the last
               Y value. In addition there is a horizontal mark to the left marking
               the second and the second to last Y value. This means that if there
               are only two values it will look like a Z, if there are only
               three values it will look like a cross. (Assuming the values
               are given in ascending order, otherweise the result might be
               unexpected.) The lines are drawn as described by GetLineColor
               and GetLineStyel. In addition the data points may be drawn as
               described by GetPointColor and GetPointStyle </li>
           <li>ctCandle draws a vertical line from the first to the last Y value
               and a vertical bar from the second to the second to last Y value.
               The line is described by GetLineColor and GetLineStyle, the bar
               by GetBarColor. In addition the data points may be drawn as
               described by GetPointColor and GetPointStyle </li>
       </ul>
       If there is only one value, nothing will be drawn but the single data
       point as described by GetPointColor and GetPointStyle. }
    function GetChartType: TMultiValueChartType;
    function GetBearishColor:TColor;
    function GetBullishColor:TColor;
  end;

type
  {: @classname implements the @link(IdzDataPoint) interface by simply storing
     the X/Y values passed to it in the constructor }
  TdzDataPoint = class(TInterfacedObject, IdzDataPoint)
  protected
    {: Stores the value returned by GetX }
    fX: double;
    {: Stores the value returned by GetY }
    fY: double;
    //    fPointLabel: string;
        {: Returns the value stored in fX }
    function GetX: double;
    {: Returns the value stored in fY }
    function GetY: double;
    //    function GetLabel: string;
  public
    {: Creates a @classname object representing a data point at X/Y
       @param(X is the X coordinate of the point)
       @param(Y is the Y coordinate of the point)
       @param(Label is an optional string describing the data point (not yet used)) }
    constructor Create(const _X, _Y: double; const _Label: string = '');
  end;

type
  {: @classname is a simple implementation of the @link(IdzSingleValueDataSeries)
     interface. It stores a list of @link(IdzDataPoint) interfaces and returns
     them when asked.}
  TdzPointDataSeries = class(TInterfacedObject, IdzDataSeries, IdzSingleValueDataSeries)
  protected
    {: Stores the value of the @link(PointColor) property }
    fPointColor: TColor;
    {: Stores the value of the @link(PointStyle) property }
    fPointStyle: TPointStyle;
    {: Stores the value of the @link(LineColor) property }
    fLineColor: TColor;
    {: Stores the value of the @link(LineStyle) property }
    fLineStyle: TPenStyle;
    {: Stores the value of the @link(FillColor) property }
    fFillColor: TColor;
    {: Stores the value of the @link(ChartType) property }
    fChartType: TSingleValueChartType;
    {: Stores a list of @link(IdzDataPoint) interfaces. Items are added
       to this list by calling @link(AddDatapoint) }
    fPoints: TInterfaceList;
    {: Stores the value of the Caption property }
    fCaption: string;
    {: Returns the number of items in the fPoints list }
    function GetPointCount: integer;
    {: Gets the Idx'th item from the fPoints list.
       @param(Idx is the index into the fpoints list, must be >=0 and < @link(GetPointCount))
       @returns a IdzDataPoint interface describing the point }
    function GetDataPoint(_Idx: integer): IdzDataPoint;
    {: Returns the line color for the data series }
    function GetLineColor: TColor;
    {: Returns the line style for the data series }
    function GetLineStyle: TPenStyle;
    {: Returns the point color for the data series }
    function GetPointColor: TColor;
    {: Returns the point style for the data series }
    function GetPointStyle: TPointStyle;
    {: Get the fill color for the data series (for ctBar and to fill the point markers) }
    function GetFillColor: TColor;
    {: Returns the chart type for the data series }
    function GetChartType: TSingleValueChartType;
    {: Returns the Caption for the data series (used in the Legend) }
    function GetCaption: string;
    {: Handles notifications, does nothing in this class }
    procedure Notification(_Component: TComponent; _Operation: TOperation);
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
    {: Creates a new @classname object with an empty data point list }
    constructor Create;
    {: Destroys a @classname object }
    destructor Destroy; override;
    {: Adds a data point to the data point list
       @param(Point is the @link(IdzDataPoint) interface to add)
       @returns(The index of the added data point) }
    function AddDataPoint(const _Point: IdzDataPoint): integer; overload;
    {: Creates a new TdzDataPoint object and adds it to the data point list
       This is short for AddDataPoint(TdzDatapoint.Create(...)).
       @param(X is the X coordinate of the new data point)
       @param(Y is the Y coordinate of the new data point)
       @param(Label is an optional string describing the data point (not used yet))
       @returns(The index of the added data point) }
    function AddDataPoint(const _X, _Y: double; const _Label: string = ''): integer; overload;
    {: Contains the point color of the data series, default is clblack. }
    property PointColor: TColor read fPointColor write fPointColor;
    {: Contains the point style of the data series, default is psCircle }
    property PointStyle: TPointStyle read fPointStyle write fPointStyle;
    {: Contains the line color of the data series, default is clBlack }
    property LineColor: TColor read fLineColor write fLineColor;
    {: Contains the line style of the data series, default is psSolid }
    property LineStyle: TPenStyle read fLineStyle write fLineStyle;
    {: Contains the chart type of the data series, default is ctXY }
    property ChartType: TSingleValueChartType read fChartType write fChartType;
    {: Containts the fill color of the data series, default is clGray }
    property FillColor: TColor read fFillColor write fFillColor;
    {: Contains the caption of the data series used in the legend, default is an empty string }
    property Caption: string read GetCaption write fCaption;
  end;

type
  {: @classname implements the @link(IdzMultiValueDataPoint) interface by simply storing
     the X and Y[] values passed to it in the constructor }
  TdzMultiValueDataPoint = class(TInterfacedObject, IdzMultiValueDataPoint)
  protected
    {: Stores the value returned by GetX }
    fX: double;
    {: Stores the value returned by GetY }
    fYArr: array of double;
    //    fPointLabel: string;
        {: Returns the value stored in fX }
    function GetX: double;
    {: Returns the number of Y values of the data point (that is Length(fYArr))}
    function GetValueCount: integer;
    {: Returns one of the values stored in fYArr }
    function GetY(_Idx: integer): double;
    //    function GetLabel: string;
  public
    {: Creates a @classname object representing a data point at X/Y
       @param(X is the X coordinate of the point)
       @param(YArr is a const arrry of double with Y values of the point)
       @param(Label is an optional string describing the data point (not yet used)) }
    constructor Create(const _X: double; const _YArr: array of double; const _Label: string = '');
  end;

  {: @classname is a simple implementation of a @link(IdzMultiValueDataSeries).
     It stores a list of @link(IdzMultiValueDataPoint) interfaces and returns
     them when asked }
  TdzMultiValueDataSeries = class(TInterfacedObject, IdzDataSeries, IdzMultiValueDataSeries)
  private
    fCaption: string;
    fFillColor: TColor;
    fLineColor: TColor;
    fPointColor: TColor;
    fLineStyle: TPenStyle;
    fPointStyle: TPointStyle;
    fChartType: TMultiValueChartType;
    fPoints: TInterfaceList;
    fbullishColor:TColor;
    fBearishColor:TColor;
  protected
    // implementation of IdzMultiValueDataSeries
    {: Returns the number of points in the data series, that is the number of different X values. }
    function GetPointCount: integer;
    {: Returns the Idx'th data point of the data series
       @param(Idx is the index of the data point, must be >=0 and < GetPointCount)
       @returns(a IdzMultiValueDataPoint interface for retrieving the values) }
    function GetDataPoint(_Idx: integer): IdzMultiValueDataPoint;
    {: Get the chart type for this data series. The following types are available:
       <ul><li>ctHighLow is a vertical line described by GetLineColor and GetLineStyle
               from the first to the last Y value. In addition the data points
               might be drawn as described by GetPointColor and GetPointStyle</li>
           <li>ctHighLowOpenClose is a vertical line from the first to the last
               Y value. In addition there is a horizontal mark to the left marking
               the second and the second to last Y value. This means that if there
               are only two values it will look like a Z, if there are only
               three values it will look like a cross. (Assuming the values
               are given in ascending order, otherweise the result might be
               unexpected.) The lines are drawn as described by GetLineColor
               and GetLineStyel. In addition the data points may be drawn as
               described by GetPointColor and GetPointStyle </li>
           <li>ctCandle draws a vertical line from the first to the last Y value
               and a vertical bar from the second to the second to last Y value.
               The line is described by GetLineColor and GetLineStyle, the bar
               by GetBarColor. In addition the data points may be drawn as
               described by GetPointColor and GetPointStyle </li>
       </ul>
       If there is only one value, nothing will be drawn but the single data
       point as described by GetPointColor and GetPointStyle. }
    function GetChartType: TMultiValueChartType;
    {: Get the color for drawing lines }
    function GetLineColor: TColor;
    {: Get the style for drawing lines }
    function GetLineStyle: TPenStyle;
    {: Get the color for drawing points }
    function GetPointColor: TColor;
    {: Get the style for drawing data points }
    function GetPointStyle: TPointStyle;
    {: Get the fill color for the data series (for ctCandle and to fill the point markers) }
    function GetFillColor: TColor;
    {: Get the the caption for the data series used in the legend }
    function GetCaption: string;
    {: This procedure is called to notify the class of some operations.
       @param(Component is the component causing the notification)
       @param(Operation is the operation causing the notification, that is one of
              opInsert and opRemove) }
    procedure Notification(_Component: TComponent; _Operation: TOperation);
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
    function GetBearishColor:TColor;
    function GetBullishColor:TColor;
  public
    {: Creates a new @classname object with an empty data point list }
    constructor Create;
    {: Destroys a @classname object }
    destructor Destroy; override;
    {: Adds a data point to the data point list
       @param(Point is the @link(IdzMultiValueDataPoint) interface to add)
       @returns(The index of the added data point) }
    function AddDataPoint(const _Point: IdzMultiValueDataPoint): integer; overload;
    {: Creates a new TdzDataPoint object and adds it to the data point list
       This is short for AddDataPoint(TdzDatapoint.Create(...)).
       @param(X is the X coordinate of the new data point)
       @param(YArr is an open array containing the Y values of the new data point)
       @param(Label is an optional string describing the data point (not used yet))
       @returns(The index of the added data point) }
    function AddDataPoint(const _X: double; const _YArr: array of double; const _Label: string = ''): integer; overload;
    {: Contains the point color of the data series, default is clblack. }
    property PointColor: TColor read fPointColor write fPointColor;
    {: Contains the point style of the data series, default is psCircle }
    property PointStyle: TPointStyle read fPointStyle write fPointStyle;
    {: Contains the line color of the data series, default is clBlack }
    property LineColor: TColor read fLineColor write fLineColor;
    {: Contains the line style of the data series, default is psSolid }
    property LineStyle: TPenStyle read fLineStyle write fLineStyle;
    {: Contains the chart type of the data series, default is ctXY }
    property ChartType: TMultiValueChartType read fChartType write fChartType;
    {: Containts the fill color of the data series, default is clGray }
    property FillColor: TColor read fFillColor write fFillColor;
    {: Contains the caption of the data series used in the legend, default is an empty string }
    property Caption: string read GetCaption write fCaption;
    property BearishColor:TColor read fBearishColor write fBearishColor;
    property BullishColor:TColor read fBullishColor write fBullishColor;
  end;

implementation

{ TdzPointDataSeries }

constructor TdzPointDataSeries.Create;
begin
  inherited Create;
  fPointColor := clBlack;
  fPointStyle := psCircle;
  fLineColor := clBlack;
  fLineStyle := psSolid;
  fFillColor := clGray;
  fChartType := ctXy;
  fPoints := TInterfaceList.Create;
end;

destructor TdzPointDataSeries.Destroy;
begin
  FreeAndNil(fPoints);
  inherited;
end;

function TdzPointDataSeries.AddDataPoint(const _Point: IdzDataPoint): integer;
begin
  Result := fPoints.Add(_Point);
end;

function TdzPointDataSeries.AddDataPoint(const _X, _Y: double;
  const _Label: string): integer;
begin
  Result := AddDataPoint(TdzDataPoint.Create(_X, _Y, _Label));
end;

function TdzPointDataSeries.GetPointCount: integer;
begin
  Result := fPoints.Count;
end;

function TdzPointDataSeries.GetDataPoint(_Idx: integer): IdzDataPoint;
begin
  Result := fPoints[_Idx] as IdzDataPoint;
end;

function TdzPointDataSeries.GetLineColor: TColor;
begin
  Result := fLineColor;
end;

function TdzPointDataSeries.GetPointColor: TColor;
begin
  Result := fPointColor;
end;

function TdzPointDataSeries.GetChartType: TSingleValueChartType;
begin
  Result := fChartType;
end;

function TdzPointDataSeries.GetFillColor: TColor;
begin
  Result := fFillColor;
end;

function TdzPointDataSeries.GetPointStyle: TPointStyle;
begin
  Result := fPointStyle;
end;

function TdzPointDataSeries.GetLineStyle: TPenStyle;
begin
  Result := fLineStyle;
end;

function TdzPointDataSeries.GetCaption: string;
begin
  Result := fCaption;
end;

procedure TdzPointDataSeries.Notification(_Component: TComponent; _Operation: TOperation);
begin
  // do nothing
end;

function TdzPointDataSeries.IsMulti: boolean;
begin
  Result := false;
end;

function TdzPointDataSeries.GetMinAndMax(out _MinX, _MaxX, _MinY, _MaxY: Double): boolean;
var
  i: integer;
  x: double;
  y: double;
  Point: IdzDataPoint;
begin
  Result := GetPointCount > 0 ;
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

{ TdzDataPoint }

constructor TdzDataPoint.Create(const _X, _Y: double; const _Label: string);
begin
  inherited Create;
  fX := _X;
  fY := _Y;
  //  fPointLabel := _Label;
end;

function TdzDataPoint.GetX: double;
begin
  Result := fX;
end;

function TdzDataPoint.GetY: double;
begin
  Result := fY;
end;

{ TdzMultiValueDataPoint }

constructor TdzMultiValueDataPoint.Create(const _X: double;
  const _YArr: array of double; const _Label: string);
var
  i: integer;
begin
  inherited Create;
  fX := _X;
  SetLength(fYArr, Length(_YArr));
  for i := Low(fYArr) to High(fYArr) do
    fYArr[i] := _YArr[i];
end;

function TdzMultiValueDataPoint.GetValueCount: integer;
begin
  Result := Length(fYArr);
end;

function TdzMultiValueDataPoint.GetX: double;
begin
  Result := fX;
end;

function TdzMultiValueDataPoint.GetY(_Idx: integer): double;
begin
  Result := fYArr[_Idx];
end;

{ TdzMultiValueDataSeries }

constructor TdzMultiValueDataSeries.Create;
begin
  inherited Create;
  fPointColor := clBlack;
  fPointStyle := psCross;
  fLineColor := clBlack;
  fLineStyle := psSolid;
  fFillColor := clGray;
  fChartType := ctHighLow;
  fBearishColor:=ClRed;
  fbullishColor:=clBlue;

  fPoints := TInterfaceList.Create;
end;

destructor TdzMultiValueDataSeries.Destroy;
begin
  FreeAndNil(fPoints);
  inherited;
end;

function TdzMultiValueDataSeries.AddDataPoint(const _Point: IdzMultiValueDataPoint): integer;
begin
  Result := fPoints.Add(_Point);
end;

function TdzMultiValueDataSeries.AddDataPoint(const _X: double;
  const _YArr: array of double; const _Label: string): integer;
begin
  Result := AddDataPoint(TdzMultiValueDataPoint.Create(_X, _YArr, _Label));
end;

function TdzMultiValueDataSeries.GetDataPoint(_Idx: integer): IdzMultiValueDataPoint;
begin
  Result := IdzMultiValueDataPoint(fPoints[_Idx]);
end;

function TdzMultiValueDataSeries.GetPointCount: integer;
begin
  Result := fPoints.Count;
end;

function TdzMultiValueDataSeries.GetFillColor: TColor;
begin
  Result := fFillColor;
end;

function TdzMultiValueDataSeries.GetCaption: string;
begin
  Result := fCaption;
end;

function TdzMultiValueDataSeries.GetChartType: TMultiValueChartType;
begin
  Result := fChartType;
end;

function TdzMultiValueDataSeries.GetLineColor: TColor;
begin
  Result := fLineColor;
end;

function TdzMultiValueDataSeries.GetLineStyle: TPenStyle;
begin
  Result := fLineStyle;
end;

function TdzMultiValueDataSeries.GetPointColor: TColor;
begin
  Result := fPointColor;
end;

function TdzMultiValueDataSeries.GetPointStyle: TPointStyle;
begin
  Result := fPointStyle;
end;

function TdzMultiValueDataSeries.GetBearishColor:TColor;
begin
  Result:=fBearishColor;
end;

function TdzMultiValueDataSeries.GetBullishColor:TColor;
begin
  Result:=fbullishColor;
end;

procedure TdzMultiValueDataSeries.Notification(_Component: TComponent;
  _Operation: TOperation);
begin
  // do nothing
end;

function TdzMultiValueDataSeries.IsMulti: boolean;
begin
  Result := true;
end;

function TdzMultiValueDataSeries.GetMinAndMax(out _MinX, _MaxX, _MinY, _MaxY: Double): boolean;
var
  PtIdx: integer;
  ValueIdx: integer;
  x: double;
  y: double;
  Point: IdzMultiValueDataPoint;
begin
  Result := (GetPointCount > 0) and (GetDataPoint(0).GetValueCount > 0);
  if Result then
    begin
      Point := GetDataPoint(0);
      _MinX := Point.GetX;
      _MaxX := _MinX;
      _MinY := Point.GetY(0);
      _MaxY := _MinY;
      for PtIdx := 1 to GetPointCount - 1 do
        begin
          Point := GetDataPoint(PtIdx);
          x := Point.GetX;
          for ValueIdx := 0 to Point.GetValueCount - 1 do
            begin
              y := Point.GetY(ValueIdx);
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
end;

end.

