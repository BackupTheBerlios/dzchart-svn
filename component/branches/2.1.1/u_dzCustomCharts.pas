(*****************************************************************************
 *                 TdzCustomChart Component
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
 * The Original Code is u_dzCustomCharts.
 *
 * The Initial Developer of the Original Code is
 * Thomas Mueller.
 * Portions created by the Initial Developer are Copyright (C) 2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * * Paul Warren
 *****************************************************************************)

 {: TdzCustomChart is a custom base class for TdzXYChart and
    other XY, scatter, line and bar charts.
    @author(Thomas Mueller)
    @author(Paul Warren)
 }

unit u_dzCustomCharts;

interface

uses
  SysUtils,
  Classes,
  Types,
  Math,
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
  u_dzQWindows, // from jvcl project
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QStdCtrls,
{$ENDIF LINUX}
  u_dzDataSeries;

type
  EdzCharts = class(Exception);

type
  {: Type of grid, none, horizontal lines only, vertical lines only and both lines }
  TGridType = (gtNone, gtHorz, gtVert, gtBoth);

type
  TdzCustomChart = class;

  {: Describes the title of a chart. }
  TdzCustomTitle = class(TComponent)
  protected
    {: Stores a reference to the chart it belongs to }
    fChart: TdzCustomChart;
    {: Stores the Title property }
    fTitle: string;
    {: Stores the TitleFont property }
    fTitleFont: TFont;
    {: Causes a redraw of the chart }
    procedure RefreshChart;
    {: Setter method for the TitleFont property, calls
       fTitleFont.Assign for the new font. This
       will cause a call to the FontChanged
       method which in turn calls RefreshChart. }
    procedure SetTitleFont(const _TitleFont: TFont);
    {: Setter method for the Title property, calls RefreshChart
       to redraw of the chart }
    procedure SetTitle(const _Title: string);
    {: Called whenever the TitleFont is changed and
       calls RefreshChart to redraw the chart. }
    procedure FontChanged(_Sender: TObject);
    {: Returns the value of fChart. }
    function GetOwner: TPersistent; override;
    {: Contains the title string of the chart }
    property Title: string read fTitle write SetTitle;
    {: Contains the font to be used when drawing the title }
    property TitleFont: TFont read fTitleFont write SetTitleFont;
  public
    {: Creates a @classname object }
    constructor Create(_Owner: TComponent); override;
    {: Destroys a @classname object }
    destructor Destroy; override;
  end;

  {: @classname publishes the properties Title and TitleFont of @inherited }
  TdzTitle = class(TdzCustomTitle)
  published
    {: Contains the title string of the chart }
    property Title stored true;
    {: Contains the font to be used when drawing the title }
    property TitleFont;
  end;

  {: Describes the frame of a chart (note to self: Check it again, the design is odd.) }
  TdzChartFrame = class(TComponent)
  protected
    {: Stores a reference to the chart it belongs to }
    fChart: TdzCustomChart;
    {: Stores the Pen property }
    fPen: TPen;
    {: Stores the Rounded property }
    fRounded: boolean;
    {: Redraws the chart }
    procedure RefreshChart;
    {: Setter method for the Rounded property, calls RefreshChart when the
       value changed }
    procedure SetRounded(const _Rounded: boolean);
    {: Called whenever the Pen property is changed, calls RefreshChart }
    procedure PenChanged(_Sender: TObject);
    {: Getter method for the Color property }
    function GetColor: TColor;
    {: Setter method for the Color property }
    procedure SetColor(const _Color: TColor);
    {: Getter method for the Width property }
    function GetWidth: integer;
    {: Setter method for the Width property }
    procedure SetWidth(const _Width: integer);
  public
    {: Creates a new @classname object
       @param(Owner is the standard owner parameter for TComponent.Create)
       @param(Chart is the chart that owns this object (could proabably
              be set to Owner anyway since this object is created by
              the chart's constructor).)}
    constructor Create(_Owner: TComponent; _Chart: TdzCustomChart); reintroduce;
    {: Destroys a @classname object }
    destructor Destroy; override;
    {: Contains the pen for drawing the frame }
    property Pen: TPen read fPen;
  published
    {: Contains the color used for drawing the frame }
    property Color: TColor read GetColor write SetColor;
    {: Contains the width used for drawing the frame }
    property Width: integer read GetWidth write SetWidth;
    {: Determines whether the frame is drawn with rounded corners }
    property Rounded: boolean read fRounded write SetRounded;
  end;

  TdzChartGrid = class(TComponent)
  protected
    fColor: TColor;
    fLineStyle: TPenStyle;
    fGridType: TGridType;
    fChart: TdzCustomChart;
    procedure SetColor(const _Color: TColor);
    procedure SetGridType(const _GridType: TGridType);
    procedure SetLineStyle(const _LineStyle: TPenStyle);
    procedure Refresh;
  public
    constructor Create(_Owner: TComponent; _Chart: TdzCustomChart); reintroduce;
  published
    property GridType: TGridType read fGridType write SetGridType;
    property Color: TColor read fColor write SetColor;
    property LineStyle: TPenStyle read fLineStyle write SetLineStyle;
  end;

  TdzCanvas = class(TCanvas)
  private
    // This is a hack, may only have static functions no inheritance takes place!
  public
{$IFDEF mswindows}
    procedure DrawPoint(_X, _Y: integer); // must not be virtual
{$ELSE} // linux
{$ENDIF}
    function TextWidthAngle(_Angle: integer; _Text: string): integer;
    procedure TextOutAngle(_Angle, _x, _y: integer; _Text: string);
    procedure Line(_x1, _y1, _x2, _y2: integer);
  end;

  TdzOffscreenBitmap = class(TBitmap)
  protected
    function GetCanvas: TdzCanvas;
  public
    property Canvas: TdzCanvas read GetCanvas;
  end;

  {: Ancestor to all chart components, declares and partially implements the
     methods and properties for drawing and calculating all frame items }
  TdzCustomChart = class(TGraphicControl)
  protected
    {: TBitmap used in offscreen drawing }
    fOffScreen: TdzOffscreenBitMap;
    {: Stores the ChartTitle subcomponent }
    FChartTitle: TdzTitle;
    {: Stores the Grid property }
    FGrid: TdzChartGrid;
    {: Stores the Background property }
    FBackGround: TColor;
    {: Stores the ChartColor property }
    FChartColor: TColor;
    {: Stores the ChartFrame subcomponent }
    FChartFrame: TdzChartFrame;
    {: internally used counter for calls to BeginUpdate / EndUpdate }
    FBeginUpdateCount: integer;
    {: Setter method for Background property, calls Refresh if changed }
    procedure SetBackGround(Value: TColor);
    {: Setter method for ChartColor property, calls Refresh if changed }
    procedure SetChartColor(Value: TColor);
    {: Calculats a TRect for the chart area, that is the component's
       size minus the space for frame items. Frame items in this
       case is only the title, but descendants may have more, e.g. a legend. }
    function CalcChartRect: TRect; virtual;
    {: Paints the chart by calling DrawOffscreen and then copying the
       result to the screen }
    procedure Paint; override;
    {: Calls the DrawXxx methods to draw an offscreen image of the chart }
    procedure DrawOffscreen; virtual;
    {: Draws the frame items, in this case that is only the title but
       descendants may draw more, e.g. a legend. }
    procedure DrawFrameItems; virtual;
    {: Does nothing, must be overridden by descendants to draw the chart's
       grid lines. }
    procedure DrawGrid(_Rect: TRect); virtual;
    {: Does nothing, must be overridden by descendants to draw the bottom scale }
    procedure DrawBottomScale; virtual;
    {: Does nothing, must be overridden by descendants to draw the left scale }
    procedure DrawLeftScale; virtual;
    {: Does nothing, must be overridden by descendants to draw the actual data points }
    procedure DrawDataPoints; virtual;
    {: Does nothing, must be overridden by descendants to scale the chart }
    procedure ScaleChart; virtual;
    {: Draws a frame around the chart as described by the Frame property }
    procedure DrawFrame; virtual;
    {: Describes the type of grid used for the chart }
    property Grid: TdzChartGrid read FGrid;
    {: Describes the background color used as background for the frame area, default is clSilver }
    property BackGround: TColor read FBackGround write SetBackGround default clSilver;
    {: Describes the chart color, defaultis clWhite }
    property ChartColor: TColor read FChartColor write SetChartColor default clWhite;
    {: Subcomponent that describes the chart's title }
    property ChartTitle: TdzTitle read FChartTitle;
    {: Subcomponents that describes the chart's frame }
    property ChartFrame: TdzChartFrame read FChartFrame;
  public
    {: Creates a @classname object }
    constructor Create(AOwner: TComponent); override;
    {: Destroys a @classname object }
    destructor Destroy; override;
    {: Paints the chart to the given device context (e.g. for printing)
       @param(DC is a HDC handle for the device context)
       @param(X is the X position for drawing)
       @param(Y is the Y position for drawing) }
    procedure PaintTo(_DC: HDC; _X, _Y: Integer); overload;
    {: Paints the chart to a bitmap
       @param(Bitmap is a TBitmap object to draw to) }
    procedure PaintTo(_Bitmap: TBitmap); overload;
    {: Prevents repainting until @link(EndUpdate) has been called. Calls to
       BeginUpdate / EndUpdate can be nested, so if your chart does not
       redraw properly checke whether there is an EndUpdate call missing }
    procedure BeginUpdate; virtual;
    {: Allows repainting the chart after a call to @link(BeginUpdate).
       Calls to BeginUpdate / EndUpdate can be nested, so if your chart does not
       redraw properly checke whether there is an EndUpdate call missing }
    procedure EndUpdate; virtual;
  published
  end;

implementation

{$IFDEF linux}

//const
//  WM_ERASEBKGND = 0;
//  WM_PAINT = 0;

{$ENDIF}

{ TdzCustomChart }

constructor TdzCustomChart.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque];
  Width := 300;
  Height := 225;

  FChartTitle := TdzTitle.Create(Self);
  fChartTitle.SetSubComponent(true);
  fChartTitle.Name := self.Name + 'ChartTitle';
  FChartTitle.Title := 'dummzeuch.de Chart';
  FChartTitle.TitleFont.Name := 'arial';
  FChartTitle.TitleFont.Style := [fsbold];
  FChartTitle.TitleFont.Size := 11;
  FChartTitle.TitleFont.Color := clRed;

  FChartFrame := TdzChartFrame.Create(Self, self);
  FChartFrame.SetSubComponent(true);
  FChartFrame.Name := self.Name + 'ChartFrame';

  FGrid := TdzChartGrid.Create(self, self);
  FGrid.SetSubComponent(true);
  FGrid.Name := self.Name + 'ChartGrid';
  FGrid.Color := clSilver;
  FGrid.LineStyle := psSolid;
  FGrid.fGridType := gtNone;

  FBackGround := clSilver;
  FChartColor := clWhite;
  fOffScreen := TdzOffscreenBitmap.Create;
end;

destructor TdzCustomChart.Destroy;
begin
  FChartTitle.Free;
  fOffScreen.Free;
  inherited Destroy;
end;

{ property access routines }

procedure TdzCustomChart.SetBackGround(Value: TColor);
begin
  if Value <> FBackGround then
    begin
      FBackGround := Value;
      Refresh;
    end;
end;

procedure TdzCustomChart.SetChartColor(Value: TColor);
begin
  if Value <> FChartColor then
    begin
      FChartColor := Value;
      Refresh;
    end;
end;

{ calculate the TRect for the whole chart including the Axis labels }

function TdzCustomChart.CalcChartRect: TRect;
var
  Canvas: TCanvas;
begin
  Result.Left := 0;
  Result.Right := fOffScreen.Width;
  Result.Bottom := fOffScreen.Height;

  // leave 1.5 times the title height for the title
  Canvas := fOffScreen.Canvas;
  Canvas.Font := FChartTitle.TitleFont;
  Result.Top := Round(Canvas.TextHeight(FChartTitle.Title) * 1.5);
end;

procedure TdzCustomChart.DrawGrid(_Rect: TRect);
begin
  // do nothing
end;

procedure TdzCustomChart.DrawBottomScale;
begin
  // do nothing
end;

procedure TdzCustomChart.DrawLeftScale;
begin
  // do nothing
end;

procedure TdzCustomChart.DrawDataPoints;
begin
  // do nothing
end;

procedure TdzCustomChart.ScaleChart;
begin
  // do nothing
end;

procedure TdzCustomChart.DrawFrameItems;
var
  Canvas: TCanvas;
  s: string;
begin
  Canvas := fOffScreen.Canvas;
  Canvas.Font := FChartTitle.TitleFont;
  s := FChartTitle.Title;
  Canvas.TextOut((Width - Canvas.TextWidth(s)) div 2, 1, s);
end;

procedure TdzCustomChart.DrawFrame;
var
  Canvas: TCanvas;
  r: TRect;
begin
  if ChartFrame.Width <= 0 then
    exit;
  Canvas := fOffScreen.Canvas;
  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := FChartColor;
  Canvas.Pen := ChartFrame.fPen;
  r := CalcChartRect;
  if ChartFrame.Rounded then
    Canvas.RoundRect(r.Left, r.Top, r.Right, r.Bottom, 15, 15)
  else
    Canvas.Rectangle(r);
end;

procedure TdzCustomChart.DrawOffscreen;
var
  Canvas: TCanvas;
begin
  fOffScreen.Width := ClientWidth;
  fOffScreen.Height := ClientHeight;
  Canvas := fOffScreen.Canvas;
{$ifdef linux}
  Canvas.Start();
  try
{$endif linux}
    Canvas.Font := self.Font;

    // draw background
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := FBackGround;
    Canvas.FillRect(ClientRect);

    DrawFrameItems;

    { scale the chart - abstract }
    ScaleChart;

    DrawFrame;

    DrawGrid(CalcChartRect);
    DrawBottomScale;
    DrawLeftScale;

    { draw data points - abstract }
    DrawDataPoints;
{$ifdef linux}
  finally
    Canvas.Stop;
  end;
{$endif linux}
end;

procedure TdzCustomChart.Paint;
begin
  if fBeginUpdateCount = 0 then
    DrawOffscreen;
  self.Canvas.Draw(0, 0, fOffScreen);
end;

procedure TdzCustomChart.PaintTo(_DC: HDC; _X, _Y: Integer);
var
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(_DC);
  try
    MoveWindowOrg(_DC, _X, _Y);
    IntersectClipRect(_DC, 0, 0, Width, Height);
{$IFDEF MSWINDOWS}
    Perform(WM_ERASEBKGND, integer(_DC), 0);
    Perform(WM_PAINT, integer(_DC), 0);
{$ENDIF MSWINDOWS}
  finally
    RestoreDC(_DC, SaveIndex);
  end;
end;

procedure TdzCustomChart.PaintTo(_Bitmap: TBitmap);
begin
  _Bitmap.Width := ClientWidth;
  _Bitmap.Height := ClientHeight;
  _Bitmap.Canvas.Lock;
  try
    PaintTo(_Bitmap.Canvas.Handle, 0, 0);
  finally
    _Bitmap.Canvas.Unlock;
  end;
end;

procedure TdzCustomChart.BeginUpdate;
begin
  Inc(fBeginUpdateCount);
end;

procedure TdzCustomChart.EndUpdate;
begin
  Dec(fBeginUpdateCount);
  if fBeginUpdateCount <= 0 then
    begin
      fBeginUpdateCount := 0;
      Refresh;
    end;
end;

{ TdzCustomTitle }

constructor TdzCustomTitle.Create(_Owner: TComponent);
begin
  inherited Create(_Owner);
  fChart := _Owner as TdzCustomChart;
  fTitleFont := TFont.Create;
  fTitleFont.OnChange := self.FontChanged;
end;

destructor TdzCustomTitle.Destroy;
begin
  fTitleFont.Free;
  inherited;
end;

function TdzCustomTitle.GetOwner: TPersistent;
begin
  Result := fChart;
end;

procedure TdzCustomTitle.SetTitleFont(const _TitleFont: TFont);
begin
  fTitleFont.Assign(_TitleFont);
  //  RefreshChart;
end;

procedure TdzCustomTitle.SetTitle(const _Title: string);
begin
  if fTitle <> _Title then
    begin
      fTitle := _Title;
      RefreshChart;
    end;
end;

procedure TdzCustomTitle.FontChanged(_Sender: TObject);
begin
  RefreshChart;
end;

procedure TdzCustomTitle.RefreshChart;
begin
  if Assigned(fChart) then
    fChart.Refresh;
end;

{ TdzChartFrame }

constructor TdzChartFrame.Create(_Owner: TComponent; _Chart: TdzCustomChart);
begin
  inherited Create(_Owner);
  fChart := _Chart;
  fPen := TPen.Create;
  fPen.OnChange := self.PenChanged;
  fRounded := false;
end;

destructor TdzChartFrame.Destroy;
begin
  fPen.Free;
  inherited;
end;

procedure TdzChartFrame.PenChanged(_Sender: TObject);
begin
  RefreshChart;
end;

function TdzChartFrame.GetColor: TColor;
begin
  Result := fPen.Color;
end;

procedure TdzChartFrame.SetColor(const _Color: TColor);
begin
  fPen.Color := _Color;
end;

procedure TdzChartFrame.SetRounded(const _Rounded: boolean);
begin
  if fRounded <> _Rounded then
    begin
      fRounded := _Rounded;
      RefreshChart;
    end;
end;

function TdzChartFrame.GetWidth: integer;
begin
  Result := fPen.Width;
end;

procedure TdzChartFrame.SetWidth(const _Width: integer);
begin
  fPen.Width := _Width;
end;

procedure TdzChartFrame.RefreshChart;
begin
  if Assigned(fChart) then
    fChart.Refresh;
end;

{ TdzCanvas }

{$IFDEF MSWINDOWS}

procedure SetFontRotation(_Font: TFont; _Angle: integer);
var
  lf: TLogFont;
begin
  GetObject(_Font.Handle, SizeOf(lf), @lf);
  lf.lfEscapement := _Angle;
  lf.lfOrientation := _Angle;
  lf.lfOutPrecision := OUT_TT_ONLY_PRECIS;
  _Font.Handle := CreateFontIndirect(lf);
end;
{$ENDIF}

procedure TdzCanvas.TextOutAngle(_Angle, _x, _y: integer; _Text: string);
{$IFDEF MSWINDOWS}
var
  OrigFont: TFont;
begin
  OrigFont := TFont.Create;
  try
    OrigFont.Assign(Font);
    SetFontRotation(Font, _Angle * 10);
    TextOut(_x, _y, _Text);
  finally
    Font.Assign(OrigFont);
    OrigFont.Free;
  end;
{$ELSE}
begin
  u_dzQWindows.TextOutAngle(self, _Angle, _X, _Y, _Text);
{$ENDIF}
end;

function TdzCanvas.TextWidthAngle(_Angle: integer; _Text: string): integer;
{$IFDEF MSWINDOWS}
var
  OrigFont: TFont;
begin
  OrigFont := TFont.Create;
  try
    OrigFont.Assign(Font);
    SetFontRotation(Font, _Angle * 10);
    Result := TextWidth(_Text);
  finally
    Font.Assign(OrigFont);
    OrigFont.Free;
  end;
{$ELSE}
begin
  result := TextWidth(_Text);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}

procedure TdzCanvas.DrawPoint(_X, _Y: integer);
begin
  // There is now DrawPoint method in VCL
  // LineTo does not draw the last point, so we
  // simulate DrawPoint by moving to the correct coordinates and
  // drawing a line to the adjacent pixel
  MoveTo(_X, _Y);
  LineTo(_X + 1, _Y);
end;

{$ENDIF MSWINDOWS}

procedure TdzCanvas.Line(_x1, _y1, _x2, _y2: integer);
begin
  MoveTo(_X1, _Y1);
  LineTo(_X2, _Y2);
  DrawPoint(_X1, _Y1);
  DrawPoint(_X2, _Y2);
end;

{ TdzOffscreenBitmap }

function TdzOffscreenBitmap.GetCanvas: TdzCanvas;
begin
  Result := TdzCanvas(inherited Canvas);
end;

{ TdzChartGrid }

constructor TdzChartGrid.Create(_Owner: TComponent; _Chart: TdzCustomChart);
begin
  inherited Create(_Owner);
  fChart := _Chart;
  fColor := clSilver;
  fLineStyle := psSolid;
  fGridType := gtNone;
end;

procedure TdzChartGrid.Refresh;
begin
  if Assigned(fChart) then
    fChart.Refresh;
end;

procedure TdzChartGrid.SetColor(const _Color: TColor);
begin
  if fColor <> _Color then
    begin
      fColor := _Color;
      Refresh;
    end;
end;

procedure TdzChartGrid.SetGridType(const _GridType: TGridType);
begin
  if fGridType <> _GridType then
    begin
      fGridType := _GridType;
      Refresh;
    end;
end;

procedure TdzChartGrid.SetLineStyle(const _LineStyle: TPenStyle);
begin
  if fLineStyle <> _LineStyle then
    begin
      fLineStyle := _LineStyle;
      Refresh;
    end;
end;

end.

