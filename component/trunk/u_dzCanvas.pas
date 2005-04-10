unit u_dzCanvas;

interface

uses
  Types,
{$ifdef linux}
  Qt,
  QGraphics,
  u_dzQWindows;
{$else}
  Graphics;
{$endif}

type
  IdzCanvas = interface ['{F824149C-03A9-D911-8B90-000854097AB5}']
    function GetBrush: TBrush;
    procedure SetBrush(const _Brush: TBrush);
    function GetFont: TFont;
    procedure SetFont(const _Font: TFont);
    function GetPen: TPen;
    procedure SetPen(const _Pen: TPen);

    procedure DrawPoint(_X, _Y: integer);
    procedure Ellipse(X1, Y1, X2, Y2: Integer); overload;
    procedure Ellipse(const Rect: TRect); overload;
    procedure FillRect(const Rect: TRect);
    procedure Line(_x1, _y1, _x2, _y2: integer);
    procedure Lock;
    procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); overload;
//    procedure Pie(X, Y, W, H, Angle, AngleLength: Integer); overload;
    procedure Polygon(const Points: array of TPoint; Winding: Boolean = False;
      StartIndex: Integer = 0; NumPts: Integer = -1);
    procedure Rectangle(const Rect: TRect); overload;
    procedure Rectangle(X1, Y1, X2, Y2: Integer); overload;
    procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
    procedure SetClipRgn(_Rgn: hRgn);
    procedure Start(FreshState: Boolean = True);
    procedure Stop;
    function TextHeight(const Text: WideString): Integer;
    procedure TextOut(X, Y: Integer; const Text: WideString);
    procedure TextOutAngle(_Angle, _x, _y: integer; _Text: string);
    function TextWidth(const Text: WideString): Integer;
    function TextWidthAngle(_Angle: integer; _Text: string): integer;
    procedure Unlock;

    property Font: TFont read GetFont write SetFont;
    property Brush: TBrush read GetBrush write SetBrush;
    property Pen: TPen read GetPen write SetPen;
  end;

type
  TdzCanvas = class(TInterfacedObject, IdzCanvas)
  private
    FCanvas: TCanvas;
    property Canvas: TCanvas read FCanvas implements IdzCanvas;

    function GetFont: TFont;
    procedure SetFont(const _Font: TFont);
    function GetBrush: TBrush;
    procedure SetBrush(const _Brush: TBrush);
    function GetPen: TPen;
    procedure SetPen(const _Pen: TPen);
  protected
{$IFDEF mswindows}
    procedure DrawPoint(_X, _Y: integer); // must not be virtual
    procedure Start(FreshState: Boolean = True);
    procedure Stop;
{$ELSE} // linux
{$ENDIF}
    procedure SetClipRgn(_Rgn: hRgn);
    function TextWidthAngle(_Angle: integer; _Text: string): integer;
    procedure TextOutAngle(_Angle, _x, _y: integer; _Text: string);
    procedure Line(_x1, _y1, _x2, _y2: integer);
  public
    constructor Create(_Canvas: TCanvas);
  end;


implementation

{ TdzCanvas }

constructor TdzCanvas.Create(_Canvas: TCanvas);
begin
  inherited Create;
  FCanvas := _Canvas;
end;

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
    Canvas.TextOut(_x, _y, _Text);
  finally
    Font.Assign(OrigFont);
    OrigFont.Free;
  end;
{$ELSE}
begin
  u_dzQWindows.TextOutAngle(Canvas, _Angle, _X, _Y, _Text);
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
    Result := Canvas.TextWidth(_Text);
  finally
    Font.Assign(OrigFont);
    OrigFont.Free;
  end;
{$ELSE}
begin
  result := Canvas.TextWidth(_Text);
{$ENDIF}
end;

{$IFDEF MSWINDOWS}

procedure TdzCanvas.DrawPoint(_X, _Y: integer);
begin
  // There is now DrawPoint method in VCL
  // LineTo does not draw the last point, so we
  // simulate DrawPoint by moving to the correct coordinates and
  // drawing a line to the adjacent pixel
  Canvas.MoveTo(_X, _Y);
  Canvas.LineTo(_X + 1, _Y);
end;

{$ENDIF MSWINDOWS}

procedure TdzCanvas.Line(_x1, _y1, _x2, _y2: integer);
begin
  Canvas.MoveTo(_X1, _Y1);
  Canvas.LineTo(_X2, _Y2);
  Canvas.DrawPoint(_X1, _Y1);
  Canvas.DrawPoint(_X2, _Y2);
end;

function TdzCanvas.GetBrush: TBrush;
begin
  Result := Canvas.Brush;
end;

function TdzCanvas.GetFont: TFont;
begin
  Result := Canvas.Font;
end;

function TdzCanvas.GetPen: TPen;
begin
  Result := Canvas.Pen;
end;

procedure TdzCanvas.SetBrush(const _Brush: TBrush);
begin
  Canvas.Brush := _Brush;
end;

procedure TdzCanvas.SetFont(const _Font: TFont);
begin
  Canvas.Font := _Font;
end;

procedure TdzCanvas.SetPen(const _Pen: TPen);
begin
  Canvas.Pen := _Pen;
end;

procedure TdzCanvas.SetClipRgn(_Rgn: hRgn);
begin
  u_dzQWindows.SelectClipRgn(Canvas.Handle, _Rgn);
end;

end.
