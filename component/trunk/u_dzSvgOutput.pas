unit u_dzSvgOutput;

interface

uses
  Types,
  SysUtils,
  Classes,
{$ifdef linux}
  QGraphics,
  u_dzQWindows,
{$else}
  Windows,
  Graphics,
{$endif}
  u_dzCanvas,
  u_dzGraphics;

type
{$ifdef linux}
  GraphicsString = WideString;
{$else}
  GraphicsString = String;
{$endif}

type
  TdzSvgWriter = class
  private
    FBrush: TBrush;
    FBrushChanged: boolean;
    FContent: TStringList;
    FFont: TFont;
    FFontChanged: boolean;
    FPen: TPen;
    FPenChanged: boolean;
    FShadow: TCanvas;

    function GetPen: TPen;
    procedure SetPen(const _Pen: TPen);
    function ColorToSvgColor(_Color: TColor): string;
    function PenModeToSvgMode(_Mode: TPenMode): string;
    function PenStyleToSvgStyle(_Style: TPenStyle): string;
    procedure OnPenChanged(Sender: TObject);
    procedure OnBrushChanged(Sender: TObject);
    procedure OnFontChanged(Sender: TObject);
    function PenSettings: string;
    function BrushSettings: string;
    function GetBrush: TBrush;
    procedure SetBrush(const _Brush: TBrush);
    function GetFont: TFont;
    procedure SetFont(const _Font: TFont);
    function FontSettings: string;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DrawPoint(_X, _Y: integer);
    procedure Ellipse(_x1, _y1, _x2, _y2: integer); overload;
    procedure Ellipse(const Rect: TRect); overload;
    procedure FillRect(const Rect: TRect);
    procedure Line(_x1, _y1, _x2, _y2: integer);
    procedure Lock;
    procedure Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer); overload;
//    procedure Pie(X, Y, W, H, Angle, AngleLength: Integer); overload;
    procedure Polygon(const _Points: array of TPoint);
    procedure Rectangle(const Rect: TRect); overload;
    procedure Rectangle(_X1, _Y1, _X2, _Y2: Integer); overload;
    procedure RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
    procedure SetClipRgn(_Rgn: hRgn);
    procedure Start(FreshState: Boolean = True);
    procedure Stop;
    function TextHeight(const _Text: GraphicsString): Integer;
    procedure TextOut(_x, _y: Integer; const _Text: GraphicsString);
    procedure TextOutAngle(_Angle, _x, _y: integer; const _Text: GraphicsString);
    function TextWidth(const _Text: GraphicsString): Integer;
    function TextWidthAngle(_Angle: integer; const _Text: GraphicsString): integer;
    procedure Unlock;

    property Brush: TBrush read GetBrush write SetBrush;
    property Font: TFont read GetFont write SetFont;
    property Pen: TPen read GetPen write SetPen;
    property Shadow: TCanvas read FShadow write FShadow;
  end;

type
  TdzSvgCanvas = class(TInterfacedObject, IdzCanvas)
  protected
    FWriter: TdzSvgWriter;
    procedure DrawPolygon(const _Points: array of TPoint);
    property Writer: TdzSvgWriter read FWriter implements IdzCanvas;
  public
    constructor Create(_Writer: TdzSvgWriter);
  end;

type
  IdzSvgGraphics = interface(IdzGraphics) ['{BA806F85-BEA9-D911-995B-000854097AB5}']
    function AsSvg: string;
    procedure WriteToFile(const _Filename: string);
    procedure WriteToStream(_st: TStream);
  end;

type
  TdzSvgGraphics = class(TInterfacedObject, IdzGraphics, IdzSvgGraphics)
  private
    FWriter: TdzSvgWriter;
    FHeight: integer;
    FWidth: integer;
  public // implementation of IdzGraphics
    function GetCanvas: IdzCanvas;
    function GetHeight: integer;
    procedure SetHeight(const _Height: integer);
    function GetWidth: integer;
    procedure SetWidth(const _Width: integer);
    property Height: integer read GetHeight write SetHeight;
    property Width: integer read GetWidth write SetWidth;
    property Canvas: IdzCanvas read GetCanvas;
  public // implementation of IdzSvgGraphics
    function AsSvg: string;
    procedure WriteToFile(const _Filename: string);
    procedure WriteToStream(_st: TStream);
  public
    constructor Create;
    destructor Destroy; override;
    property Writer: TdzSvgWriter read FWriter;
  end;

implementation

procedure DrawTextAngle(_Canvas: TCanvas; _Angle, _x, _y: integer; const _Text: string);

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

var
  OrigFont: TFont;
begin
  OrigFont := TFont.Create;
  try
    OrigFont.Assign(_Canvas.Font);
    SetFontRotation(_Canvas.Font, _Angle * 10);
    _Canvas.TextOut(_x, _y, _Text);
  finally
    _Canvas.Font.Assign(OrigFont);
    OrigFont.Free;
  end;
{$else}
begin
  TextOutAngle(_Canvas.Handle, _Angle, _x, _y, _Text);
{$endif}
end;

{ TdzSvgWriter }

constructor TdzSvgWriter.Create;
begin
  inherited Create;
  FContent := TStringList.Create;
  FPenChanged := true;
  FBrushChanged := true;
  FFontChanged := true;
  FPen := TPen.Create;
  FBrush := TBrush.Create;
  FFont := TFont.Create;
  FPen.OnChange := self.OnPenChanged;
  FBrush.OnChange := self.OnBrushChanged;
  FFont.OnChange := self.OnFontChanged;
end;

destructor TdzSvgWriter.Destroy;
begin
  FFont.Free;
  FPen.Free;
  FBrush.Free;
  FContent.Free;
  inherited;
end;

function TdzSvgWriter.GetPen: TPen;
begin
  Result := fPen;
end;

procedure TdzSvgWriter.SetPen(const _Pen: TPen);
begin
  FPen.Assign(_Pen);
  FPenChanged := true;
end;

function TdzSvgWriter.ColorToSvgColor(_Color: TColor): string;
begin
  Result := Format('#%.2x', [ColorToRgb(_Color) and $FF])
    + Format('%.2x', [(ColorToRgb(_Color) shr 8) and $FF])
    + Format('%.2x', [ColorToRgb(_Color) shr 16]);
end;

function TdzSvgWriter.PenStyleToSvgStyle(_Style: TPenStyle): string;
begin
  //  TPenStyle = (psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear);
  // not implemented
end;

function TdzSvgWriter.PenModeToSvgMode(_Mode: TPenMode): string;
begin
  //  TPenMode = (pmBlack, pmWhite, pmNop, pmNot, pmCopy, pmNotCopy,
  //    pmMergePenNot, pmMaskPenNot, pmMergeNotPen, pmMaskNotPen, pmMerge,
  //    pmNotMerge, pmMask, pmNotMask, pmXor, pmNotXor);
  // not implemented
end;

function TdzSvgWriter.PenSettings: string;
begin
  Result := Format('stroke:%s; stroke-width:%d', [ColorToSvgColor(Pen.Color), Pen.Width]);
end;

function TdzSvgWriter.BrushSettings: string;
begin
  Result := Format('fill:%s', [ColorToSvgColor(Brush.Color)]);
end;

function TdzSvgWriter.FontSettings: string;
var
  Style: string;
  Weight: string;
  Decoration: string;
begin
  if fsBold in Font.Style then
    Weight := 'bold'
  else
    Weight := 'normal';

  if fsItalic in Font.Style then
    Style := 'italic'
  else
    Style := 'normal';

  if fsUnderline in Font.Style then begin
    if fsStrikeOut in Font.Style then
      Decoration := 'underline line-through'
    else
      Decoration := 'underline'
  end else if fsStrikeOut in Font.Style then
    Decoration := 'line-through'
  else
    Decoration := 'none';
  Result := Format('font-family:%s; font-size:%d; fill:%s; stroke-width:0; font-weight:%s; font-style:%s; text-decoration: %s',
    [Font.Name, Font.Size, ColorToSvgColor(Font.Color), Weight, Style, Decoration]);
end;

procedure TdzSvgWriter.Line(_x1, _y1, _x2, _y2: integer);
begin
  if Assigned(Shadow) then begin
    Shadow.Pen := Pen;
    Shadow.Brush := Brush;
    Shadow.MoveTo(_x1, _y1);
    Shadow.LineTo(_x2, _y2);
  end;
  FContent.Add(Format('<line style="%s" x1="%d" y1="%d" x2="%d" y2="%d"/>', [PenSettings, _x1, _y1, _x2, _y2]));
end;

procedure TdzSvgWriter.OnPenChanged(Sender: TObject);
begin
  FPenChanged := true;
end;

function TdzSvgWriter.GetBrush: TBrush;
begin
  Result := FBrush;
end;

procedure TdzSvgWriter.SetBrush(const _Brush: TBrush);
begin
  FBrush.Assign(_Brush);
end;

procedure TdzSvgWriter.OnBrushChanged(Sender: TObject);
begin
  FBrushChanged := true;
end;

procedure TdzSvgWriter.TextOut(_x, _y: integer; const _Text: GraphicsString);
begin
  if Assigned(Shadow) then begin
    Shadow.Pen := Pen;
    Shadow.Brush := Brush;
    Shadow.Font := Font;
    Shadow.TextOut(_x, _y, _Text);
  end;
  FContent.Add(Format('<text style="%s" x="%d" y="%d">%s</text>',
    [FontSettings, _x, _y + Font.Height, _Text]));
end;
// <text x="20" y="25" style="font-size: 12; fill: black"> This draws 12 pixels high. </text>
// <text x="20" y="50" style="font-size: 12px; fill: red"> This draws 12 pixels high. </text>
// <g transform="scale(2)">
//   <text x="20" y="50" style="font-size: 12; fill:green"> This draws 24 pixels high. </text>
//   <text x="20" y="62.5" style="font-size: 12px; fill:blue"> This draws 12 pixels high. </text>
// </g>
function TdzSvgWriter.GetFont: TFont;
begin
  Result := FFont;
end;

procedure TdzSvgWriter.SetFont(const _Font: TFont);
begin
  FFont.Assign(_Font);
end;

procedure TdzSvgWriter.OnFontChanged(Sender: TObject);
begin
  FFontChanged := true;
end;

procedure TdzSvgWriter.DrawPoint(_X, _Y: integer);
begin
{$ifdef linux}
  if Assigned(Shadow) then begin
    Shadow.Pen := Pen;
    Shadow.Brush := Brush;
    Shadow.Font := Font;
    Shadow.DrawPoint(_x, _y);
  end;
{$endif}
  FContent.Add(Format('<path style="%s; stroke-linecap:round; stroke-width:1" d="M%d %d L%d %d" />', [PenSettings, _x, _y, _x, _y]));
end;

procedure TdzSvgWriter.Ellipse(const Rect: TRect);
begin
  Ellipse(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

procedure TdzSvgWriter.Ellipse(_x1, _y1, _x2, _y2: integer);
var
  rx, ry: integer;
  cx, cy: integer;
begin
  if Assigned(Shadow) then begin
    Shadow.Pen := Pen;
    Shadow.Brush := Brush;
    Shadow.Ellipse(_x1, _y1, _x2, _y2);
  end;

  if _x1 > _x2 then begin
    rx := (_x1 - _x2) div 2;
  end else begin
    rx := (_x2 - _x1) div 2;
  end;
  cx := (_x1 + _x2) div 2;

  if _y1 > _y2 then begin
    ry := (_y1 - _y2) div 2;
  end else begin
    ry := (_y2 - _y1) div 2;
  end;
  cy := (_y1 + _y2) div 2;

  FContent.Add(Format('<ellipse style="%s;%s" cx="%d" cy="%d" rx="%d" ry="%d"/>',
    [PenSettings, BrushSettings, cx, cy, rx, ry]));
end;

procedure TdzSvgWriter.FillRect(const Rect: TRect);
begin
  Rectangle(Rect);
end;

procedure TdzSvgWriter.Lock;
begin
  // do nothing
end;

procedure TdzSvgWriter.Pie(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer);
begin
  // not implemented
end;

procedure TdzSvgWriter.Polygon(const _Points: array of TPoint);
var
  Lines: string;
  i: integer;
begin
  if Assigned(Shadow) then begin
    Shadow.Pen := Pen;
    Shadow.Brush := Brush;
    Shadow.Polygon(_Points);
  end;

  Lines := Format('M%d %d', [_Points[0].X, _Points[0].Y]);
  for i := 1 to Length(_Points) - 1 do begin
    Lines := Lines + Format(' L%d %d', [_Points[i].X, _Points[i].Y]);
  end;
  if (_Points[0].X <> _Points[Length(_Points) - 1].X)
     or (_Points[0].Y <> _Points[Length(_Points) - 1].Y) then
    Lines := Lines + Format(' L%d %d', [_Points[0].X, _Points[0].Y]);

  FContent.Add(Format('<path style="%s;%s" d="%s"/>',
    [PenSettings, BrushSettings, Lines]));
end;

procedure TdzSvgWriter.Rectangle(const Rect: TRect);
begin
  Rectangle(Rect.Left, Rect.Top, Rect.Right, Rect.Bottom);
end;

procedure TdzSvgWriter.Rectangle(_x1, _y1, _x2, _y2: integer);
var
  w, h: integer;
begin
  if Assigned(Shadow) then begin
    Shadow.Pen := Pen;
    Shadow.Brush := Brush;
    Shadow.Rectangle(_x1, _y1, _x2, _y2);
  end;

  if _x1 > _x2 then begin
    w := _x1 - _x2;
    _x1 := _x2;
  end else
    w := _x2 - _x1;

  if _y1 > _y2 then begin
    h := _y1 - _y2;
    _y1 := _y2;
  end else
    h := _y2 - _y1;

  FContent.Add(Format('<rect style="%s;%s" x="%d" y="%d" width="%d" height="%d"/>',
    [PenSettings, BrushSettings, _x1, _y1, w, h]));
end;

procedure TdzSvgWriter.RoundRect(X1, Y1, X2, Y2, X3, Y3: Integer);
begin
  // not implemented
end;

procedure TdzSvgWriter.SetClipRgn(_Rgn: hRgn);
begin
  // do nothing
end;

procedure TdzSvgWriter.Start(FreshState: Boolean);
begin
  // do nothing
end;

procedure TdzSvgWriter.Stop;
begin
  // do nothing
end;

function TdzSvgWriter.TextHeight(const _Text: GraphicsString): Integer;
begin
  Result := FFont.Height;
end;

procedure TdzSvgWriter.TextOutAngle(_Angle, _x, _y: integer; const _Text: Graphicsstring);
begin
  if Assigned(Shadow) then begin
    Shadow.Font := Font;
    DrawTextAngle(Shadow, _Angle, _x, _y, _Text);

  end;
  FContent.Add(Format('<text style="%s" x="%d" y="%d" transform="rotate(-%d %d %d)">%s</text>',
    [FontSettings, _x, _y + Font.Height, _Angle, _x, _y, _Text]));
end;

function TdzSvgWriter.TextWidth(const _Text: GraphicsString): Integer;
begin
  Result := Length(_Text) * FFont.Size;
end;

function TdzSvgWriter.TextWidthAngle(_Angle: integer; const _Text: Graphicsstring): integer;
begin
  Result := Length(_Text) * FFont.Size;
end;

procedure TdzSvgWriter.Unlock;
begin
  // do nothing
end;

{ TdzSvgGraphics }

constructor TdzSvgGraphics.Create;
begin
  inherited Create;
  FWriter := TdzSvgWriter.Create;
end;

destructor TdzSvgGraphics.Destroy;
begin
  FWriter.Free;
  inherited;
end;

function TdzSvgGraphics.AsSvg: string;
begin
  Result := '<svg>' + FWriter.FContent.Text + '</svg>';
end;

function TdzSvgGraphics.GetCanvas: IdzCanvas;
begin
  Result := TdzSvgCanvas.Create(FWriter);
end;

function TdzSvgGraphics.GetHeight: integer;
begin
  Result := FHeight;
end;

function TdzSvgGraphics.GetWidth: integer;
begin
  Result := FWidth;
end;

procedure TdzSvgGraphics.SetHeight(const _Height: integer);
begin
  FHeight := _Height;
end;

procedure TdzSvgGraphics.SetWidth(const _Width: integer);
begin
  FWidth := _Width;
end;

procedure TdzSvgGraphics.WriteToFile(const _Filename: string);
var
  st: TFileStream;
begin
  st := TFileStream.Create(_Filename, fmCreate or fmShareExclusive);
  try
    WriteToStream(st);
  finally
    st.Free
  end;
end;

procedure TdzSvgGraphics.WriteToStream(_st: TStream);
var
  S: string;
begin
  S := AsSvg;
  _st.WriteBuffer(Pointer(S)^, Length(S));
end;

{ TdzSvgCanvas }

constructor TdzSvgCanvas.Create(_Writer: TdzSvgWriter);
begin
  inherited Create;
  FWriter := _Writer;
end;

procedure TdzSvgCanvas.DrawPolygon(const _Points: array of TPoint);
begin
  FWriter.Polygon(_Points);
end;

end.
