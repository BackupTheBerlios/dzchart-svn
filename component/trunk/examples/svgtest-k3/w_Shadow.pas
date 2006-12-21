unit w_Shadow;

interface

uses
  SysUtils,
  Types,
  Classes,
  Variants,
  QTypes,
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QStdCtrls,
  QExtCtrls,
  u_dzSvgOutput;

type
  TForm1 = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    FSvg: TdzSvgGraphics;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FSvg := TdzSvgGraphics.Create;
  FSvg.Writer.Shadow := Image1.Canvas;

  with FSvg.Writer do begin
    Pen.Color := clBlue;
    Pen.Width := 5;
    Brush.Color := clGreen;
//    Line(10, 10, 100, 100);
    Pen.Color := clBlack;
//    Rectangle(50, 50, 70, 70);
    Brush.Color := clYellow;
    Pen.Width := 2;
//    Ellipse(50, 20, 100, 60);
    Pen.Color := clRed;
    Font.Color := clBlue;
    Font.Height := 50;
    Font.Style := [fsBold, fsItalic, fsUnderline, fsStrikeOut];
    TextOut(20, 150, 'arial');
    Font.Name := 'comic sans ms';
    Font.Style := [];
    TextOut(20, 50, 'comic');
    Font.Name := 'arial';
    Font.Height := 12;
    TextOutAngle(90, 200, 100, '90 degrees');
    Polygon([Point(10, 10), Point(10, 100), Point(100, 10)]);
  end;

  FSvg.WriteToFile('/home/twm/dzsvntest.svg');
end;

end.
