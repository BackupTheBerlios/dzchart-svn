unit wf_TileFrame;

interface

uses
  SysUtils,
  Types,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  ExtCtrls,
  StdCtrls,
  Buttons;

type
  TOnMove = procedure(_Sender: TObject; _MouseX, _MouseY: integer) of object;
  TResizeDirection = (rdTop, rdRight, rdBottom, rdLeft);
  TResizeDirections = set of TResizeDirection;

type
  Tfr_TileFrame = class(TFrame)
    p_Bottom: TPanel;
    p_BottomLeft: TPanel;
    p_BottomRight: TPanel;
    p_Client: TPanel;
    p_Frame: TPanel;
    p_Left: TPanel;
    p_Right: TPanel;
    p_Title: TPanel;
    p_Top: TPanel;
    p_TopLeft: TPanel;
    p_TopRight: TPanel;
    p_TitleRight: TPanel;
    p_TitleLeft: TPanel;
    p_TitleCaption: TPanel;
    procedure p_BottomLeftMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure p_BottomMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure p_BottomRightMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure p_LeftMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure p_ResizeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure p_ResizeMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure p_RightMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure p_TitleMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure p_TitleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure p_TitleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure p_TopLeftMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure p_TopMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure p_TopRightMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FMoving: boolean;
    FOnEndMove: TOnMove;
    FOnMove: TOnMove;
    FOnStartMove: TOnMove;
    FResizing: TResizeDirections;
    FResizingX: integer;
    FResizingY: integer;
    FOnResized: TNotifyEvent;
    procedure StartResize(_Direction: TResizeDirections; _Button: TMouseButton; _Shift: TShiftState; _X, _Y: Integer);
    procedure EndResize;
    function GetCaption: string;
    procedure SetCaption(const _Caption: string);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;

    procedure Activate;
    procedure Deactivate;

    property Caption: string read GetCaption write SetCaption;

    property OnEndMove: TOnMove read FOnEndMove write FOnEndMove;
    property OnMove: TOnMove read FOnMove write FOnMove;
    property OnStartMove: TOnMove read FOnStartMove write FOnStartMove;
    property OnResized: TNotifyEvent read FOnResized write FOnResized;
  end;

implementation

{$R *.dfm}

{ Tfr_Table }

constructor Tfr_TileFrame.Create(_Owner: TComponent);
begin
  inherited Create(_Owner);
end;

destructor Tfr_TileFrame.Destroy;
begin
  inherited;
end;

procedure Tfr_TileFrame.EndResize;
begin
  FResizing := [];
  if Assigned(FOnResized) then
    FOnResized(Self);
end;

function Tfr_TileFrame.GetCaption: string;
begin
  Result := p_TitleCaption.Caption;
end;

procedure Tfr_TileFrame.p_TitleMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pnt: TPoint;
begin
  if (Button <> mbLeft) or not Assigned(FOnStartMove) then
    exit;
  FMoving := true;
  pnt := ClientToParent(Point(X, Y), Parent);
  FOnStartMove(Self, pnt.X, pnt.Y);
end;

procedure Tfr_TileFrame.p_TitleMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  pnt: TPoint;
begin
  if not FMoving or (Button <> mbLeft) or (Shift <> []) or not Assigned(FOnEndMove) then
    exit;
  FMoving := false;
  pnt := ClientToParent(Point(X, Y), Parent);
  FOnEndMove(Self, pnt.X, pnt.Y);
end;

procedure Tfr_TileFrame.p_TitleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  pnt: TPoint;
begin
  if not FMoving or not Assigned(FOnEndMove) then
    exit;
  pnt := ClientToParent(Point(X, Y), Parent);
  FOnMove(Self, pnt.X, pnt.Y);
end;

procedure Tfr_TileFrame.p_ResizeMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  dx: integer;
  dy: integer;
begin
  if (FResizing = []) or not Assigned(FOnEndMove) then
    exit;
  dx := X - FResizingX;
  dy := Y - FResizingY;
  if rdTop in FResizing then begin
    Top := Top + dy;
    Height := Height - dy;
  end;
  if rdRight in FResizing then
    Width := Width + dx;
  if rdBottom in FResizing then
    Height := Height + dy;
  if rdLeft in FResizing then begin
    Left := Left + dx;
    Width := Width - dx;
  end;
end;

procedure Tfr_TileFrame.p_ResizeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (FResizing = []) or (Button <> mbLeft) or not Assigned(FOnEndMove) then
    exit;
  EndResize;
end;

procedure Tfr_TileFrame.SetCaption(const _Caption: string);
begin
  p_TitleCaption.Caption := _Caption;
end;

procedure Tfr_TileFrame.StartResize(_Direction: TResizeDirections;
  _Button: TMouseButton; _Shift: TShiftState; _X, _Y: Integer);
begin
  if _Button <> mbLeft then
    exit;
  FResizing := _Direction;
  FResizingX := _X;
  FResizingY := _Y;
end;

procedure Tfr_TileFrame.p_TopMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  StartResize([rdTop], Button, Shift, X, Y);
end;

procedure Tfr_TileFrame.p_BottomMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  StartResize([rdBottom], Button, Shift, X, Y);
end;

procedure Tfr_TileFrame.p_LeftMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  StartResize([rdLeft], Button, Shift, X, Y);
end;

procedure Tfr_TileFrame.p_RightMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  StartResize([rdRight], Button, Shift, X, Y);
end;

procedure Tfr_TileFrame.p_BottomLeftMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  StartResize([rdLeft, rdBottom], Button, Shift, X, Y);
end;

procedure Tfr_TileFrame.p_TopLeftMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  StartResize([rdLeft, rdTop], Button, Shift, X, Y);
end;

procedure Tfr_TileFrame.p_TopRightMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  StartResize([rdRight, rdTop], Button, Shift, X, Y);
end;

procedure Tfr_TileFrame.p_BottomRightMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  StartResize([rdRight, rdBottom], Button, Shift, X, Y);
end;

procedure Tfr_TileFrame.Activate;
begin
  BringToFront;
  p_Title.Color := clActiveCaption;
  p_Title.Font.Color := clCaptionText;
end;

procedure Tfr_TileFrame.Deactivate;
begin
  p_Title.Color := clInactiveCaption;
  p_Title.Font.Color := clCaptionText;
end;

end.

