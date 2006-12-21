unit u_dzGraphics;

interface

uses
{$ifdef linux}
  QGraphics,
{$else}
  Graphics,
{$endif}
  u_dzCanvas;

type
  IdzGraphics = interface ['{720D1697-08A9-D911-8B90-000854097AB5}']
    function GetCanvas: IdzCanvas;
    function GetHeight: integer;
    procedure SetHeight(const Value: integer);
    function GetWidth: integer;
    procedure SetWidth(const Value: integer);

    property Height: integer read GetHeight write SetHeight;
    property Width: integer read GetWidth write SetWidth;
    property Canvas: IdzCanvas read GetCanvas;
  end;

type
  TdzBitmap = class(TInterfacedObject, IdzGraphics)
  private
    FBitmap: TBitmap;
    function GetCanvas: IdzCanvas;
    function GetHeight: integer;
    procedure SetHeight(const _Height: integer);
    function GetWidth: integer;
    procedure SetWidth(const _Width: integer);
  public
    constructor Create(_Bitmap: TBitmap);
  end;

implementation

{ TdzBitmap }

constructor TdzBitmap.Create(_Bitmap: TBitmap);
begin
  inherited Create;
  FBitmap := _Bitmap;
end;

function TdzBitmap.GetCanvas: IdzCanvas;
begin
  Result := TdzCanvas.Create(FBitmap.Canvas);
end;

function TdzBitmap.GetHeight: integer;
begin
  Result := FBitmap.Height;
end;

function TdzBitmap.GetWidth: integer;
begin
  Result := FBitmap.Width;
end;

procedure TdzBitmap.SetHeight(const _Height: integer);
begin
  FBitmap.Height := _Height;
end;

procedure TdzBitmap.SetWidth(const _Width: integer);
begin
  FBitmap.Width := _Width;
end;

end.
