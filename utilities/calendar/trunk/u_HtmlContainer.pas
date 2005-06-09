unit u_HtmlContainer;

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  u_HtmlComponent;

type
  THtmlContainer = class(THtmlComponent)
  protected
    FItems: TObjectList;
    procedure WriteContent; override;
  public
    constructor Create(const _Tag: string);
    destructor Destroy; override;
    procedure Add(_Item: THtmlComponent);
  end;

implementation

constructor THtmlContainer.Create(const _Tag: string);
begin
  inherited Create(_Tag);
  FItems := TObjectList.Create;
end;

destructor THtmlContainer.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure THtmlContainer.Add(_Item: THtmlComponent);
begin
  FItems.Add(_Item);
end;

procedure THtmlContainer.WriteContent;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do begin
    Write((FItems[i] as THtmlComponent).Html);
  end;
end;

end.
