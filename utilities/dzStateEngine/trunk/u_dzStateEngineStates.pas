unit u_dzStateEngineStates;

interface

uses
  SysUtils,
  Classes,
  u_dzQuicksort;

type
  TStateEngineState = class
  protected
    FKey: integer;
    FName: string;
    FData: pointer;
  public
    constructor Create(_Key: integer; const _Name: string; _Data: pointer = nil);
    function GetKey: integer;
    property Name: string read FName write FName;
    property Data: pointer read FData;
  end;

{$DEFINE __DZ_INTEGER_SORTED_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _LIST_CONTAINER_ = TList;
  _ITEM_TYPE_ = TStateEngineState;
{$INCLUDE t_dzIntegerSortedObjectListTemplate.tpl}

type
  TStateEngineStatesList = class(_DZ_INTEGER_SORTED_OBJECT_LIST_TEMPLATE_)
  private
    FOwnsItems: boolean;
  protected
    {: calls the Item's Free method }
    procedure FreeItem(_Item: TStateEngineState); override;
    function KeyOf(const _Item: TStateEngineState): integer; override;
  public
    property OwnsItems: boolean read FOwnsItems write FOwnsItems;
  end;

implementation

{ TStateEngineState }

constructor TStateEngineState.Create(_Key: integer; const _Name: string; _Data: pointer = nil);
begin
  inherited Create;
  FKey := _Key;
  FName := _Name;
  FData := _Data;
end;

function TStateEngineState.GetKey: integer;
begin
  Result := FKey;
end;

{ TStateEngineStatesList }

{$INCLUDE t_dzIntegerSortedObjectListTemplate.tpl}

procedure TStateEngineStatesList.FreeItem(_Item: _ITEM_TYPE_);
begin
  if OwnsItems then
    inherited FreeItem(_Item);
end;

function TStateEngineStatesList.KeyOf(const _Item: TStateEngineState): integer;
begin
  Result := _Item.GetKey;
end;

end.

