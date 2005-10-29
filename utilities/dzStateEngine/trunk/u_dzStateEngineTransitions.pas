unit u_dzStateEngineTransitions;

interface

uses
  SysUtils,
  Classes,
  u_dzQuicksort,
  u_dzStateEngineStates;

type
  TStateEngineStateTransition = class
  private
    FKey: integer;
    FName: string;
    FFromStates: TStateEngineStatesList;
    FToState: TStateEngineState;
    procedure SetFromStates(const Value: TStateEngineStatesList);
  public
    constructor Create(_Key: integer; const _Name: string;
      _FromStates: array of TStateEngineState; _ToState: TStateEngineState);
    function GetKey: integer;
    property Key: integer read FKey;
    property Name: string read FName;
    property FromStates: TStateEngineStatesList read FFromStates write SetFromStates;
    property ToState: TStateEngineState read FToState;
  end;

{$DEFINE __DZ_INTEGER_SORTED_OBJECT_LIST_TEMPLATE__}
type
  _LIST_PARENT_ = TInterfacedObject; // or TObject or anything else you like
  _ITEM_TYPE_ = TStateEngineStateTransition;
{$INCLUDE t_dzIntegerSortedObjectListTemplate.tpl}

type
  TStateEngineStateTransitionList = class(_DZ_INTEGER_SORTED_OBJECT_LIST_TEMPLATE_)
  protected
    function KeyOf(const _Item: TStateEngineStateTransition): integer; override;
  end;

implementation

{$INCLUDE t_dzIntegerSortedObjectListTemplate.tpl}

{ TStateEngineStateTransition }

constructor TStateEngineStateTransition.Create(_Key: integer; const _Name: string;
  _FromStates: array of TStateEngineState; _ToState: TStateEngineState);
var
  i: integer;
begin
  inherited Create;
  FKey := _Key;
  FName := _Name;
  FFromStates := TStateEngineStatesList.Create;
  FFromStates.OwnsItems := false;
  for i := Low(_FromStates) to High(_FromStates) do
    FFromStates.Insert(_FromStates[i]);
  FToState := _ToState;
end;

function TStateEngineStateTransition.GetKey: integer;
begin
  Result := FKey;
end;

procedure TStateEngineStateTransition.SetFromStates(
  const Value: TStateEngineStatesList);
begin
  FFromStates := Value;
end;

{ TStateEngineStateTransitionList }

function TStateEngineStateTransitionList.KeyOf(const _Item: TStateEngineStateTransition): integer;
begin
  Result := _Item.Key;
end;

end.

