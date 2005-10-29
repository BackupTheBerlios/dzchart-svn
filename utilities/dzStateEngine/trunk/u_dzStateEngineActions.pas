unit u_dzStateEngineActions;

interface

uses
  SysUtils,
  Classes,
  u_dzException,
  u_dzQuicksort,
  u_dzStateEngineStates,
  u_dzStateEngineTransitions;

type
  EStateEngineAction = class(EdzException);
  EDuplicateFromState = class(EStateEngineAction);

type
  TStateEngineAction = class
  protected
    FKey: integer;
    FName: string;
    FTransitions: TStateEngineStateTransitionList;
  public
    constructor Create(_Key: integer; const _Name: string);
    destructor Destroy; override;
    procedure AddStateTransition(_FromStates: array of TStateEngineState; _ToState: TStateEngineState);
    function HasFromState(_State: TStateEngineState): boolean; overload;
    function HasFromState(_State: TStateEngineState; out _ToState: TStateEngineState): boolean; overload;
    function GetKey: integer;
    property Name: string read FName write FName;
  end;

{$DEFINE __DZ_INTEGER_SORTED_OBJECT_LIST_TEMPLATE__}
type
  _ITEM_TYPE_ = TStateEngineAction;
{$INCLUDE t_dzIntegerSortedObjectListTemplate.tpl}

type
  TStateEngineActionList = class(_DZ_INTEGER_SORTED_OBJECT_LIST_TEMPLATE_)
  private
    FOwnsItems: boolean;
  protected
    procedure FreeItem(_Item: TStateEngineAction); override;
    function KeyOf(const _Item: TStateEngineAction): integer; override;
  public
    constructor Create; 
    property OwnsItems: boolean read FOwnsItems write FOwnsItems;
  end;

implementation

{ TStateEngineActionList }

constructor TStateEngineActionList.Create;
begin
  inherited;
  FOwnsItems := true;
end;

procedure TStateEngineActionList.FreeItem(_Item: TStateEngineAction);
begin
  if OwnsItems then
    inherited;
end;

function TStateEngineActionList.KeyOf(const _Item: TStateEngineAction): integer;
begin
  Result := _Item.GetKey;
end;

{$INCLUDE t_dzIntegerSortedObjectListTemplate.tpl}

{ TStateEngineAction }

procedure TStateEngineAction.AddStateTransition(_FromStates: array of TStateEngineState; _ToState: TStateEngineState);
var
  i: integer;
  State: TStateEngineState;
  Id: integer;
begin
  // check for from action conflicts
  for i := Low(_FromStates) to High(_FromStates) do begin
    State := _FromStates[i];
    if HasFromState(State) then
      raise EDuplicateFromState.CreateFmt('Duplicate FromState %s in action %s',
        [State.Name, Name]);
  end;

  // no conflicts, add it
  Id := FTransitions.Count + 1;
  FTransitions.Insert(TStateEngineStateTransition.Create(Id, Name + IntToStr(Id), _FromStates, _ToState));
end;

constructor TStateEngineAction.Create(_Key: integer; const _Name: string);
begin
  inherited Create;
  FKey := _Key;
  FName := _Name;
  FTransitions := TStateEngineStateTransitionList.Create;
end;

destructor TStateEngineAction.Destroy;
begin
  FTransitions.free;
  inherited;
end;

function TStateEngineAction.GetKey: integer;
begin
  Result := FKey;
end;

function TStateEngineAction.HasFromState(_State: TStateEngineState): boolean;
var
  ToState: TStateEngineState;
begin
  Result := HasFromState(_State, ToState);
end;

function TStateEngineAction.HasFromState(_State: TStateEngineState;
  out _ToState: TStateEngineState): boolean;
var
  TransIdx: integer;
  Transition: TStateEngineStateTransition;
  Idx: integer;
begin
  for TransIdx := 0 to FTransitions.Count - 1 do begin
    Transition := FTransitions[TransIdx];
    Result := Transition.FromStates.Search(_State.GetKey, Idx);
    if Result then begin
      _ToState := Transition.ToState;
      exit;
    end;
  end;
  Result := false;
end;

end.

