unit u_dzStateEngine;

interface

uses
  SysUtils,
  u_dzStateEngineStates,
  u_dzStateEngineActions,
  u_dzException;

type
  EStateEngine = class(EdzException);
  EInvalidState = class(EStateEngine);
  EActionNotAllowed = class(EStateEngine);

type
  TStateEngine = class
  private
    FCurrentState: TStateEngineState;
    FStates: TStateEngineStatesList;
    FActions: TStateEngineActionList;
    FEndStates: TStateEngineStatesList;
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterState(const _Name: string; _Data: pointer = nil): TStateEngineState;
    function RegisterAction(const _Name: string): TStateEngineAction;
    procedure SetInitialState(_State: TStateEngineState);
    procedure AddEndState(_State: TStateEngineState);
    function GetState: TStateEngineState;
    procedure ExecuteAction(_Action: TStateEngineAction);
    function IsActionAllowed(_Action: TStateEngineAction): boolean;
    function IsInEndState: boolean;
  end;

implementation

{ TStateEngine }

constructor TStateEngine.Create;
begin
  inherited Create;
  FStates := TStateEngineStatesList.Create;
  FStates.OwnsItems := true;
  FActions := TStateEngineActionList.Create;
  FCurrentState := nil;
  FEndStates := TStateEngineStatesList.Create;
  FEndStates.OwnsItems := false;
end;

destructor TStateEngine.Destroy;
begin
  FEndStates.Free;
  FStates.Free;
  inherited;
end;

procedure TStateEngine.AddEndState(_State: TStateEngineState);
begin
  FEndStates.Insert(_State);
end;

procedure TStateEngine.ExecuteAction(_Action: TStateEngineAction);
var
  ToState: TStateEngineState;
begin
  Assert(Assigned(_Action));
  if not _Action.HasFromState(FCurrentState, ToState) then
    raise EActionNotAllowed.CreateFmt('Action %s not allowed in state %s.',
      [_Action.Name, FCurrentState.Name]);
  _Action.doBeforeExecute;
  FCurrentState := ToState;
  _Action.doAfterExecute;
end;

function TStateEngine.GetState: TStateEngineState;
begin
  Result := FCurrentState;
end;

function TStateEngine.RegisterAction(const _Name: string): TStateEngineAction;
begin
  Result := TStateEngineAction.Create(FActions.Count + 1, _Name);
  FActions.Insert(Result);
end;

function TStateEngine.RegisterState(const _Name: string; _Data: pointer = nil): TStateEngineState;
begin
  Result := TStateEngineState.Create(FStates.Count + 1, _Name, _Data);
  FStates.Insert(Result);
end;

procedure TStateEngine.SetInitialState(_State: TStateEngineState);
begin
  Assert(Assigned(_State));

  FCurrentState := _State;
end;

function TStateEngine.IsActionAllowed(_Action: TStateEngineAction): boolean;
var
  ToState: TStateEngineState;
begin
  Result := _Action.HasFromState(FCurrentState, ToState);
end;

function TStateEngine.IsInEndState: boolean;
var
  Idx: integer;
begin
  Result := FEndStates.Search(FCurrentState.GetKey, Idx);
end;

end.

