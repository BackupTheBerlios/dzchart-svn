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
  public
    constructor Create;
    destructor Destroy; override;
    function RegisterState(const _Name: string; _Data: pointer = nil): TStateEngineState;
    function RegisterAction(const _Name: string): TStateEngineAction;
    procedure SetInitialState(_State: TStateEngineState);
    function GetState: TStateEngineState;
    procedure ExecuteAction(_Action: TStateEngineAction);
    function IsActionAllowed(_Action: TStateEngineAction): boolean;
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
end;

destructor TStateEngine.Destroy;
begin
  FStates.Free;
  inherited;
end;

procedure TStateEngine.ExecuteAction(_Action: TStateEngineAction);
var
  ToState: TStateEngineState;
begin
  Assert(Assigned(_Action));
  if not _Action.HasFromState(FCurrentState, ToState) then
    raise EActionNotAllowed.CreateFmt('Action %s not allowed in state %s.',
      [_Action.Name, FCurrentState.Name]);
  FCurrentState := ToState;
end;

function TStateEngine.GetState: TStateEngineState;
begin
  Result := FCurrentState;
end;

function TStateEngine.RegisterAction(const _Name: string): TStateEngineAction;
begin
  Result := TStateEngineAction.Create(FActions.Count + 1, _Name);
  FActions.Insert(Result)
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

end.

