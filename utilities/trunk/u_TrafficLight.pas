unit u_TrafficLight;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  Buttons,
  StdCtrls,
  u_dzStateEngine,
  u_dzStateEngineStates,
  u_dzStateEngineActions;

type
  Tf_TrafficLight = class(TForm)
    b_Close: TButton;
    sb_Red: TSpeedButton;
    sb_Yellow: TSpeedButton;
    sb_Green: TSpeedButton;
  private
    FEngine: TStateEngine;

    FStateGreen: TStateEngineState;
    FStateYellow: TStateEngineState;
    FStateRed: TStateEngineState;
    FStateRedYellow: TStateEngineState;

    FActionSwitchToRed: TStateEngineAction;
    FActionSwitchToGreen: TStateEngineAction;
    FActionSwitchToYellow: TStateEngineAction;
    FActionSwitchToRedYellow: TStateEngineAction;

    FActions: TStateEngineActionList;

    procedure ExecuteAction(_Action: TStateEngineAction);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    procedure Run;
  end;

var
  f_TrafficLight: Tf_TrafficLight;

implementation

{$R *.dfm}

{ Tf_TrafficLight }

constructor Tf_TrafficLight.Create(_Owner: TComponent);
begin
  inherited;
  FEngine := TStateEngine.Create;

  FStateGreen := FEngine.RegisterState('green');
  FStateYellow := FEngine.RegisterState('yellow');
  FStateRed := FEngine.RegisterState('red');
  FStateRedYellow := FEngine.RegisterState('red / yellow');

  FActions := TStateEngineActionList.Create;
  FActions.OwnsItems := false;

  FActionSwitchToRed := FEngine.RegisterAction('SwitchToRed');
  FActionSwitchToRed.AddStateTransition([FStateYellow], FStateRed);
  FActions.Insert(FActionSwitchToRed);

  FActionSwitchToGreen := FEngine.RegisterAction('SwitchToGreen');
  FActionSwitchToGreen.AddStateTransition([FStateRedYellow], FStateGreen);
  FActions.Insert(FActionSwitchToGreen);

  FActionSwitchToYellow := FEngine.RegisterAction('SwitchToYellow');
  FActionSwitchToYellow.AddStateTransition([FStateGreen], FStateYellow);
  FActions.Insert(FActionSwitchToYellow);

  FActionSwitchToRedYellow := FEngine.RegisterAction('SwitchToRedYellow');
  FActionSwitchToRedYellow.AddStateTransition([FStateRed], FStateRedYellow);
  FActions.Insert(FActionSwitchToRedYellow);

  FEngine.SetInitialState(FStateRed);
end;

destructor Tf_TrafficLight.Destroy;
begin
  FActions.Free;
  FEngine.Free;
  inherited;
end;

procedure Tf_TrafficLight.ExecuteAction(_Action: TStateEngineAction);
var
  OldState: TStateEngineState;
begin
  Assert(FEngine.IsActionAllowed(_Action));
  OldState := FEngine.GetState;
  FEngine.ExecuteAction(_Action);
  WriteLn('executing action ', _Action.Name, ' changed state from ', OldState.Name, ' to ', FEngine.GetState.Name);
end;

procedure Tf_TrafficLight.Run;
var
  Cycles: integer;
  i: integer;
begin
  for Cycles := 1 to 100 do begin
    for i := 0 to FActions.Count - 1 do begin
      if FEngine.IsActionAllowed(FActions[i]) then
        ExecuteAction(FActions[i]);
    end;
  end;
  readln;
end;

end.

