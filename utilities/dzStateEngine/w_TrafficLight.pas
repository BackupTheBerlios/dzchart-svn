{: This is a very simple example for using a state engine: A traffic light
   It has four different states:
     * Red
     * Red / Yellow
     * Yellow
     * Green
   It has four state transitions:
     * Red          -> Red / Yellow
     * Red / Yellow -> Green
     * Green        -> Yellow
     * Yellow       -> Red
   Each transitions corresponds to one action which uses that transition.

   Since there is only one allowed action for each state the program is very simple:
   Every time the user presses the Switch button, the program searches for the
   action that is allowed in the current state and executes it.

   Everything else is just a little bit of eye candy. }

unit w_TrafficLight;

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
  u_dzStateEngineActions,
  ExtCtrls;

type
  Tf_TrafficLight = class(TForm)
    b_Close: TButton;
    p_Red: TPanel;
    p_Yellow: TPanel;
    p_Green: TPanel;
    b_Switch: TButton;
    {: searches for the allowed action and executes it }
    procedure b_SwitchClick(Sender: TObject);
    {: closes the form }
    procedure b_CloseClick(Sender: TObject);
  private
    {: This is the actual state engine managing state transitions and actions }
    FEngine: TStateEngine;

    {: four different states are possible }
    FStateGreen: TStateEngineState;
    FStateYellow: TStateEngineState;
    FStateRed: TStateEngineState;
    FStateRedYellow: TStateEngineState;

    {: four actions are possible }
    FActionSwitchToRed: TStateEngineAction;
    FActionSwitchToGreen: TStateEngineAction;
    FActionSwitchToYellow: TStateEngineAction;
    FActionSwitchToRedYellow: TStateEngineAction;

    {: stores the actions }
    FActions: TStateEngineActionList;

    {: executes the action and calls UpdateDisplay to visualize the new state }
    procedure ExecuteEngineAction(_Action: TStateEngineAction);
    {: Visualizes the current state of the engine }
    procedure UpdateDisplay;
  public
    {: initializes the state engine and the form }
    constructor Create(_Owner: TComponent); override;
    {: destroys everything }
    destructor Destroy; override;
  end;

var
  f_TrafficLight: Tf_TrafficLight;

implementation

{$R *.dfm}

{ Tf_TrafficLight }

constructor Tf_TrafficLight.Create(_Owner: TComponent);
var
  Panels: TList;
begin
  inherited;
  FEngine := TStateEngine.Create;

  // each state gets a list of panels which are to be made active in UpdateDisplay

  // in green state the green panel is active
  Panels := TList.Create;
  Panels.Add(p_Green);
  FStateGreen := FEngine.RegisterState('green', Panels);

  // in yellow state the yellow panel is active
  Panels := TList.Create;
  Panels.Add(p_Yellow);
  FStateYellow := FEngine.RegisterState('yellow', Panels);

  // in red state the red panel is active
  Panels := TList.Create;
  Panels.Add(p_Red);
  FStateRed := FEngine.RegisterState('red', Panels);

  // in red/yellow state both, the red and the yellow panels are active
  Panels := TList.Create;
  Panels.Add(p_Red);
  Panels.Add(p_Yellow);
  FStateRedYellow := FEngine.RegisterState('red / yellow', Panels);

  // this list will store all actions
  FActions := TStateEngineActionList.Create;
  FActions.OwnsItems := false;

  // this action switches the traffic light to red
  FActionSwitchToRed := FEngine.RegisterAction('SwitchToRed');
  // it is only allowed if the current state is yellow
  // it will change the state to red
  FActionSwitchToRed.AddStateTransition([FStateYellow], FStateRed);
  FActions.Insert(FActionSwitchToRed);

  // this action switches the traffic light to green
  FActionSwitchToGreen := FEngine.RegisterAction('SwitchToGreen');
  // it is only allowed if the current state is red/yellow
  // it will change the state to green
  FActionSwitchToGreen.AddStateTransition([FStateRedYellow], FStateGreen);
  FActions.Insert(FActionSwitchToGreen);

  // this action switches the traffic light to yellow
  FActionSwitchToYellow := FEngine.RegisterAction('SwitchToYellow');
  // it is only allowed if the current state is green
  // it will change the state to yellow
  FActionSwitchToYellow.AddStateTransition([FStateGreen], FStateYellow);
  FActions.Insert(FActionSwitchToYellow);

  // this action switches the traffic light to yellow
  FActionSwitchToRedYellow := FEngine.RegisterAction('SwitchToRedYellow');
  // it is only allowed if the current state is red
  // it will change the state to red/yellow
  FActionSwitchToRedYellow.AddStateTransition([FStateRed], FStateRedYellow);
  FActions.Insert(FActionSwitchToRedYellow);

  // we start with the traffic light showing red
  FEngine.SetInitialState(FStateRed);

  // and display that to the user
  UpdateDisplay;
end;

destructor Tf_TrafficLight.Destroy;
begin
  TList(FStateGreen.Data).Free;
  TList(FStateYellow.Data).Free;
  TList(FStateRed.Data).Free;
  TList(FStateRedYellow.Data).Free;
  FActions.Free;
  FEngine.Free;
  inherited;
end;

// execute the given action and display the result

procedure Tf_TrafficLight.ExecuteEngineAction(_Action: TStateEngineAction);
begin
  Assert(FEngine.IsActionAllowed(_Action));
  FEngine.ExecuteAction(_Action);
  UpdateDisplay;
end;

// change the display according to the current state of the engine

procedure Tf_TrafficLight.UpdateDisplay;
var
  List: TList;

  // set the color of pnl to Color if it is in the list, or to clBlack otherwise

  procedure SetColor(_pnl: TPanel; _Color: TColor);
  begin
    if List.IndexOf(_pnl) = -1 then
      _pnl.Color := clBlack
    else
      _pnl.Color := _Color;
  end;

begin
  List := FEngine.GetState.Data;
  SetColor(p_Red, clRed);
  SetColor(p_Yellow, clYellow);
  SetColor(p_Green, clLime);
end;

procedure Tf_TrafficLight.b_CloseClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_TrafficLight.b_SwitchClick(Sender: TObject);
var
  i: integer;
begin
  // search for the first action that is allowed in the current state
  // execute it and exit
  for i := 0 to FActions.Count - 1 do begin
    if FEngine.IsActionAllowed(FActions[i]) then begin
      ExecuteEngineAction(FActions[i]);
      exit;
    end;
  end;
end;

end.

