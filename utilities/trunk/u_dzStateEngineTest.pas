unit u_dzStateEngineTest;

interface

uses
  SysUtils,
  u_dzStateEngineActions,
  u_dzStateEngineTransitions,
  u_dzStateEngineStates,
  u_dzStateEngine;

type
  TStateEngineTest = class
  private
  public
  end;

procedure Test;

implementation

procedure Test;
var
  EngineTest: TStateEngineTest;
begin
  EngineTest := TStateEngineTest.Create;
  try
    EngineTest.Run;
  finally
    EngineTest.Free;
  end;
end;

{ TStateEngineTest }

constructor TStateEngineTest.Create;
begin
end;

destructor TStateEngineTest.Destroy;
begin
end;

procedure TStateEngineTest.ExecuteAction(_Action: TStateEngineAction);
end;

procedure TStateEngineTest.Run;
end;

end.

