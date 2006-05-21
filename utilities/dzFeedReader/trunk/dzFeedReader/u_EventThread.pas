unit u_EventThread;

interface

uses
  Classes;

type
  TEventThread = class(TThread)
  private
    FOnExecute: TNotifyEvent;
    function GetTerminated: boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(_OnExecute: TNotifyEvent);
    property Terminated: boolean read GetTerminated;
  end;

implementation

{ TEventThread }

constructor TEventThread.Create(_OnExecute: TNotifyEvent);
begin
  inherited Create(true);
  FOnExecute := _OnExecute;
end;

procedure TEventThread.Execute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

function TEventThread.GetTerminated: boolean;
begin
  Result := inherited Terminated;
end;

end.

