unit u_dzBeep;

interface

uses
  Windows,
  SysUtils,
  SyncObjs,
  u_dzNamedThread;

type
  ///<summary> Windows.Beep is synchronous, so it does not return until
  ///          the beep's duration has passed. This is a problem if you
  ///          cannot afford to block the current thread that long.
  ///          This class creates a thread (singleton) that does the
  ///          call for other threads. Note that I have not put much
  ///          work into making it really threadsafe, I rely on
  ///          writes to DWords being atomic operations and just
  ///          use two fields to pass the parameters. It is quite
  ///          possible for other threads to change these parameters
  ///          before they ever reach the Beeper thread, but all that
  ///          would cause is some weird beeping, so I can't be bothered.
  TBeeper = class(TNamedThread)
  private
    FFrequency: DWord;
    FDuration: DWord;
    FEvent: TEvent;
    procedure Terminate;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Beep(_Frequency, _Duration: DWord);
  end;

var
  Beeper: TBeeper = nil;

implementation

{ TBeeper }

procedure TBeeper.Beep(_Frequency, _Duration: DWord);
begin
  if _Duration > 0 then begin
    FFrequency := _Frequency;
    FDuration := _Duration;
    FEvent.SetEvent;
  end;
end;

constructor TBeeper.Create;
begin
  FEvent := TEvent.Create();
  inherited Create(false);
end;

destructor TBeeper.Destroy;
begin
  FEvent.Free;
  inherited;
end;

procedure TBeeper.Execute;
var
  Duration: DWord;
begin
  inherited;
  while not Terminated and (FEvent.WaitFor(INFINITE) = wrSignaled) do begin
    FEvent.ResetEvent;
    if Terminated then
      exit;
    Duration := FDuration;
    FDuration := 0;
    if Duration > 0 then
      Windows.Beep(FFrequency, Duration);
  end;
end;

procedure TBeeper.Terminate;
begin
  inherited Terminate;
  FEvent.SetEvent;
end;

initialization
  Beeper := TBeeper.Create;
finalization
  if Assigned(Beeper) then begin
    Beeper.Terminate;
    FreeAndNil(Beeper);
  end;
end.

