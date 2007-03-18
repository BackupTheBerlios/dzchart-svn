{GXFormatter.config=twm}
{: Implements a logging interface which can be used in a procedural or OO way }
unit u_dzLogging;

{$I jedi.inc}

interface

type
  {: supported log levels }
  TLogLevel = (llError, llWarning, llInfo, llDebug, llTrace, llDump);

type
  {: Logger-Interface, must be implemented by all classes that are used for logging }
  ILogger = interface
    {: Logs a line
       @param s is the string to log
       @param Level is the log level, defaults to llDump }
    procedure LogLine(const _s: string; _Level: TLogLevel = llDump);
    {: Logs a string at error level }
    procedure LogError(const _s: string);
    {: Logs a string at warning level }
    procedure LogWarning(const _s: string);
    {: Logs a string at info level }
    procedure LogInfo(const _s: string);
    {: Logs a string at debug level }
    procedure LogDebug(const _s: string);
    {: Logs a string at trace level }
    procedure LogTrace(const _s: string);
    {: Logs a string at dump level }
    procedure LogDump(const _s: string);
    {: Logs the entry into a procedure }
    procedure LogProcEnter(const _Proc: string = '');
    {: Logs the exit from a procedure }
    procedure LogProcExit(const _Proc: string = '');
  end;

type
  {: An abstract ancestor that can be used as an ancestor for classes implementing
     the ILogger interface. Descendants then only need to override LogLine }
  TAbstractLogger = class(TInterfacedObject)
  protected // implements ILogger
    {: Logs a line
       @param s is the string to log
       @param Level is the log level, defaults to llDump }
    procedure LogLine(const _s: string; _Level: TLogLevel = llDump); virtual; abstract;
    {: Logs a string at error level }
    procedure LogError(const _s: string);
    {: Logs a string at warning level }
    procedure LogWarning(const _s: string);
    {: Logs a string at info level }
    procedure LogInfo(const _s: string);
    {: Logs a string at debug level }
    procedure LogDebug(const _s: string);
    {: Logs a string at trace level }
    procedure LogTrace(const _s: string);
    {: Logs a string at dump level }
    procedure LogDump(const _s: string);
    {: Logs the entry into a procedure }
    procedure LogProcEnter(const _Proc: string = '');
    {: Logs the exit from a procedure }
    procedure LogProcExit(const _Proc: string = '');
  end;

type
  {: used for the OnLogLine event the TEventLogger class }
  TOnLogLine = procedure(const _s: string; _Level: TLogLevel) of object;
  {: used for the OnLogLineNoLevel event the TEventLogger class }
  TOnLogLineNoLevel = procedure(const _s: string) of object;

type
  {: Implements the ILogger interface by sending all logging calls to
     an event handler }
  TEventLogger = class(TAbstractLogger, ILogger)
  private
    {: Normal OnLogLine event, called from the LogLine method }
    FOnLogLine: TOnLogLine;
    {: simplified OnLogLine event, ignores the level }
    FOnLogLineNoLevel: TOnLogLineNoLevel;
    {: used internally if the constructor is called with a TOnLogLineNoLevel event
       to redirect logging to this event }
    procedure LogLineNoLevel(const _s: string; _Level: TLogLevel);
    {: used internally when Disconnect is called }
    procedure LogLineIgnore(const _s: string; _Level: TLogLevel);
  protected
    {: calls the FOnLogLine event }
    procedure LogLine(const _s: string; _Level: TLogLevel = llDump); override;
  public
    {: Default constructor, creates a TEventLogger instance and sets the
       FOnLogLine field }
    constructor Create(_OnLogLine: TOnLogLine); overload;
    {: simplified constructor, creates a TEventLogger instance, sets the
       FOnLogLineNoLevel field and sets the FOnLogLine field to the LogLineNoLevel
       method }
    constructor Create(_OnLogLine: TOnLogLineNoLevel); overload;
    {: disconnects the event by changing the FOnLogLine field to the LogLineIgnore method }
    procedure Disconnect;
  end;

type
  {: Dummy implementation for the ILogger interface that ignores everything written to it
     An instance of this class is automatically created and assigned to the gblLogger
     variable. }
  TNoLogging = class(TAbstractLogger, ILogger)
  protected
    {: does nothing }
    procedure LogLine(const _s: string; _Level: TLogLevel = llDump); override;
  end;

var
  {: Global ILogger interface, automtically initialized to a TNoLogging class }
  gblLogger: ILogger = nil;

{: procedureal interface for calling the gblLogger.LogLine method }
procedure LogLine(const _s: string; _Level: TLogLevel = llDump);
{: procedureal interface for calling the gblLogger.LogError method }
procedure LogError(const _s: string);
{: procedureal interface for calling the gblLogger.LogWarning method }
procedure LogWarning(const _s: string);
{: procedureal interface for calling the gblLogger.LogInfo method }
procedure LogInfo(const _s: string);
{: procedureal interface for calling the gblLogger.LogDebug method }
procedure LogDebug(const _s: string);
{: procedureal interface for calling the gblLogger.LogTrace method }
procedure LogTrace(const _s: string);
{: procedureal interface for calling the gblLogger.LogDump method }
procedure LogDump(const _s: string);
{: procedureal interface for calling the gblLogger.LogProcEnter method }
procedure LogProcEnter(const _Proc: string = '');
{: procedureal interface for calling the gblLogger.LogProcExit method }
procedure LogProcExit(const _Proc: string = '');

implementation

procedure LogLine(const _s: string; _Level: TLogLevel = llDump);
begin
  gblLogger.LogLine(_s, _Level);
end;

procedure LogError(const _s: string);
begin
  gblLogger.LogError(_s);
end;

procedure LogWarning(const _s: string);
begin
  gblLogger.LogWarning(_s);
end;

procedure LogInfo(const _s: string);
begin
  gblLogger.LogInfo(_s);
end;

procedure LogDebug(const _s: string);
begin
  gblLogger.LogDebug(_s);
end;

procedure LogTrace(const _s: string);
begin
  gblLogger.LogTrace(_s);
end;

procedure LogDump(const _s: string);
begin
  gblLogger.LogDump(_s);
end;

procedure LogProcEnter(const _Proc: string = '');
begin
  gblLogger.LogProcEnter(_Proc);
end;

procedure LogProcExit(const _Proc: string = '');
begin
  gblLogger.LogProcExit(_Proc);
end;

{ TAbstractLogger }

procedure TAbstractLogger.LogDebug(const _s: string);
begin
  LogLine(_s, llDebug);
end;

procedure TAbstractLogger.LogDump(const _s: string);
begin
  LogLine(_s, llDump);
end;

procedure TAbstractLogger.LogError(const _s: string);
begin
  LogLine(_s, llError);
end;

procedure TAbstractLogger.LogInfo(const _s: string);
begin
  LogLine(_s, llInfo);
end;

procedure TAbstractLogger.LogProcEnter(const _Proc: string);
begin
  LogLine('>>' + _Proc, llTrace);
end;

procedure TAbstractLogger.LogProcExit(const _Proc: string);
begin
  LogLine('<<' + _Proc, llTrace);
end;

procedure TAbstractLogger.LogTrace(const _s: string);
begin
  LogLine(_s, llTrace);
end;

procedure TAbstractLogger.LogWarning(const _s: string);
begin
  LogLine(_s, llWarning);
end;

{ TEventLogger }

constructor TEventLogger.Create(_OnLogLine: TOnLogLine);
begin
  inherited Create;
  FOnLogLine := _OnLogLine;
end;

constructor TEventLogger.Create(_OnLogLine: TOnLogLineNoLevel);
begin
  inherited Create;
  FOnLogLineNoLevel := _OnLogLine;
  FOnLogLine := LogLineNoLevel;
end;

procedure TEventLogger.Disconnect;
begin
  FOnLogLine := LogLineIgnore;
end;

procedure TEventLogger.LogLine(const _s: string; _Level: TLogLevel);
begin
  FOnLogLine(_s, _Level);
end;

procedure TEventLogger.LogLineIgnore(const _s: string; _Level: TLogLevel);
begin
  // ignore
end;

procedure TEventLogger.LogLineNoLevel(const _s: string; _Level: TLogLevel);
begin
  FOnLogLineNoLevel(_s);
end;

{ TNoLogging }

procedure TNoLogging.LogLine(const _s: string; _Level: TLogLevel);
begin
  // do nothing
end;

initialization
  gblLogger := TNoLogging.Create;
end.

