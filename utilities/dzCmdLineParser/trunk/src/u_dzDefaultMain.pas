{GXFormatter.config=twm}
unit u_dzDefaultMain;

{$I jedi.inc}

interface

uses
  u_dzTranslator,
  u_dzGetOpt;

type
  {: An "application" class for console programs.
     To use this class, assign a descendant of TDefaultMain to the MainClass variable and
     call Main in the project's project file like this:
     @longcode(
     begin
       Application.Initialize;
       Application.Title := '<your application's title here>';
       MainClass := <your TDefaultMain descendant class>;
       System.ExitCode := Main;
     end.
     )
     Your descendant should override at least the following methods
     * InitCmdLineParser
     * doExecute }
  TDefaultMain = class
  private
    function GetProgName: string;
    function GetExeName: string;
  protected
    {: stores the exit code }
    FExitCode: integer;
    {: Created in InitCmdLineParser, allows reading the parameters }
    FGetOpt: TGetOpt;
    {: Initializes FGetOptr with a Parser, should be overridden, but make
       sure you call inherited if you want to have the --help option }
    procedure InitCmdLineParser; virtual;
    {: Shows a short help generated by the TGetOpt and exits the program by calling
       SysUtils.Abort.
       @param Error is an optional error message, which is displayed if not empty }
    procedure Usage(const _Error: string = ''); virtual;
    {: Calls the Parse method of the commandline parser. Afterwards the parameters
       and options can be accessed using the perser's methods and properties.
       If there is an error during parsing it automatically calls Usage with
       the error message. If a help option (-?, -h or -Help) is found, usage
       will be called without an error message. }
    procedure ParseCmdLine; virtual;
    {: This method should be overridden to implement the actual program functionality.
       It is called after the commandline has ben parsed so you can be sure that
       the parameters are OK.
       @returns the exit code for the program }
    function doExecute: integer; virtual;
  public
    {: Creates a TDefaultMain instance and sets the exit code to 1 (=error)
       also, does a very simple parsing of the commandline to determine whether
       the --StartupLog option was given and initializes logging, if it was }
    constructor Create; virtual;
    {: Frees a TDefaultMain instance and writes the progrem end into the log }
    destructor Destroy; override;
    {: Call this method after the instance of TDefaultMain has been created.
       It initializes the commandline parser, executes it on the given
       commandline and calls doExecute within a try..except block.
       If doExecute raises an exception it will write an error message to the
       console or show an error dialog and set the exit code to 1 (=error).
       If doExecute exits normally it uses the result as exit code.
       @returns the exit code for the program }
    function Execute: integer;
    ///<summary> Returns the programm name that was used to start this application
    property ProgName: string read GetProgName;
    property ExeName: string read GetExeName;
  end;

type
  {: Class-Type for the global variable MainClass }
  TMainClass = class of TDefaultMain;

var
  {: global variable that points to the main class. This variable is used in
     @link(Main) to create a TDefaultMain descendant and exeute it. }
  MainClass: TMainClass = TDefaultMain;

  {: Creates an instance of MainClass, calls its execute method and returns
     the exit code.
     @returns the exit code for the program }
function Main: integer;

implementation

uses
  Windows,
  SysUtils,
  Classes,
  Dialogs,
  u_dzLogging,
  w_dzDialog; // libs\dzlib\forms

function _(const _s: string): string; inline;
begin
  Result := dzDGetText(_s, 'dzCmdLineParser');
end;

constructor TDefaultMain.Create;
const
  STARTUP_LOG = '--startuplog=';
var
  s: string;
  p: integer;
begin
  inherited;
  // default: error
  FExitCode := 1;

  s := GetCommandLine;
  s := LowerCase(s);
  p := Pos(STARTUP_LOG, s);
  if p > 0 then begin
    s := Copy(s, p + Length(STARTUP_LOG));
    p := Pos(' ', s);
    if p > 0 then
      s := Copy(s, 1, p - 1);
    SetGlobalLogger(TFileLogger.Create(s));
  end;
  FGetOpt := TGetOpt.Create();
end;

destructor TDefaultMain.Destroy;
begin
  FreeAndNil(FGetOpt);
  LogInfo('Program finished.');
  inherited;
end;

function TDefaultMain.doExecute: integer;
begin
  MessageDlg(_('This program does nothing yet.'), mtInformation, [mbOK], 0);
  Result := 0;
end;

procedure TDefaultMain.Usage(const _Error: string = '');
var
  s: string;
begin
  if _Error <> '' then begin
    s := Format(_('Error: %s') + #13#10#13#10, [_Error]);
    LogError(_Error);
  end;
  MessageDlg(s + Format(
    _('Synopsis: %s %s') + #13#10#13#10 +
    _('Parameters:') + #13#10 +
    '%s'#13#10#13#10 +
    _('Options:') + #13#10 +
    '%s',
    [FGetOpt.ProgName, FGetOpt.GetCmdLineDesc, FGetOpt.GetParamHelp, FGetOpt.GetOptionHelp]),
    mtError, [mbOK], 0);
  FExitCode := 1;
  SysUtils.Abort;
end;

procedure TDefaultMain.InitCmdLineParser;
begin
  FGetOpt.RegisterHelpOptions;
  FGetOpt.RegisterOption('StartupLog', _('Write a startup log to the given file.'), true);
end;

procedure TDefaultMain.ParseCmdLine;
begin
  LogDebug('Cmdline: ' + System.CmdLine);
  try
    FGetOpt.Parse;
  except
    on e: exception do
      Usage(e.Message);
  end;
  if FGetOpt.HelpOptionFound then
    Usage;
end;

function TDefaultMain.Execute: integer;
var
  s: string;
begin
  InitCmdLineParser;
  ParseCmdLine;
  try
    FExitCode := DoExecute;
  except
    on e: EAbort do begin
      // we do not want to show an error if the code called Abort because this
      // is supposed to be a silent exception. So we log it and terminate with
      // an exit code of 1
      LogError(e.Message + '(' + e.ClassName + ')');
      FExitCode := 1;
    end;
    on e: Exception do begin
      s := 'Exception: ' + e.Message + ' (' + e.ClassName + ')';
      LogError(s);
      if IsConsole then
        WriteLn(s)
      else
        Tf_dzDialog.ShowException(e);
      FExitCode := 1;
    end;
  end;
  Result := FExitCode;
end;

function TDefaultMain.GetExeName: string;
var
  ModuleName: AnsiString;
begin
  SetLength(ModuleName, 255);
  GetModuleFileNameA(MainInstance, PAnsiChar(ModuleName), Length(ModuleName));
  OemToAnsi(PAnsiChar(ModuleName), PAnsiChar(ModuleName));
  ModuleName := PAnsiChar(ModuleName);
  Result := string(ModuleName);
end;

function TDefaultMain.GetProgName: string;
begin
  Result := FGetOpt.ProgName;
end;

function Main: integer;
var
  MainObj: TDefaultMain;
begin
  Result := 1;
  try
    MainObj := MainClass.Create;
    try
      Result := MainObj.Execute;
    finally
      FreeAndNil(MainObj);
    end;
  except
    // Exception handling is done within the MainObj. If something gets here
    // it cannot be handled anyway.
    // - but maybe log it? -- AS
    on e: Exception do begin
      LogError('Exception in function Main: ' + e.Message + ' ' + e.ClassName);
    end;
  end;
  if (DebugHook <> 0) and IsConsole then begin
    Write('Exit Code: ', Result, ' -- press Enter');
    Readln;
  end;
end;

end.

