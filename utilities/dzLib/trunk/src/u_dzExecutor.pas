{GXFormatter.config=twm}
{: Implements a wrapper object around the Win32 API CreateProcess and
   related functions.
   @author twm
}
unit u_dzExecutor;

{$I jedi.inc}

interface

uses
  Windows,
  SysUtils,
  Classes,
  u_dzFileStreams;

type
  {: ancestor to all exceptions raised in this unit }
  EExecutor = class(Exception);
  {: raised, when calling GetExitCode while Status is esInvalid }
  ENoProcess = class(EExecutor);
  {: raised, when calling GetStdOut or GetStdErr while Status is esRunning }
  EProcessRunning = class(EExecutor);
  {: raised, when trying to destroy the object while a process with I/O redirection
     is still running. }
  ERedirectedProcess = class(EProcessRunning);
  {: raised when calling TExecutor methods which do not work when the process
     has already terminated. }
  EProcessTerminated = class(EExecutor);
  {: raised when either StdOut or StdErr is accessed when RedirectStdXxx is not
     true }
  ENotRedirected = class(EExecutor);

type
  {: Status of the TExecutor object
     esInvalid = Process has not yet been started
     esRunning = Process is running
     esTerminated = Process has terminated }
  TExecutorStatus = (esInvalid, esRunning, esTerminated);
  TExecutorStatusSet = set of TExecutorStatus;

type
  {: Class wrapper around the Win32 functions for starting child processes.
     It allows to start a process, redirect stdIn/Out/Err, wait for it to finish,
     get the exit code and terminate it.
     Usage:<code>
       Executor := TExecutor.Create;
       Executor.WorkingDir := 's:\';
       Executor.Commandline := 'cvs -n -q update';
       Executor.Execute;
       Executor.Wait;
       MessageDlg(Format('StdOut: %s'#13#10+
                         'StdErr: %s', [Executor.StdOut, Executor.StdErr]),
                  mtInformation, [mbOK], 0);
       Executor.Free;</code>
  }
  TExecutor = class
  protected
    {: stores the ExeName property }
    fExeName: string;
    {: stores the Commandline property }
    fCommandline: string;
    {: stores the Environment property }
    FEnvironment: TStringList;
    {: stores the WorkingDir property }
    fWorkingDir: string;
    {: stores the RedirectStdOut property }
    fRedirectStdOut: boolean;
    {: stores the RedirectStdErr property }
    fRedirectStdErr: boolean;
    {: stores the StdIn property }
    fStdIn: AnsiString;
    {: stores the ExitCode property }
    fExitCode: DWORD;
    {: stores the Visible property }
    fVisible: boolean;
    {: temporary file stream to be used as standard input for the process }
    fInputFile: TdzTempFile;
    {: temporary file stream to be used as standard output for the process }
    fOutputFile: TdzTempFile;
    {: temporary file stream to be used as standard error for the process }
    fErrorFile: TdzTempFile;
    {: Process information for CreateProcess }
    fProcessInfo: TProcessInformation;
    {: stores the Status property }
    fStatus: TExecutorStatus;
    {: gets the process handle, raises an ENoProcess exception if status is esInvalid
       @returns the process handle value }
    function GetProcessHandle: THandle;
    {: gets the thread handle, raises an ENoProcess exception if status is esInvalid
       @returns the process main thread handle }
    function GetThreadHandle: THandle;
    {: gets the process exit code, raises an ENoProcess exception if status is esInvalid
       @returns the process exit code }
    function GetExitCode: DWORD;
    {: gets the process standard error, raises an ENoProcess exception if status is
       esInvalid and an EProcessRunning exception if status is esRunning
       @returns the process standard error }
    function GetStdErr: string;
    {: gets the process standard output, raises an ENoProcess exception if status is
       esInvalid and an EProcessRunning exception if status is esRunning
       @returns the process standard output }
    function GetStdOut: string;
    {: gets the Status property which is esInvalid when no process has been started,
       esRunning while the process is still running and esTerminated when the
       process has terminated. }
    function GetStatus: TExecutorStatus;
    {: checks whether the current status is in the set passed and raises one of
       ENoProcess, EProcessRunning, EProcessTerminated if not.
       This method is called by various other methods to assert that their
       functions are allowed in the current Status.
       @param ValidStatusSet is a set of valid Status values for the check. }
    procedure AssertStatus(_ValidStatusSet: TExecutorStatusSet; const _Method: string = '');
    {: Get method for property GetRedirectStdIn
       @returns true if StdIn is not an empty string }
    function GetRedirectStdIn: boolean;
  public
    {: Creates a TExecutor object }
    constructor Create;
    {: Destroys a TExecutor object. If the Ststus is esRunning and the process
       uses I/O redirection, this raises an ERedirectedProcess exception }
    destructor Destroy; override;
    {: Waits for the process to terminate. Raises ENoProcess if Status is
       esInvalid. If Status is esTerminated this funtion returns immediately.
       @param Timeout is a DWORD giving the time to wait in milliseconds
       @returns the result of the call to WaitForSingleObject }
    function Wait(_Timeout: DWORD = INFINITE): DWORD;
    {: Kills a running process, avoid using this since it may not free all
       resources.
       @returns true on success, false otherwise }
    function Kill: boolean;
    {: Searches for the executable given in the ExeName property using the
       system search path. If found the ExeName is set to the fully qualified
       name of the executable.
       @returns true if found, false otherwise }
    function FindExecutable(_ExeName: string = ''): boolean;
    {: Executes the program given either by ExeName or as the first parameter
       in Commandline.
       @returns the result of the call to CreateProcess }
    function Execute: boolean;
    {: This is a quick hack to allow freeing the object without waiting for the program
       to terminate }
    procedure ResetStatus;
    {: Fully qualified name of the executable to start. FindExecutable can be
       used if only the filename is known. Alternatively this can be left
       empty in which case the first parameter in CommanLine is used as
       executable name. }
    property ExeName: string read fExeName write fExeName;
    {: Commandline to pass to the process. If ExeName is empty the first
       parameter is used as executable name. }
    property Commandline: string read fCommandline write fCommandline;
    property Environment: TStringList read FEnvironment;
    {: determines whether the process is started visible or not. If set
       to true, starting a commandline program form a GUI app will open
       a console window. }
    property Visible: boolean read fVisible write fVisible;
    {: True if StdIn is redirected }
    property RedirectStdIn: boolean read GetRedirectStdIn;
    {: Set to true to redirect the process' standard output. This will result
       in having all text the process writes to its standard output handle
       copied to the StdOut property. }
    property RedirectStdOut: boolean read fRedirectStdOut write fRedirectStdOut;
    {: Set to true to redirect the process' standard output. This will result
       in having all text the process writes to its standard output handle
       copied to the StdOut property. }
    property RedirectStdErr: boolean read fRedirectStdErr write fRedirectStdErr;
    {: Set this to the standard input you want to supply to the process. }
    property StdIn: string read fStdIn write fStdIn;
    {: After the process has terminated this contains the standard output. When
       accessed before the process has terminated this will raise either a ENoProcess
       or EProcessRunning exception }
    property StdOut: string read GetStdOut;
    {: After the process has terminated this contains the standard error. When
       accessed before the process has terminated this will raise either a ENoProcess
       or EProcessRunning exception }
    property StdErr: string read GetStdErr;
    {: Contains the exit code of the process. If accessed while Status is esInvalid
       raises an ENoProcess exception. While the process is still running ExitCode
       is STILL_ACTIVE. }
    property ExitCode: DWORD read GetExitCode;
    {: Contains the handle of the process, raises an ENoProcess exception while
       status is esInvalid. }
    property ProcessHandle: THandle read GetProcessHandle;
    {: Contains the handle of the process' main thread,raises an ENoProcess exception while
       status is esInvalid. }
    property ThreadHandle: THandle read GetThreadHandle;
    {: Contains the status of a TExecutor which is esInvalid when no process
       has been started, esRunning while the process is still running and
       esTerminated when the process has terminated. }
    property Status: TExecutorStatus read GetStatus;
    {: Contains the working directory the process should be started in. If
       left empty the process will start in the current directory. }
    property WorkingDir: string read fWorkingDir write fWorkingDir;
  end;

implementation

uses
  JclSysInfo,
  u_dzMiscUtils;

{ TExecutor }

constructor TExecutor.Create;
begin
  inherited;
  fStatus := esInvalid;
  fRedirectStdOut := false;
  fRedirectStdErr := false;
  FEnvironment := TStringList.Create;
  FEnvironment.Sorted := false;
  GetEnvironmentVars(FEnvironment, False);
end;

destructor TExecutor.Destroy;
begin
  inherited;
  if (Status = esRunning) and
    (RedirectStdIn or RedirectStdOut or RedirectStdErr) then
    raise ERedirectedProcess.Create('Can not free Executor while a process' +
      ' using redirection is still running.');
  fInputFile.Free;
  fOutputFile.Free;
  fErrorFile.Free;
  //  Kernel objects, like the process and the files we created in this case,
  //  are maintained by a usage count.
  //  So, for cleaning up purposes we have to close the handles
  //  to inform the system that we don't need the objects anymore
  if fProcessInfo.hThread <> 0 then
    CloseHandle(fProcessInfo.hThread);
  if fProcessInfo.hProcess <> 0 then
    CloseHandle(fProcessInfo.hProcess);
end;

function TExecutor.FindExecutable(_ExeName: string): boolean;
var
  SearchPath: string;
  Found: string;
begin
  if _ExeName = '' then
    _ExeName := fExeName;
  Result := _ExeName <> '';
  if Result then begin
    SearchPath := GetEnvironmentVariable('path');
    Found := FileSearch(_ExeName, SearchPath);
    Result := Found <> '';
    if Result then
      fExeName := Found;
  end;
end;

procedure TExecutor.AssertStatus(_ValidStatusSet: TExecutorStatusSet; const _Method: string);
var
  Stat: TExecutorStatus;
begin
  Stat := GetStatus;
  if not (Stat in _ValidStatusSet) then
    case Stat of
      esInvalid: raise ENoProcess.CreateFmt('Process has not yet been started.'#13#10'%s', [_Method]);
      esRunning: raise EProcessRunning.CreateFmt('Process is still running.'#13#10'%s', [_Method]);
      esTerminated: raise EProcessTerminated.CreateFmt('Process has terminated.'#13#10'%s', [_Method]);
    end;
end;

function TExecutor.GetStatus: TExecutorStatus;
begin
  if fStatus = esRunning then begin
    Win32Check(GetExitCodeProcess(fProcessInfo.hProcess, fExitCode));
    if fExitCode = STILL_ACTIVE then
      fStatus := esRunning
    else
      fStatus := esTerminated;
  end;
  Result := fStatus;
end;

function TExecutor.GetExitCode: DWORD;
begin
  AssertStatus([esRunning, esTerminated]);
  Result := fExitCode;
end;

function TExecutor.Wait(_Timeout: DWORD): DWORD;
begin
  AssertStatus([esRunning, esTerminated]);
  Result := WaitforSingleObject(ProcessHandle, _Timeout);
end;

function TExecutor.Kill: boolean;
begin
  case GetStatus of
    esInvalid: raise ENoProcess.Create('Process has not yet been started');
    esRunning: Result := TerminateProcess(fProcessInfo.hProcess, $FFFFFFFF);
  else
    Result := true;
  end;
end;

function TExecutor.GetProcessHandle: THandle;
begin
  AssertStatus([esRunning, esTerminated]);
  Result := fProcessInfo.hProcess;
end;

function TExecutor.GetThreadHandle: THandle;
begin
  AssertStatus([esRunning, esTerminated]);
  Result := fProcessInfo.hThread;
end;

function TExecutor.GetRedirectStdIn: boolean;
begin
  Result := fStdIn <> '';
end;

function TExecutor.Execute: boolean;
var
  StartupInfo: TStartupInfo;
  SecurityAttributes: TSecurityAttributes;
  Cmdline: string;
  env: string;
  i: Integer;
begin
  // prepare SecurityAttribute, set InheritHandle to true
  FillChar(SecurityAttributes, SizeOf(SecurityAttributes), #0);
  SecurityAttributes.nLength := SizeOf(SecurityAttributes);
  SecurityAttributes.lpSecurityDescriptor := nil;
  SecurityAttributes.bInheritHandle := true;

  // prepare StartupInfo structure
  FillChar(StartupInfo, SizeOf(StartupInfo), #0);
  StartupInfo.cb := SizeOf(StartupInfo);
  StartupInfo.hStdOutput := 0;
  StartupInfo.hStdInput := 0;
  StartupInfo.hStdError := 0;
  StartupInfo.dwFlags := 0;

  if RedirectStdIn then begin
    fInputFile := TdzTempFile.Create;
    try
      fInputFile.SecurityAttributes := @SecurityAttributes;
      fInputFile.Open;
      fInputFile.Write(fStdIn[1], Length(fStdIn));
      fInputFile.Seek(0, soFromBeginning);
      StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESTDHANDLES;
      StartupInfo.hStdInput := fInputFile.Handle;
    except
      fInputFile.Free;
      fInputFile := nil;
      raise;
    end;
  end;

  if RedirectStdOut then begin
    fOutputFile := TdzTempFile.Create;
    fOutputFile.SecurityAttributes := @SecurityAttributes;
    fOutputFile.Open;
    StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESTDHANDLES;
    StartupInfo.hStdOutput := fOutputFile.Handle;
  end;

  if RedirectStdErr then begin
    fErrorFile := TdzTempFile.Create;
    fErrorFile.SecurityAttributes := @SecurityAttributes;
    fErrorFile.Open;
    StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESTDHANDLES;
    StartupInfo.hStdError := fErrorFile.Handle;
  end;

  if not fVisible then begin
    StartupInfo.dwFlags := StartupInfo.dwFlags or STARTF_USESHOWWINDOW;
    StartupInfo.wShowWindow := SW_HIDE;
  end;

  if fExeName <> '' then
    CmdLine := Format('"%s" %s', [fExeName, fCommandLine])
  else
    CmdLine := fCommandline;

  env := '';
  for i := 0 to Environment.Count - 1 do begin
    if Env <> '' then
      Env := Env + #0;
    Env := Env + Environment[i];
  end;
  Env := Env + #0#0;

  // start the program, ExeName and Commandline are casted to pointers rather
  // than PChar because we want to pass a nil pointer if the string is empty.
  Result := CreateProcess(
    pointer(fExeName), // pointer to the executable (or nil)
    pointer(CmdLine), // pointer to command line string (or nil)
    nil, // pointer to process security attributes
    nil, // pointer to thread security attributes
    true, // handle inheritance flag
    NORMAL_PRIORITY_CLASS, // creation flags
    PChar(Env), // pointer to new environment block
    pointer(fWorkingDir), // pointer to current directory name
    StartupInfo, // pointer to STARTUPINFO
    fProcessInfo); // pointer to PROCESS_INF

  if Result then
    fStatus := esRunning
  else
    RaiseLastOsErrorEx('%1:s (%0:d) in CreateProcess("' + fExeName + '","' + CmdLine + '")');
end; // TExecutor.Execute

procedure TExecutor.ResetStatus;
begin
  fStatus := esTerminated;
end;

function TExecutor.GetStdErr: string;
var
  Size: integer;
begin
  AssertStatus([esTerminated]);
  if RedirectStdErr then begin
    fErrorFile.Seek(0, soFromBeginning);
    Size := fErrorFile.Size;
    if Size <> 0 then begin
      SetLength(Result, Size);
      fErrorFile.Read(Result[1], Size);
    end;
  end else
    raise ENotRedirected.Create('StdErr was not redirected');
end;

function TExecutor.GetStdOut: string;
var
  Size: integer;
begin
  AssertStatus([esTerminated]);
  if RedirectStdOut then begin
    fOutputFile.Seek(0, soFromBeginning);
    Size := fOutputFile.Size;
    if Size <> 0 then begin
      SetLength(Result, Size);
      fOutputFile.Read(Result[1], Size);
    end;
  end else
    raise ENotRedirected.Create('StdOut was not redirected');
end;

end.

