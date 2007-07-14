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
  protected
    {: stores the exit code }
    FExitCode: integer;
    {: Created in InitCmdLineParser, allows reading the parameters }
    FGetOpt: TGetOpt;
    {: Initializes FCmdLineParser with a Parser, should be overridden }
    procedure InitCmdLineParser; virtual;
    {: Zeigt eine vom FCmdLineParser generierte Kurzhilfe zum Programm an und
       beendet das Programm durch Aufruf von SysUtils.Abort.
       @param Error ist eine optionale Fehlermeldung, wenn nicht leer, wird sie ausgegeben }
    procedure Usage(const _Error: string = ''); virtual;
    {: Ruft die Execute-Methode des Commandline-Parsers auf, danach stehen die
       Parameter zur Verfuegung. Falls beim Parsen ein Fehler auftritt, so wird
       automatisch Usage mit der entsprechenden Fehlermeldung aufgerufen. }
    procedure ParseCmdLine; virtual;
    {: Diese Methode sollte ueberschrieben werden und die eigentliche Programmfunktion
       ausfuehren. Sie wird nach dem Parsen der Commandline aufgerufen, so dass
       zu diesem Zeitpunkt sichergestellt ist, dass die Parameter OK sind.
       @returns den Exit-Code fuer das Programm }
    function doExecute: integer; virtual;
  public
    {: Erzeugt eine TDefaultMain-Instanz, setzt den Exit-Code auf 1 (=Fehler)
       und initialisiert das Logging. }
    constructor Create; virtual;
    {: Gibt eine TDefaultMain-Instanz frei und schreibt das
       Programmende ins Log. }
    destructor Destroy; override;
    {: Diese Methode sollte aufgerufen werden. Sie initialisiert den Commandline-Parser,
       fuehrt ihn aus und ruft dann doExecute innerhalb eines Try-Except-Blocks auf
       Falls in doExecute eine Exception geraised wird, so wird sie hier abgefangen,
       eine Fehlermeldung auf die Konsole ausgegeben und der Exit-Code auf 1 (=Fehler) gesetzt
       Falls doExecute ganz normal beendet wird, so wird dessen Ergebnis als
       Exit-Code verwendet.
       @returns den Exit-Code des Programms }
    function Execute: integer;
  end;

type
  {: Class-Type fuer die globale Variable MainClass }
  TMainClass = class of TDefaultMain;

var
  {: Globale Variable, die auf die Main-Klasse zeigt. Diese
     Variable wird innerhalb der Funktion @link(Main) verwendet,
     um eine Klasse zu erzeugen und auszufuehren }
  MainClass: TMainClass = TDefaultMain;

  {: Erzeugt eine Instanz von MainClass, ruft deren Execute-Methode auf und liefert
     den Exit-Code zurueck
     @returns den Exit-Code des Programms }
function Main: integer;

implementation

uses
  Windows,
  SysUtils,
  Classes,
  Dialogs,
  u_dzLogging;

constructor TDefaultMain.Create;
begin
  inherited;
  // default: Fehler
  FExitCode := 1;
end;

destructor TDefaultMain.Destroy;
begin
  FreeAndNil(FGetOpt);
  LogInfo('Program finished.');
  inherited;
end;

function TDefaultMain.doExecute: integer;
begin
  MessageDlg('This program does nothing yet.', mtInformation, [mbOK], 0);
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
  FGetOpt := TGetOpt.Create();
  FGetOpt.RegisterHelpOptions;
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
    on e: Exception do begin
      s := 'Exception ' + e.ClassName + ': ' + e.Message;
      LogError(s);
      if IsConsole then
        WriteLn(s);
      FExitCode := 1;
    end;
  end;
  Result := FExitCode;
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
      MainObj.Free;
    end;
  except
    // Exception handling findet im MainObj statt, was bis hierher kommt,
    // kann man eh nicht mehr behandeln - aber evt. Loggen ? AS
    on e: Exception do begin
      LogError('Exception in function Main: ' + e.Message + ' ' + e.ClassName);
    end;
  end;
end;

end.

