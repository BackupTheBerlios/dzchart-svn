unit u_dzCmdLineParser;

interface

uses
  SysUtils,
  Classes;

type
  EStateEngineError = class(exception);

const
  ALPHANUMERIC_CHARS = ['a'..'z', 'A'..'Z', '0'..'9'];
  ALLCHARS_BUT_NULL = [#1..#255];

type
  IEngineContext = interface ['{F6FB6D03-C90F-468D-9ACC-716C58697CCA}']
    {: returns the next character to parse }
    function GetNextChar: char;
    {: appends a character to the FOption field }
    procedure AddToOption(_c: char);
    {: appends a character to the FParameter field }
    procedure AddToParameter(_c: char);
    {: This is called whenever an option and parameter have been finished.
       It checks what kind of parameter it was, possibly combines them and
       adds it to the appropriate list. }
    procedure HandleCmdLinePart;
  end;

type
  IEngineState = interface ['{B8ADC607-8549-4B4A-A15A-278DAEE5F6CE}']
    function Execute(const _Context: IEngineContext): IEngineState;
    function GetClassName: string;
  end;

type
  TEngineStateAbstract = class(TInterfacedObject)
  private
    function GetClassName: string;
  end;

type
  TEngineStateError = class(TEngineStateAbstract, IEngineState)
  private
    FError: string;
    function Execute(const _Context: IEngineContext): IEngineState;
  public
    constructor Create(const _Error: string);
  end;

type
  TEngineStateSpace = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateDash = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateDoubleDash = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateLongOption = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateShortOption = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateShortSwitch = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateShortParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateQuotedShortParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateLongParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateQuotedLongParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TEngineStateQuotedParam = class(TEngineStateAbstract, IEngineState)
  private
    function Execute(const _Context: IEngineContext): IEngineState;
  public
  end;

type
  TCmdLineParser = class
  public
    {: parses the CmdLine string and returns the options and paramteters }
    class procedure Execute(const _CmdLine: string; _Options: TStrings; _Params: TStrings); overload;
    {: parses the application's commandline and returns the options and paramteters }
    class procedure Execute(_Options: TStrings; _Params: TStrings); overload;
  end;

implementation

type
  IEngineContextEx = interface ['{CD19DB13-F344-4E1A-B97F-D235B445B463}']
    procedure GetOptions(_Options: TStrings);
    procedure GetParams(_Params: TStrings);
  end;

type
  {: stores the engine context, passed to the execute method of all engine states }
  TEngineContext = class(TInterfacedObject, IEngineContext, IEngineContextEx)
  protected
    FReadIdx: integer;
    FInput: string;
    {: buffer for the currently handled option }
    FOption: string;
    {: buffer for the currently handled parameter }
    FParameter: string;
    {: Stores options as <optionname>[=<value] in the order they appeared on the
       commandline., note that options can appear multiple times, so using
       FParameter.Values[<name>] might not be appropriate }
    FOptions: TStringList;
    {: stores the Params property }
    FParams: TStringList;
  protected // implements IEngineContext
    {: returns the next character to parse }
    function GetNextChar: char;
    {: appends a character to the FOption field }
    procedure AddToOption(_c: char);
    {: appends a character to the FParameter field }
    procedure AddToParameter(_c: char);
    {: This is called whenever an option and parameter have been finished.
       It checks what kind of parameter it was, possibly combines them and
       adds it to the appropriate list. }
    procedure HandleCmdLinePart;
  protected // implements IEngineContextEx
    procedure GetOptions(_Options: TStrings);
    procedure GetParams(_Params: TStrings);
  public
    constructor Create(_Input: string);
    destructor Destroy; override;
    {: Stores options as <optionname>[=<value] in the order they appeared on the
       commandline., note that options can appear multiple times, so using
       FParameter.Values[<name>] might not be appropriate }
    property Options: TStringList read FOptions;
    {: Stores the parameters, ordered as they appear on the commandline }
    property Params: TStringList read FParams;
  end;

{ TEngineStateAbstract }

function TEngineStateAbstract.GetClassName: string;
begin
  Result := ClassName;
end;

{ TEngineStateError }

constructor TEngineStateError.Create(const _Error: string);
begin
  inherited Create;
  FError := _Error;
end;

function TEngineStateError.Execute(const _Context: IEngineContext): IEngineState;
begin
  raise EStateEngineError.Create(FError);
end;

{ TEngineStateSpace }

function TEngineStateSpace.Execute(const _Context: IEngineContext): IEngineState;
var
  c: char;
begin
  c := _Context.GetNextChar;
  case c of
    '-':
      Result := TEngineStateDash.Create;
    #0:
      Result := nil; // end state
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedParam.Create;
      end;
    ' ':
      Result := self;
  else
    _Context.AddToParameter(c);
    Result := TEngineStateParam.Create;
  end;
end;

{ TEngineStateParam }

function TEngineStateParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedParam.Create;
      end;
    #0, ' ': begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end;
  else
    _Context.AddToParameter(c);
    Result := Self;
  end;
end;

{ TEngineStateQuotedParam }

function TEngineStateQuotedParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateParam.Create;
      end;
    #0:
      Result := TEngineStateError.Create(Format('Invalid character "%s".', [c]));
  else
    _Context.AddToParameter(c);
    Result := self;
  end;
end;

{ TEngineStateDash }

function TEngineStateDash.Execute(const _Context: IEngineContext): IEngineState;
var
  c: char;
begin
  c := _Context.GetNextChar;
  if c in ALPHANUMERIC_CHARS then begin
    _Context.AddToOption(c);
    Result := TEngineStateShortOption.Create;
  end else if c = '-' then
    Result := TEngineStateDoubleDash.Create
  else
    Result := TEngineStateError.Create(Format('Invalid character "%s".', [c]));
end;

{ TEngineStateDoubleDash }

function TEngineStateDoubleDash.Execute(const _Context: IEngineContext): IEngineState;
var
  c: char;
begin
  c := _Context.GetNextChar;
  if c in ALPHANUMERIC_CHARS then begin
    _Context.AddToOption(c);
    Result := TEngineStateLongOption.Create;
  end else
    Result := TEngineStateError.Create(Format('Invalid character "%s".', [c]));
end;

{ TEngineStateShortOption }

function TEngineStateShortOption.Execute(const _Context: IEngineContext): IEngineState;
var
  c: char;
begin
  c := _Context.GetNextChar;
  case c of
    ' ': begin
        Result := TEngineStateShortParam.Create;
      end;
    '-', '+': begin
        _Context.AddToParameter(c);
        Result := TEngineStateShortSwitch.Create;
      end;
    #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end;
  else
    Result := TEngineStateError.Create(Format('Invalid character "%s".', [c]));
  end;
end;

{ TEngineStateShortSwitch }

function TEngineStateShortSwitch.Execute(const _Context: IEngineContext): IEngineState;
var
  c: char;
begin
  c := _Context.GetNextChar;
  case c of
    ' ', #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end else
    Result := TEngineStateError.Create(Format('Invalid character "%s".', [c]));
  end;
end;

{ TEngineStateShortParam }

function TEngineStateShortParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: char;
begin
  c := _Context.GetNextChar;
  case c of
    ' ', #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end;
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedShortParam.Create;
      end;
    '-': begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateDash.Create;
      end;
  else
    _Context.AddToParameter(c);
    Result := self;
  end;
end;

{ TEngineStateQuotedShortParam }

function TEngineStateQuotedShortParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateShortParam.Create;
      end;
    #0:
      Result := TEngineStateError.Create(Format('Invalid character "%s".', [c]));
  else
    _Context.AddToParameter(c);
    Result := self;
  end;
end;

{ TEngineStateLongOption }

function TEngineStateLongOption.Execute(const _Context: IEngineContext): IEngineState;
var
  c: char;
begin
  c := _Context.GetNextChar;
  case c of
    '=':
      Result := TEngineStateLongParam.Create;
    ' ', #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end;
    '"', '''':
      Result := TEngineStateError.Create(Format('Invalid character "%s".', [c]));
  else
    _Context.AddToOption(c);
    Result := TEngineStateLongOption.Create;
  end;
end;

{ TEngineStateLongParam }

function TEngineStateLongParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateQuotedLongParam.Create;
      end;
    ' ', #0: begin
        _Context.HandleCmdLinePart;
        Result := TEngineStateSpace.Create;
      end;
  else
    _Context.AddToParameter(c);
    Result := TEngineStateLongParam.Create;
  end;
end;

{ TEngineStateQuotedLongParam }

function TEngineStateQuotedLongParam.Execute(const _Context: IEngineContext): IEngineState;
var
  c: char;
begin
  c := _Context.GetNextChar;
  case c of
    '"': begin
        _Context.AddToParameter(c);
        Result := TEngineStateLongParam.Create;
      end;
    #0:
      Result := TEngineStateError.Create(Format('Invalid character "%s".', [c]));
  else
    _Context.AddToParameter(c);
    Result := TEngineStateQuotedLongParam.Create;
  end;
end;

{ TStateParams }

procedure TEngineContext.AddToOption(_c: char);
begin
  FOption := FOption + _c;
end;

procedure TEngineContext.AddToParameter(_c: char);
begin
  FParameter := FParameter + _c;
end;

constructor TEngineContext.Create(_Input: string);
begin
  inherited Create;
  FOptions := TStringList.Create;
  FParams := TStringList.Create;
  FInput := _Input;
  FReadIdx := 0;
end;

destructor TEngineContext.Destroy;
begin
  FParams.Free;
  FOptions.Free;
  inherited;
end;

function TEngineContext.GetNextChar: char;
begin
  if FReadIdx >= Length(FInput) then
    Result := #0
  else begin
    Inc(FReadIdx);
    Result := FInput[FReadIdx];
  end;
end;

procedure TEngineContext.GetOptions(_Options: TStrings);
begin
  _Options.Assign(FOptions);
end;

procedure TEngineContext.GetParams(_Params: TStrings);
begin
  _Params.Assign(FParams);
end;

procedure TEngineContext.HandleCmdLinePart;
begin
  if FOption <> '' then
    FOptions.Add(FOption + '=' + FParameter)
  else
    FParams.Add(FParameter);
  FParameter := '';
  FOption := '';
end;

{ TCdmLineParser }

class procedure TCmdLineParser.Execute(const _CmdLine: string; _Options, _Params: TStrings);
var
  Context: IEngineContext;
  ContextEx: IEngineContextEx;
  State: IEngineState;
begin
  Context := TEngineContext.Create(_CmdLine);
  State := TEngineStateSpace.Create;
  while State <> nil do
    State := State.Execute(Context);
  ContextEx := Context as IEngineContextEx;
  ContextEx.GetOptions(_Options);
  ContextEx.GetParams(_Params);
end;

class procedure TCmdLineParser.Execute(_Options, _Params: TStrings);
begin
  Execute(CmdLine, _Options, _Params);
end;

end.

