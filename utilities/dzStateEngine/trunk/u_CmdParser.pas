unit u_CmdParser;

interface

uses
  SysUtils,
  u_dzStateEngine,
  u_dzStateEngineActions,
  u_dzStateEngineStates,
  u_dzStateEngineTransitions,
  u_CharSortedListOfActionLists,
  u_CmdOptionList;

type
  EParsingError = class(Exception)
  private
    FErrorChar: char;
  public
    constructor Create(_ErrorChar: char);
    property ErrorChar: char read FErrorChar;
  end;

type
  EParsingErrorAt = class(EParsingError)
  private
    FErrorPosition: integer;
  public
    constructor Create(_ErrorPosition: integer; _ErrorChar: char = #0);
    property ErrorPosition: integer read FErrorPosition;
  end;

type
  TCmdParser = class
  private
    FEngine: TStateEngine;
    // Engine states:
    FStateSpace: TStateEngineState;
    FStateQuotedParam: TStateEngineState;
    FStateParam: TStateEngineState;
    FStateDash: TStateEngineState;
    FStateShortOption: TStateEngineState;
    FStateShortParam: TStateEngineState;
    FStateQuotedShortParam: TStateEngineState;
    FStateDoubleDash: TStateEngineState;
    FStateLongOption: TStateEngineState;
    FStateLongParam: TStateEngineState;
    FStateQuotedLongParam: TStateEngineState;
    // Engine actions:
    FActionSpace2Space: TStateEngineAction;
    FActionSpace2Dash: TStateEngineAction;
    FActionDash2DoubleDash: TStateEngineAction;
    FActionDash2ShortOption: TStateEngineAction;
    FActionShortOption2ShortParam: TStateEngineAction;
    FActionShortParam2ShortParam: TStateEngineAction;
    FActionShortParam2QuotedShortParam: TStateEngineAction;
    FActionQuotedShortParam2QuotedShortParam: TStateEngineAction;
    FActionQuotedShortParam2ShortParam: TStateEngineAction;
    FActionShortParam2Space: TStateEngineAction;
    FActionDoubleDash2LongOption: TStateEngineAction;
    FActionLongOption2LongOption: TStateEngineAction;
    FActionLongOption2LongParam: TStateEngineAction;
    FActionLongParam2LongParam: TStateEngineAction;
    FActionLongParam2QuotedLongParam: TStateEngineAction;
    FActionQuotedLongParam2QuotedLongParam: TStateEngineAction;
    FActionQuotedLongParam2LongParam: TStateEngineAction;
    FActionLongParam2Space: TStateEngineAction;
    FActionStartQuotedParam: TStateEngineAction;
    FActionQuotedParam2Param: TStateEngineAction;
    FActionQuotedParam2QuotedParam: TStateEngineAction;
    FActionSpace2Param: TStateEngineAction;
    FActionParam2Param: TStateEngineAction;
    FActionParam2Space: TStateEngineAction;
    FCharActions: TCharSortedListOfActionLists;
    //
    FCurrentOption: TCmdOption;
    FOptions: TCmdOptionList;
    //
    FStartPos: integer;
    FReadPos: integer;
    FCmdLine: string;
    FActionEndShortOption: TStateEngineAction;
    FStateShortSwitch: TStateEngineState;
    FActionShortOption2ShortSwitch: TStateEngineAction;
    FActionShortSwitch2Space: TStateEngineAction;
    procedure OnStartSwitch(_Sender: TObject);
    procedure HandleChar(_c: char);
    procedure OnStartOption(_Sender: TObject);
    procedure OnEndOption(_Sender: TObject);
    procedure OnStartValue(_Sender: TObject);
    procedure OnEndValue(_Sender: TObject);
    procedure OnStartParam(_Sender: TObject);
    procedure OnEndParam(_Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const _CmdLine: string);
    property Options: TCmdOptionList read FOptions;
  end;

implementation

{ EParsingError }

constructor EParsingError.Create(_ErrorChar: char);
begin
  inherited CreateFmt('Parsing error, "%s" is not allowed here.', [_ErrorChar]);
  FErrorChar := _ErrorChar;
end;

{ EParsingErrorAt }

constructor EParsingErrorAt.Create(_ErrorPosition: integer; _ErrorChar: char = #0);
var
  s: string;
begin
  if _ErrorChar = #0 then
    s := '<nul>'
  else
    s := _ErrorChar;
  inherited CreateFmt('Parsing error at position %d, "%s" is not allowed here.', [_ErrorPosition, s]);
  FErrorChar := _ErrorChar;
  FErrorPosition := _ErrorPosition;
end;

{ TCmdParser }

constructor TCmdParser.Create;
const
  ALL_CHARS = [#1..#255];
var
  FActionLongOption2Space: TStateEngineAction;
begin
  inherited Create;
  FEngine := TStateEngine.Create;

  // States
  FStateSpace := FEngine.RegisterState('Space');
  FStateQuotedParam := FEngine.RegisterState('QuotedParam');
  FStateParam := FEngine.RegisterState('Param');
  FStateDash := FEngine.RegisterState('Dash');
  FStateShortOption := FEngine.RegisterState('ShortOption');
  FStateShortSwitch := FEngine.RegisterState('ShortSwitch');
  FStateShortParam := FEngine.RegisterState('ShortParam');
  FStateQuotedShortParam := FEngine.RegisterState('QuotedShortParam');
  FStateDoubleDash := FEngine.RegisterState('DoubleDash');
  FStateLongOption := FEngine.RegisterState('LongOption');
  FStateLongParam := FEngine.RegisterState('LongParam');
  FStateQuotedLongParam := FEngine.RegisterState('QuotedLongParam');

  // actions
  FActionSpace2Space := FEngine.RegisterAction('Space2Space');
  FActionSpace2Space.AddStateTransition([FStateSpace], FStateSpace);

  FActionSpace2Dash := FEngine.RegisterAction('Space2Dash');
  FActionSpace2Dash.AddStateTransition([FStateSpace], FStateDash);

  FActionDash2DoubleDash := FEngine.RegisterAction('Dash2DoubleDash');
  FActionDash2DoubleDash.AddStateTransition([FStateDash], FStateDoubleDash);

  FActionDash2ShortOption := FEngine.RegisterAction('Dash2ShortOption');
  FActionDash2ShortOption.AddStateTransition([FStateDash], FStateShortOption);
  FActionDash2ShortOption.BeforeExecute := OnStartOption;

  FActionEndShortOption := FEngine.RegisterAction('EndShortOption'); // ??
  FActionEndShortOption.AddStateTransition([FStateShortParam], FStateDash);
  FActionEndShortOption.BeforeExecute := OnEndValue;

  FActionShortOption2ShortSwitch := FEngine.RegisterAction('ShortOption2ShortSwitch');
  FActionShortOption2ShortSwitch.AddStateTransition([FStateShortOption], FStateShortSwitch);
  FActionShortOption2ShortSwitch.BeforeExecute := OnEndOption;
  FActionShortOption2ShortSwitch.AfterExecute := OnStartSwitch;

  FActionShortSwitch2Space := FEngine.RegisterAction('ShortSwitch2Space');
  FActionShortSwitch2Space.AddStateTransition([FStateShortSwitch], FStateSpace);
  FActionShortSwitch2Space.BeforeExecute := OnEndValue;

  FActionShortOption2ShortParam := FEngine.RegisterAction('ShortOption2ShortParam');
  FActionShortOption2ShortParam.AddStateTransition([FStateShortOption], FStateShortParam);
  FActionShortOption2ShortParam.BeforeExecute := OnEndOption;
  FActionShortOption2ShortParam.AfterExecute := OnStartValue;

  FActionShortParam2ShortParam := FEngine.RegisterAction('ShortParam2ShortParam');
  FActionShortParam2ShortParam.AddStateTransition([FStateShortParam], FStateShortParam);

  FActionShortParam2QuotedShortParam := FEngine.RegisterAction('ShortParam2QuotedShortParam');
  FActionShortParam2QuotedShortParam.AddStateTransition([FStateShortParam], FStateQuotedShortParam);

  FActionQuotedShortParam2QuotedShortParam := FEngine.RegisterAction('ShortParam2QuotedShortParam');
  FActionQuotedShortParam2QuotedShortParam.AddStateTransition([FStateQuotedShortParam], FStateQuotedShortParam);

  FActionQuotedShortParam2ShortParam := FEngine.RegisterAction('QuotedShortParam2ShortParam');
  FActionQuotedShortParam2ShortParam.AddStateTransition([FStateQuotedShortParam], FStateShortParam);

  FActionShortParam2Space := FEngine.RegisterAction('ShortParam2Space');
  FActionShortParam2Space.AddStateTransition([FStateShortParam], FStateSpace);
  FActionShortParam2Space.BeforeExecute := OnEndValue;

  FActionDoubleDash2LongOption := FEngine.RegisterAction('DoubleDash2LongOption');
  FActionDoubleDash2LongOption.AddStateTransition([FStateDoubleDash], FStateLongOption);
  FActionDoubleDash2LongOption.BeforeExecute := OnStartOption;

  FActionLongOption2LongOption := FEngine.RegisterAction('LongOption2LongOption');
  FActionLongOption2LongOption.AddStateTransition([FStateLongOption], FStateLongOption);

  FActionLongOption2Space := FEngine.RegisterAction('LongOption2Space');
  FActionLongOption2Space.AddStateTransition([FStateLongOption], FStateSpace);
  FActionLongOption2Space.BeforeExecute := OnEndOption;

  FActionLongOption2LongParam := FEngine.RegisterAction('LongOption2LongParam');
  FActionLongOption2LongParam.AddStateTransition([FStateLongOption], FStateLongParam);
  FActionLongOption2LongParam.BeforeExecute := OnEndOption;
  FActionLongOption2LongParam.AfterExecute := OnStartValue;

  FActionLongParam2LongParam := FEngine.RegisterAction('LongParam2LongParam');
  FActionLongParam2LongParam.AddStateTransition([FStateLongParam], FStateLongParam);

  FActionLongParam2QuotedLongParam := FEngine.RegisterAction('LongParam2QuotedLongParam');
  FActionLongParam2QuotedLongParam.AddStateTransition([FStateLongParam], FStateQuotedLongParam);

  FActionQuotedLongParam2QuotedLongParam := FEngine.RegisterAction('QuotedLongParam2QuotedLongParam');
  FActionQuotedLongParam2QuotedLongParam.AddStateTransition([FStateQuotedLongParam], FStateQuotedLongParam);

  FActionQuotedLongParam2LongParam := FEngine.RegisterAction('QuotedLongParam2LongParam');
  FActionQuotedLongParam2LongParam.AddStateTransition([FStateQuotedLongParam], FStateLongParam);

  FActionLongParam2Space := FEngine.RegisterAction('LongParam2Space');
  FActionLongParam2Space.AddStateTransition([FStateLongParam], FStateSpace);
  FActionLongParam2Space.BeforeExecute := OnEndValue;

  FActionStartQuotedParam := FEngine.RegisterAction('StartQuotedParam');
  FActionStartQuotedParam.AddStateTransition([FStateSpace, FStateParam], FStateQuotedParam);
  FActionStartQuotedParam.BeforeExecute := OnStartParam;

  FActionQuotedParam2QuotedParam := FEngine.RegisterAction('QuotedParam2QuotedParam');
  FActionQuotedParam2QuotedParam.AddStateTransition([FStateQuotedParam], FStateQuotedParam);

  FActionQuotedParam2Param := FEngine.RegisterAction('EndQuotedParam');
  FActionQuotedParam2Param.AddStateTransition([FStateQuotedParam], FStateParam);

  FActionSpace2Param := FEngine.RegisterAction('Space2Param');
  FActionSpace2Param.AddStateTransition([FStateSpace], FStateParam);
  FActionSpace2Param.BeforeExecute := OnStartParam;

  FActionParam2Param := FEngine.RegisterAction('Param2Param');
  FActionParam2Param.AddStateTransition([FStateParam], FStateParam);

  FActionParam2Space := FEngine.RegisterAction('Param2Space');
  FActionParam2Space.AddStateTransition([FStateParam], FStateSpace);
  FActionParam2Space.BeforeExecute := OnEndParam;

  // Create the character to action map
  FCharActions := TCharSortedListOfActionLists.Create;

  // and initialize it
  FCharActions.MapCharsToActions([' '], FActionSpace2Space);
  FCharActions.MapCharsToActions(['-'], FActionSpace2Dash);
  FCharActions.MapCharsToActions(['-'], FActionDash2DoubleDash);
  FCharActions.MapCharsToActions(['a'..'z', 'A'..'Z', '0'..'9'], FActionDash2ShortOption);
  FCharActions.MapCharsToActions(['-'], FActionEndShortOption);
  FCharActions.MapCharsToActions(['-','+'], FActionShortOption2ShortSwitch);
  FCharActions.MapCharsToActions([' '], FActionShortSwitch2Space);
  FCharActions.MapCharsToActions([' '], FActionShortOption2ShortParam);
  FCharActions.MapCharsToActions(['"'], FActionShortParam2QuotedShortParam);
  FCharActions.MapCharsToActions(ALL_CHARS - ['"'], FActionQuotedShortParam2QuotedShortParam);
  FCharActions.MapCharsToActions(['"'], FActionQuotedShortParam2ShortParam);
  FCharActions.MapCharsToActions(ALL_CHARS - ['"', ' ', '-'], FActionShortParam2ShortParam);
  FCharActions.MapCharsToActions([' '], FActionShortParam2Space);
  FCharActions.MapCharsToActions(ALL_CHARS - [' ', '"', '='], FActionDoubleDash2LongOption);
  FCharActions.MapCharsToActions(ALL_CHARS - [' ', '"', '='], FActionLongOption2LongOption);
  FCharActions.MapCharsToActions([' '], FActionLongOption2Space);
  FCharActions.MapCharsToActions(['='], FActionLongOption2LongParam);
  FCharActions.MapCharsToActions(['"'], FActionLongParam2QuotedLongParam);
  FCharActions.MapCharsToActions(ALL_CHARS - ['"'], FActionQuotedLongParam2QuotedLongParam);
  FCharActions.MapCharsToActions(['"'], FActionQuotedLongParam2LongParam);
  FCharActions.MapCharsToActions(ALL_CHARS - [' ', '"'], FActionLongParam2LongParam);
  FCharActions.MapCharsToActions([' '], FActionLongParam2Space);
  FCharActions.MapCharsToActions(['"'], FActionStartQuotedParam);
  FCharActions.MapCharsToActions(ALL_CHARS - ['"'], FActionQuotedParam2QuotedParam);
  FCharActions.MapCharsToActions(['"'], FActionQuotedParam2Param);
  FCharActions.MapCharsToActions(ALL_CHARS - [' ', '"'], FActionParam2Param);
  FCharActions.MapCharsToActions(ALL_CHARS - [' ', '"', '-'], FActionSpace2Param);
  FCharActions.MapCharsToActions([' '], FActionParam2Space);

  // set the possible end states for the engine
  FEngine.AddEndState(FStateSpace);

  FOptions := TCmdOptionList.Create;
end;

destructor TCmdParser.Destroy;
begin
  FOptions.Free;
  FCharActions.Free;
  FEngine.Free;
  inherited;
end;

procedure TCmdParser.HandleChar(_c: char);
var
  Actions: TCharActions;
  Action: TStateEngineAction;
begin
  // search for the character
  if not FCharActions.Search(_c, Actions) then
    raise EParsingError.Create(_c);
  // search the (only one) allowed action for the character
  if not Actions.SearchAllowedAction(FEngine, Action) then
    raise EParsingError.Create(_c);
  // execute the action
  FEngine.ExecuteAction(Action);
end;

procedure TCmdParser.OnEndOption(_Sender: TObject);
begin
  FCurrentOption := TCmdOption.Create(Copy(FCmdLine, FStartPos, FReadPos - FStartPos));
  FOptions.Insert(FCurrentOption);
end;

procedure TCmdParser.OnEndParam(_Sender: TObject);
begin
  FCurrentOption.Parameter := Copy(FCmdLine, FStartPos, FReadPos - FStartPos);
end;

procedure TCmdParser.OnEndValue(_Sender: TObject);
begin
  FCurrentOption.Parameter := Copy(FCmdLine, FStartPos, FReadPos - FStartPos);
end;

procedure TCmdParser.OnStartSwitch(_Sender: TObject);
begin
  FStartPos := FReadPos;
end;

procedure TCmdParser.OnStartValue(_Sender: TObject);
begin
  FStartPos := FReadPos + 1;
end;

procedure TCmdParser.OnStartOption(_Sender: TObject);
begin
  FStartPos := FReadPos;
end;

procedure TCmdParser.OnStartParam(_Sender: TObject);
begin
  FCurrentOption := TCmdOption.Create('');
  FOptions.Insert(FCurrentOption);
  FStartPos := FReadPos;
end;

procedure TCmdParser.Parse(const _CmdLine: string);
begin
  // restart the engine
  FEngine.SetInitialState(FStateSpace);

  FOptions.FreeAll;

  FCmdLine := _CmdLine;
  FStartPos := 0;
  FReadPos := 1;
  try
    while FReadPos <= Length(FCmdLine) do begin
      HandleChar(FCmdline[FReadPos]);
      Inc(FReadPos);
    end;
    // two spaces at the end should move to the end state
    HandleChar(' ');
    HandleChar(' ');
  except
    on e: EParsingError do begin
      raise EParsingErrorAt.Create(FReadPos, e.ErrorChar);
    end;
  end;
  if not FEngine.IsInEndState then
    raise EParsingErrorAt.Create(FReadPos);
end;

end.

