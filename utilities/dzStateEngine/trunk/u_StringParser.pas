{: This is a more complex example for using the state engine:
   a parser for Pascal string literals }
unit u_StringParser;

interface

uses
  SysUtils,
  u_dzStateEngineStates,
  u_dzStateEngineActions,
  u_dzStateEngine,
  u_CharSortedListOfActionLists;

type
  TIsStringLiteral = (slNo, slYes, slPartial, slError);

type
  {: A TStringParser validates a Pascal string literal
     Valid string literals are:
     * '<some text here>'
     * '''' (quoted single quote)
     * #5#2#5
     * ^m
     * #$A
     and any combinations of the above,
     eg: 'hello ''user''#13#$a^m^j }
  TStringParser = class
  private
    {: State for "at end of quotes" }
    FStateEndQuote: TStateEngineState;
    {: State for "inside quotes" }
    FStateInQuote: TStateEngineState;
    {: State for "after caret (^)" }
    FStateCaret: TStateEngineState;
    {: State for "after hash (#)" }
    FStateHash: TStateEngineState;
    {: State for "after hash dollar (#$)" }
    FStateHashDollar: TStateEngineState;
    {: State for "in decimal hash" }
    FStateDecHash: TStateEngineState;
    {: State for "in hexadecimal hash" }
    FStateHexHash: TStateEngineState;
    {: Action for "just read a quote char" }
    FActionQuote: TStateEngineAction;
    {: Action for "just read a caret char" }
    FActionCaret: TStateEngineAction;
    {: Action for "just read a hash char" }
    FActionHash: TStateEngineAction;
    {: Action for "just read an alpha char" }
    FActionAtoZaTOz: TStateEngineAction;
    {: Action for "just read a dollar char" }
    FActionDollar: TStateEngineAction;
    {: Action for "just read a hexadecimal digit" }
    FActionHexDigit: TStateEngineAction;
    {: Action for "just read a decimal digit" }
    FActionDigit: TStateEngineAction;
    {: Action for "Just read a non quote non CR and non LF character" }
    FActionAnyNonQuoteCrLf: TStateEngineAction;
    {: the actual state engine }
    FEngine: TStateEngine;
    {: a mapping between characters and actions }
    FCharActions: TCharSortedListOfActionLists;
  public
    {: initialises the parser }
    constructor Create;
    {: frees everything }
    destructor Destroy; override;
    {: validates that s contains a valid string literal
       @param s is the string to check
       @raises exception if there is an error }
    procedure Execute(const _s: string);
    {: parses the 0 terminated string Buffer and returns true, if it starts with a string literal
       @Buffer on input points to the first character, on output points to the
          last character that is part of the string literal
       @returns
           slNo, if there is no string literal at the start
           slYes, if the whole is a string literal
           slPartial, if there is a string literal at the start but followed by other stuff
           slError, if there is a string literal at the start but it wasn't finished at the end }
    function Parse(var _Buffer: PChar): TIsStringLiteral;
  end;

implementation

{ TStringParser }

constructor TStringParser.Create;
begin
  inherited;

  // create the state engine
  FEngine := TStateEngine.Create;

  // initialize the engine states
  FStateEndQuote := FEngine.RegisterState('EndQuote');
  FStateInQuote := FEngine.RegisterState('InQuote');
  FStateCaret := FEngine.RegisterState('Caret');
  FStateHash := FEngine.RegisterState('Hash');
  FStateDecHash := FEngine.RegisterState('DecHash');
  FStateHashDollar := FEngine.RegisterState('HashDollar');
  FStateHexHash := FEngine.RegisterState('HexHash');

  // initialize the actions
  FActionQuote := FEngine.RegisterAction('Quote');
  FActionCaret := FEngine.RegisterAction('Caret');
  FActionHash := FEngine.RegisterAction('Hash');
  FActionDollar := FEngine.RegisterAction('Dollar');
  FActionAtoZaTOz := FEngine.RegisterAction('AtoZaTOz');
  FActionHexDigit := FEngine.RegisterAction('HexDigit');
  FActionDigit := FEngine.RegisterAction('Digit');
  FActionAnyNonQuoteCrLf := FEngine.RegisterAction('AnyNonCrLf');

  // add state transitions to actions
  FActionQuote.AddStateTransition([FStateInQuote], FStateEndQuote);
  FActionQuote.AddStateTransition([FStateEndQuote], FStateInQuote);
  FActionQuote.AddStateTransition([FStateDecHash, FStateHexHash], FStateInQuote);

  FActionCaret.AddStateTransition([FStateEndQuote], FStateCaret);
  FActionAtoZaTOz.AddStateTransition([FStateCaret], FStateEndQuote);

  FActionHash.AddStateTransition([FStateEndQuote, FStateDecHash, FStateHexHash], FStateHash);
  FActionDigit.AddStateTransition([FStateHash, FStateDecHash], FStateDecHash);
  FActionDollar.AddStateTransition([FStateHash], FStateHashDollar);
  FActionHexDigit.AddStateTransition([FStateHashDollar, FStateHexHash], FStateHexHash);
  FActionAnyNonQuoteCrLf.AddStateTransition([FStateInQuote], FStateInQuote);

  // Create the character to action map
  FCharActions := TCharSortedListOfActionLists.Create;

  // and initialize it
  FCharActions.MapCharsToActions([''''], FActionQuote);
  FCharActions.MapCharsToActions(['^'], FActionCaret);
  FCharActions.MapCharsToActions(['#'], FActionHash);
  FCharActions.MapCharsToActions(['$'], FActionDollar);
  FCharActions.MapCharsToActions(['a'..'z', 'A'..'Z'], FActionAtoZaTOz);
  FCharActions.MapCharsToActions(['0'..'9', 'a'..'f', 'A'..'F'], FActionHexDigit);
  FCharActions.MapCharsToActions(['0'..'9'], FActionDigit);
  FCharActions.MapCharsToActions([#1..#255] - [#13] - [#10] - [''''], FActionAnyNonQuoteCrLf);

  // set the possible end states for the engine
  FEngine.AddEndState(FStateEndQuote);
  FEngine.AddEndState(FStateHexHash);
  FEngine.AddEndState(FStateDecHash);
end;

destructor TStringParser.Destroy;
begin
  FCharActions.Free;
  FEngine.Free;
  inherited;
end;

// slNo, if there is no string literal at the start
// slYes, if the whole is a string literal
// slPartial, if there is a string literal at the start but followed by other stuff
// slError, if there is a string literal at the start but it wasn't finished at the end }

function TStringParser.Parse(var _Buffer: PChar): TIsStringLiteral;
var
  Actions: TCharActions;
  Action: TStateEngineAction;
  LastEndStatePos: PChar;
  p: PChar;
begin
  // restart the engine
  FEngine.SetInitialState(FStateEndQuote);

  LastEndStatePos := nil;
  p := _Buffer;
  while p^ <> #0 do begin
    // search for the character
    if not FCharActions.Search(p^, Actions) then begin
      if LastEndStatePos = nil then
        // we didn't even find a valid string part
        Result := slNo
      else begin
        _Buffer := LastEndStatePos;
        Result := slPartial;
      end;
      exit;
    end;
    // search for the (only one) allowed action for the character
    // search the (only one) allowed action for the character
    if not Actions.SearchAllowedAction(FEngine, Action) then begin
      // if there is none, we found the end of the valid string part
      if LastEndStatePos = nil then
        // we didn't even find a valid string part
        Result := slNo
      else begin
        _Buffer := LastEndStatePos;
        Result := slPartial;
      end;
      exit;
    end;
    // execute the action
    FEngine.ExecuteAction(Action);
    if FEngine.IsInEndState then
      LastEndStatePos := p;
    Inc(p);
  end;
  if FEngine.IsInEndState then begin
    if LastEndStatePos = nil then
      // we didn't even find a valid string part
      Result := slNo
    else
      Result := slYes;
  end else begin
    Result := slError;
  end;
  _Buffer := p - 1;
end;

procedure TStringParser.Execute(const _s: string);
var
  Buffer: PChar;
  p: PChar;
begin
  Buffer := PChar(_s);
  p := Buffer;
  case Parse(p) of
    slYes: exit;
    slNo: raise exception.Create('This does not even start with a valid string literal');
    slPartial: raise exception.CreateFmt('This is a string literal up to the %dth character (#%d %s)',
        [p - Buffer + 1, Ord(p^), p^]);
    slError: raise exception.Create('This is an unterminated string literal');
  end;

//  // restart the engine
//  FEngine.SetInitialState(FStateEndQuote);
//
//  // for each character in s
//  i := 1;
//  while i <= Length(_s) do begin
//    // search the character
//    if not FCharActions.Search(_s[i], Actions) then
//      raise exception.CreateFmt('Invalid string literal (could not find action for character #%d  %s)', [i, _s[i]]);
//    // search the (only one) allowed action for the character
//    if not Actions.SearchAllowedAction(FEngine, Action) then
//      raise exception.CreateFmt('Invalid string literal (could not find allowed action for character #%d %s)', [i, _s[i]]);
//    // execute the action
//    FEngine.ExecuteAction(Action);
//    Inc(i);
//  end;
//  // check that the engine is in an allowed end state
//  if not FEngine.IsInEndState then
//    raise exception.CreateFmt('Invalid string literal (invalid end state %s)', [FEngine.GetState.Name]);
end;

end.

