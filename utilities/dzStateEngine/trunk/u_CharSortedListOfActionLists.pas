unit u_CharSortedListOfActionLists;

interface

uses
  SysUtils,
  Classes,
  u_dzStateEngineActions,
  u_dzStateEngine,
  u_dzQuicksort;

type
  TCharActions = class(TStateEngineActionList)
  private
    FCharacter: char;
  public
    constructor Create(_Character: char);
    function SearchAllowedAction(_Engine: TStateEngine; out _Action: TStateEngineAction): boolean;
    property Character: char read FCharacter;
  end;

{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
type
  _LIST_PARENT_ = TObject;
  _ITEM_TYPE_ = TCharActions;
  _KEY_TYPE_ = char;
{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

type
  TCharSortedListOfActionLists = class(_DZ_SORTED_OBJECT_LIST_TEMPLATE_)
  protected
    function KeyOf(const _Item: TCharActions): char; override;
    function Compare(const _Key1, _Key2: char): integer; override;
  end;

implementation

{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

{ TCharActions }

constructor TCharActions.Create(_Character: char);
begin
  inherited Create;
  OwnsItems := false;
  FCharacter := _Character;
end;

function TCharActions.SearchAllowedAction(_Engine: TStateEngine;
  out _Action: TStateEngineAction): boolean;
var
  i: integer;
  Action: TStateEngineAction;
begin
  Result := false;
  for i := 0 to Count - 1 do begin
    Action := Items[i];
    if _Engine.IsActionAllowed(Action) then begin
      Result := not Result;
      if Result then
        _Action := Action
      else
        exit; // we found the second allowed action which is an error
    end;
  end;
end;

{ TCharSortedListOfActionLists }

function TCharSortedListOfActionLists.KeyOf(const _Item: TCharActions): char;
begin
  Result := _Item.Character;
end;

function TCharSortedListOfActionLists.Compare(const _Key1, _Key2: char): integer;
begin
  Result := Ord(_Key1) - Ord(_Key2);
end;

end.

