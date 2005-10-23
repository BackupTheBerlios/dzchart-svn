{$IFNDEF __DZ_SORTED_LIST_TEMPLATE__}
unit t_dzSortedListTemplate;

interface

{: any class built on this template must add these units to the uses clause }
uses
  Classes,
  u_dzQuicksort;

{: these types must be declared for any class built on this template }
type
  {: the ancestor class for the template, can be TObject or TInterfacedObject
     or anything else you like}
  _LIST_ANCESTOR_ = TInterfacedObject;
  {: Container type used to actually store the items: TList or TInterfacelist }
  _LIST_CONTAINER_ = TList;
  {: The item type to be stored in the list }
  _ITEM_TYPE_ = TObject;
  {: The type of the item's keys }
  _KEY_TYPE_ = integer;

{$ENDIF __DZ_SORTED_LIST_TEMPLATE__}

{$IFNDEF __DZ_SORTED_LIST_TEMPLATE_SECOND_PASS__}

{$DEFINE __DZ_LIST_TEMPLATE__}
{$INCLUDE 't_dzListTemplate.tpl'}

type
  {: extends _DZ_LIST_TEMPLATE_ to store the items sorted and allow searching for them }
  _DZ_SORTED_LIST_TEMPLATE_ = class(_DZ_LIST_TEMPLATE_)
  private
    FDuplicates: TDuplicates;
  protected
    {: abstract function to return the key of an item for comparison }
    function KeyOf(const _Item: _ITEM_TYPE_): _KEY_TYPE_; virtual; abstract;
    {: abstract function to compare the keys of two items, must return a value
       <0 if Key1 < Key2, =0 if Key1 = Key2 and >0 if Key1 > Key2 }
    function Compare(const _Key1, _Key2: _KEY_TYPE_): integer; virtual; abstract;
    {: compares the given key to the key of the item at index Idx }
    function CompareTo(const _Key; _Idx: integer): integer; virtual;
  public
    {: creates a new sorted list }
    constructor Create;
    {: Inserts an item at the position determined by comparing its key with the existing
       items. }
    function Insert(_Item: _ITEM_TYPE_): integer; override;
    {: searches for the item with the given Key
       @param Key is the sought item's key
       @param Idx is the index of the item, if found, only valid if the function returns true
       @returns true, if the item has been found, false otherwise }
    function Search(_Key: _KEY_TYPE_; out _Idx: integer): boolean; overload;
{$IFNDEF __DZ_SORTED_LIST_TEMPLATE_ITEM_TYPE_IS_INTEGER__}
    {: searches for the item with the given key
       @param Key is the sought item's key
       @param Item is the item, if found, only valid if the function returns true
       @returns true, if the item has been found, false otherwise
       @note: if ITEM_TYPE = integer we can not create overloaded Search methods,
              in this case declare the conditional define above in your unit. }
    function Search(_Key: _KEY_TYPE_; out _Item: _ITEM_TYPE_): boolean; overload;
{$ENDIF __DZ_SORTED_LIST_TEMPLATE_ITEM_TYPE_IS_INTEGER__}
    {: determines what to do if trying to insert an item with a key that already exists
       in the list:
       * dupError: will raise EListError
       * dupIgnore: do not insert the item and return -1
       * dupAccept: insert the item before the existing one }
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

{$ENDIF __DZ_SORTED_LIST_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_SORTED_LIST_TEMPLATE__}
{$DEFINE __DZ_SORTED_LIST_TEMPLATE_SECOND_PASS__}
implementation
{$ENDIF __DZ_SORTED_LIST_TEMPLATE__}

{$IFDEF __DZ_SORTED_LIST_TEMPLATE_SECOND_PASS__}

{ _DZ_SORTED_LIST_TEMPLATE_ }

{$INCLUDE 't_dzListTemplate.tpl'}

function _DZ_SORTED_LIST_TEMPLATE_.CompareTo(const _Key; _Idx: integer): integer;
begin
  Result :=  Compare(_KEY_TYPE_(_Key), KeyOf(_ITEM_TYPE_(FItems[_Idx])));
end;

constructor _DZ_SORTED_LIST_TEMPLATE_.Create;
begin
  inherited Create;
  FDuplicates := dupError;
end;

function _DZ_SORTED_LIST_TEMPLATE_.Insert(_Item: _ITEM_TYPE_): integer;
begin
  if Search(KeyOf(_Item), Result) then
    case Duplicates of
    dupError:
      raise EListError.Create('List does not allow duplicates');
    dupIgnore: begin
        Result := -1;
        exit;
      end;
    end;

  // dupAccept:
  FItems.Insert(Result, _Item);
end;

function _DZ_SORTED_LIST_TEMPLATE_.Search(_Key: _KEY_TYPE_; out _Idx: integer): boolean;
begin
  Result := BinarySearch(0, FItems.Count - 1, _Idx, _Key, CompareTo)
end;

function _DZ_SORTED_LIST_TEMPLATE_.Search(_Key: _KEY_TYPE_; out _Item: _ITEM_TYPE_): boolean;
var
  Idx: integer;
begin
  Result := Search(_Key, Idx);
  if Result then
    _Item := _ITEM_TYPE_(FItems[Idx]);
end;

{$ENDIF __DZ_SORTED_LIST_TEMPLATE_SECOND_PASS__}

{$DEFINE __DZ_SORTED_LIST_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_SORTED_LIST_TEMPLATE__}
{$WARNINGS OFF}
end.
{$ENDIF __DZ_SORTED_LIST_TEMPLATE__}

