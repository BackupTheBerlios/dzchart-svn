unit u_dzSortedListTest;

interface

uses
  Classes,
  u_dzQuicksort,
  u_dzListTest;

{$DEFINE __DZ_SORTED_LIST_TEMPLATE__}
type
  _ITEM_TYPE_ = TMyItem;
  _KEY_TYPE_ = integer;
{$INCLUDE 't_dzSortedListTemplate.tpl'}

type
  TMySortedList = class(_DZ_SORTED_LIST_TEMPLATE_)
  protected
    function KeyOf(_Item: _ITEM_TYPE_): _KEY_TYPE_; override;
    function Compare(const _Key1, _Key2: _KEY_TYPE_): integer; override;
  end;

implementation

{ TMySortedList }

{$INCLUDE 't_dzSortedListTemplate.tpl'}

function TMySortedList.KeyOf(_Item: _ITEM_TYPE_): _KEY_TYPE_;
begin
  Result := _Item.Id;
end;

function TMySortedList.Compare(const _Key1, _Key2: _KEY_TYPE_): integer;
begin
  Result := _Key1 - _Key2;
end;

end.

