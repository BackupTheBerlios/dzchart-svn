{$IFNDEF __DZ_SORTED_LIST_INTERFACE_TEMPLATE__}
unit t_dzSortedListInterfaceTemplate;

interface

type
  _ITEM_TYPE_ = TObject;
type
  TOnCompareItems = function(_Item1, _Item2: _ITEM_TYPE_): integer of object;
{$ENDIF __DZ_SORTED_LIST_INTERFACE_TEMPLATE__}

{$IFNDEF __DZ_SORTED_LIST_INTERFACE_TEMPLATE_SECOND_PASS__}

type
  _DZ_SORTED_LIST_TEMPLATE_INTERFACE_ = interface
    function Count: integer;
    procedure DeleteAll;
    procedure Exchange(_Idx1, _Idx2: integer);
    function Extract(_Idx: integer): _ITEM_TYPE_;
    procedure FreeAll;
    procedure FreeItem(_Item: _ITEM_TYPE_);
    function GetItems(_Idx: integer): _ITEM_TYPE_;
    function Insert(_Item: _ITEM_TYPE_): integer;
    property Items[_Idx: integer]: _ITEM_TYPE_ read GetItems;
  end;

{$ENDIF __DZ_SORTED_LIST_INTERFACE_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_SORTED_LIST_INTERFACE_TEMPLATE__}
{$DEFINE __DZ_SORTED_LIST_INTERFACE_TEMPLATE_SECOND_PASS__}
implementation
{$ENDIF __DZ_SORTED_LIST_INTERFACE_TEMPLATE__}

{$DEFINE __DZ_SORTED_LIST_INTERFACE_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_SORTED_LIST_INTERFACE_TEMPLATE__}
{$WARNINGS OFF}
end.
{$ENDIF __DZ_SORTED_LIST_INTERFACE_TEMPLATE__}

