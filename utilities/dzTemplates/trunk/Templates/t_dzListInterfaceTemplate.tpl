{$IFNDEF __DZ_LIST_INTERFACE_TEMPLATE__}
unit t_dzListInterfaceTemplate;

interface

{: These types must be declared any the class based on this template }
type
  {: the object type to be stored in the list }
  _ITEM_TYPE_ = TObject;
{$ENDIF __DZ_LIST_INTERFACE_TEMPLATE__}

{$IFNDEF __DZ_LIST_INTERFACE_TEMPLATE_SECOND_PASS__}

type
  _DZ_LIST_INTERFACE_TEMPLATE_ = interface
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

{$ENDIF __DZ_LIST_INTERFACE_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_LIST_INTERFACE_TEMPLATE__}
{$DEFINE __DZ_LIST_INTERFACE_TEMPLATE_SECOND_PASS__}
implementation
{$ENDIF __DZ_LIST_INTERFACE_TEMPLATE__}

{$DEFINE __DZ_LIST_INTERFACE_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_LIST_INTERFACE_TEMPLATE__}
{$WARNINGS OFF}
end.
{$ENDIF __DZ_LIST_INTERFACE_TEMPLATE__}

