{$IFNDEF __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
unit t_dzSortedObjectListTemplate;

interface

{: any class built on this template must add these units to the uses clause }
uses
  Classes,
  u_dzQuicksort;

{: these types must be declared for any class built on this template }
type
  _LIST_PARENT_ = TInterfacedObject; // or TObject or anything else you like
  _ITEM_TYPE_ = TObject;

{$ENDIF __DZ_SORTED_OBJECT_LIST_TEMPLATE__}

{$IFNDEF __DZ_SORTED_OBJECT_LIST_TEMPLATE_SECOND_PASS__}

{$DEFINE __DZ_SORTED_LIST_TEMPLATE__}
{$INCLUDE 't_dzSortedListTemplate.tpl'}

type
  {: Extends _DZ_SORTED_LIST_TEMPLATE_ to call the item's Free method in FreeItem,
     thereby allowing to store any TObject descendant. }
  _DZ_SORTED_OBJECT_LIST_TEMPLATE_ = class(_DZ_SORTED_LIST_TEMPLATE_)
  protected
    {: calls the Item's Free method }
    procedure FreeItem(_Item: _ITEM_TYPE_); override;
  end;

{$ENDIF __DZ_SORTED_OBJECT_LIST_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE_SECOND_PASS__}
implementation
{$ENDIF __DZ_SORTED_OBJECT_LIST_TEMPLATE__}

{$IFDEF __DZ_SORTED_OBJECT_LIST_TEMPLATE_SECOND_PASS__}

{ _DZ_SORTED_OBJECT_LIST_TEMPLATE_ }

{$INCLUDE 't_dzSortedListTemplate.tpl'}

procedure _DZ_SORTED_OBJECT_LIST_TEMPLATE_.FreeItem(_Item: _ITEM_TYPE_);
begin
  _Item.Free;
end;

{$ENDIF __DZ_SORTED_OBJECT_LIST_TEMPLATE_SECOND_PASS__}

{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
{$WARNINGS OFF}
end.
{$ENDIF __DZ_SORTED_OBJECT_LIST_TEMPLATE__}

