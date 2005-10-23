{$IFNDEF __DZ_LIST_TEMPLATE__}
unit t_dzListTemplate;

interface

{: These units must be added to the uses clause of any class built on this template }
uses
  Classes;

{: These types must be declared for each class built on this template }
type
  {: the ancestor class for the template, can be TObject or TInterfacedObject
     or anything else you like}
  _LIST_ANCESTOR_ = TInterfacedObject;
  {: Container type used to actually store the items: TList or TInterfacelist }
  _LIST_CONTAINER_ = TList;
  {: The item type to be stored in the list }
  _ITEM_TYPE_ = TObject;

{$ENDIF __DZ_LIST_TEMPLATE__}

{$IFNDEF __DZ_LIST_TEMPLATE_SECOND_PASS__}

type
  _DZ_LIST_TEMPLATE_ = class(_LIST_ANCESTOR_)
  private
    FItems: _LIST_CONTAINER_;
    function GetItems(_Idx: integer): _ITEM_TYPE_;
    procedure FreeItem(_Item: _ITEM_TYPE_); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    procedure DeleteAll;
    function Extract(_Idx: integer): _ITEM_TYPE_;
    procedure FreeAll;
    function Insert(_Item: _ITEM_TYPE_): integer; virtual;
    procedure Exchange(_Idx1, _Idx2: integer);
    property Items[_Idx: integer]: _ITEM_TYPE_ read GetItems; default;
  end;

{$ENDIF __DZ_LIST_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_LIST_TEMPLATE__}
{$DEFINE __DZ_LIST_TEMPLATE_SECOND_PASS__}
implementation
{$ENDIF __DZ_LIST_TEMPLATE__}

{$IFDEF __DZ_LIST_TEMPLATE_SECOND_PASS__}

{ _DZ_LIST_TEMPLATE_ }

function _DZ_LIST_TEMPLATE_.Count: integer;
begin
  Result := FItems.Count;
end;

constructor _DZ_LIST_TEMPLATE_.Create;
begin
  inherited Create;
  FItems := _LIST_CONTAINER_.Create;
end;

procedure _DZ_LIST_TEMPLATE_.DeleteAll;
begin
  FItems.Clear;
end;

destructor _DZ_LIST_TEMPLATE_.Destroy;
var
  i: integer;
  Item: _ITEM_TYPE_;
begin
  if Assigned(FItems) then begin
    for i := 0 to FItems.Count - 1 do begin
      Item := _ITEM_TYPE_(FItems[i]);
      FreeItem(Item);
    end;
  end;
  FItems.Free;
  inherited;
end;

procedure _DZ_LIST_TEMPLATE_.Exchange(_Idx1, _Idx2: integer);
begin
  FItems.Exchange(_Idx1, _Idx2);
end;

function _DZ_LIST_TEMPLATE_.Extract(_Idx: integer): _ITEM_TYPE_;
begin
  Result := _ITEM_TYPE_(FItems[_Idx]);
  Fitems.Delete(_Idx);
end;

procedure _DZ_LIST_TEMPLATE_.FreeAll;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do begin
    FreeItem(_ITEM_TYPE_(FItems[i]));
  end;
  FItems.Clear;
end;

procedure _DZ_LIST_TEMPLATE_.FreeItem(_Item: _ITEM_TYPE_);
begin
//  _Item.Free;
end;

function _DZ_LIST_TEMPLATE_.GetItems(_Idx: integer): _ITEM_TYPE_;
begin
  Result := _ITEM_TYPE_(FItems[_Idx]);
end;

function _DZ_LIST_TEMPLATE_.Insert(_Item: _ITEM_TYPE_): integer;
begin
  Result := FItems.Add(_Item);
end;

{$ENDIF __DZ_LIST_TEMPLATE_SECOND_PASS__}

{$DEFINE __DZ_LIST_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_LIST_TEMPLATE__}
{$WARNINGS OFF}
end.
{$ENDIF __DZ_LIST_TEMPLATE__}

