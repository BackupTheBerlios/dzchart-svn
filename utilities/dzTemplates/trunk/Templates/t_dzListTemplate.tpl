{$IFNDEF __DZ_LIST_TEMPLATE__}
unit t_dzListTemplate;

interface

uses
  Classes,
  u_dzQuicksort;

type
  _ITEM_TYPE_ = TObject;
{$ENDIF __DZ_LIST_TEMPLATE__}

{$IFNDEF __DZ_LIST_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_LIST_INTERFACE_TEMPLATE__}
type
  TOnCompareItems = function(_Item1, _Item2: _ITEM_TYPE_): integer of object;
{$ENDIF __DZ_LIST_INTERFACE_TEMPLATE__}

type
  _DZ_LIST_TEMPLATE_ = class
  {$IFDEF __DZ_LIST_TEMPLATE_IS_INTERFACED__}(TInterfacedObject){$ENDIF}
  private
    FItems: TList;
    FOnCompareItems: TOnCompareItems;
    function GetItems(_Idx: integer): _ITEM_TYPE_;
    function CompareItems(_Idx1, _Idx2: integer): integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    procedure DeleteAll;
    function Extract(_Idx: integer): _ITEM_TYPE_;
    procedure FreeAll;
    procedure FreeItem(_Item: _ITEM_TYPE_); virtual;
    function Insert(_Item: _ITEM_TYPE_): integer; virtual;
    procedure Sort(_OnCompareItems: TOnCompareItems);
    procedure Exchange(_Idx1, _Idx2: integer);
    property Items[_Idx: integer]: _ITEM_TYPE_ read GetItems;
  end;

{$ENDIF __DZ_LIST_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_LIST_TEMPLATE__}
{$DEFINE __DZ_LIST_TEMPLATE_SECOND_PASS__}
implementation
{$ENDIF __DZ_LIST_TEMPLATE__}

{$IFDEF __DZ_LIST_TEMPLATE_SECOND_PASS__}

{ _DZ_LIST_TEMPLATE_ }

function _DZ_LIST_TEMPLATE_.CompareItems(_Idx1, _Idx2: integer): integer;
begin
  Result := FOnCompareItems(FItems[_Idx1], FItems[_Idx2]);
end;

function _DZ_LIST_TEMPLATE_.Count: integer;
begin
  Result := FItems.Count;
end;

constructor _DZ_LIST_TEMPLATE_.Create;
begin
  inherited Create;
  FItems := TList.Create;
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
      Item := FItems[i];
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
  Result := FItems[_Idx];
  Fitems.Delete(_Idx);
end;

procedure _DZ_LIST_TEMPLATE_.FreeAll;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do begin
    _ITEM_TYPE_(FItems[i]).Free;
  end;
  FItems.Clear;
end;

procedure _DZ_LIST_TEMPLATE_.FreeItem(_Item: _ITEM_TYPE_);
begin
  _Item.Free;
end;

function _DZ_LIST_TEMPLATE_.GetItems(_Idx: integer): _ITEM_TYPE_;
begin
  Result := FItems[_Idx];
end;

function _DZ_LIST_TEMPLATE_.Insert(_Item: _ITEM_TYPE_): integer;
begin
  Result := FItems.Add(_Item);
end;

procedure _DZ_LIST_TEMPLATE_.Sort(_OnCompareItems: TOnCompareItems);
begin
  FOnCompareItems := _OnCompareItems;
  QuickSort(0, Count - 1, CompareItems, Exchange);
end;

{$ENDIF __DZ_LIST_TEMPLATE_SECOND_PASS__}

{$DEFINE __DZ_LIST_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_LIST_TEMPLATE__}
{$WARNINGS OFF}
end.
{$ENDIF __DZ_LIST_TEMPLATE__}

