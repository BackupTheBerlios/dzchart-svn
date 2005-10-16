{$IFNDEF __DZ_SORTED_LIST_TEMPLATE__}
unit t_dzSortedListTemplate;

interface

uses
  Classes,
  u_dzQuicksort;

type
  _KEY_TYPE_ = integer;
  _ITEM_TYPE_ = TObject;

{$ENDIF __DZ_SORTED_LIST_TEMPLATE__}

{$IFNDEF __DZ_SORTED_LIST_TEMPLATE_SECOND_PASS__}

{$DEFINE __DZ_LIST_TEMPLATE__}
{$IFDEF __DZ_SORTED_LIST_TEMPLATE_IS_INTERFACED__}
{$DEFINE __DZ_LIST_TEMPLATE_IS_INTERFACED__}
{$ENDIF __DZ_SORTED_LIST_TEMPLATE_IS_INTERFACED__}
{$INCLUDE 't_dzListTemplate.tpl'}

type
  _DZ_SORTED_LIST_TEMPLATE_ = class(_DZ_LIST_TEMPLATE_)
  private
    FDuplicates: TDuplicates;
  protected
    function KeyOf(_Item: _ITEM_TYPE_): _KEY_TYPE_; virtual; abstract;
    function Compare(const _Key1, _Key2: _KEY_TYPE_): integer; virtual; abstract;
    function CompareTo(const _Key; _Idx: integer): integer; virtual;
  public
    constructor Create;
    function Insert(_Item: _ITEM_TYPE_): integer; override;
    function Search(_Key: _KEY_TYPE_; out _Idx: integer): boolean; overload;
{$IFNDEF __DZ_SORTED_LIST_TEMPLATE_ITEM_TYPE_IS_INTEGER__}
    // if ITEM_TYPE = integer we can not create overload Search methods,
    // in this case declare the conditional define above in your unit.
    function Search(_Key: _KEY_TYPE_; out _Item: _ITEM_TYPE_): boolean; overload;
{$ENDIF __DZ_SORTED_LIST_TEMPLATE_ITEM_TYPE_IS_INTEGER__}
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
  Result :=  Compare(_KEY_TYPE_(_Key), KeyOf(FItems[_Idx]));
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
    _Item := FItems[Idx];
end;

{$ENDIF __DZ_SORTED_LIST_TEMPLATE_SECOND_PASS__}

{$DEFINE __DZ_SORTED_LIST_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_SORTED_LIST_TEMPLATE__}
{$WARNINGS OFF}
end.
{$ENDIF __DZ_SORTED_LIST_TEMPLATE__}

