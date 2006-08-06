unit u_dzOptionNameList;

interface

uses
  SysUtils,
  Classes,
  u_dzQuicksort,
  u_dzOptionDescList;

type
  TOptionName = class
  protected
    FName: string;
    FOptionDesc: TOptionDesc;
  public
    constructor Create(const _Name: string; _OptionDesc: TOptionDesc);
    function GetPrimaryName: string;
    property Name: string read FName;
    property OptionDesc: TOptionDesc read FOptionDesc;
  end;

{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _LIST_CONTAINER_ = TList;
  _LIST_CONTAINER_ITEM_TYPE_ = pointer;
  _ITEM_TYPE_ = TOptionName;
  _KEY_TYPE_ = string;
{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

type
  {: List for storing TOptionName items sorted by string }
  TOptionNameList = class(_DZ_SORTED_OBJECT_LIST_TEMPLATE_)
  protected
    {: return the key of an item for comparison }
    function KeyOf(const _Item: TOptionName): string; override;
    {: compare the keys of two items, must return a value
       < 0 if Key1 < Key2, = 0 if Key1 = Key2 and > 0 if Key1 > Key2 }
    function Compare(const _Key1, _Key2: string): integer; override;
  end;

implementation

{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

function TOptionNameList.KeyOf(const _Item: TOptionName): string;
begin
  Result := _Item.Name;
end;

function TOptionNameList.Compare(const _Key1, _Key2: string): integer;
begin
  // Note: This compares 1 character options case sensitively ( eg. -A <> -a )
  //       but long options case insensitively (e.g. --hallo = --Hallo )
  if (Length(_Key1) <> 1) or (Length(_Key2) <> 1) then
    Result := CompareText(_Key1, _Key2)
  else
    Result := CompareStr(_Key1, _Key2);
end;

constructor TOptionName.Create(const _Name: string; _OptionDesc: TOptionDesc);
begin
  inherited Create;
  FName := _Name;
  FOptionDesc := _OptionDesc;
end;

function TOptionName.GetPrimaryName: string;
begin
  Result := FOptionDesc.PrimaryName;
end;

end.

