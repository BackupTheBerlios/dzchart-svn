unit u_dzNameValueList;

interface

uses
  Classes,
  u_dzQuicksort;

type
  TNameValue = class
  private
    FName: string;
    FValue: string;
  public
    constructor Create(const _Name: string; _Value: string);
    property Name: string read FName;
    property Value: string read FValue write FValue;
  end;

{$DEFINE __DZ_SORTED_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TNameValue;
  _KEY_TYPE_ = string;
{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

type
  {: Sorted list for storing TNameValue items }
  TNameValueList = class(_DZ_SORTED_OBJECT_LIST_TEMPLATE_)
  protected
    function KeyOf(const _Item: TNameValue): string; override;
    function Compare(const _Key1, _Key2: string): integer; override;
  public
    function Find(const _Name: string; out _Value: string): boolean; overload;
  end;

implementation

uses
  SysUtils;

{$INCLUDE 't_dzSortedObjectListTemplate.tpl'}

{ TNameValue }

constructor TNameValue.Create(const _Name: string; _Value: string);
begin
  inherited Create;
  FName := _Name;
  FValue := _Value;
end;

{ TNameValueList }

function TNameValueList.Compare(const _Key1, _Key2: string): integer;
begin
  Result := CompareText(_Key1, _Key2);
end;

function TNameValueList.Find(const _Name: string; out _Value: string): boolean;
var
  nv: TNameValue;
begin
  Result := Find(_Name, nv);
  if Result then
    _Value := nv.Value;
end;

function TNameValueList.KeyOf(const _Item: TNameValue): string;
begin
  Result := _Item.Name;
end;

end.

