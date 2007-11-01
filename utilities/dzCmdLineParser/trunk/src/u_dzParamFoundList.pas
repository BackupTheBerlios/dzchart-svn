unit u_dzParamFoundList;

interface

uses
  Classes,
  u_dzParamDescList;

type
  TParamFound = class
  private
    FValue: string;
    FParamDesc: TParamDesc;
  public
    constructor Create(_Paramdesc: TParamDesc; const _Value: string);
    property ParamDesc: TParamDesc read FParamDesc;
    property Value: string read fValue;
  end;

{$DEFINE __DZ_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _LIST_CONTAINER_ = TList;
  _LIST_CONTAINER_ITEM_TYPE_ = pointer;
  _ITEM_TYPE_ = TParamFound;
{$INCLUDE 't_dzObjectListTemplate.tpl'}

type
  {: List for storing TParamFound items }
  TParamFoundList = class(_DZ_OBJECT_LIST_TEMPLATE_)

  end;

implementation

{$INCLUDE 't_dzObjectListTemplate.tpl'}

constructor TParamFound.Create(_Paramdesc: TParamDesc; const _Value: string);
begin
  inherited Create;
  FParamDesc := _Paramdesc;
  FValue := _Value;
end;

end.

