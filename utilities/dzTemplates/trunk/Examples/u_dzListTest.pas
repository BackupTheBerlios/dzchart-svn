unit u_dzListTest;

interface

uses
  Classes,
  u_dzQuicksort;

type
  TMyItem = class
  private
    FId: integer;
  public
    constructor Create(_Id: integer);
    property Id: integer read FId;
  end;

type
  _ITEM_TYPE_ = TMyItem;

{$DEFINE __DZ_LIST_INTERFACE_TEMPLATE__}
{$INCLUDE 't_dzListInterfaceTemplate.tpl'}

{$DEFINE __DZ_LIST_TEMPLATE__}
{$DEFINE __DZ_LIST_TEMPLATE_IS_INTERFACED__}
{$INCLUDE 't_dzListTemplate.tpl'}

type
  IMyItemList = interface(_DZ_LIST_TEMPLATE_INTERFACE_)['{75759FED-F7FA-407F-92F0-F8C6A6A6109C}']
  end;

type
  TMyItemList = class(_DZ_LIST_TEMPLATE_, IMyItemList)
  public
  end;

implementation

{ TMyItemList }

{$INCLUDE 't_dzListTemplate.tpl'}

{ TMyItem }

constructor TMyItem.Create(_Id: integer);
begin
  inherited Create;
  FId := _Id;
end;

end.

