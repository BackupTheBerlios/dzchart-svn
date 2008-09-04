unit u_dzNameBoolList;

interface

uses
  Classes,
  u_dzQuicksort;

type
  TNameBool = class
  private
    FName: string;
    FBool: boolean;
  public
    constructor Create(const _Name: string; _Bool: boolean);
    property Name: string read FName;
    property Bool: boolean read FBool write FBool;
  end;

{$DEFINE __DZ_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _ITEM_TYPE_ = TNameBool;
{$INCLUDE 't_dzObjectListTemplate.tpl'}

type
  {: Sorted list for storing TNameBool items }
  TNameBoolList = class(_DZ_OBJECT_LIST_TEMPLATE_)
  end;

implementation

uses
  SysUtils;

{$INCLUDE 't_dzObjectListTemplate.tpl'}

{ TNameBool }

constructor TNameBool.Create(const _Name: string; _Bool: boolean);
begin
  inherited Create;
  FName := _Name;
  FBool := _Bool;
end;

end.

