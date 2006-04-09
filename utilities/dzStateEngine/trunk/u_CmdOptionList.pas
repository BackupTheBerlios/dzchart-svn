unit u_CmdOptionList;

interface

uses
  Classes;

type
  {: stores options and parameters, a parameter is an option without a name }
  TCmdOption = class
  private
    FParameter: string;
    FName: string;
  public
    {: Create a TCmdOption instance
       @param Name is the name of the option, empty for paramters }
    constructor Create(const _Name: string);
    {: The Option's name, empty if it an parameter }
    property Name: string read FName write FName;
    { the option's parameter or the parameter itself if it is not an option }
    property Parameter: string read FParameter write FParameter;
  end;

{$DEFINE __DZ_OBJECT_LIST_TEMPLATE__}
type
  _LIST_ANCESTOR_ = TObject;
  _LIST_CONTAINER_ = TList;
  _ITEM_TYPE_ = TCmdOption;
{$INCLUDE 't_dzObjectListTemplate.tpl'}

type
  {: List for storing TCmdOption items }
  TCmdOptionList = class(_DZ_OBJECT_LIST_TEMPLATE_)

  end;

implementation

{$INCLUDE 't_dzObjectListTemplate.tpl'}
{ TCmdOption }

constructor TCmdOption.Create(const _Name: string);
begin
  inherited Create;
  FName := _Name;
end;

end.

