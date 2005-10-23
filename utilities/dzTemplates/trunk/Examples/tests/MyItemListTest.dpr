program MyItemListTest;

{$APPTYPE CONSOLE}

{%File '..\..\Templates\t_dzListInterfaceTemplate.tpl'}
{%File '..\..\Templates\t_dzListTemplate.tpl'}
{%File '..\..\Templates\t_dzObjectListTemplate.tpl'}

uses
  SysUtils,
  u_MyItemList in '..\u_MyItemList.pas',
  u_MyItem in '..\u_MyItem.pas',
  u_dzQuicksort in '..\..\Units\u_dzQuicksort.pas',
  i_MyItemList in '..\i_MyItemList.pas';

var
  MyItemList: TMyItemList;

begin
  MyItemList := TMyItemList.Create;
  MyItemList.Insert(TMyItem.Create(0));
  MyItemList.Insert(TMyItem.Create(1));
  MyItemList.Free;
end.
