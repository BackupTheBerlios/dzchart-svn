program MyItemSorterTest;

{$APPTYPE CONSOLE}

{%File '..\..\Templates\t_dzListInterfaceTemplate.tpl'}
{%File '..\..\Templates\t_dzObjectListTemplate.tpl'}
{%File '..\..\Templates\t_dzListSorterTemplate.tpl'}

uses
  SysUtils,
  u_MyItem in '..\u_MyItem.pas',
  i_MyItemList in '..\i_MyItemList.pas',
  u_MyItemList in '..\u_MyItemList.pas',
  u_dzQuicksort in '..\..\Units\u_dzQuicksort.pas';

var
  List: TSortableMyItemList;

begin
  List := TSortableMyItemList.Create;
  List.Insert(TMyItem.Create(2));
  List.Insert(TMyItem.Create(1));
  List.Insert(TMyItem.Create(0));
  List.Sort;
  List.Free;
end.
