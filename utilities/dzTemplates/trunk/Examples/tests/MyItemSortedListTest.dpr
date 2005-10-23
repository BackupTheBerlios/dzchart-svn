program MyItemSortedListTest;

{$APPTYPE CONSOLE}

{%File '..\..\Templates\t_dzListInterfaceTemplate.tpl'}
{%File '..\..\Templates\t_dzListTemplate.tpl'}
{%File '..\..\Templates\t_dzSortedObjectListTemplate.tpl'}
{%File '..\..\Templates\t_dzSortedListInterfaceTemplate.tpl'}
{%File '..\..\Templates\t_dzSortedListTemplate.tpl'}
{%File '..\..\Templates\t_dzObjectListTemplate.tpl'}
{%File '..\..\Templates\t_dzIntegerSortedObjectListTemplate.tpl'}

uses
  SysUtils,
  i_MyItemSortedList in '..\i_MyItemSortedList.pas',
  u_MyItemSortedList in '..\u_MyItemSortedList.pas',
  u_MyItem in '..\u_MyItem.pas',
  u_dzQuicksort in '..\..\Units\u_dzQuicksort.pas';

var
  MyItemSortedList: TMyItemSortedList;

begin
  MyItemSortedList := TMyItemSortedList.Create;
  MyItemSortedList.Insert(TMyItem.Create(0));
  MyItemSortedList.Insert(TMyItem.Create(1));
  MyItemSortedList.Free;
end.
