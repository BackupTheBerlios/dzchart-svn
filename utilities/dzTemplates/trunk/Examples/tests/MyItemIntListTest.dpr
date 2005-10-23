program MyItemIntListTest;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  u_MyItem in '..\u_MyItem.pas',
  u_MyItemIntList in '..\u_MyItemIntList.pas';

var
  MyItemIntList: TMyItemIntList;

begin
  MyItemIntList := TMyItemIntList.Create;
  MyItemIntList.Insert(TMyItem.Create(0));
  MyItemIntList.Insert(TMyItem.Create(1));
  MyItemIntList.Free;
end.
