program MyItemQueueTest;

{$APPTYPE CONSOLE}

{%File '..\..\Templates\t_dzQueueTemplate.tpl'}

uses
  SysUtils,
  u_MyItemQueue in '..\u_MyItemQueue.pas',
  u_MyItem in '..\u_MyItem.pas';

var
  Queue: TMyItemQueue;
begin
  Queue := TMyItemQueue.Create;
  Queue.Push(TMyItem.Create(0));
  Assert(not Queue.IsEmpty);
  Assert(Queue.Count = 1);
  Queue.Push(TMyItem.Create(1));
  Assert(not Queue.IsEmpty);
  Assert(Queue.Count = 2);
  with Queue.Pop do
    begin
      Assert(Key = 0);
      Free;
    end;
  Assert(not Queue.IsEmpty);
  Assert(Queue.Count = 1);
  with Queue.Pop do
    begin
      Assert(Key = 1);
      Free;
    end;
  Assert(Queue.IsEmpty);
  Assert(Queue.Count = 0);
  Queue.Free;
end.
