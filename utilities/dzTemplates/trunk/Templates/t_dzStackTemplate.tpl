{$IFNDEF __STACK_TEMPLATE__}
unit t_dzStackTemplate;

interface

uses
  Classes;

type
  _STACK_ITEM_ = integer;
  _STACK_CONTAINER_TYPE_ = TList; // or TInterfaceList
  _STACK_ANCESTOR_ = TObject; // or TInterfacedObject

{$ENDIF __STACK_TEMPLATE__}

{$IFNDEF __STACK_TEMPLATE_SECOND_PASS__}

type
  _STACK_TEMPLATE_ = class(_STACK_ANCESTOR_)
  private
    FList: _STACK_CONTAINER_TYPE_;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Push(_Item: _STACK_ITEM_);
    function Pop: _STACK_ITEM_;
    function Peek: _STACK_ITEM_;
    function IsEmpty: boolean;
    function Depth: integer;
  end;
  
{$ENDIF __STACK_TEMPLATE_SECOND_PASS__}

{$IFNDEF __STACK_TEMPLATE__}
implementation
{$DEFINE __STACK_TEMPLATE_SECOND_PASS__}
{$ENDIF __STACK_TEMPLATE__}

{$IFDEF __STACK_TEMPLATE_SECOND_PASS__}

{ _STACK_TEMPLATE_ }

constructor _STACK_TEMPLATE_.Create;
begin
  inherited Create;
  FList := _STACK_CONTAINER_TYPE_.Create;
end;

function _STACK_TEMPLATE_.Depth: integer;
begin
  Result := FList.Count;
end;

destructor _STACK_TEMPLATE_.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure _STACK_TEMPLATE_.Push(_Item: _STACK_ITEM_);
begin
  FList.Add(pointer(_Item));
end;

function _STACK_TEMPLATE_.Pop: _STACK_ITEM_;
begin
  Assert(FList.Count >= 0);

  Result := _STACK_ITEM_(FList[FList.Count - 1]);
  FList.Delete(FList.Count - 1);
end;

function _STACK_TEMPLATE_.Peek: _STACK_ITEM_;
begin
  Assert(FList.Count >= 0);

  Result := _STACK_ITEM_(FList[FList.Count - 1]);
end;

function _STACK_TEMPLATE_.IsEmpty: boolean;
begin
  Result := FList.Count = 0;
end;

{$ENDIF __STACK_TEMPLATE_SECOND_PASS__}

{$IFNDEF __STACK_TEMPLATE__}
{$WARNINGS off}
end.
{$ELSE __STACK_TEMPLATE__}

{$DEFINE __STACK_TEMPLATE_SECOND_PASS__}

{$ENDIF __STACK_TEMPLATE__}

