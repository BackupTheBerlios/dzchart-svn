{$IFNDEF __HASH_TEMPLATE__}
unit t_dzHashTemplate;

interface

uses
  Classes,
  u_MyItem;

type
  _HASH_ANCESTOR_ = TObject; // or TInterfacedObject
  _HASH_ITEM_ = TMyItem;
const
  _HASH_EMPTY_ITEM_ = nil;

{$ENDIF __HASH_TEMPLATE__}

{$IFNDEF __HASH_TEMPLATE_SECOND_PASS__}

type
  _HASH_TEMPLATE_ = class(_HASH_ANCESTOR_)
  private
    FList: TStringList;
    procedure SetValues(const _Key: string; _Item: _HASH_ITEM_);
    function GetValues(const _Key: string): _HASH_ITEM_;
    function GetKeys(_Idx: integer): string;
    function GetItems(_Idx: integer): _HASH_ITEM_;
  public
    constructor Create;
    destructor Destroy; override;
    function Contains(const _Key: string): boolean;
    function Extract(const _Key: string): _HASH_ITEM_;
    function FindKeyOf(_Item: _HASH_ITEM_; out _Key: string): boolean;
    function GetAllKeys(_Keys: TStrings): integer;
    function Count: integer;
    property Keys[_Idx: integer]: string read GetKeys;
    property Items[_Idx: integer]: _HASH_ITEM_ read GetItems;
    property Values[const _Key: string]: _HASH_ITEM_ read GetValues write SetValues; default;
  end;

{$ENDIF __HASH_TEMPLATE_SECOND_PASS__}

{$IFNDEF __HASH_TEMPLATE__}
implementation
{$DEFINE __HASH_TEMPLATE_SECOND_PASS__}
{$ENDIF __HASH_TEMPLATE__}

{$IFDEF __HASH_TEMPLATE_SECOND_PASS__}

{ _HASH_TEMPLATE_ }

constructor _HASH_TEMPLATE_.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  Flist.Sorted := true;
  FList.Duplicates := dupError;
end;

destructor _HASH_TEMPLATE_.Destroy;
var
  i: integer;
begin
  if Assigned(FList) then begin
    for i := 0 to FList.Count - 1 do begin
      _HASH_ITEM_(FList.Objects[i]).Free;
    end;
  end;
  FList.Free;
  inherited;
end;

function _HASH_TEMPLATE_.Contains(const _Key: string): boolean;
var
  Idx: integer;
begin
  Result := FList.Find(_Key, Idx);
end;

function _HASH_TEMPLATE_.Count: integer;
begin
  Result := FList.Count;
end;

function _HASH_TEMPLATE_.Extract(const _Key: string): _HASH_ITEM_;
var
  Idx: integer;
begin
  if FList.Find(_Key, Idx) then begin
    Result := _HASH_ITEM_(FList.Objects[Idx]);
    FList.Delete(Idx);
  end else
    Result := _HASH_EMPTY_ITEM_;
end;

function _HASH_TEMPLATE_.FindKeyOf(_Item: _HASH_ITEM_;
  out _Key: string): boolean;
var
  i: integer;
begin
  for i := 0 to FList.Count - 1 do begin
    Result := FList.Objects[i] = _Item;
    if Result then begin
      _Key := FList[i];
      exit;
    end;
  end;
  Result := false;
end;

function _HASH_TEMPLATE_.GetAllKeys(_Keys: TStrings): integer;
begin
  if Assigned(_Keys) then
    _Keys.Assign(FList);
  Result := FList.Count;
end;

function _HASH_TEMPLATE_.GetItems(_Idx: integer): _HASH_ITEM_;
begin
  Result := _HASH_ITEM_(FList.Objects[_Idx]);
end;

function _HASH_TEMPLATE_.GetKeys(_Idx: integer): string;
begin
  Result := FList[_Idx];
end;

function _HASH_TEMPLATE_.GetValues(const _Key: string): _HASH_ITEM_;
var
  Idx: integer;
begin
  if FList.Find(_Key, Idx) then
    Result := _HASH_ITEM_(FList.Objects[Idx])
  else
    Result := _HASH_EMPTY_ITEM_;
end;

procedure _HASH_TEMPLATE_.SetValues(const _Key: string; _Item: _HASH_ITEM_);
var
  Idx: integer;
begin
  Assert(_Key <> '');
  if FList.Find(_Key, Idx) then begin
    if _Item = _HASH_EMPTY_ITEM_ then
      FList.Delete(Idx)
    else
      FList.Objects[Idx] := _Item;
  end else
    FList.AddObject(_Key, _Item);
end;

{$ENDIF __HASH_TEMPLATE_SECOND_PASS__}

{$IFNDEF __HASH_TEMPLATE__}
{$WARNINGS off}
end.
{$ELSE}
{$DEFINE __HASH_TEMPLATE_SECOND_PASS__}
{$ENDIF __HASH_TEMPLATE__}

