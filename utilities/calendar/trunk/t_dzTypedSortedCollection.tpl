{$IFNDEF TYPED_SORTED_COLLECTION_TEMPLATE}
unit t_dzTypedSortedCollection;

interface

uses
  SysUtils,
  u_hkQuicksort,
  u_HkCollection;

type
  _COLLECTION_ITEM_ = pointer;
  _COLLECTION_KEY_ = pointer;

{$ENDIF TYPED_SORTED_COLLECTION_TEMPLATE}

{$IFNDEF TYPED_SORTED_COLLECTION_TEMPLATE_SECOND_PASS}
type
  _TYPED_SORTED_COLLECTION_ = class
  protected
    fColl: TdzSortedCollection;
    function GetItems(_Idx: integer): _COLLECTION_ITEM_;
    function GetCount: integer;
    procedure FreeItem(_Item: _COLLECTION_ITEM_); virtual;
    function GetDuplicates: boolean;
    procedure SetDuplicates(_Duplicates: boolean);
    function Compare(const _Key1, _Key2: _COLLECTION_KEY_): integer; virtual; abstract;
    function KeyOf(_Item: _COLLECTION_ITEM_): _COLLECTION_KEY_; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function Insert(_Item: _COLLECTION_ITEM_): integer;
    procedure DeleteAll;
    procedure AtDelete(_Idx: integer);
    function IndexOf(_Item: _COLLECTION_ITEM_): integer;
    procedure AtInsert(_Idx: integer; _Item: _COLLECTION_ITEM_);
    procedure Delete(_Item: _COLLECTION_ITEM_);
    function isValidIndex(_Idx: integer): boolean;
    function Search(_Key: _COLLECTION_KEY_; var _Index: integer): boolean; virtual;

    property Items[_Idx: integer]: _COLLECTION_ITEM_ read GetItems; default;
    property Count: integer read GetCount;
    property Duplicates: boolean read GetDuplicates write SetDuplicates;
  end;

{$ENDIF TYPED_SORTED_COLLECTION_TEMPLATE_SECOND_PASS}

{$IFNDEF TYPED_SORTED_COLLECTION_TEMPLATE}
implementation
{$DEFINE TYPED_SORTED_COLLECTION_TEMPLATE_SECOND_PASS}
{$ENDIF TYPED_SORTED_COLLECTION_TEMPLATE}

{$IFDEF TYPED_SORTED_COLLECTION_TEMPLATE_SECOND_PASS}

type
  TOwnedSortedColl = class(TdzSortedCollection)
    fOwner: _TYPED_SORTED_COLLECTION_;
    function KeyOf(_Item: TObject): pointer; override;
    function Compare(_Key1, _Key2: pointer): integer; override;
    constructor Create(_Parent: _TYPED_SORTED_COLLECTION_);
  end;

constructor _TYPED_SORTED_COLLECTION_.Create;
begin
  inherited;
  fColl := TOwnedSortedColl.Create(self);
end;

destructor _TYPED_SORTED_COLLECTION_.Destroy;
begin
  if assigned(fColl) then
    begin
      DeleteAll;
      fColl.Free;
      fColl := nil;
    end;
  inherited;
end;

function _TYPED_SORTED_COLLECTION_.GetCount: integer;
begin
  Result := fColl.Count;
end;

function _TYPED_SORTED_COLLECTION_.GetItems(_Idx: integer): _COLLECTION_ITEM_;
begin
  Result := _COLLECTION_ITEM_(fColl[_Idx]);
end;

function _TYPED_SORTED_COLLECTION_.Insert(_Item: _COLLECTION_ITEM_): integer;
begin
  Result := fColl.Insert(pointer(_Item));
end;

procedure _TYPED_SORTED_COLLECTION_.DeleteAll;
var
  i: integer;
begin
  for i := 0 to pred(fColl.Count) do
    FreeItem(_COLLECTION_ITEM_(fColl[i]));
  fColl.DeleteAll;
end;

procedure _TYPED_SORTED_COLLECTION_.AtDelete(_Idx: integer);
begin
  fColl.AtDelete(_Idx);
end;

function _TYPED_SORTED_COLLECTION_.IndexOf(_Item: _COLLECTION_ITEM_): integer;
begin
  Result := fColl.IndexOf(pointer(_Item));
end;

procedure _TYPED_SORTED_COLLECTION_.AtInsert(_Idx: integer; _Item: _COLLECTION_ITEM_);
begin
  fColl.AtInsert(_Idx, pointer(_Item));
end;

procedure _TYPED_SORTED_COLLECTION_.Delete(_Item: _COLLECTION_ITEM_);
begin
  fColl.Delete(pointer(_Item));
end;

procedure _TYPED_SORTED_COLLECTION_.FreeItem(_Item: _COLLECTION_ITEM_);
begin
  _Item.Free;
end;

function _TYPED_SORTED_COLLECTION_.GetDuplicates: boolean;
begin
  Result := fColl.Duplicates;
end;

procedure _TYPED_SORTED_COLLECTION_.SetDuplicates(_Duplicates: boolean);
begin
  fColl.Duplicates := _Duplicates;
end;

function _TYPED_SORTED_COLLECTION_.Search(_Key: _COLLECTION_KEY_; var _Index: integer): boolean;
begin
  Result := fColl.Search(pointer(_Key), _Index);
end;

function _TYPED_SORTED_COLLECTION_.isValidIndex(_Idx: integer): boolean;
begin
  Result := fColl.isValidIndex(_Idx);
end;

{ TOwnedSortedColl }

constructor TOwnedSortedColl.Create(_Parent: _TYPED_SORTED_COLLECTION_);
begin
  inherited Create;
  fOwner := _Parent;
end;

function TOwnedSortedColl.KeyOf(_Item: TObject): pointer;
begin
  Result := pointer(fOwner.KeyOf(_COLLECTION_ITEM_(_Item)));
end;

function TOwnedSortedColl.Compare(_Key1, _Key2: pointer): integer;
begin
  Result := fOwner.Compare(_COLLECTION_KEY_(_Key1), _COLLECTION_KEY_(_Key2));
end;

{$WARNINGS off}
{$IFNDEF TYPED_SORTED_COLLECTION_TEMPLATE}
end.
{$ENDIF TYPED_SORTED_COLLECTION_TEMPLATE}
{$ENDIF TYPED_SORTED_COLLECTION_TEMPLATE_SECOND_PASS}
{$DEFINE TYPED_SORTED_COLLECTION_TEMPLATE_SECOND_PASS}

