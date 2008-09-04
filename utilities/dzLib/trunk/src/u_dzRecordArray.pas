unit u_dzRecordArray;

interface

uses
  Windows,
  SysUtils,
  Classes;

type
  TRecordArray = class
  private
    FData: array of byte;
    FLength: integer;
    FRecordSize: Integer;
    FGrowBy: extended;
    function GetCapacity: integer; inline;
  public
    constructor Create(_RecordSize: integer; _InitialCapacity: integer = 0; _GrowBy: extended = 0.1);
    destructor Destroy; override;
    procedure SetItem(_Idx: integer; const _Item);
    procedure GetItem(_Idx: integer; var _Item);
    function GetItemPtr(_Idx: integer): PByte;
    procedure SetCapacity(_NewCapacity: integer);
    procedure SetLength(_NewLength: integer);
    procedure ShrinkToLength;
    property Length: integer read FLength;
    property RecordSize: integer read FRecordSize;
    property Capacity: integer read GetCapacity;
  end;

implementation

uses
  RTLConsts;

resourcestring
  RS_NEW_CAPACITY_CANNOT_BE_LESS_THAN_CURRENT_LENGTH = 'New capacity cannot be less than current length.';

{ TRecordArray }

constructor TRecordArray.Create(_RecordSize: integer; _InitialCapacity: integer = 0; _GrowBy: extended = 0.1);
begin
  inherited Create;
  FRecordSize := _RecordSize;
  FGrowBy := _GrowBy;
  SetCapacity(_InitialCapacity);
end;

destructor TRecordArray.Destroy;
begin
  System.SetLength(FData, 0);
  inherited;
end;

function TRecordArray.GetCapacity: integer;
begin
  Result := System.Length(FData) div FRecordSize;
end;

procedure TRecordArray.SetCapacity(_NewCapacity: integer);
var
  NewArraySize: integer;
begin
  NewArraySize := _NewCapacity * FRecordSize;
  if System.Length(FData) <> NewArraySize then begin
    if _NewCapacity < FLength then
      raise Exception.Create(RS_NEW_CAPACITY_CANNOT_BE_LESS_THAN_CURRENT_LENGTH);
    System.SetLength(FData, NewArraySize);
  end;
end;

procedure TRecordArray.SetItem(_Idx: integer; const _Item);
begin
  if (_Idx < 0) or (_Idx >= FLength) then
    raise EListError.CreateFmt(SListIndexError, [_Idx]);
  Move(_Item, FData[_Idx * FRecordSize], FRecordSize);
end;

procedure TRecordArray.GetItem(_Idx: integer; var _Item);
begin
  if (_Idx < 0) or (_Idx >= FLength) then
    raise EListError.CreateFmt(SListIndexError, [_Idx]);
  Move(FData[_Idx * FRecordSize], _Item, FRecordSize);
end;

function TRecordArray.GetItemPtr(_Idx: integer): PByte;
begin
  if (_Idx < 0) or (_Idx >= FLength) then
    raise EListError.CreateFmt(SListIndexError, [_Idx]);
  Result := @FData[_Idx * FRecordSize];
end;

procedure TRecordArray.SetLength(_NewLength: integer);
begin
  if _NewLength > Capacity then
    SetCapacity(_NewLength);
  FLength := _NewLength;
end;

procedure TRecordArray.ShrinkToLength;
begin
  SetCapacity(FLength);
end;

end.
