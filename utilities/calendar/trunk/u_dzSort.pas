// ***************************************************************
// Description:  This unit implements a Quicksort procedure that can
//               be used to sort anything as well as a binary sarch
//               function. There are two versions of each,
//               one that takes method pointers and another one
//               that takes normal function/procedure pointers.
// Usage:        Call Quicksort with two function/method pointers for
//               comparing and swapping two elements.
//               Examples: Quicksort(@dataarray, 0, Count-1, CompareItems, SwapItems);
//                         Quicksort(0, Count-1, self.CompareItems, self.SwapItems);
//               Call BinarySearch with one function/method pointer that
//               compares an index to the Item sought.
//               Examples: ItemIdx := BinarySearch(Key, 0, count-1, Self.CompareToKey);
//                         ItemIdx := BinarySarch(@DataArray, Key, 0, Count-1, CompareToKey)
// Defines:
// ***************************************************************

unit u_dzSort;

interface

type
  // for quicksort
  TCompareItemsFunc = function(_Data: pointer; _Idx1, _Idx2: integer): integer;
  TSwapItemsProc = procedure(_Data: pointer; _Idx1, _Idx2: integer);

  TCompareItemsMeth = function(_Idx1, _Idx2: integer): integer of object;
  TSwapItemsMeth = procedure(_Idx1, _Idx2: integer) of object;

  // for binary search
  TCompareToItemMeth = function(_Key: pointer; _Idx: integer): integer of object;
  TCompareToItemFunc = function(_Data: pointer; _Key: pointer; _Idx: integer): integer;

procedure QuickSort(_Left, _Right: integer; _CompareMeth: TCompareItemsMeth;
  _SwapMeth: TSwapItemsMeth); overload;
procedure QuickSort(_Data: pointer; _Left, _Right: integer; _CompareFunc: TCompareItemsFunc;
  _SwapProc: TSwapItemsProc); overload;

function BinarySearch(_Left, _Right: integer; var _Index: integer;
  _Key: pointer; _CompareMeth: TCompareToItemMeth;
  _Duplicates: boolean = false): boolean; overload;
function BinarySearch(_Left, _Right: integer; var _Index: integer;
  _Key: pointer; _Data: pointer; _CompareFunc: TCompareToItemFunc;
  _Duplicates: boolean = false): boolean; overload;

implementation

procedure QuickSort(_Left, _Right: integer; _CompareMeth: TCompareItemsMeth;
  _SwapMeth: TSwapItemsMeth);
var
  I, J, P: Integer;
begin
  if _Left >= _Right then
    exit;
  repeat
    I := _Left;
    J := _Right;
    P := (_Left + _Right) shr 1;
    repeat
      while _CompareMeth(I, P) < 0 do
        Inc(I);
      while _CompareMeth(J, P) > 0 do
        Dec(J);
      if I <= J then
        begin
          if I < J then
            _SwapMeth(I, J);
          if P = I then
            P := J
          else if P = J then
            P := I;
          Inc(I);
          Dec(J);
        end;
    until I > J;
    if _Left < J then
      QuickSort(_Left, J, _CompareMeth, _SwapMeth);
    _Left := I;
  until I >= _Right;
end;

function BinarySearch(_Left, _Right: integer; var _Index: integer;
  _Key: pointer; _CompareMeth: TCompareToItemMeth;
  _Duplicates: boolean = false): boolean;
var
  p, c: LongInt;
begin
  Result := False;
  while _Left <= _Right do
    begin
      p := (_Left + _Right) shr 1;
      c := _CompareMeth(_Key, p);
      if c > 0 then
        _Left := p + 1
      else
        begin
          _Right := p - 1;
          if c = 0 then
            begin
              Result := True;
              if not _Duplicates then
                _Left := p;
            end;
        end;
    end;
  _Index := _Left;
end;

// Do not look any further, this is a bit ugly. ;-)

type
  TQuicksortHack = class
  protected
    fData: pointer;
    fCompareFunc: TCompareItemsFunc;
    fSwapProc: TSwapItemsProc;
  public
    constructor Create(_Data: pointer; _CompareFunc: TCompareItemsFunc;
      _SwapProc: TSwapItemsProc);
    function Compare(_Idx1, _Idx2: integer): integer;
    procedure Swap(_Idx1, _Idx2: integer);
  end;

procedure QuickSort(_Data: pointer; _Left, _Right: integer; _CompareFunc: TCompareItemsFunc;
  _SwapProc: TSwapItemsProc);
var
  HackObj: TQuicksortHack;
begin
  HackObj := TQuicksortHack.Create(_Data, _CompareFunc, _SwapProc);
  try
    QuickSort(_Left, _Right, HackObj.Compare, HackObj.Swap);
  finally
    HackObj.Free;
  end;
end;

type
  TSearchHack = class
  protected
    fData: pointer;
    fCompareFunc: TCompareToItemFunc;
  public
    constructor Create(_Data: pointer; _CompareFunc: TCompareToItemFunc);
    function CompareToItem(_Key: pointer; _Idx: integer): integer;
  end;

function BinarySearch(_Left, _Right: integer; var _Index: integer;
  _Key: pointer; _Data: pointer; _CompareFunc: TCompareToItemFunc;
  _Duplicates: boolean = false): boolean;
var
  HackObj: TSearchHack;
begin
  HackObj := TSearchHack.Create(_Data, _CompareFunc);
  try
    Result := BinarySearch(_Left, _Right, _Index, _Key, HackObj.CompareToItem,
      _Duplicates);
  finally
    HackObj.Free;
  end;
end;

{ TQuicksortHack }

constructor TQuicksortHack.Create(_Data: pointer;
  _CompareFunc: TCompareItemsFunc; _SwapProc: TSwapItemsProc);
begin
  fData := _Data;
  fCompareFunc := _CompareFunc;
  fSwapProc := _SwapProc;
end;

function TQuicksortHack.Compare(_Idx1, _Idx2: integer): integer;
begin
  Result := fCompareFunc(fData, _Idx1, _Idx2);
end;

procedure TQuicksortHack.Swap(_Idx1, _Idx2: integer);
begin
  fSwapProc(fData, _Idx1, _Idx2);
end;

{ TSearchHack }

constructor TSearchHack.Create(_Data: pointer;
  _CompareFunc: TCompareToItemFunc);
begin
  inherited Create;
  fData := _Data;
  fCompareFunc := _CompareFunc;
end;

function TSearchHack.CompareToItem(_Key: pointer; _Idx: integer): integer;
begin
  Result := fCompareFunc(fData, _Key, _Idx);
end;

end.
