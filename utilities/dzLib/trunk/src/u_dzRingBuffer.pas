unit u_dzRingBuffer;

{$I jedi.inc}

interface

uses
  Windows,
  SysUtils;

type
  EdzRingBuffer = class(Exception);
  EBufferFull = class(EdzRingBuffer);
  EBufferEmpty = class(EdzRingBuffer);
  EIndexOutOfBounds = class(EdzRingBuffer);

type
  TdzCustomRingBuffer = class
  protected
    {: size of one element stored in this buffer }
    fElementSize: integer;
    {: length of the buffer }
    fLength: integer;
    {: pointer to a memory block that stores the buffer }
    fBuffer: PByte;
    {: position of the first element stored in the buffer,
       if fFirstUsed = fFirstFree the buffer is empty }
    fFirstUsed: integer;
    {: position of the next element bo be stored in the buffer
       if fFirstUsed = fFirstFree the buffer is empty }
    fFirstFree: integer;
    {: number of elements stored in the buffer }
    fElementCount: integer;
    {: is called by the destructor to do any finalization that might be
       necessary for the elements stored in the buffer. Does nothing here. }
    procedure FinalizeElements; virtual;
    {: inserts the given Element in front of the buffer }
    procedure InsertFront(const _Element); virtual;
    {: inserts the given Element at the end of the buffer }
    procedure InsertEnd(const _Element); virtual;
    {: extracts the first Element from the buffer }
    procedure ExtractFront(var _Element); virtual;
    {: extracts the last Element from the buffer }
    procedure ExtractEnd(var _Element); virtual;
    {: gets the Element with the index Idx }
    procedure GetElement(_Idx: integer; var _Element); virtual;
    {: sets the Element with the index Idx, note: No finalization is done
       for the element previously stored at Idx. }
    procedure SetElement(_Idx: integer; const _Element); virtual;
    {: gets the first Element from the buffer }
    procedure GetFirst(var _Element); virtual;
    {: gets the last Element from the buffer }
    procedure GetLast(var _Element); virtual;
    {: checks whether there is enough space for another element in the buffer }
    procedure CheckFull; virtual;
    {: checks whether there are any elements in the buffer }
    procedure CheckEmpty; virtual;
    {: returns true, if the buffer is full }
    function IsFull: boolean; virtual;
    {: returns true, if the buffer is empty }
    function IsEmpty: boolean; virtual;
    {: returns the number of elements stored in the buffer }
    function GetCount: integer; virtual;
    {: deletes all elements from the buffer }
    procedure Clear; virtual;
  public
    {: creates a ringbuffer with enough space to store Length elements of
       ElementSize bytes size }
    constructor Create(_ElementSize: integer; _Length: integer);
    {: calls FinalizeElements and frees the memory allocated for the buffer }
    destructor Destroy; override;
  end;

type
  {: publishes all methods of TdzCustomRingBuffer }
  TdzRingBuffer = class(TdzCustomRingbuffer)
  public
    {: inserts the given Element in front of the buffer }
    procedure InsertFront(const _Element); override;
    {: inserts the given Element at the end of the buffer }
    procedure InsertEnd(const _Element); override;
    {: extracts the first Element from the buffer }
    procedure ExtractFront(var _Element); override;
    {: extracts the last Element from the buffer }
    procedure ExtractEnd(var _Element); override;
    {: gets the Element with the index Idx }
    procedure GetElement(_Idx: integer; var _Element); override;
    {: sets the Element with the index Idx, note: No finalization is done
       for the element previously stored at Idx. }
    procedure SetElement(_Idx: integer; const _Element); override;
    {: gets the first Element from the buffer }
    procedure GetFirst(var _Element); override;
    {: gets the last Element from the buffer }
    procedure GetLast(var _Element); override;
    {: checks whether there is enough space for another element in the buffer }
    procedure CheckFull; override;
    {: checks whether there are any elements in the buffer }
    procedure CheckEmpty; override;
    {: returns true, if the buffer is full }
    function IsFull: boolean; override;
    {: returns true, if the buffer is empty }
    function IsEmpty: boolean; override;
    {: returns the number of elements stored in the buffer }
    function GetCount: integer; override;
    {: deletes all elements from the buffer }
    procedure Clear; override;
  end;

type
  {: makes only those methods public that are useful for a stack }
  TdzRingStack = class(TdzCustomRingbuffer)
  public
    {: inserts the given Element at the end of the buffer }
    procedure InsertEnd(const _Element); override;
    {: extracts the last Element from the buffer }
    procedure ExtractEnd(var _Element); override;
    {: gets the Element with the index Idx }
    procedure GetElement(_Idx: integer; var _Element); override;
    {: sets the Element with the index Idx, note: No finalization is done
       for the element previously stored at Idx. }
    procedure SetElement(_Idx: integer; const _Element); override;
    {: gets the last Element from the buffer }
    procedure GetLast(var _Element); override;
    {: returns true, if the buffer is full }
    function IsFull: boolean; override;
    {: returns true, if the buffer is empty }
    function IsEmpty: boolean; override;
    {: returns the number of elements stored in the buffer }
    function GetCount: integer; override;
    {: deletes all elements from the buffer }
    procedure Clear; override;
  end;

type
  {: makes only those methods public that are usefull for a queue }
  TdzRingQueue = class(TdzCustomRingbuffer)
    {: inserts the given Element at the end of the buffer }
    procedure InsertEnd(const _Element); override;
    {: extracts the first Element from the buffer }
    procedure ExtractFront(var _Element); override;
    {: gets the Element with the index Idx }
    procedure GetElement(_Idx: integer; var _Element); override;
    {: sets the Element with the index Idx, note: No finalization is done
       for the element previously stored at Idx. }
    procedure SetElement(_Idx: integer; const _Element); override;
    {: gets the first Element from the buffer }
    procedure GetFirst(var _Element); override;
    {: returns true, if the buffer is full }
    function IsFull: boolean; override;
    {: returns true, if the buffer is empty }
    function IsEmpty: boolean; override;
    {: returns the number of elements stored in the buffer }
    function GetCount: integer; override;
    {: deletes all elements from the buffer }
    procedure Clear; override;
  end;

implementation

resourcestring
  RS_INDEX_OUT_OF_BOUNDS_D = 'Index %d out of bounds.';
  RS_BUFFER_IS_FULL = 'Buffer is full';
  RS_BUFFER_IS_EMPTY = 'Buffer is empty';

{ TdzCustomRingBuffer }

constructor TdzCustomRingBuffer.Create(_ElementSize, _Length: integer);
begin
  inherited Create;
  fLength := _Length;
  fElementSize := _ElementSize;
  fFirstUsed := 0;
  fFirstFree := 0;
  GetMem(fBuffer, fLength * fElementSize);
end;

destructor TdzCustomRingBuffer.Destroy;
begin
  if Assigned(fBuffer) and (fLength > 0) then begin
    FinalizeElements;
    FreeMem(fBuffer);
  end;
  inherited;
end;

procedure TdzCustomRingBuffer.FinalizeElements;
begin
  // does nothing, override if elements need finalization
end;

procedure TdzCustomRingBuffer.InsertFront(const _Element);
var
  p: PByte;
begin
  CheckFull;
  fFirstUsed := (fFirstUsed - 1) mod fLength;
  p := fBuffer;
  Inc(p, fFirstUsed * fElementSize);
  Move(_Element, p^, fElementSize);
  Inc(fElementCount);
end;

procedure TdzCustomRingBuffer.InsertEnd(const _Element);
var
  p: PByte;
begin
  CheckFull;
  p := fBuffer;
  Inc(p, fFirstFree * fElementSize);
  Move(_Element, p^, fElementSize);
  fFirstFree := (fFirstFree + 1) mod fLength;
  Inc(fElementCount);
end;

procedure TdzCustomRingBuffer.ExtractFront(var _Element);
var
  p: PByte;
begin
  CheckEmpty;
  p := fBuffer;
  Inc(p, fFirstUsed * fElementSize);
  Move(p^, _Element, fElementSize);
  fFirstUsed := (fFirstUsed + 1) mod fLength;
  Dec(fElementCount);
end;

procedure TdzCustomRingBuffer.ExtractEnd(var _Element);
var
  p: PByte;
begin
  CheckEmpty;
  fFirstFree := (fFirstFree - 1) mod fLength;
  p := fBuffer;
  Inc(p, fFirstFree * fElementSize);
  Move(p^, _Element, fElementSize);
  Dec(fElementCount);
end;

procedure TdzCustomRingBuffer.GetElement(_Idx: integer; var _Element);
var
  p: PByte;
begin
  if (fFirstUsed + _Idx) mod fLength >= fFirstFree then
    raise EIndexOutOfBounds.CreateFmt(RS_INDEX_OUT_OF_BOUNDS_D, [_Idx]);
  p := fBuffer;
  Inc(p, (fFirstUsed + _Idx) * fElementSize);
  Move(p^, _Element, fElementSize);
end;

procedure TdzCustomRingBuffer.SetElement(_Idx: integer; const _Element);
var
  p: PByte;
begin
  if (fFirstUsed + _Idx) mod fLength >= fFirstFree then
    raise EIndexOutOfBounds.CreateFmt(RS_INDEX_OUT_OF_BOUNDS_D, [_Idx]);
  p := fBuffer;
  Inc(p, (fFirstUsed + _Idx) * fElementSize);
  Move(_Element, p^, fElementSize);
end;

procedure TdzCustomRingBuffer.GetFirst(var _Element);
begin
  GetElement(0, _Element);
end;

procedure TdzCustomRingBuffer.GetLast(var _Element);
begin
  GetElement(GetCount - 1, _Element);
end;

function TdzCustomRingBuffer.IsFull: boolean;
begin
  Result := fElementCount >= fLength;
end;

procedure TdzCustomRingBuffer.CheckFull;
begin
  if IsFull then
    raise EBufferFull.Create(RS_BUFFER_IS_FULL);
end;

function TdzCustomRingBuffer.IsEmpty: boolean;
begin
  Result := fElementCount = 0;
end;

procedure TdzCustomRingBuffer.CheckEmpty;
begin
  if IsEmpty then
    raise EBufferEmpty.Create(RS_BUFFER_IS_EMPTY);
end;

function TdzCustomRingBuffer.GetCount: integer;
begin
  Result := fElementCount;
end;

procedure TdzCustomRingBuffer.Clear;
begin
  FinalizeElements;
  fFirstUsed := 0;
  fFirstFree := 0;
  fElementCount := 0;
end;

{ TdzRingBuffer }

procedure TdzRingBuffer.CheckEmpty;
begin
  inherited;
end;

procedure TdzRingBuffer.CheckFull;
begin
  inherited;
end;

procedure TdzRingBuffer.Clear;
begin
  inherited;
end;

procedure TdzRingBuffer.ExtractEnd(var _Element);
begin
  inherited;
end;

procedure TdzRingBuffer.ExtractFront(var _Element);
begin
  inherited;
end;

function TdzRingBuffer.GetCount: integer;
begin
  Result := inherited GetCount;
end;

procedure TdzRingBuffer.GetElement(_Idx: integer; var _Element);
begin
  inherited;
end;

procedure TdzRingBuffer.GetFirst(var _Element);
begin
  inherited;
end;

procedure TdzRingBuffer.GetLast(var _Element);
begin
  inherited;
end;

procedure TdzRingBuffer.InsertEnd(const _Element);
begin
  inherited;
end;

procedure TdzRingBuffer.InsertFront(const _Element);
begin
  inherited;
end;

function TdzRingBuffer.IsEmpty: boolean;
begin
  Result := inherited IsEmpty;
end;

function TdzRingBuffer.IsFull: boolean;
begin
  Result := inherited IsFull;
end;

procedure TdzRingBuffer.SetElement(_Idx: integer; const _Element);
begin
  inherited;
end;

{ TdzRingStack }

procedure TdzRingStack.Clear;
begin
  inherited;
end;

procedure TdzRingStack.ExtractEnd(var _Element);
begin
  inherited;
end;

function TdzRingStack.GetCount: integer;
begin
  Result := inherited GetCount;
end;

procedure TdzRingStack.GetElement(_Idx: integer; var _Element);
begin
  inherited;
end;

procedure TdzRingStack.GetLast(var _Element);
begin
  inherited;
end;

procedure TdzRingStack.InsertEnd(const _Element);
begin
  inherited;
end;

function TdzRingStack.IsEmpty: boolean;
begin
  Result := inherited IsEmpty;
end;

function TdzRingStack.IsFull: boolean;
begin
  Result := inherited IsFull;
end;

procedure TdzRingStack.SetElement(_Idx: integer; const _Element);
begin
  inherited;
end;

{ TdzRingQueue }

procedure TdzRingQueue.Clear;
begin
  inherited;
end;

procedure TdzRingQueue.ExtractFront(var _Element);
begin
  inherited;
end;

function TdzRingQueue.GetCount: integer;
begin
  Result := inherited GetCount;
end;

procedure TdzRingQueue.GetElement(_Idx: integer; var _Element);
begin
  inherited;
end;

procedure TdzRingQueue.GetFirst(var _Element);
begin
  inherited;
end;

procedure TdzRingQueue.InsertEnd(const _Element);
begin
  inherited;
end;

function TdzRingQueue.IsEmpty: boolean;
begin
  Result := inherited IsEmpty;
end;

function TdzRingQueue.IsFull: boolean;
begin
  Result := inherited IsFull;
end;

procedure TdzRingQueue.SetElement(_Idx: integer; const _Element);
begin
  inherited;
end;

end.

