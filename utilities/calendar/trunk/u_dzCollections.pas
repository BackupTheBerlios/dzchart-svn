{: implements several class based collections
   @author       twm
   @Usage        TdzCollection: Override FreeItem if the items do not descend
                                from TObject.

                 TdzSortedCollection: Override FreeItem, Compare and KeyOf
                                      as needed.

                 TdzIntegerSortedCollection: Override FreeItem and KeyOf as needed

                 TdzLongintCollection: No overrides needed

                 TdzStrCollection: No overrides needed

   Last updated: 02.11.2000
}

unit u_dzCollections;

interface

uses
  Sysutils;

const
{: Maximum TCollection size }
  MAX_COLLECTION_SIZE = MaxInt div SizeOf(pointer);

{ TCollection exceptions }
type
  {: Parent of all exceptions raised in u_Collections }
  ECollection = class(Exception);
  {: Raised by several methods if the index passed to them is too large or
     less than 0.}
  ECollectionIndexOutOfRange = class(ECollection);
  {: Raised if the collection gets too large (> MAX_COLLECTION_SIZE) }
  ECollectionOverflow = class(ECollection);

type
{ TCollection types }

  {: Used internally by TdzCollection to store pointers to objects. }
  PItemList = ^TItemList;
  {: Used internally by TdzCollection to store pointers to objects. }
  TItemList = array[0..MAX_COLLECTION_SIZE - 1] of TObject;

  {: Function method pointer type used by FirstThat and LastThat to check the
     items.
     @param Item is the TObject to test.
     @returns true, if the condition is met by the Item. }
  TConditionMeth = function(_Item: TObject): boolean of object;
  {: Procedure method pointer type used by the ForEach method.
     @param Item is the TObject to process. }
  TActionMeth = procedure(_Item: TObject) of object;
  {: Function pointer type used by FirstThat and LastThat to check the
     items.
     @param Item is the TObject to test.
     @returns true, if the condition is met by the Item. }
  TConditionFunc = function(_Item: TObject): boolean;
  {: Procedure method pointer type used by the ForEach method.
     @param Item is the TObject to process. }
  TActionProc = procedure(_Item: TObject);

  {: Type of the BeforeDelete event.
     @param Idx is the index of the item that is about to be deleted.
     @param Item is the item that is about to be deleted. }
  TBeforeDelete = procedure(_Idx: integer; _Item: TObject) of object;
  {: Type of the AfterInsert event.
     @param Idx is the index of the item that hast just been inserted.
     @param Item is the item that has just been inserted. }
  TAfterInsert = procedure(_Idx: integer; _Item: TObject) of object;
  {: Type of the OnError event.
     @param Idx is the index of the item causing the error or -1 if not available.
     @param Item is the item causing the error or nil if not available.
     @param Error is the error message for the error.
     @param Resolved can be set to true if the error condition was resolved,
            <strong>Note that just setting this parameter to true will not handle
            the problem but most likely will make the program crash.</strong> }
  TOnError = procedure(_Idx: integer; _Item: TObject; const _Error: string;
    var _Resolved: boolean) of object;

{ TdzCollection }

  {: The good old collection as introduced by Borland's Turbo Vision in Turbo
     Pascal 5.
     This is basically a port of the old object style implementation to the
     new class style model. There are a few new features.
     A collection is used to store objects. It normally "owns" these objects,
     e.g. when the collection is destroyed it will automatically destroy the
     objects stored in it.
     Use Insert to insert items into the collection, use the Items property to
     access them by index. }
  TdzCollection = class
  private
    {: Stores the OnError event }
    fOnError: TOnError;
    {: Stores the AfterInsert event }
    fAfterInsert: TAfterInsert;
    {: Stores the BeforeDelete event }
    fBeforeDelete: TBeforeDelete;
    {: Stores the items in the collection. This list is dynamically allocated
       and re-allocated when more space is needed. }
    fItems: PItemList;
    {: Stores the number of items in the collection. The Count property
       manages this field. }
    fCount: LongInt;
    {: Stores the number of items that can be stored in the currently allocated
       fItems list. The Limit property manages this field. }
    fLimit: LongInt;
    {: Stores the number of items the fItems list will grow if it runs out of
       space. The Delta property manages this field. }
    fDelta: LongInt;
  protected
    {: Get-Method for the Items property.
       Gets the item at index Idx.
       @param Idx is the index of the item to get.
       @returns the TObject at index Idx. }
    function GetItem(_Idx: LongInt): TObject; virtual;
    {: Set-Method for the Items property.
       Sets the Item a index Idx.
       @param Idx is the index to place the item in.
       @param Item is the TObject to store. }
    procedure SetItem(_Idx: LongInt; _Item: TObject); virtual;
    {: Change the allocated size of fItems to allow storing Limit items
       in the item list. If the list already stores more than Limit items,
       it is set to exactly match the number of items. So SetLimit(0) will
       shrink the memory usage to the amount actually needed.
       @param Limit is the number of items to make space for. }
    procedure SetLimit(_Limit: LongInt); virtual;
    {: Set the Count property. This is for internal use only.
       @param Count is the new value for Count. }
    procedure SetCount(_Count: LongInt); virtual;
    {: Checks whether the passed Index is within the allowed range (0..Count-1).
       If the check fails it calls the OnError event, if assigned, and raises
       an ECollectionIndexOutOfRange error.
       @param Index is the index to check.
       @param Correction is the correction factor to add to Count before checking.
       @raises ECollectionIndexOutOfRange if Index is not valid and OnError is
               not set. }
    procedure CheckIndexBounds(_Index: LongInt; _Correction: integer = 0); virtual;
    {: Get-Method for the Delta property
       @returns the value of fDelta }
    function GetDelta: integer;
    {: Frees the given Item.
       FreeItem assumes that Item is a descendant of TObject and just calls
       its Free method. Collections that do not store TObject descendants
       must override this method.
       @param Item is the TObject to free. }
    procedure FreeItem(_Item: TObject); virtual;
  public
    {: Default constructor for a collection with a Limit of 0 and a Delta of 1.
       This is the most memory efficient way do create a collection, but since
       with a Delta of 1 it needs to grow the item list every time a new item
       is inserted, it is not very fast on insert. The recommended way to use
       a Delta of 1 is to set the Limit property manually to roughly the number
       of items the program is about to insert and then set Limit to 0, causing
       the collection to free all memory it does no longer need. }
    constructor Create; overload;
    {: Constructs a collection with the given Limit and a Delta of 1.
       @param Limit is the Limit value the collection should start with. }
    constructor Create(_Limit: integer); overload;
    {: Constructs a collection with the given Limit and Delta.
       @param Limit is the Limit value the collection should start with.
       @param Delta is the Delta value the collection should start with. }
    constructor Create(_Limit, _Delta: integer); overload;
    {: Standard Destructor for a collection. Frees all items stored in the
       collection by calling the FreeItem method and then frees the memory
       used by the collection itself. }
    destructor Destroy; override;
    {: Overloaded method to override TObject.Free, if called without parameters
       just calls the inherited Free method. }
    procedure Free; overload;
    {: Inserts the given Item at the end of the collection. Descendant objects
       may override this method to insert the new item elsewhere, e.g. in a
       sorted order.
       @param Item is the TObject to insert.
       @returns the index where the Item was stored. }
    function Insert(_Item: TObject): integer; virtual;
    {: Deletes the given Item from the collection. This method searches the
       collection for the passed Item and removes it if found. If the item
       is not in the collection it raises an ECollectionIndexOutOfRange exception.
       This method just calls IndexOf and AtDelete.
       @param Item is the TObject to delete.
       @Returns the index the Item was stored. }
    function Delete(_Item: TObject): integer; virtual;
    {: Deletes the item stored at Index and returns it.
       @param Index the index of the item to delete.
       @returns the Item that was deleted. }
    function AtDelete(_Index: LongInt): TObject; virtual;
    {: Deletes the item at Index and Frees it.
       This method combines AtDelete and FreeItem.
       @param Index is the index of the item to free. }
    procedure AtFree(_Index: LongInt); virtual;
    {: Inserts the Item at Index. This method inserts the Item at the given
       Index and moves all Items with higher indices to make space.
       @param Index is the index to insert at.
       @param Item is the TObject to insert. }
    procedure AtInsert(_Index: LongInt; _Item: TObject); virtual;
    {: Deletes all itesm from the collection, but does not free them. }
    procedure DeleteAll; virtual;
    {: Frees the given item. This method combines IndexOf and AtFree. Note that
       this is different from FreeItem which does not delete the Item from the
       collection.
       @param Item is the TObject to free. }
    procedure Free(_Item: TObject); overload; virtual;
    {: Frees and deletes all items in the collection. }
    procedure FreeAll; virtual;
    {: Determines the index the given Item is stored.
       @Item is the TObject to search.
       @returns the index of the given Item or -1 if not found. }
    function IndexOf(_Item: TObject): LongInt; virtual;
    {: Copies the content of the given collection without duplicating the
       items stored in it. Note that this is a potential problem since it
       creates a situation where the same items are stored in two different
       collections but are owned by only one of them. Make sure that the items
       are not destructed twice.
       @param Source is the source collection from where to copy the items }
    procedure Assign(_Source: TdzCollection); virtual;
    {: Adds the content of the given collection without duplicating the
       items stored in it. Note that this is a potential problem since it
       creates a situation where the same items are stored in two different
       collections but are owned by only one of them. Make sure that the items
       are not destructed twice.
       @param Source is the source collection from where to copy the items }
    procedure Add(_Source: TdzCollection); virtual;
    {: Calls the given function for all items starting from the beginning until
       the function returns true. There are two overloaded versions of this
       method, one takes a plain function pointer the other a method pointer.
       @param Condtion is a pointer to the function to call.
       @returns the item where the function returned true. }
    function FirstThat(_Condition: tConditionFunc): TObject; overload; virtual;
    {: Calls the given function for all items starting from the end until
       the function returns true. There are two overloaded versions of this
       method, one takes a plain function pointer the other a method pointer.
       @param Condtion is a pointer to the function to call.
       @returns the item where the function returned true. }
    function LastThat(_Condition: tConditionFunc): TObject; overload; virtual;
    {: Calls the given procedure for all items in the collection starting from
       the beginning. There are two overloaded versions of this method, one
       takes a plain procedure pointer the other a method pointer.
       @param Action is the a pointer to the procedure to call }
    procedure ForEach(_Action: tActionProc); overload; virtual;
    {: Calls the given method function for all items starting from the end until
       the function returns true. There are two overloaded versions of this
       method, one takes a plain function pointer the other a method pointer.
       @param Condtion is a pointer to the method function to call.
       @returns the item where the function returned true. }
    function FirstThat(_Condition: tConditionMeth): TObject; overload; virtual;
    {: Calls the given method function for all items starting from the end until
       the function returns true. There are two overloaded versions of this
       method, one takes a plain function pointer the other a method pointer.
       @param Condtion is a pointer to the method function to call.
       @returns the item where the function returned true. }
    function LastThat(_Condition: tConditionMeth): TObject; overload; virtual;
    {: Calls the given procedure for all items in the collection starting from
       the beginning. There are two overloaded versions of this method, one
       takes a plain procedure pointer the other a method pointer.
       @param Action is the a pointer to the procedure to call }
    procedure ForEach(_Action: tActionMeth); overload; virtual;
    {: Checks the given Idx against the valid index bounds [0..Count-1].
       @param Index is the index to check
       @param Correction is the correction factor to add to Count before checking.
       @returns true, if Idx is a valid index, false otherwise }
    function isValidIndex(_Index: integer; _Correction: integer = 0): boolean; virtual;

    {: Gives access to all items stored in the collection by index. It calls
       the GetItem and SetItem methods. }
    property Items[_Index: LongInt]: TObject read GetItem write SetItem; default;
    {: The number of items stored in the collection }
    property Count: LongInt read fCount;
    {: The number of items that can be stored in the collection without
       allocating more memory. If the program tries to store more than
       Limit items in the collection, it will grow automatically. Setting the
       Limit to a value smaller than Count will set it to Count and thus
       releasing all memory not needed to store the currently stored items. }
    property Limit: LongInt read fLimit write SetLimit;
    {: The number of items the collection will grow if more space is needed.
       Inserting the Limit+1 th item into the collection will result in the
       collection growing so it can store Delta more items. }
    property Delta: LongInt read GetDelta write fDelta;
    {: Event called when an error occurs. }
    property OnError: TOnError read fOnError write fOnError;
    {: Event called after an Item has been inserted into the collection. }
    property AfterInsert: TAfterInsert read fAfterInsert write fAfterInsert;
    {: Event called before an Item is deleted from the collection. }
    property BeforeDelete: TBeforeDelete read fBeforeDelete write fBeforeDelete;
  end;

  {: This extends TdzCollection to store the items in sorted order. To allow
     this a descendant must override the Compare method to provide the rule
     used for comparing two items. It can optionally override the KeyOf
     method to not pass the whole item to Compare but only a key. }
  TdzSortedCollection = class(TdzCollection)
  protected
    {: Stores the Duplicats property value. }
    fDuplicates: boolean;
  public
    {: Compares Key to the item stored at index Idx. It calls Compare with
       Key and the result of KeyOf(Items[Idx]).
       @param Key is the key of the first item to compare.
       @param Idx is the index of the second item to compare.
       @returns 0 if the items are the same, <0 if the first is smaller than
                the second and >0 if the first is greater than the second. }
    function CompareTo(_Key: pointer; _Idx: integer): integer;
    {: Compares the Keys of two items. This methode must be overridden by all
       descendants of this class.
       @param Key1 is the key of the first item to compare.
       @param Key2 is the key of the second item to compare.
       @returns 0 if the items are the same, <0 if the first is smaller than
                the second and >0 if the first is greater than the second. }
    function Compare(_Key1, _Key2: pointer): integer; virtual; abstract;
    {: True, if duplicate items are allowed in the collection, false if
       trying to insert a duplicate should ignore it. Defauls to
       false. }
    property Duplicates: boolean read fDuplicates write fDuplicates;
    {: Standard constructor, calls the inherited constructor and sets
       Duplicates to false. }
    constructor Create;
    {: Method to retrieve the key value of the given item. This method
       returns a pointer to the item itself but can be overridden by
       descendants to return a key value only.
       @param Item is the TObject whose key shold be returned.
       @returns the key value for Item. }
    function KeyOf(_Item: TObject): pointer; virtual;
    {: Uses Search to search for the given item and returns its index.
       Calls Search to search the given item. If found, returns its index,
       otherwise returns -1.
       @param Item is the TObject to search.
       @param the Item's index If found, -1 otherwise. }
    function IndexOf(_Item: TObject): LongInt; override;
    {: Overrides the inherited method to insert the new item in the correct
       sort order. If an item with the same key already exists, what happens
       to the new item depends on the Duplicats property. If it is true, the
       new Item will be inserted, if it is falls, it will be ignored.
       @param Item is the TObject to insert.
       @returns the index where the new Item was inserted or -1 if it was ignored
                as a duplicate. }
    function Insert(_Item: TObject): integer; override;
    {: Does a binary search in the collection to find the index for the Item
       with the given Key. Returns true, if the Item is in the collection.
       The returned index is either the index of the first Item (duplicates
       might be true!) that matches the key or of the position where this
       item should be inserted. This method uses the BinarySearch function
       from dzQuicksort passing the collection's CompareTo method as
       parameter.
       @param Key is the key of the item to search.
       @param Index returns the index for the given key.
       @returns true, if an item was found, false otherwise. }
    function Search(_Key: pointer; var _Index: integer): boolean; virtual;
  end;

type
  {: a sorted collection whose key is an integer, don't forget to override
     the KeyOf method if you don't actually store integers! }
  TdzIntegerSortedCollection = class(TdzSortedCollection)
  public
    function Compare(_Key1, _Key2: pointer): integer; override;
  end;

  {: Specialised descendant of TdzIntegerSortedCollection to store Longints instead
     of TObjects. it overrides FreeItem to suit the content. }
  TdzLongintCollection = class(TdzIntegerSortedCollection)
  protected
    {: Overrides the inherited method to do nothing since the Longints stored
       in the collection don't need to be freed. }
    procedure FreeItem(_Item: TObject); override;
  public
  end;

  {: Specialised descendant of TdzSortedCollection to store PChars instead
     of TObjects. Note that the PChars stored in the collection are owned
     by it, that is they will be released when the collection itself is
     destroyed. }
  TdzStrCollection = class(TdzSortedCollection)
  protected
    {: Overrides the inherited method to free PChars rather than TObjects }
    procedure FreeItem(_Item: TObject); override;
  public
    {: Overrides the inherited method to compare two PChars case sensitively. }
    function Compare(_Key1, _Key2: pointer): integer; override;
  end;

implementation

uses
  u_dzSort;

resourcestring
  STR_COLLECTION_INDEX_OUT_OF_RANGE_D = 'Collection index out of range (%d)';
  STR_COLLECTION_OVERFLOW = 'Collection overflow';

{ TdzCollection }

constructor TdzCollection.Create;
begin
  inherited Create;
  fItems := nil;
  fCount := 0;
  fLimit := 0;
  fDelta := 10;
end;

constructor TdzCollection.Create(_Limit: integer);
begin
  Create;
  SetLimit(_Limit);
end;

constructor TdzCollection.Create(_Limit, _Delta: integer);
begin
  Create;
  fDelta := _Delta;
  SetLimit(_Limit);
end;

destructor TdzCollection.Destroy;
begin
  FreeAll;
  SetLimit(0);
  inherited Destroy;
end;

function TdzCollection.isValidIndex(_Index: integer; _Correction: integer = 0): boolean;
begin
  Result := (_Index >= 0) and (_Index < Count + _Correction);
end;

procedure TdzCollection.CheckIndexBounds(_Index: LongInt; _Correction: integer = 0);
var
  Resolved: boolean;
begin
  if not isValidIndex(_Index, _Correction) then
    begin
      Resolved := false;
      if Assigned(fOnError) then
        fOnError(_Index, nil, Format(STR_COLLECTION_INDEX_OUT_OF_RANGE_D, [_Index]), Resolved);
      if not Resolved then
        raise ECollectionIndexOutOfRange.CreateFmt(STR_COLLECTION_INDEX_OUT_OF_RANGE_D, [_Index]);
    end;
end;

procedure TdzCollection.SetCount(_Count: LongInt);
begin
  fCount := _Count;
end;

procedure TdzCollection.SetLimit(_Limit: LongInt);
begin
  if _Limit < Count then
    _Limit := Count;
  if _Limit > MAX_COLLECTION_SIZE then
    _Limit := MAX_COLLECTION_SIZE;
  if _Limit <> fLimit then
    begin
      fLimit := _Limit;
      ReallocMem(fItems, fLimit * SizeOf(pointer));
    end;
end;

function TdzCollection.GetDelta: integer;
begin
  if fDelta = 0 then
    // try to guess a good delta
    Result := Count div 3 + 1
  else
    Result := fDelta;
end;

function TdzCollection.GetItem(_Idx: LongInt): TObject;
begin
  CheckIndexBounds(_Idx);
  Result := fItems^[_Idx];
end;

procedure TdzCollection.SetItem(_Idx: LongInt; _Item: TObject);
begin
  CheckIndexBounds(_Idx);
  fItems^[_Idx] := _Item;
end;

function TdzCollection.AtDelete(_Index: LongInt): TObject;
begin
  CheckIndexBounds(_Index);
  Result := fItems^[_Index];
  if Assigned(fBeforeDelete) then
    fBeforeDelete(_Index, Result);
  Move(fItems^[_Index + 1], fItems^[_Index], (Count - _Index - 1) * SizeOf(Pointer));
  SetCount(Count - 1);
end;

procedure TdzCollection.AtFree(_Index: LongInt);
begin
  FreeItem(AtDelete(_Index));
end;

procedure TdzCollection.AtInsert(_Index: LongInt; _Item: TObject);
var
  Resolved: boolean;
begin
  CheckIndexBounds(_Index, 1);
  if Count = Limit then
    begin
      Limit := Limit + Delta; {Limit ist property -> SetLimit}
      if Limit = Count then
        begin
          Resolved := false;
          if Assigned(fOnError) then
            fOnError(_Index, _Item, STR_COLLECTION_OVERFLOW, Resolved);
          if not Resolved then
            raise ECollectionOverflow.Create(STR_COLLECTION_OVERFLOW);
        end;
    end;
  Move(fItems^[_Index], fItems^[_Index + 1], (Count - _Index) * SizeOf(Pointer));
  fItems^[_Index] := _Item;
  SetCount(Count + 1);
  if Assigned(fAfterInsert) then
    fAfterInsert(_Index, _Item);
end;

function TdzCollection.Insert(_Item: TObject): integer;
begin
  Result := Count;
  AtInsert(Result, _Item);
end;

function TdzCollection.Delete(_Item: TObject): integer;
begin
  Result := IndexOf(_Item);
  AtDelete(Result);
end;

procedure TdzCollection.DeleteAll;
begin
  SetCount(0);
end;

procedure TdzCollection.Free(_Item: TObject);
begin
  AtFree(IndexOf(_Item));
end;

procedure TdzCollection.Free;
begin
  inherited;
end;

procedure TdzCollection.FreeAll;
var
  I: LongInt;
begin
  if Assigned(fItems) then
    for I := Count - 1 downto 0 do
      FreeItem(fItems^[I]);
  DeleteAll;
end;

procedure TdzCollection.FreeItem(_Item: TObject);
begin
  _Item.Free;
end;

function TdzCollection.IndexOf(_Item: TObject): LongInt;
var
  i: LongInt;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    begin
      if fItems^[i] = _Item then
        begin
          Result := i;
          Break;
        end;
    end;
end;

function TdzCollection.FirstThat(_Condition: tConditionFunc): TObject;
var
  i: integer;
begin
  i := 0;
  while i < Count do
    begin
      Result := fItems^[i];
      if _Condition(Result) then
        exit;
      Inc(i);
    end;
  Result := nil;
end;

function TdzCollection.LastThat(_Condition: tConditionFunc): TObject;
var
  i: integer;
begin
  i := Count - 1;
  while i >= 0 do
    begin
      Result := fItems^[i];
      if _Condition(Result) then
        exit;
      Dec(i);
    end;
  Result := nil;
end;

procedure TdzCollection.ForEach(_Action: tActionProc);
var
  i: integer;
begin
  for i := 0 to pred(Count) do
    _Action(fItems^[i]);
end;

function TdzCollection.FirstThat(_Condition: tConditionMeth): TObject;
var
  i: integer;
begin
  i := 0;
  while i < Count do
    begin
      Result := fItems^[i];
      if _Condition(Result) then
        exit;
      Inc(i);
    end;
  Result := nil;
end;

function TdzCollection.LastThat(_Condition: tConditionMeth): TObject;
var
  i: integer;
begin
  i := Count - 1;
  while i >= 0 do
    begin
      Result := fItems^[i];
      if _Condition(Result) then
        exit;
      Dec(i);
    end;
  Result := nil;
end;

procedure TdzCollection.ForEach(_Action: tActionMeth);
var
  i: integer;
begin
  for i := 0 to pred(Count) do
    _Action(fItems^[i]);
end;

procedure TdzCollection.Assign(_Source: TdzCollection);
begin
  FreeAll;
  Add(_Source);
end;

procedure TdzCollection.Add(_Source: TdzCollection);
var
  i: integer;
begin
  for i := 0 to _Source.Count - 1 do
    Insert(_Source[i]);
end;

{ TdzSortedCollection }

constructor TdzSortedCollection.Create;
begin
  inherited Create;
  Duplicates := False;
end;

function TdzSortedCollection.IndexOf(_Item: TObject): LongInt;
var
  I: LongInt;
begin
  IndexOf := -1;
  if Search(KeyOf(_Item), I) then
    begin
      if Duplicates then
        while (I < Count) and (_Item <> fItems^[I]) do
          Inc(I);
      if I < Count then
        IndexOf := I;
    end;
end;

function TdzSortedCollection.Insert(_Item: TObject): integer;
begin
  if not Search(KeyOf(_Item), Result) or Duplicates then
    AtInsert(Result, _Item)
  else
    Result := -1;
end;

function TdzSortedCollection.KeyOf(_Item: TObject): pointer;
begin
  Result := _Item;
end;

function TdzSortedCollection.CompareTo(_Key: pointer; _Idx: integer): integer;
begin
  Result := Compare(_Key, KeyOf(fItems^[_Idx]));
end;

// returns true and the Index of the item which matches the Key
// if Duplicates are allowed, returns the smallest Index of a matching
// item

function TdzSortedCollection.Search(_Key: pointer; var _Index: LongInt): boolean;
begin
  Result := BinarySearch(0, Count - 1, _Index, _Key, CompareTo, Duplicates);
end;

{ TdzIntegerSortedCollection }

function TdzIntegerSortedCollection.Compare(_Key1, _Key2: pointer): integer;
begin
  Result := integer(_Key1) - integer(_Key2);
end;

// TdzStrCollection

function TdzStrCollection.Compare(_Key1, _Key2: pointer): integer;
begin
  Compare := StrComp(pChar(_Key1), pChar(_Key2));
end;

procedure TdzStrCollection.FreeItem(_Item: TObject);
begin
  StrDispose(pChar(_Item));
end;

{ TdzLongintCollection }

procedure TdzLongintCollection.FreeItem(_Item: TObject);
begin
  // do nothing
end;

end.

