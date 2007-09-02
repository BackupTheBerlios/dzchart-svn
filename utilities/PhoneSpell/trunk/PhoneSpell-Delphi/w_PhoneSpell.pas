unit w_PhoneSpell;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls;

type
  {: partial implementation of dotNET system.char }
  TChar = record
  public
    class function ToLower(_c: char): char; static;
  end;

type
  {: partial implementation of dotNET system.collections.HashTable }
  THashTable = class
  private
    FList: TStringList;
    function GetItem(const _Key: string): TObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const _Key: string; _Value: TObject);
    function ContainsKey(const _Key: string): boolean;
    property Item[const _Key: string]: TObject read GetItem;
  end;

type
  {: partial implementation of dotNET system.collections.StringDictionary }
  TStringDictionary = class
  private
    FList: TStringList;
    function GetItem(const _Key: string): string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(_Key, _Value: string);
    function ContainsKey(const _Key: string): boolean;
    property Item[const _Key: string]: string read GetItem;
  end;

type
  Tf_PhoneSpell = class(TForm)
    l_What: TLabel;
    ed_What: TEdit;
    l_Alphabet: TLabel;
    cmb_Which: TComboBox;
    lb_Result: TListBox;
    procedure cmb_WhichChange(Sender: TObject);
    procedure ed_WhatChange(Sender: TObject);
  private
    FJointForcesDict: TStringDictionary;
    FNatoDict: TStringDictionary;
    FDeutschlandDict: TStringDictionary;
    FOesterreichDict: TStringDictionary;
    FSchweizDict: TStringDictionary;
    FDictionaries: THashTable;
    FCurrentDict: TStringDictionary;
    procedure InitDeutschlandDict;
    procedure InitJointForcesDict;
    procedure InitNatoDict;
    procedure InitOesterreichDict;
    procedure InitSchweizDict;
    procedure AddDictionary(const _Name: string; _Dictionary: TStringDictionary);
    procedure UpdateSpelling(const _s: string);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

var
  f_PhoneSpell: Tf_PhoneSpell;

implementation

{$R *.dfm}

uses
  StrUtils;

constructor Tf_PhoneSpell.Create(_Owner: TComponent);
begin
  inherited;
  FDictionaries := THashtable.Create;

  FDeutschlandDict := TStringDictionary.Create;
  AddDictionary('Deutschland', FDeutschlandDict);
  FOesterreichDict := TStringDictionary.Create;
  AddDictionary('Österreich', FOesterreichDict);
  FSchweizDict := TStringDictionary.Create;
  AddDictionary('Schweiz', FSchweizDict);
  FNatoDict := TStringDictionary.Create;
  AddDictionary('NATO', FNatoDict);
  FJointForcesDict := TStringDictionary.Create;
  AddDictionary('Joint Army/Navy', FJointForcesDict);

  FCurrentDict := FDeutschlandDict;
  cmb_Which.ItemIndex := 0;

  InitDeutschlandDict;
  InitOesterreichDict;
  InitSchweizDict;
  InitNatoDict;
  InitJointForcesDict;
end;

destructor Tf_PhoneSpell.Destroy;
begin
  FDictionaries.Free;
  inherited;
end;

procedure Tf_PhoneSpell.ed_WhatChange(Sender: TObject);
begin
  UpdateSpelling(ed_What.Text);
end;

procedure Tf_PhoneSpell.AddDictionary(const _Name: string; _Dictionary: TStringDictionary);
begin
  cmb_Which.Items.Add(_Name);
  FDictionaries.Add(_Name, _Dictionary);
end;

procedure Tf_PhoneSpell.InitJointForcesDict;
var
  d: TStringDictionary;
begin
  d := FJointForcesDict;
  d.Add('a', 'Able');
  d.Add('b', 'Baker');
  d.Add('c', 'Charlie');
  d.Add('d', 'Dog');
  d.Add('e', 'Easy');
  d.Add('f', 'Fox');
  d.Add('g', 'George');
  d.Add('h', 'How');
  d.Add('i', 'Item');
  d.Add('j', 'Jig');
  d.Add('k', 'King');
  d.Add('l', 'Love');
  d.Add('m', 'Mike');
  d.Add('n', 'Nan');
  d.Add('o', 'Oboe');
  d.Add('p', 'Peter');
  d.Add('q', 'Queen');
  d.Add('r', 'Roger');
  d.Add('s', 'Sugar');
  d.Add('t', 'Tare');
  d.Add('u', 'Uncle');
  d.Add('v', 'Victor');
  d.Add('w', 'William');
  d.Add('x', 'X-Ray');
  d.Add('y', 'Yoke');
  d.Add('z', 'Zebra');
end;

procedure Tf_PhoneSpell.InitNatoDict;
var
  d: TStringDictionary;
begin
  d := FNatoDict;
  d.Add('a', 'Alpha');
  d.Add('b', 'Bravo');
  d.Add('c', 'Charlie');
  d.Add('d', 'Delta');
  d.Add('e', 'Echo');
  d.Add('f', 'Foxtrott');
  d.Add('g', 'Golf');
  d.Add('h', 'Hotel');
  d.Add('i', 'India');
  d.Add('j', 'Juliet');
  d.Add('k', 'Kilo');
  d.Add('l', 'Lima');
  d.Add('m', 'Mike');
  d.Add('n', 'November');
  d.Add('o', 'Oscar');
  d.Add('p', 'Papa');
  d.Add('q', 'Quebec');
  d.Add('r', 'Romeo');
  d.Add('s', 'Sierra');
  d.Add('t', 'Tango');
  d.Add('u', 'Uniform');
  d.Add('v', 'Victor');
  d.Add('w', 'Whisky');
  d.Add('x', 'X-Ray');
  d.Add('y', 'Yankee');
  d.Add('z', 'Zulu');
end;

procedure Tf_PhoneSpell.InitDeutschlandDict;
var
  d: TStringDictionary;
begin
  d := FDeutschlandDict;
  d.Add('a', 'Anton');
  d.Add('ä', 'Ärger');
  d.Add('b', 'Berta');
  d.Add('c', 'Cäsar');
  d.Add('d', 'Dora');
  d.Add('e', 'Emil');
  d.Add('f', 'Friedrich');
  d.Add('g', 'Gustav');
  d.Add('h', 'Heinrich');
  d.Add('i', 'Ida');
  d.Add('j', 'Julius');
  d.Add('k', 'Kaufmann');
  d.Add('l', 'Ludwig');
  d.Add('m', 'Martha');
  d.Add('n', 'Nordpol');
  d.Add('o', 'Otto');
  d.Add('ö', 'Ökonom');
  d.Add('p', 'Paula');
  d.Add('q', 'Quelle');
  d.Add('r', 'Richard');
  d.Add('s', 'Samuel');
  d.Add('t', 'Theodor');
  d.Add('u', 'Ulrich');
  d.Add('ü', 'Übermut');
  d.Add('v', 'Viktor');
  d.Add('w', 'Wilhelm');
  d.Add('x', 'Xanthippe');
  d.Add('y', 'Ypsilon');
  d.Add('z', 'Zacharias');
end;

procedure Tf_PhoneSpell.InitOesterreichDict;
var
  d: TStringDictionary;
begin
  d := FOesterreichDict;
  d.Add('a', 'Anton');
  d.Add('ä', 'Ärger');
  d.Add('b', 'Berta');
  d.Add('c', 'Csar');
  d.Add('d', 'Dora');
  d.Add('e', 'Emil');
  d.Add('f', 'Friedrich');
  d.Add('g', 'Gustav');
  d.Add('h', 'Heinrich');
  d.Add('i', 'Ida');
  d.Add('j', 'Julius');
  d.Add('k', 'Konrad');
  d.Add('l', 'Ludwig');
  d.Add('m', 'Martha');
  d.Add('n', 'Nordpol');
  d.Add('o', 'Otto');
  d.Add('ö', 'Österreich');
  d.Add('p', 'Paula');
  d.Add('q', 'Quelle');
  d.Add('r', 'Richard');
  d.Add('s', 'Siegfried');
  d.Add('t', 'Theodor');
  d.Add('u', 'Ulrich');
  d.Add('ü', 'Übel');
  d.Add('v', 'Viktor');
  d.Add('w', 'Wilhelm');
  d.Add('x', 'Xaver');
  d.Add('y', 'Ypsilon');
  d.Add('z', 'Zürich');
end;

procedure Tf_PhoneSpell.InitSchweizDict;
var
  d: TStringDictionary;
begin
  d := FSchweizDict;
  d.Add('a', 'Anna');
  d.Add('ä', 'Äsch');
  d.Add('b', 'Berta');
  d.Add('c', 'Csar');
  d.Add('d', 'Daniel');
  d.Add('e', 'Emil');
  d.Add('f', 'Friedrich');
  d.Add('g', 'Gustav');
  d.Add('h', 'Heinrich');
  d.Add('i', 'Ida');
  d.Add('j', 'Jakob');
  d.Add('k', 'Kaiser');
  d.Add('l', 'Leopold');
  d.Add('m', 'Marie');
  d.Add('n', 'Niklaus');
  d.Add('o', 'Otto');
  d.Add('ö', 'Örlikon');
  d.Add('p', 'Peter');
  d.Add('q', 'Quasi');
  d.Add('r', 'Rosa');
  d.Add('s', 'Sophie');
  d.Add('t', 'Theodor');
  d.Add('u', 'Ulrich');
  d.Add('ü', 'Übermut');
  d.Add('v', 'Viktor');
  d.Add('w', 'Wilhelm');
  d.Add('x', 'Xaver');
  d.Add('y', 'Yverdon');
  d.Add('z', 'Zürich');
end;

procedure Tf_PhoneSpell.UpdateSpelling(const _s: string);
var
  i: integer;
  c: Char;
  d: TStringDictionary;
begin
  d := FCurrentDict;
  lb_Result.Items.Clear;
  for i := 1 to Length(_s) do begin
    c := _s[i];
    c := TChar.ToLower(c);
    if d.ContainsKey(c) then
      lb_Result.Items.Add(d.Item[c])
    else
      lb_Result.Items.Add('<unknown>');
  end;
end;

procedure Tf_PhoneSpell.cmb_WhichChange(Sender: TObject);
var
  s: string;
  Idx: integer;
begin
  Idx := cmb_Which.ItemIndex;
  if Idx = -1 then
    exit;
  s := cmb_Which.Items[Idx];
  if not FDictionaries.ContainsKey(s) then
    exit;
  FCurrentDict := FDictionaries.Item[s] as TStringDictionary;
  UpdateSpelling(ed_What.Text);
end;

{ TStringDictionary }

constructor TStringDictionary.Create;
begin
  inherited Create;
  FList := TStringList.Create;
end;

destructor TStringDictionary.Destroy;
begin
  FList.Free;
  inherited;
end;

function TStringDictionary.GetItem(const _Key: string): string;
begin
  Result := FList.Values[_Key];
end;

procedure TStringDictionary.Add(_Key, _Value: string);
begin
  FList.Values[_Key] := _Value;
end;

function TStringDictionary.ContainsKey(const _Key: string): boolean;
begin
  Result := FList.Values[_Key] <> '';
end;

{ THashTable }

function THashTable.ContainsKey(const _Key: string): boolean;
var
  Idx: Integer;
begin
  Result := FList.Find(_Key, Idx);
end;

constructor THashTable.Create;
begin
  inherited Create;
  FList := TStringList.Create;
  FList.Sorted := true;
end;

destructor THashTable.Destroy;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    if Assigned(FList.Objects[i]) then
      FList.Objects[i].Free;
  FList.Free;
  inherited;
end;

function THashTable.GetItem(const _Key: string): TObject;
var
  Idx: integer;
begin
  if not FList.Find(_Key, Idx) then
    raise Exception.CreateFmt('Could not find item %s in list', [_Key]);
  Result := FList.Objects[Idx];
end;

procedure THashTable.Add(const _Key: string; _Value: TObject);
begin
  FList.AddObject(_Key, _Value);
end;

{ TChar }

class function TChar.ToLower(_c: char): char;
begin
  Result := LowerCase(_c)[1];
end;

end.

