unit w_PhoneSpell;

interface

uses
  System.Drawing,
  System.Collections,
  System.Collections.Specialized,
  System.ComponentModel,
  System.Windows.Forms,
  System.Data,
  System.Globalization;

type
  TPhoneSpell = class(System.Windows.Forms.Form)
{$REGION 'Designer Managed Code'}
  strict private
    /// <summary>
    /// Required designer variable.
    /// </summary>
    Components: System.ComponentModel.Container;
    Label1: System.Windows.Forms.Label;
    TextBox1: System.Windows.Forms.TextBox;
    cmb_Which: System.Windows.Forms.ComboBox;
    Label2: System.Windows.Forms.Label;
    lb_Result: System.Windows.Forms.ListBox;
    /// <summary>
    /// Required method for Designer support - do not modify
    /// the contents of this method with the code editor.
    /// </summary>
    procedure InitializeComponent;
    procedure TextBox1_TextChanged(sender: System.object;
      e: System.EventArgs);
    procedure cmb_Which_SelectionChangeCommitted(sender: System.object;
      e: System.EventArgs);
{$ENDREGION}
  strict protected
    /// <summary>
    /// Clean up any resources being used.
    /// </summary>
    procedure Dispose(Disposing: Boolean); override;
  private
    FDeutschlandDict: StringDictionary;
    FSchweizDict: StringDictionary;
    FOesterreichDict: StringDictionary;
    FNatoDict: StringDictionary;
    FJointForcesDict: StringDictionary;

    FDictionaries: Hashtable;
    FCurrentDict: StringDictionary;
    procedure UpdateSpelling(const _s: string);
    procedure InitNatoDict;
    procedure InitJointForcesDict;
    procedure InitDeutschlandDict;
    procedure InitSchweizDict;
    procedure InitOesterreichDict;
    procedure AddDictionary(const _Name: string; _Dictionary: StringDictionary);
  public
    constructor Create;
  end;

  [assembly: RuntimeRequiredAttribute(TypeOf(TPhoneSpell))]

implementation

{$AUTOBOX ON}

{$REGION 'Windows Form Designer generated code'}
/// <summary>
/// Required method for Designer support -- do not modify
/// the contents of this method with the code editor.
/// </summary>

procedure TPhoneSpell.InitializeComponent;
begin
  Self.Label1 := System.Windows.Forms.Label.Create;
  Self.TextBox1 := System.Windows.Forms.TextBox.Create;
  Self.cmb_Which := System.Windows.Forms.ComboBox.Create;
  Self.Label2 := System.Windows.Forms.Label.Create;
  Self.lb_Result := System.Windows.Forms.ListBox.Create;
  Self.SuspendLayout;
  // 
  // Label1
  // 
  Self.Label1.AutoSize := True;
  Self.Label1.Location := System.Drawing.Point.Create(8, 8);
  Self.Label1.Name := 'Label1';
  Self.Label1.Size := System.Drawing.Size.Create(140, 16);
  Self.Label1.TabIndex := 0;
  Self.Label1.Text := 'What do you want to spell?';
  // 
  // TextBox1
  // 
  Self.TextBox1.Location := System.Drawing.Point.Create(8, 24);
  Self.TextBox1.Name := 'TextBox1';
  Self.TextBox1.Size := System.Drawing.Size.Create(184, 20);
  Self.TextBox1.TabIndex := 1;
  Self.TextBox1.Text := 'type it here ...';
  Include(Self.TextBox1.TextChanged, Self.TextBox1_TextChanged);
  // 
  // cmb_Which
  // 
  Self.cmb_Which.DropDownStyle := System.Windows.Forms.ComboBoxStyle.DropDownList;
  Self.cmb_Which.Location := System.Drawing.Point.Create(8, 72);
  Self.cmb_Which.Name := 'cmb_Which';
  Self.cmb_Which.Size := System.Drawing.Size.Create(184, 21);
  Self.cmb_Which.TabIndex := 5;
  Include(Self.cmb_Which.SelectionChangeCommitted, Self.cmb_Which_SelectionChangeCommitted);
  // 
  // Label2
  // 
  Self.Label2.AutoSize := True;
  Self.Label2.Location := System.Drawing.Point.Create(8, 56);
  Self.Label2.Name := 'Label2';
  Self.Label2.Size := System.Drawing.Size.Create(168, 16);
  Self.Label2.TabIndex := 6;
  Self.Label2.Text := 'Which phonetic alphabet to use?';
  // 
  // lb_Result
  // 
  Self.lb_Result.Location := System.Drawing.Point.Create(8, 104);
  Self.lb_Result.Name := 'lb_Result';
  Self.lb_Result.Size := System.Drawing.Size.Create(184, 225);
  Self.lb_Result.TabIndex := 7;
  // 
  // TPhoneSpell
  // 
  Self.AutoScaleBaseSize := System.Drawing.Size.Create(5, 13);
  Self.ClientSize := System.Drawing.Size.Create(200, 336);
  Self.Controls.Add(Self.lb_Result);
  Self.Controls.Add(Self.Label2);
  Self.Controls.Add(Self.TextBox1);
  Self.Controls.Add(Self.Label1);
  Self.Controls.Add(Self.cmb_Which);
  Self.MaximizeBox := False;
  Self.Name := 'TPhoneSpell';
  Self.StartPosition := System.Windows.Forms.FormStartPosition.CenterScreen;
  Self.Text := 'PhoneSpell';
  Self.ResumeLayout(False);
end;
{$ENDREGION}

procedure TPhoneSpell.Dispose(Disposing: Boolean);
begin
  if Disposing then begin
    if Components <> nil then
      Components.Dispose();
  end;
  inherited Dispose(Disposing);
end;

constructor TPhoneSpell.Create;
begin
  inherited Create;
  //
  // Required for Windows Form Designer support
  //
  InitializeComponent;
  //
  // TODO: Add any constructor code after InitializeComponent call
  //

  FDictionaries := Hashtable.Create;

  FDeutschlandDict := StringDictionary.Create;
  AddDictionary('Deutschland', FDeutschlandDict);
  FOesterreichDict := StringDictionary.Create;
  AddDictionary('Österreich', FOesterreichDict);
  FSchweizDict := StringDictionary.Create;
  AddDictionary('Schweiz', FSchweizDict);
  FNatoDict := StringDictionary.Create;
  AddDictionary('NATO', FNatoDict);
  FJointForcesDict := StringDictionary.Create;
  AddDictionary('Joint Army/Navy', FJointForcesDict);

  FCurrentDict := FDeutschlandDict;
  cmb_Which.SelectedIndex := 0;

  InitDeutschlandDict;
  InitOesterreichDict;
  InitSchweizDict;
  InitNatoDict;
  InitJointForcesDict;
end;

procedure TPhoneSpell.cmb_Which_SelectionChangeCommitted(sender: System.object;
  e: System.EventArgs);
var
  s: string;
  Item: &Object;
begin
  Item := cmb_Which.SelectedItem;
  if not Assigned(Item) then
    exit;
  s := Item.ToString;
  if not FDictionaries.ContainsKey(s) then
    exit;
  FCurrentDict := FDictionaries.Item[s] as StringDictionary;
  UpdateSpelling(TextBox1.Text);
end;

procedure TPhoneSpell.AddDictionary(const _Name: string; _Dictionary: StringDictionary);
begin
  cmb_Which.Items.Add(_Name);
  FDictionaries.Add(_Name, _Dictionary);
end;

procedure TPhoneSpell.InitJointForcesDict;
var
  d: StringDictionary;
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

procedure TPhoneSpell.InitNatoDict;
var
  d: StringDictionary;
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

procedure TPhoneSpell.InitDeutschlandDict;
var
  d: StringDictionary;
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

procedure TPhoneSpell.InitOesterreichDict;
var
  d: StringDictionary;
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

procedure TPhoneSpell.InitSchweizDict;
var
  d: StringDictionary;
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

procedure TPhoneSpell.TextBox1_TextChanged(sender: System.object;
  e: System.EventArgs);
var
  s: string;
begin
  s := TextBox1.Text;
  UpdateSpelling(s);
end;

procedure TPhoneSpell.UpdateSpelling(const _s: string);
var
  i: integer;
  c: Char;
  d: StringDictionary;
begin
  d := FCurrentDict;
  lb_Result.Items.Clear;
  for i := 0 to _s.Length - 1 do begin
    c := _s.Chars[i];
    c := System.Char.ToLower(c);
    if d.ContainsKey(c) then
      lb_Result.Items.Add(d.Item[c])
    else
      lb_Result.Items.Add('<unknown>');
  end;
end;

end.

