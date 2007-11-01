{GXFormatter.config=twm}
{: Implements functions which work on components but are not methods.
   @autor        twm }
unit u_dzVclUtils;

interface

uses
  Classes,
  Windows,
  SysUtils,
  Graphics,
  Forms,
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls,
  Grids,
  DbGrids;

type
  {: Ancestor to all exceptions raised in this unit. }
  EdzVclUtils = class(Exception);

  {: raised if the Combobox passed to SetOwnerDrawComboItemCount is not ownder drawn. }
  EdzComboBoxNotOwnerDraw = class(EdzVclUtils);

  {: raised if the Listbox passed to SetOwnerDrawListboxItemCount is not ownder drawn. }
  EdzListBoxNotOwnerDraw = class(EdzVclUtils);

type
  {: This is a copy of the TFileFormatsList class from Graphics which
     is unfortunately only declaread in the implementation section }
  TFileFormatsList = class(TList)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Ext, Desc: string; DescID: Integer; AClass: TGraphicClass);
    function FindExt(Ext: string): TGraphicClass;
    function FindClassName(const Classname: string): TGraphicClass;
    procedure Remove(AClass: TGraphicClass);
    {: returns a file select filter string for all descendants of the given TGraphicClass }
    procedure BuildFilterStrings(GraphicClass: TGraphicClass; var Descriptions, Filters: string);
    function GetFilterString(GraphicClass: TGraphicClass = nil): string;
  end;

  {: returns the global file formats list }
function GetFileFormats: TFileFormatsList;

{: checks whether the integer array contains the given element
   @param Element is the integer to check for
   @param Arr is the array to check
   @returns true, if Arr contains Element }
function ArrayContains(_Element: integer; const _Arr: array of integer): boolean;

type
  {: used in ResizeStringGrid and ResizeDbGrid to specify additional options
    <ul>
      <li>roUseGridWidth -> make the columns take up the whole grid width</li>
      <li>roIgnoreHeader -> do not use the column header to calculate the column
                            width</li>
      <li>roUseAllRows -> use all Grid rows to calculate the minimum width, not
                          just the first 10</li>
    </ul> }
  TResizeOptions = (roUseGridWidth, roIgnoreHeader, roUseAllRows);
  TResizeOptionSet = set of TResizeOptions;

  {: Resizes the columns of a TCustomGrid to fit their contents
     @param Grid is the TCustomGrid to work on
     @param Options is a TResizeOptionSet specifying additional options, default
            is an empty set. }
procedure TGrid_Resize(_Grid: TCustomGrid); overload;
procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet); overload;
procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet; _RowOffset: integer); overload;
procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet; const _ConstantCols: array of integer); overload;
procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet; const _ConstantCols: array of integer; _RowOffset: integer); overload;

{: Resizes the columns of a TDbGrid to fit their contents
   @param Grid is the TCustomDbGrid to work on
   @param Options is a TResizeOptionSet specifying additional options, default
          is an empty set. }
procedure TDbGrid_Resize(_Grid: TCustomDbGrid; _Options: TResizeOptionSet = []);

{: Returns the content of a StringGrid as a string
   @param Grid is the TCustomGrid to read from.
   @returns a string containing the contents of the grid columns separated by TAB
            rows sepearated by CRLF. }
function TGrid_GetText(_Grid: TCustomGrid): string;

{: sets the row count to FixedRows + 1 and clears all non-fixed cells }
procedure TStringGrid_Clear(_Grid: TStringGrid);

{: sets the row count, taking the fixed rows into account }
procedure TStringGrid_SetRowCount(_Grid: TStringGrid; _RowCount: integer);

{: sets the column count, taking the fixed columns into account }
procedure TStringGrid_SetColCount(_Grid: TStringGrid; _ColCount: integer);

{: deletes the given row from the string grid and moves all rows below it up by one,
   if there is only one non-fixed row left, this row is cleared but not deleted.
   @param Grid is the StringGrid to change
   @param Row is the index of the row to delete, or -1 to delete the current row
   @returns true, if the row was deleted }
function TStringGrid_DeleteRow(_Grid: TStringGrid; _Row: integer = -1): boolean;

{: inserts a row at the given index into the string grid and moves all rows below it down by one.
   @param Grid is the StringGrid to change
   @param Row is the index of the row to insert, or -1 to insert at the current row }
procedure TStringGrid_InsertRow(_Grid: TStringGrid; _Row: integer = -1);

{: Tries to convert the grid cell to a double, if an error occurs, it raises
   an exception and optionally focuses the cell.
   @param Grid is the grid containing the data
   @param Col is the cell's column (zero based)
   @param Row is the cell's row (zero based)
   @param FocusCell is a boolean which determines whether to focus the grid and cell
          if it does not contain a valid value or not
   @returns the cell's content as a double
   @raises EConvertError if the cell's content could not be converted }
function TStringGrid_CellToDouble(_grid: TStringGrid; _Col, _Row: integer; _FocusCell: boolean = true): double;

{: Tries to convert the grid cell to an integer, if an error occurs, it raises
   an exception and optionally focuses the cell.
   @param Grid is the grid containing the data
   @param Col is the cell's column (zero based)
   @param Row is the cell's row (zero based)
   @param FocusCell is a boolean which determines whether to focus the grid and cell
          if it does not contain a valid value or not
   @returns the cell's content as an integer
   @raises EConvertError if the cell's content could not be converted }
function TStringGrid_CellToInt(_grid: TStringGrid; _Col, _Row: integer; _FocusCell: boolean = true): integer;

{: Deletes the top lines of the memo so it only contains Retain lines
   @param Memo is the meme to work on
   @param Retain is the number of lines to retain }
procedure TMemo_DeleteTopLines(_Memo: TMemo; _Retain: integer);

{: Scrolls the memo to the end }
procedure TMemo_ScrollToEnd(_Memo: TMemo);

{: Tries to convert the edit control text to a double, if an error occurs, it raises
   an exception and optionally focuses the control.
   @param ed is the edit control
   @param FocusControl is a boolean which determines whether to focus the control
          if it does not contain a valid value or not
   @returns the controls content as a double
   @raises EConvertError if the controls content could not be converted }
function TEdit_TextToDouble(_ed: TEdit; _FocusControl: boolean = true): double;

{: Tries to convert the edit control text to an integer, if an error occurs, it raises
   an exception and optionally focuses the control.
   @param ed is the edit control
   @param FocusControl is a boolean which determines whether to focus the control
          if it does not contain a valid value or not
   @returns the controls content as an integer
   @raises EConvertError if the controls content could not be converted }
function TEdit_TextToInt(_ed: TEdit; _FocusControl: boolean = true): integer;

{: returns the contents of the tree view as a string with indentations
   @param Tree is the TTreeView to process
   @param Indentation is the number of spaces for indentation
   @param Marker is a marker character to use for each item #0 for no marker }
function TTreeView_GetAsText(_Tree: TTreeView; _Indentation: integer = 2; _Marker: char = #0): string;

{: adds a new TTabSheet to the given PageControl with the given Caption and returns it }
function TPageControl_AddTabSheet(_PageControl: TPageControl; const _Caption: string): TTabSheet;

{: Draws the tab text for a TPageControl as horizontal text, useful, if you
   want to have the tabs on the left or right but don't want vertical text.
   Set the TPageControl's OwnerDraw property to true, the TabHeight property
   (which actually gives the width of the tabs if they are on the left or right
   hand side) sufficiently large, the TabWidth (which is actually is the height)
   to 18 and assign a OnDrawTab event. From this event call this function.
   @param PageControl is the TPageControl to draw
   @param TabIndex is the index of the tab to draw
   @param Rect is a TRect giving the drawing area
   @param Active is a boolean that is true if the Tab is currently active }
procedure TPageControl_DrawTab(_PageControl: TPageControl; _TabIndex: integer;
  const _Rect: TRect; _Active: boolean);

{: Draws the tab text for a TTabControl as horizontal text, useful, if you
   want to have the tabs on the left or right but don't want vertical text.
   Set the TTabControl's OwnerDraw property to true, the TabHeight property
   (which actually gives the width of the tabs if they are on the left or right
   hand side) sufficiently large, the TabWidth (which is actually is the height)
   to 18 and assign a OnDrawTab event. From this event call this function.
   @param PageControl is the TPageControl to draw
   @param TabIndex is the index of the tab to draw
   @param Rect is a TRect giving the drawing area
   @param Active is a boolean that is true if the Tab is currently active }
procedure TTabControl_DrawTab(_TabControl: TTabControl; _TabIndex: integer;
  const _Rect: TRect; _Active: boolean);

{: Sets a TTabControl's tab width based on the text it displays, usefull
   to display horizontal text in tabs on the left or right hand side }
procedure TTabControl_AdjustTabWidth(_TabControl: TTabControl; _Form: TForm; _MinWidth: integer = 80);

{: sets the control and all its child controls Enabled property and changes their
   caption to reflect this
   @param Control is the TControl to change
   @param Enabled is a boolean with the new value for the Enabled property. }
procedure SetControlEnabled(_Control: TControl; _Enabled: boolean);

{: Selects the entry in a combobox that has an object pointer matching Value
   @param cmb is the TCustomCombobox (descendant) to select
   @param Value is the desired object value
   @returns true, if the value could be found, false otherwise }
function TComboBox_SelectByObject(_cmb: TCustomCombobox; _Value: pointer): boolean;

{: Gets the string of a combobox entry that has an object pointer matching Obj
   @param cmb is the TCustomCombobox (descendant) to select
   @param Obj is the desired object value
   @param s is the string of the combobox entry, only valid if the function returns true
   @returns true, if the object could be found, false otherwise }
function TComboBox_GetObjectCaption(_cmb: TCustomCombobox; _Obj: pointer; out _s: string): boolean;

{: Gets the object pointer of the selected combobox item
   @param cmb is the TCustomCombobox (descendant) to read from
   @param Idx is the combobox's ItemIndex, only valid if the function returns true
   @param Obj is the value of the object pointer of the selected item, only valid
          if the function returns true
   @param FocusControl is a boolean which determines whether to focus the control
          if it does not contain a valid value or not, default = false
   @returns true, if these values are valid }
function TComboBox_GetSelectedObject(_cmb: TCustomCombobox; out _Idx: integer;
  out _Obj: pointer; _FocusControl: boolean = false): boolean;

{: Gets the caption of the selected combobox item
   @param cmb is the TCustomCombobox (descendant) to read from
   @param Item is the selected item, only valid if the function returns true
   @param FocusControl is a boolean which determines whether to focus the control
          if it does not contain a valid value or not, default = false
   @returns true, if an item was selected }
function TComboBox_GetSelected(_cmb: TCustomComboBox; out _Item: string;
  _FocusControl: boolean = false): boolean;

{: Selects the item if it is in the list and returns the new ItemIndex
   @param cmb is the TCustomCombobox (descendant) to use
   @param Item is the item to select
   @param DefaultIdx is the ItemIndex to use if no item matches.
   @returns the index of the newly selected item or -1 if it doesn't exist }
function TComboBox_Select(_cmb: TCustomComboBox; const _Item: string; _DefaultIdx: integer = -1): integer;

{: Gets the object pointer of the selected listbox item
   @param cmb is the TCustomListbox (descendant) to read from
   @param Idx is the listbox's ItemIndex, only valid if the function returns true
   @param Obj is the value of the object pointer of the selected item, only valid
          if the function returns true
   @returns true, if these values are valid }
function TListBox_GetSelectedObject(_lst: TCustomListbox; out _Idx: integer; out _Obj: pointer): boolean;

{: Gets the caption of the given or selected item in the RadioGroup
   @param rg is the TCustomRadioGroup descendant to read
   @param Caption returns a string with the requested caption with
                  Ampersands ('&') stripped, only valid, if
                  the function returns true
   @param Idx is the item index to read, defaults to -1 meaning 'selected item'
   @returns true, if the caption could be read }
function TRadioGroup_GetItemCaption(_rg: TCustomRadioGroup;
  out _Caption: string; _Idx: integer = -1): boolean;

{: Gets the object pointer of the selected RadioGroup item
   @param cmb is the TCustomListbox (descendant) to read from
   @param Idx is the listbox's ItemIndex, only valid if the function returns true
   @param Obj is the value of the object pointer of the selected item, only valid
          if the function returns true
   @returns true, if these values are valid }
function TRadioGroup_GetSelectedObject(_rg: TCustomRadioGroup; out _Idx: integer; out _Obj: pointer): boolean;

{: Writes a TPicture object to a String. The Format is
   <pictureformat>#26<picturedata> }
function TPicture_WriteToString(_Pic: TPicture): string;

{: Reads a TPicture object from a String which was created using
   Picture_WriteToString }
procedure TPicture_ReadFromString(_Pic: TPicture; const _S: string);

{: Writes a TRichEdit to a string including all formatting }
function TRichEdit_WriteToString(_Re: TRichEdit): string;

{: Reads a TRichEdit from a string including all formatting }
procedure TRichEdit_ReadFromString(_Re: TRichEdit; const _S: string);

{: Returns the characater offset of the first character of the given row
   example: RicheEdit1.SelStart := RichEdit_GetRowCharIndex(RichEdit1, 5);
   @param Re is a TRichEdit
   @param Row is the row number (0 based)
   @returns the character offset (0 based) of the first character of the row }
function TRichEdit_RowToCharIndex(_Re: TRichEdit; _Row: integer): integer;

{: Returns the row which contains the given character index }
function TRichEdit_CharIndexToRow(_Re: TRichEdit; _Idx: integer): integer;

{: Returns the current row number (0 based) of the RichEdit }
function TRichEdit_GetCurrentRow(_Re: TRichEdit): integer;

{: Scrolls the rich edit to the current caret position }
procedure TRichEdit_ScrollToCaret(_Re: TRichEdit);

{: Write a line to a RichEdit, optionally specifying color and font style }
procedure TRichEdit_WriteLn(_Re: TRichEdit; const _s: string; _Color: TColor = clBlack; _Style: TFontStyles = []);

{: Fuegt ein Control und ein zugehoeriges Label ein, Zeilenabstand ist 24 Punkte
   mit 16 Punkten Abstand vom oberen und 8 vom linken Rand. Es wird angenommen,
   dass _ctrl bereits die korrekte x-Position hat, lediglich Top wird angepasst }
function AddLabeledControl(_Idx: integer; const _Caption: string; _ctrl: TControl): TLabel;

{: Calculates the height for writing a Text on a control }
function CalcTextHeight(_Ctrl: TWinControl; const _Text: string; _Width: integer = -1): integer; overload;
function CalcTextHeight(_Ctrl: TGraphicControl; const _Text: string; _Width: integer = -1): integer; overload;

{: I don't quite remember what this is supposed to do and where it is used,
   Please, if you find a call to this function somewhere, tell me. -- twm }
function TStringGrid_IsScrollBarVisible(_Grid: TCustomGrid; _Code: integer): boolean;

{: Returns the path to the application's executable including the trailing backslash }
function GetApplicationPath: string;

{: Center the child on the parent}
procedure TControl_Center(_Child: TControl; _Parent: TControl);

{: sets the Checked property without firing an OnClick event }
procedure TCheckBox_SetCheckedNoOnClick(_Chk: TCustomCheckBox; _Checked: boolean);

{: switches off "Windows Ghosting" in Win 2000 and XP
  This is a workaround for the bug that modal forms sometimes aren't modal in W2K and XP.
  Call in application startup. }
procedure DisableProcessWindowsGhosting;

implementation

uses
  Menus, // for StripHotKey function
  Messages,
  Consts,
  JPEG,
  StrUtils,
{$IFDEF GIFByRx}
  RxGConst,
  rxGif,
{$ENDIF GIFByRx}
  u_dzTranslator,
  u_dzConvertUtils,
  u_dzStringUtils;

// we need this to access protected methods
type
  TGridHack = class(TCustomGrid);
  TDbGridHack = class(TCustomDbGrid);

function TGrid_GetText(_Grid: TCustomGrid): string;
var
  Grid: TGridHack;
  Row, Col: integer;
begin
  Grid := TGridHack(_Grid);
  Result := '';
  for Row := 0 to Grid.RowCount - 1 do begin
    for Col := 0 to Grid.ColCount - 1 do begin
      if Col > 0 then
        Result := Result + #9;
      Result := Result + Grid.GetEditText(Col, Row);
    end;
    Result := Result + #13#10;
  end;
end;

procedure TStringGrid_SetRowCount(_Grid: TStringGrid; _RowCount: integer);
begin
  if _Grid.FixedRows >= _RowCount then
    _Grid.RowCount := _Grid.FixedRows + 1
  else
    _Grid.RowCount := _RowCount;
end;

procedure TStringGrid_SetColCount(_Grid: TStringGrid; _ColCount: integer);
begin
  if _Grid.FixedCols >= _ColCount then
    _Grid.ColCount := _Grid.FixedCols + 1
  else
    _Grid.ColCount := _ColCount;
end;

procedure TStringGrid_Clear(_Grid: TStringGrid);
var
  c: integer;
begin
  _Grid.RowCount := _Grid.FixedRows + 1;
  for c := _Grid.FixedCols to _Grid.ColCount - 1 do
    _Grid.Cells[c, _Grid.FixedRows] := '';
end;

function TStringGrid_DeleteRow(_Grid: TStringGrid; _Row: integer): boolean;
var
  r: integer;
  c: integer;
begin
  Assert(Assigned(_Grid));
  Assert(_Grid.FixedRows < _Grid.RowCount);

  if _Row = -1 then
    _Row := _Grid.Row;
  if (_Row < _Grid.FixedRows) or (_Row >= _Grid.RowCount) then begin
    Result := false;
    exit;
  end;
  if _Grid.RowCount <= _Grid.FixedRows + 1 then begin
    for c := 0 to _Grid.ColCount - 1 do
      _Grid.Cells[c, _Grid.FixedRows] := '';
    Result := true;
    exit;
  end;

  if _Grid.Row = _Grid.RowCount - 1 then
    _Grid.Row := _Grid.Row - 1;

  for r := _Row + 1 to _Grid.RowCount - 1 do begin
    for c := 0 to _Grid.ColCount - 1 do
      _Grid.Cells[c, r - 1] := _Grid.Cells[c, r];
  end;
  _Grid.RowCount := _Grid.RowCount - 1;
  Result := true;
end;

procedure TStringGrid_InsertRow(_Grid: TStringGrid; _Row: integer = -1);
var
  r: Integer;
  c: Integer;
begin
  Assert(Assigned(_Grid));

  if _Row = -1 then
    _Row := _Grid.Row;
  if (_Row < _Grid.FixedRows) or (_Row >= _Grid.RowCount) then
    exit;

  _Grid.RowCount := _Grid.RowCount + 1;
  for r := _Grid.RowCount - 1 downto _Row + 1 do begin
    for c := 0 to _Grid.ColCount - 1 do
      _Grid.Cells[c, r] := _Grid.Cells[c, r - 1];
  end;
  for c := 0 to _Grid.ColCount - 1 do
    _Grid.Cells[c, _Row] := '';
end;

function TStringGrid_CellToDouble(_grid: TStringGrid; _Col, _Row: integer; _FocusCell: boolean = true): double;
var
  s: string;
begin
  s := _grid.Cells[_Col, _Row];
  if not TryStr2Float(s, Result, #0) then begin
    if _FocusCell then begin
      _grid.Row := _Row;
      _grid.Col := _Col;
      _grid.SetFocus;
    end;
    raise EConvertError.CreateFmt(_('"%s" is not a valid floating point value.'), [s]);
  end;
end;

function TStringGrid_CellToInt(_grid: TStringGrid; _Col, _Row: integer; _FocusCell: boolean = true): integer;
var
  s: string;
begin
  s := _grid.Cells[_Col, _Row];
  if not TryStrToInt(s, Result) then begin
    if _FocusCell then begin
      _grid.Row := _Row;
      _grid.Col := _Col;
      _grid.SetFocus;
    end;
    raise EConvertError.CreateFmt(_('"%s" is not a valid integer value.'), [s]);
  end;
end;

function TEdit_TextToDouble(_ed: TEdit; _FocusControl: boolean = true): double;
var
  s: string;
begin
  s := _ed.Text;
  if not TryStr2Float(s, Result, #0) then begin
    if _FocusControl then begin
      _ed.SetFocus;
    end;
    raise EConvertError.CreateFmt(_('"%s" is not a valid floating point value.'), [s]);
  end;
end;

function TEdit_TextToInt(_ed: TEdit; _FocusControl: boolean = true): integer;
var
  s: string;
begin
  s := _ed.Text;
  if not TryStrToInt(s, Result) then begin
    if _FocusControl then begin
      _ed.SetFocus;
    end;
    raise EConvertError.CreateFmt(_('"%s" is not a valid integer value.'), [s]);
  end;
end;

function TTreeView_GetAsText(_Tree: TTreeView; _Indentation: integer = 2; _Marker: char = #0): string;
var
  Level: integer;
  Marker: string;

  function GetSubnodes(_tn: TTreeNode): string;
  var
    Child: TTreeNode;
  begin
    if Assigned(_tn) then begin
      Result := StringOfChar(' ', Level * _Indentation) + Marker + _tn.Text + #13#10;
      Inc(Level);
      try
        Child := _tn.getFirstChild;
        while Assigned(Child) do begin
          Result := Result + GetSubNodes(Child);
          Child := Child.getNextSibling;
        end;
      finally
        Dec(Level);
      end;
    end else
      Result := '';
  end;

begin
  if _Marker = #0 then
    Marker := ''
  else
    Marker := _Marker;
  Result := GetSubnodes(_Tree.Items.GetFirstNode);
end;

function ArrayContains(_Element: integer; const _Arr: array of integer): boolean;
var
  i: integer;
begin
  Result := false;
  for i := low(_Arr) to High(_Arr) do begin
    Result := _Arr[i] = _Element;
    if Result then
      exit;
  end;
end;

procedure HandleRow(_Grid: TGridHack; _Col, _Row: integer; var _MinWidth: integer);
var
  ColWidth: integer;
  ColText: string;
begin
  ColText := _Grid.GetEditText(_Col, _Row);
  ColWidth := _Grid.Canvas.TextWidth(ColText);
  if ColWidth > _MinWidth then
    _MinWidth := ColWidth;
end;

procedure TGrid_Resize(_Grid: TCustomGrid);
begin
  TGrid_Resize(_Grid, [], [], -1);
end;

procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet);
begin
  TGrid_Resize(_Grid, _Options, [], -1);
end;

procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet; _RowOffset: integer);
begin
  TGrid_Resize(_Grid, _Options, [], _RowOffset);
end;

procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet; const _ConstantCols: array of integer);
begin
  TGrid_Resize(_Grid, _Options, _ConstantCols, -1);
end;

procedure TGrid_Resize(_Grid: TCustomGrid; _Options: TResizeOptionSet; const _ConstantCols: array of integer; _RowOffset: integer);
var
  Col, Row: integer;
  Grid: TGridHack;
  MinWidth: integer;
  MinCol: integer;
  MaxCol: integer;
  MaxRow: integer;
  ColWidths: array of integer;
  FirstRow: integer;
  SumWidths: integer;
  Additional: integer;
begin
  Grid := TGridHack(_Grid);
  MaxCol := Grid.ColCount - 1;
  MinCol := 0;
  SetLength(ColWidths, MaxCol + 1);

  if _RowOffset = -1 then
    FirstRow := Grid.FixedRows
  else
    FirstRow := _RowOffset;

  MaxRow := FirstRow + 10;
  if (MaxRow >= Grid.RowCount) or (roUseAllRows in _Options) then
    MaxRow := Grid.RowCount - 1;
  SumWidths := MaxCol; // one spare pixel per column
  if goVertLine in Grid.Options then
    Inc(SumWidths, Grid.GridLineWidth);
  for Col := MinCol to MaxCol do begin
    if ArrayContains(Col, _ConstantCols) then
      MinWidth := Grid.ColWidths[Col]
    else begin
      MinWidth := Grid.DefaultColWidth;

      if not (roIgnoreHeader in _Options) then
        for Row := 0 to Grid.FixedRows - 1 do
          HandleRow(Grid, Col, Row, MinWidth);

      for Row := FirstRow to MaxRow do
        HandleRow(Grid, Col, Row, MinWidth);

      if goVertLine in Grid.Options then
        Inc(MinWidth, Grid.GridLineWidth);
      Inc(MinWidth, 4); // 2 Punkte rechts und links, wie in TStringGrid.DrawCell
    end;
    ColWidths[Col] := MinWidth;
    Inc(SumWidths, MinWidth);
  end;
  if (SumWidths < Grid.ClientWidth) and (roUseGridWidth in _Options)
    and (Length(_ConstantCols) < MaxCol + 1) then begin
    Additional := (Grid.ClientWidth - SumWidths) div (MaxCol + 1 - Length(_ConstantCols));
    for Col := MinCol to MaxCol do begin
      if not ArrayContains(Col, _ConstantCols) then begin
        Inc(ColWidths[Col], Additional);
        Inc(SumWidths, Additional);
      end;
    end;
    if SumWidths < Grid.ClientWidth then begin
      Col := MaxCol;
      while ArrayContains(Col, _ConstantCols) do
        Dec(Col);
      Inc(ColWidths[Col], Grid.ClientWidth - SumWidths);
    end;
  end;
  for Col := MinCol to MaxCol do
    Grid.ColWidths[Col] := ColWidths[Col];
end;

procedure TDbGrid_Resize(_Grid: TCustomDbGrid; _Options: TResizeOptionSet = []);
var
  Col, Row: integer;
  Grid: TDbGridHack;
  MinWidth: integer;
  ColWidth: integer;
  ColText: string;
  MinCol: integer;
  MaxCol: integer;
  MaxRow: integer;
  ColWidths: array of integer;
  FirstRow: integer;
  SumWidths: integer;
  Additional: integer;
  DBColumn: TColumn;
begin
  Grid := TDbGridHack(_Grid);
  MaxCol := Grid.ColCount - 1 - Grid.IndicatorOffset;
  MinCol := 0;
  SetLength(ColWidths, MaxCol + 1);
  FirstRow := 0;
  MaxRow := 10;
  if (MaxRow >= Grid.RowCount) or (roUseAllRows in _Options) then
    MaxRow := Grid.RowCount - 1;
  SumWidths := 4; // for some reason this must be 4 and not 0
  if dgColLines in Grid.Options then
    // there is one more grid line than there are columns
    Inc(SumWidths, Grid.GridLineWidth);
  if dgIndicator in Grid.Options then
    Inc(SumWidths, 21); // ColWidht[0] does not work :-(
  for Col := MinCol to MaxCol do begin
    MinWidth := 21;
    if not (roIgnoreHeader in _Options) then begin
      DBColumn := Grid.Columns[Col];
      ColText := DBColumn.Title.Caption;
      ColWidth := Grid.Canvas.TextWidth(ColText);
      if ColWidth > MinWidth then
        MinWidth := ColWidth;
    end;
    for Row := FirstRow to MaxRow do begin
      ColText := Grid.GetEditText(Col + Grid.IndicatorOffset, Row);
      ColWidth := Grid.Canvas.TextWidth(ColText);
      if ColWidth > MinWidth then
        MinWidth := ColWidth;
    end;
    if dgColLines in Grid.Options then
      Inc(MinWidth, Grid.GridLineWidth);
    Inc(MinWidth, 4); // 2 Punkte rechts und links, wie in TStringGrid.DrawCell
    ColWidths[Col] := MinWidth;
    Inc(SumWidths, MinWidth);
  end;
  if (SumWidths < Grid.ClientWidth) and (roUseGridWidth in _Options) then begin
    Additional := (Grid.ClientWidth - SumWidths) div (MaxCol + 1);
    for Col := MinCol to MaxCol do begin
      Inc(ColWidths[Col], Additional);
      Inc(SumWidths, Additional);
    end;
    if SumWidths < Grid.ClientWidth then
      Inc(ColWidths[MaxCol], Grid.ClientWidth - SumWidths);
  end;
  for Col := MinCol to MaxCol do
    Grid.Columns[Col].Width := ColWidths[Col];
end;

function TPageControl_AddTabSheet(_PageControl: TPageControl; const _Caption: string): TTabSheet;
begin
  Result := TTabSheet.Create(_PageControl);
  Result.Parent := _PageControl;
  Result.PageControl := _PageControl;
  Result.Caption := _Caption;
end;

procedure DrawTab(_TabControl: TCustomTabControl; const _Caption: string;
  const _Rect: TRect; _Active: boolean);
var
  TopOffs: integer;
begin
  if _Active then
    TopOffs := 4
  else
    TopOffs := 0;
  _TabControl.Canvas.TextRect(_Rect, _Rect.Left + 4, _Rect.Top + TopOffs, _Caption);
end;

procedure TPageControl_DrawTab(_PageControl: TPageControl; _TabIndex: integer;
  const _Rect: TRect; _Active: boolean);
begin
  DrawTab(_PageControl, _PageControl.Pages[_TabIndex].Caption, _Rect, _Active);
end;

procedure TTabControl_DrawTab(_TabControl: TTabControl; _TabIndex: integer;
  const _Rect: TRect; _Active: boolean);
begin
  DrawTab(_TabControl, _TabControl.Tabs[_TabIndex], _Rect, _Active);
end;

procedure TTabControl_AdjustTabWidth(_TabControl: TTabControl; _Form: TForm; _MinWidth: integer = 80);
var
  i: integer;
  MinWidth: integer;
  w: integer;
begin
  MinWidth := _MinWidth;
  for i := 0 to _TabControl.Tabs.Count - 1 do begin
    w := _TabControl.Canvas.TextWidth(_TabControl.Tabs[i]) + 16;
    if w > MinWidth then
      MinWidth := w;
  end;
  w := _TabControl.TabHeight;
  if (w < MinWidth) or (w > _MinWidth) then begin
    w := MinWidth - w;
    if Assigned(_Form) then
      _Form.Width := _Form.Width + w;
    if not Assigned(_Form) or not (akRight in _TabControl.Anchors) then
      _TabControl.Width := _TabControl.Width + w;
    _TabControl.TabHeight := MinWidth;
  end;
end;

procedure SetControlEnabled(_Control: TControl; _Enabled: boolean);
var
  i: integer;
  Container: TWinControl;
  ctrl: TControl;
begin
  if _Control is TWinControl then begin
    Container := _Control as TWinControl;
    for i := 0 to Container.ControlCount - 1 do begin
      ctrl := Container.Controls[i];
      SetControlEnabled(Ctrl, _Enabled);
    end;
  end;
  _Control.Enabled := _Enabled;
end;

function TComboBox_SelectByObject(_cmb: TCustomCombobox; _Value: pointer): boolean;
var
  i: integer;
begin
  for i := 0 to _cmb.Items.Count - 1 do begin
    Result := (_cmb.Items.Objects[i] = _Value);
    if Result then begin
      _cmb.ItemIndex := i;
      exit;
    end;
  end;
  Result := false;
end;

function TComboBox_GetObjectCaption(_cmb: TCustomCombobox; _Obj: pointer; out _s: string): boolean;
var
  i: integer;
begin
  for i := 0 to _cmb.Items.Count - 1 do begin
    Result := (_cmb.Items.Objects[i] = _Obj);
    if Result then begin
      _s := _cmb.Items[i];
      exit;
    end;
  end;
  Result := false;
end;

function TComboBox_GetSelectedObject(_cmb: TCustomCombobox; out _Idx: integer;
  out _Obj: pointer; _FocusControl: boolean = false): boolean;
begin
  _Idx := _cmb.ItemIndex;
  Result := _Idx <> -1;
  if Result then
    _Obj := _cmb.Items.Objects[_Idx]
  else if _FocusControl then
    _cmb.SetFocus;
end;

function TComboBox_GetSelected(_cmb: TCustomComboBox; out _Item: string;
  _FocusControl: boolean = false): boolean;
var
  Idx: integer;
begin
  Idx := _cmb.ItemIndex;
  Result := Idx <> -1;
  if Result then
    _Item := _cmb.Items[Idx]
  else if _FocusControl then
    _cmb.SetFocus;
end;

function TListBox_GetSelectedObject(_lst: TCustomListbox; out _Idx: integer; out _Obj: pointer): boolean;
begin
  _Idx := _lst.ItemIndex;
  Result := _Idx <> -1;
  if Result then
    _Obj := _lst.Items.Objects[_Idx];
end;

function TComboBox_Select(_cmb: TCustomComboBox; const _Item: string; _DefaultIdx: integer = -1): integer;
begin
  Result := _Cmb.Items.IndexOf(_Item);
  if Result = -1 then
    Result := _DefaultIdx;
  _Cmb.ItemIndex := Result;
end;

type
  TRadioGroupHack = class(TCustomRadioGroup);

function TRadioGroup_GetItemCaption(_rg: TCustomRadioGroup;
  out _Caption: string; _Idx: integer = -1): boolean;
var
  Hack: TRadioGroupHack;
begin
  Hack := TRadioGroupHack(_rg);
  if _Idx = -1 then
    _Idx := Hack.ItemIndex;
  Result := (_Idx <> -1) and (_Idx < Hack.Items.Count);
  if Result then
    _Caption := StripHotKey(Hack.Items[_Idx]);
end;

function TRadioGroup_GetSelectedObject(_rg: TCustomRadioGroup; out _Idx: integer; out _Obj: pointer): boolean;
var
  Hack: TRadioGroupHack;
begin
  Hack := TRadioGroupHack(_rg);
  _Idx := Hack.ItemIndex;
  Result := _Idx <> -1;
  if Result then
    _Obj := Hack.Items.Objects[_Idx];
end;

function TRichEdit_WriteToString(_Re: TRichEdit): string;
var
  st: TMemoryStream;
begin
  st := TMemoryStream.Create;
  try
    _Re.Lines.SaveToStream(st);
    Result := PChar(st.Memory);
  finally
    st.Free;
  end;
end;

procedure TRichEdit_ReadFromString(_Re: TRichEdit; const _S: string);
var
  st: TMemoryStream;
begin
  st := TMemoryStream.Create;
  try
    st.Write(_s[1], Length(_s));
    st.Position := 0;
    _Re.Lines.LoadFromStream(st);
  finally
    st.Free;
  end;
end;

function TPicture_WriteToString(_Pic: TPicture): string;
var
  st: TStringStream;
begin
  Result := '';
  st := TStringStream.Create('');
  try
    if Assigned(_Pic.Graphic) then begin
      Result := _Pic.Graphic.ClassName;
      _Pic.Graphic.SaveToStream(st);
      Result := Result + #26 + st.DataString;
    end
  finally
    st.Free;
  end;
end;

procedure TPicture_ReadFromString(_Pic: TPicture; const _S: string);
var
  st: TStringStream;
  Klasse: string;
  Data: string;
  p: integer;
  GraphicClass: TGraphicClass;
  GraphicObj: TGraphic;
begin
  if _s = '' then
    exit;
  p := Pos(#26, _s);
  if p = 0 then
    exit;
  Klasse := LeftStr(_s, p - 1);
  Data := TailStr(_s, p + 1);

  st := TStringStream.Create(Data);
  try
    GraphicClass := GetFileFormats.FindClassName(Klasse);
    if GraphicClass <> nil then begin
      GraphicObj := GraphicClass.Create;
      GraphicObj.LoadFromStream(st);
      _Pic.Graphic := GraphicObj;
    end;
  finally
    st.Free;
  end;
end;

function AddLabeledControl(_Idx: integer; const _Caption: string; _ctrl: TControl): TLabel;
begin
  Result := TLabel.Create(_ctrl.Owner);
  Result.Parent := _ctrl.Parent;
  Result.Left := 8;
  Result.Caption := _Caption;
  _ctrl.Top := _Idx * 24 + 16;
  Result.Top := _ctrl.Top + (_ctrl.Height - Result.Height) div 2;
end;

function CalcTextHeight(_Ctrl: TWinControl; const _Text: string; _Width: integer = -1): integer;
var
  Rect: TRect;
begin
  _Ctrl.HandleNeeded;
  Rect := _Ctrl.BoundsRect;
  if _Width <> -1 then
    Rect.Right := Rect.Left + _Width - 1;
  Result := DrawText(_Ctrl.Handle, PChar(_Text), Length(_Text), Rect,
    DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
end;

type
  TGraphicControlHack = class(TGraphicControl)
  end;

function CalcTextHeight(_Ctrl: TGraphicControl; const _Text: string; _Width: integer = -1): integer; overload;
var
  Rect: TRect;
begin
  Rect := _Ctrl.BoundsRect;
  if _Width <> -1 then
    Rect.Right := Rect.Left + _Width - 1;
  Result := DrawText(TGraphicControlHack(_Ctrl).Canvas.Handle, PChar(_Text),
    Length(_Text), Rect, DT_LEFT or DT_WORDBREAK or DT_CALCRECT);
end;

type
  THackGrid = class(TCustomGrid);

function TStringGrid_IsScrollBarVisible(_Grid: TCustomGrid; _Code: integer): boolean;
var
  Min, Max: integer;
  Grid: THackGrid;
begin
  Result := false;
  if not _Grid.HandleAllocated then
    exit;
  Grid := THackGrid(_Grid);
  if (Grid.Scrollbars = ssBoth) or
    ((_Code = SB_HORZ) and (Grid.Scrollbars = ssHorizontal)) or
    ((_Code = SB_VERT) and (Grid.Scrollbars = ssVertical)) then begin
    GetScrollRange(_Grid.Handle, _Code, Min, Max);
    Result := Min <> Max;
  end;
end;

function GetApplicationPath: string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
end;

function TRichEdit_RowToCharIndex(_Re: TRichEdit; _Row: integer): integer;
begin
  Result := _Re.Perform(EM_LINEINDEX, _Row, 0);
end;

function TRichEdit_CharIndexToRow(_Re: TRichEdit; _Idx: integer): integer;
begin
  Result := _Re.Perform(EM_LINEFROMCHAR, _Idx, 0);
end;

function TRichEdit_GetCurrentRow(_Re: TRichEdit): integer;
begin
  Result := TRichEdit_CharIndexToRow(_Re, _Re.SelStart);
end;

procedure TRichEdit_ScrollToCaret(_Re: TRichEdit);
begin
  _Re.Perform(EM_SCROLLCARET, 0, 0);
end;

procedure TRichEdit_WriteLn(_Re: TRichEdit; const _s: string; _Color: TColor = clBlack; _Style: TFontStyles = []);
begin
  _Re.SelAttributes.Color := _Color;
  _Re.SelAttributes.Style := _Style;
  _Re.Lines.Add(_s);
  _Re.SelAttributes.Color := clBlack;
  _Re.SelAttributes.Style := [];
end;

procedure TControl_Center(_Child: TControl; _Parent: TControl);
begin
  _child.left := (_parent.width - _child.width) div 2;
  _child.top := (_parent.height - _child.height) div 2;
end;

type
  TCheckBox = class(TCustomCheckBox)
  end;

procedure TCheckBox_SetCheckedNoOnClick(_Chk: TCustomCheckBox; _Checked: boolean);
var
  Chk: TCheckBox;
begin
  Chk := TCheckBox(_Chk);
  Chk.ClicksDisabled := true;
  try
    Chk.Checked := _Checked;
  finally
    Chk.ClicksDisabled := false;
  end;
end;

procedure DisableProcessWindowsGhosting;
var
  DisableProcessWindowsGhostingProc: procedure;
begin
  DisableProcessWindowsGhostingProc := GetProcAddress(
    GetModuleHandle('user32.dll'),
    'DisableProcessWindowsGhosting');
  if Assigned(DisableProcessWindowsGhostingProc) then
    DisableProcessWindowsGhostingProc;
end;

type
  PFileFormat = ^TFileFormat;
  TFileFormat = record
    GraphicClass: TGraphicClass;
    Extension: string;
    Description: string;
    DescResID: Integer;
  end;

constructor TFileFormatsList.Create;
begin
  inherited Create;
  Add('wmf', SVMetafiles, 0, TMetafile);
  Add('emf', SVEnhMetafiles, 0, TMetafile);
  Add('ico', SVIcons, 0, TIcon);
  Add('bmp', SVBitmaps, 0, TBitmap);
{$IFDEF GIFByRx}
  Add('gif', LoadStr(SGIFImage), 0, TGIFImage);
{$ENDIF GIFByRx}
  Add('jpg', 'JPEG Files', 0, TJPEGImage);
end;

destructor TFileFormatsList.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Dispose(PFileFormat(Items[I]));
  inherited Destroy;
end;

procedure TFileFormatsList.Add(const Ext, Desc: string; DescID: Integer;
  AClass: TGraphicClass);
var
  NewRec: PFileFormat;
begin
  New(NewRec);
  with NewRec^ do begin
    Extension := AnsiLowerCase(Ext);
    GraphicClass := AClass;
    Description := Desc;
    DescResID := DescID;
  end;
  inherited Add(NewRec);
end;

function TFileFormatsList.FindExt(Ext: string): TGraphicClass;
var
  I: Integer;
begin
  Ext := AnsiLowerCase(Ext);
  for I := Count - 1 downto 0 do
    with PFileFormat(Items[I])^ do
      if Extension = Ext then begin
        Result := GraphicClass;
        Exit;
      end;
  Result := nil;
end;

function TFileFormatsList.GetFilterString(GraphicClass: TGraphicClass = nil): string;
var
  s: string;
begin
  if GraphicClass = nil then
    GraphicClass := TGraphic;
  BuildFilterStrings(GraphicClass, Result, s);
end;

function TFileFormatsList.FindClassName(const ClassName: string): TGraphicClass;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do begin
    Result := PFileFormat(Items[I])^.GraphicClass;
    if Result.ClassName = Classname then
      Exit;
  end;
  Result := nil;
end;

procedure TFileFormatsList.Remove(AClass: TGraphicClass);
var
  I: Integer;
  P: PFileFormat;
begin
  for I := Count - 1 downto 0 do begin
    P := PFileFormat(Items[I]);
    if P^.GraphicClass.InheritsFrom(AClass) then begin
      Dispose(P);
      Delete(I);
    end;
  end;
end;

procedure TFileFormatsList.BuildFilterStrings(GraphicClass: TGraphicClass;
  var Descriptions, Filters: string);
var
  C, I: Integer;
  P: PFileFormat;
begin
  Descriptions := '';
  Filters := '';
  C := 0;
  for I := Count - 1 downto 0 do begin
    P := PFileFormat(Items[I]);
    if P^.GraphicClass.InheritsFrom(GraphicClass) and (P^.Extension <> '') then
      with P^ do begin
        if C <> 0 then begin
          Descriptions := Descriptions + '|';
          Filters := Filters + ';';
        end;
        if (Description = '') and (DescResID <> 0) then
          Description := LoadStr(DescResID);
        FmtStr(Descriptions, '%s%s (*.%s)|*.%2:s', [Descriptions, Description, Extension]);
        FmtStr(Filters, '%s*.%s', [Filters, Extension]);
        Inc(C);
      end;
  end;
  if C > 1 then
    FmtStr(Descriptions, '%s (%s)|%1:s|%s', [sAllFilter, Filters, Descriptions]);
end;

var
  FileFormats: TFileFormatsList = nil;

function GetFileFormats: TFileFormatsList;
begin
  if FileFormats = nil then
    FileFormats := TFileFormatsList.Create;
  Result := FileFormats;
end;

procedure TMemo_DeleteTopLines(_Memo: TMemo; _Retain: integer);
const
  EmptyStr: PChar = '';
var
  Offset: Integer;
  cnt: Integer;
begin
  cnt := _Memo.Lines.Count;
  if cnt <= _Retain then
    Exit;
  Dec(cnt, _Retain);

  Offset := SendMessage(_Memo.Handle, EM_LINEINDEX, cnt - 1, 0);
  if (Offset < 0) or (cnt = 0) then
    Offset := SendMessage(_Memo.Handle, EM_LINELENGTH, 0, 0);
  SendMessage(_Memo.Handle, EM_SETSEL, 0, Offset);
  SendMessage(_Memo.Handle, EM_REPLACESEL, 0, Longint(EmptyStr));
end;

procedure TMemo_ScrollToEnd(_Memo: TMemo);
var
  cnt: Integer;
begin
  cnt := SendMessage(_Memo.Handle, EM_GETLINECOUNT, 0, 0);
  SendMessage(_Memo.Handle, EM_LINESCROLL, 0, cnt);
end;

end.

