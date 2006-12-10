unit WebSearchDMUnit;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ActnList,
  Menus,
  ExtCtrls;

type
  TWebSearchDM = class(TDataModule)
    ActionList1: TActionList;
    WebSearchAction: TAction;
    PopupMenu1: TPopupMenu;
    WebSearchItem: TMenuItem;
    N1: TMenuItem;
    Timer1: TTimer;
    ScriptRunAction: TAction;
    ScriptRunItem: TMenuItem;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure WebSearchSubItemClick(Sender: TObject);
    procedure WebSearchExplainClick(Sender: TObject);
    procedure RunScriptSubItemClick(Sender: TObject);
    procedure RunScriptExplainClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FLoadedMain: Boolean;
    FWebMenuItems: TList;
    FScriptItems: TList;
    FURLS: TStringList;
    FWebItems: TStringList;
    FScripts: TStringList;

    procedure ClearMenuItems(List: TList);
    procedure ClearScriptMenuItems;
    procedure ClearWebMenuItems;
    procedure LoadMainPopup(HelpMenu: TMenuItem);
    procedure LoadScripts;
    procedure LoadUrls;
    procedure RefreshMenuItems;
    procedure RefreshScriptMenuItems;
    procedure RefreshWebMenuItems;
    procedure SetTimerRefreshing;
  public
  end;

var
  WebSearchDM: TWebSearchDM;

implementation

uses
  HttpApp,
  MSScriptControl_TLB,
  MLRStringsLib_TLB,
  MLROTExpertsUnit,
  Registry,
  ShellAPI;

procedure EditStrings(const MainSub: string;
  Code: TStrings; Selection: TStrings; SiteHandle: THandle;
  var Modified: Boolean);
var
  SelectionDispatch: IMLRAutoStrings;
  Script: TMSSScriptControl;
begin
  Script := TMSSScriptControl.Create(nil);
  try
    SelectionDispatch := CoMLRAutoStrings.Create;
    SelectionDispatch.Text := Selection.Text;
    SelectionDispatch.Modified := False;

    Script.Language := 'VBScript';
    Script.AddObject('Selection', SelectionDispatch, False);
    Script.AllowUI := True;
    Script.SitehWnd := SiteHandle;
    Script.AddCode(Code.Text);
    Script.ExecuteStatement(MainSub);
    Modified := SelectionDispatch.Modified;
    if Modified then
      Selection.Text := SelectionDispatch.Text;
  finally
    Script.Free;
  end;
end;

{$R *.DFM}

procedure TWebSearchDM.DataModuleCreate(Sender: TObject);
begin
  FWebMenuItems := TList.Create;
  FScriptItems := TList.Create;
  FWebItems := TStringList.Create;
  FURLS := TStringList.Create;
  FScripts := TStringList.Create;

  RefreshMenuItems;
end;

procedure TWebSearchDM.DataModuleDestroy(Sender: TObject);
begin
  ClearWebMenuItems;
  ClearScriptMenuItems;

  FWebItems.Free;
  FURLS.Free;
  FWebMenuItems.Free;
  FScripts.Free;
  FScriptItems.Free;
end;

procedure TWebSearchDM.LoadUrls;
var
  i: Integer;
  Reg: TRegistry;
begin
  FURLS.Clear;
  FWebItems.Clear;
  Reg := TRegistry.Create;
  try
    Reg.OpenKey(GetBaseRegistryKey, True);
    Reg.OpenKey('MLRWebSearch', True);
    { Add some defaults. }
    if not Reg.ValueExists('CodeCentral') then
      Reg.WriteString('CodeCentral',
        'http://ww6.borland.com/codecentral/ccweb.exe/finder?edKeywords=%s');
    if not Reg.ValueExists('MER Systems') then
      Reg.WriteString('MER Systems',
        'http://www.mers.com/cgi-bin/srchcgi.exe/EXECSEARCH?pageno=1&' +
        'linktype=search&searchtext=%s&grouptext=');
    if not Reg.ValueExists('Yahoo!') then
      Reg.WriteString('Yahoo!',
        'http://search.yahoo.com/bin/search?p=%s');
    Reg.GetValueNames(FWebItems);
    for i := 0 to FWebItems.Count - 1 do
      FURLS.Add(Reg.ReadString(FWebItems[i]));
  finally
    Reg.Free;
  end;
end;

procedure ListFiles(const Path: string; Strings: TStrings);
var
  Found: Integer;
  SearchRec: TSearchRec;
begin
  Found := FindFirst(Path, faAnyFile, SearchRec);
  while Found = 0 do begin
    if (SearchRec.Attr and faDirectory) <> faDirectory then
      Strings.Add(SearchRec.Name);
    Found := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
end;

procedure TWebSearchDM.LoadScripts;
var
  Folder: string;
  i: Integer;
  Reg: TRegistry;
begin
  FWebItems.Clear;
  Reg := TRegistry.Create;
  try
    Reg.OpenKey(GetBaseRegistryKey, True);
    Reg.OpenKey('MLRScriptsFolder', True);
    { Add the compiler's folder. }
    if not Reg.ValueExists('Folder') then
      Reg.WriteString('Folder',
        IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) +
        'Scripts')
    else
      Folder := Reg.ReadString('Folder');
  finally
    Reg.Free;
  end;
  Folder := IncludeTrailingPathDelimiter(Folder);
  FScripts.Clear;
  ListFiles(Folder + '*.vbs', FScripts);
  for i := 0 to FScripts.Count - 1 do
    FScripts[i] := Folder + FScripts[i];
end;

procedure TWebSearchDM.WebSearchSubItemClick(Sender: TObject);
var
  Index: Integer;
  Value: string;
begin
  Index := (Sender as TMenuItem).Tag;
  Value := GetSelectedText;
  if Value = '' then
    if not InputQuery(FWebItems[Index], 'What would you like to search for?',
      Value) then Exit;
  Value := HttpEncode(Value);
  Value := Format(FURLS[Index], [Value]);
  ShellExecute(0, nil, PChar(Value), nil, nil, SW_NORMAL);
end;

procedure TWebSearchDM.WebSearchExplainClick(Sender: TObject);
begin
  ShowMessage(
    'You can add and remove items from the Current User Registry key:'#13#13 +
    GetBaseRegistryKey +
    #13#13'subkey: MLRWebSearch' +
    #13#13'You can also turn off refreshing (which may freeze the menu)' +
    #13'with the Options\Refresh boolean value (set to 0).' +
    #13'You can still refresh the menu by clicking this item.');
  RefreshMenuItems;
end;

procedure TWebSearchDM.RunScriptExplainClick(Sender: TObject);
begin
  ShowMessage(
    'Scripts should be placed in the folder pointed by '#13 +
    'the registry value at the Current User Registry key:'#13#13 +
    GetBaseRegistryKey +
    #13#13'subkey: MLRScriptsFolder' +
    #13#13'The value should be assigned to "Folder".' +
    #13'The refreshing behaviour is shared with the Web Items menu.' +
    #13'You can always refresh the menu by clicking this item.' +
    #13'Have a nice day.'
    );
  RefreshMenuItems;
end;

procedure TWebSearchDM.Timer1Timer(Sender: TObject);
begin
  RefreshMenuItems;
end;

procedure TWebSearchDM.RefreshMenuItems;
begin
  RefreshScriptMenuItems;
  RefreshWebMenuItems;
end;

procedure TWebSearchDM.RefreshScriptMenuItems;
var
  i: Integer;
  Item, HelpMenu: TMenuItem;
begin
  HelpMenu := GetDelphiCustomMenu('MLR');
  if HelpMenu = nil then begin { The menu may not exist yet. }
    Timer1.Enabled := True;
    Exit;
  end;

  LoadScripts;
  ClearScriptMenuItems;

  if not FLoadedMain then
    LoadMainPopup(HelpMenu);

  for i := 0 to FScripts.Count - 1 do begin
    Item := TMenuItem.Create(nil);
    FScriptItems.Add(Item);
    Item.Caption := ChangeFileExt(ExtractFileName(FScripts[i]), '');
    Item.Tag := i;
    Item.OnClick := RunScriptSubItemClick;
    ScriptRunItem.Add(Item);
  end;

  Item := TMenuItem.Create(nil);
  FScriptItems.Add(Item);
  Item.Caption := '(customize)';
  Item.OnClick := RunScriptExplainClick;
  ScriptRunItem.Add(Item);
  SetTimerRefreshing;

  ScriptRunItem.Visible := FScripts.Count > 0;
end;

procedure TWebSearchDM.RefreshWebMenuItems;
var
  i: Integer;
  Item, HelpMenu: TMenuItem;
begin
  HelpMenu := GetDelphiCustomMenu('MLR');
  if HelpMenu = nil then begin { The menu may not exist yet. }
    Timer1.Enabled := True;
    Exit;
  end;

  LoadUrls;
  ClearWebMenuItems;

  if not FLoadedMain then
    LoadMainPopup(HelpMenu);

  for i := 0 to FURLS.Count - 1 do begin
    Item := TMenuItem.Create(nil);
    FWebMenuItems.Add(Item);
    Item.Caption := FWebItems[i];
    Item.Tag := i;
    Item.OnClick := WebSearchSubItemClick;
    WebSearchItem.Add(Item);
  end;

  Item := TMenuItem.Create(nil);
  FWebMenuItems.Add(Item);
  Item.Caption := '(customize)';
  Item.OnClick := WebSearchExplainClick;
  WebSearchItem.Add(Item);
  SetTimerRefreshing;

  if FURLS.Count = 0 then
    WebSearchItem.Visible := False
  else
    WebSearchItem.Visible := True;
end;

procedure TWebSearchDM.ClearMenuItems(List: TList);
var
  i: Integer;
begin
  if (not Assigned(List)) or (List.Count = 0) then
    Exit;
  for i := 0 to List.Count - 1 do
    TMenuItem(List[i]).Free;
  List.Clear;
end;

procedure TWebSearchDM.ClearScriptMenuItems;
begin
  ClearMenuItems(FScriptItems);
end;

procedure TWebSearchDM.ClearWebMenuItems;
begin
  ClearMenuItems(FWebMenuItems);
end;

procedure TWebSearchDM.SetTimerRefreshing;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.OpenKey(GetBaseRegistryKey, True);
    Reg.OpenKey('MLRWebSearch', True);
    Reg.OpenKey('Options', True);
    if Reg.ValueExists('Refresh') then begin
      Timer1.Enabled := Reg.ReadBool('Refresh');
    end else begin
      Reg.WriteBool('Refresh', True);
      Timer1.Enabled := True;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TWebSearchDM.RunScriptSubItemClick(Sender: TObject);
var
  Code: TStringList;
  Modified: Boolean;
  Strings: TStringList;
  FileName: string;
begin
  FileName := FScripts[(Sender as TMenuItem).Tag];
  Code := nil;
  Strings := TStringList.Create;
  try
    Code := TStringList.Create;
    Code.LoadFromFile(FileName);
    Strings.Text := GetSelectedText;
    EditStrings('Main', Code, Strings, 0, Modified);
    if Modified then
      SetSelectedText(Strings.Text);
  finally
    Code.Free;
    Strings.Free;
  end;
end;

procedure TWebSearchDM.LoadMainPopup(HelpMenu: TMenuItem);
var
  i: Integer;
  Item: TMenuItem;
  InsertPosition: Integer;
begin
  InsertPosition := HelpMenu.Count;
  for i := PopupMenu1.Items.Count - 1 downto 0 do begin
    Item := PopupMenu1.Items[i];
    PopupMenu1.Items.Delete(i);
    HelpMenu.Insert(InsertPosition, Item);
  end;
  FLoadedMain := True;
end;

end.

