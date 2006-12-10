unit ActionsToMenuDMUnit;

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
  TActionsToMenuDM = class(TDataModule)
    Actions: TActionList;
    ActionsToMenuAction: TAction;
    PopupMenu1: TPopupMenu;
    ActionsToMenuMenu: TMenuItem;
    N1: TMenuItem;
    SortLinesAction: TAction;
    SortLinesMenu: TMenuItem;
    SortProceduresAction: TAction;
    SortProceduresMenu: TMenuItem;
    procedure ActionsToMenuActionExecute(Sender: TObject);
    procedure ActionsToMenuActionUpdate(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
    procedure SortLinesActionExecute(Sender: TObject);
    procedure SortLinesActionUpdate(Sender: TObject);
    procedure SortProceduresActionExecute(Sender: TObject);
    procedure SortProceduresActionUpdate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ActionsToMenuDM: TActionsToMenuDM;

implementation

uses DesignIntf,
  MLROTExpertsUnit;

{$R *.DFM}

procedure TActionsToMenuDM.DataModuleCreate(Sender: TObject);
var
  Action: TCustomAction;
  i, InsertPosition: Integer;
  DataMenu, Item: TMenuItem;
begin
  //DataMenu := GetDelphiDataMenu;
  DataMenu := GetDelphiCustomMenu('MLR');
  InsertPosition := DataMenu.Count;
  for i := PopupMenu1.Items.Count - 1 downto 0 do begin
    Item := PopupMenu1.Items[i];
    if (not Item.Bitmap.Empty) and (Item.Action <> nil) then
      if Item.Action is TCustomAction then begin
        Action := TCustomAction(Item.Action);
        Action.ImageIndex := AddDelphiImage(Item.Bitmap, clFuchsia);
      end;
    PopupMenu1.Items.Delete(i);
    DataMenu.Insert(InsertPosition, Item);
  end;
end;

procedure TActionsToMenuDM.ActionsToMenuActionUpdate(Sender: TObject);
var
  Component: TComponent;
begin
  Component := GetSelectedComponent;
  (Sender as TAction).Enabled := Assigned(Component) and
    (Component is TActionList);
end;

procedure TActionsToMenuDM.ActionsToMenuActionExecute(Sender: TObject);
var
  Action: TContainedAction;
  ActionList: TActionList;
  Component: TComponent;
  i: Integer;
  Menu: TMainMenu;
  MenuItem: TMenuItem;
  TopMenuItem: TMenuItem;
begin
  ActionList := GetSelectedComponent as TActionList;
  Component := CreateCurrentFormComponent('TMainMenu', 24, 24, 24, 24);
  if not Assigned(Component) then Exit;
  Menu := Component as TMainMenu;
  for i := 0 to ActionList.ActionCount - 1 do begin
    Action := ActionList.Actions[i];
    if Action.Category = '' then
      continue;
    TopMenuItem := Menu.Items.Find(Action.Category);
    if TopMenuItem = nil then begin
      TopMenuItem := TMenuItem(
        CreateCurrentFormComponent('TMenuItem', 24, 24, 24, 24));
      if TopMenuItem = nil then continue;
      TopMenuItem.Caption := '&' + Action.Category;
      Menu.Items.Add(TopMenuItem);
    end;
    MenuItem := TMenuItem(
      CreateCurrentFormComponent('TMenuItem', 24, 24, 24, 24));
    if MenuItem = nil then continue;
    TopMenuItem.Add(MenuItem);
    MenuItem.Action := Action;
  end;
end;

procedure TActionsToMenuDM.SortLinesActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := GetSelectedText <> '';
end;

procedure TActionsToMenuDM.SortLinesActionExecute(Sender: TObject);
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Text := GetSelectedText;
    if Strings.Count <= 1 then Exit;
    Strings.Sort;
    SetSelectedText(Strings.Text);
  finally
    Strings.Free;
  end;
end;

procedure TActionsToMenuDM.SortProceduresActionUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := GetSelectedText <> '';
end;

function ProcedureSort(List: TStringList; Index1, Index2: Integer): Integer;
type
  TDeclarationKind = (dkNone, dkConstructor, dkDestructor, dkMethod,
    dkProperty, dkEvent);
var
  Declaration1, Declaration2: TDeclarationKind;
  Name1, Name2: string;

  procedure AnalyzeDeclaration(Text: string; var Declaration: TDeclarationKind;
    var Name: string);
  type
    TParseState = (psLeading, psType, psName, psDone);
  const
    WhiteSpace = [' ', #13, #10, #9];
    IdDelim = [' ', #13, #10, #9, '(', '[', ';', ':'];
  var
    FirstWord: string;
    P: PChar;
    ParseState: TParseState;
  begin
    Declaration := dkNone;
    Name := '';
    if Text = '' then Exit;
    P := PChar(Text);
    ParseState := psLeading;
    while (P^ <> #0) and (ParseState <> psDone) do begin
      case ParseState of
        psLeading:
          if not (P^ in WhiteSpace) then begin
            FirstWord := P^;
            ParseState := psType;
          end;
        psType:
          if not (P^ in WhiteSpace) then
            FirstWord := FirstWord + P^
          else begin
            while (P^ <> #0) and (P^ in WhiteSpace) do
              Inc(P);
            ParseState := psName;
            if P^ <> #0 then
              Name := P^;
          end;
        psName:
          if P^ in IdDelim then
            ParseState := psDone
          else
            Name := Name + P^;
      end;
      Inc(P);
    end;
    if not (ParseState in [psName, psDone]) then
      Exit;
    FirstWord := UpperCase(Trim(FirstWord));
    if FirstWord = 'CONSTRUCTOR' then
      Declaration := dkConstructor
    else if FirstWord = 'DESTRUCTOR' then
      Declaration := dkDestructor
    else if (FirstWord = 'PROCEDURE') or (FirstWord = 'FUNCTION') then
      Declaration := dkMethod
    else if FirstWord = 'PROPERTY' then begin
      if UpperCase(Copy(Name, 1, 2)) = 'ON' then
        Declaration := dkEvent
      else
        Declaration := dkProperty;
    end else begin
      Declaration := dkNone;
      Name := FirstWord;
    end;
  end;
begin
  { The sorting rules are as follows:
    . fields go to the very very top.
    . constructors go to the top, followed by destructors.
    . functions/procedures are considered to be the same thing.
    . properties follow function/procedures.
    . property Onxxxx are sent to the bottom (events go at the end).
    The sort needs all declarations to be one-liners.
  }
  AnalyzeDeclaration(List[Index1], Declaration1, Name1);
  AnalyzeDeclaration(List[Index2], Declaration2, Name2);
  Result := Ord(Declaration1) - Ord(Declaration2);
  if Result = 0 then
    Result := AnsiCompareText(Name1, Name2);
end;

procedure TActionsToMenuDM.SortProceduresActionExecute(Sender: TObject);
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    Strings.Text := GetSelectedText;
    if Strings.Count <= 1 then Exit;
    DebugOTA('Before sort:');
    DebugOTA(Strings.Text);
    Strings.CustomSort(ProcedureSort);
    DebugOTA('After sort:');
    DebugOTA(Strings.Text);
    SetSelectedText(Strings.Text);
  finally
    Strings.Free;
  end;
end;

end.

