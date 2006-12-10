unit MLROTExpertsUnit;

interface

uses
  Classes,
  Forms,
  Graphics,
  Menus,
  ToolsAPI;

type
  { We use just one expert, even if it's named after the first expert it
    sets up. }
  TMLRActionsToMenuExpert = class(TInterfacedObject, IOTAWizard, IOTANotifier)
    { IOTANotifier implementation. }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
    { IOTAWizard implementation. }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    { Object-specific. }
    constructor Create;
    destructor Destroy; override;
  end;

function AddDelphiImage(Bitmap: TBitmap; MaskColor: TColor = clWindow): Integer;
function CreateCurrentFormComponent(const TypeName: string;
  X, Y, w, H: Integer): TComponent;
function DebugDisabled(const Creator: string): Boolean;
procedure DebugOTA(const Text: string);
function CurrentForm: TCustomForm;
function GetBaseRegistryKey: string;
function GetDelphiCustomMenu(const Caption: string): TMenuItem;
function GetDelphiDataMenu: TMenuItem;
function GetDelphiHelpMenu: TMenuItem;
function GetSelectedComponent: TComponent;
function GetSelectedText: string;
procedure Register;
procedure SetSelectedText(Text: string);

implementation

uses
  ActionsToMenuDMUnit,
  OTADebugFormUnit,
  Registry,
  WebSearchDMUnit,
  Windows;

function GetEditView: IOTAEditView; forward;
function GetFormEditor: IOTAFormEditor; forward;
function GetSourceEditor: IOTASourceEditor; forward;

function AddDelphiImage(Bitmap: Graphics.TBitmap; MaskColor: TColor): Integer;
begin
  Result := (BorlandIDEServices as INTAServices).AddMasked(Bitmap, MaskColor);
end;

function CreateCurrentFormComponent(const TypeName: string;
  X, Y, w, H: Integer): TComponent;
var
  FormEditor: IOTAFormEditor;
  NTAComponent: INTAComponent;
  OTAComponent: IOTAComponent;
begin
  Result := nil;
  FormEditor := GetFormEditor;
  OTAComponent := FormEditor.CreateComponent(FormEditor.GetRootComponent,
    TypeName, X, Y, w, H);
  if Assigned(OTAComponent) and
    (OTAComponent.QueryInterface(INTAComponent, NTAComponent) = S_OK) then
    Result := NTAComponent.GetComponent;
end;

function DebugDisabled(const Creator: string): Boolean;
const
  DebugEnabledValue = 'DebugOTAEnabled';
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.OpenKey(GetBaseRegistryKey, True);
    Reg.OpenKey(Creator, True);
    if Reg.ValueExists(DebugEnabledValue) then
      Result := not Reg.ReadBool(DebugEnabledValue)
    else begin
      Reg.WriteBool(DebugEnabledValue, False);
      Result := True;
    end;
  finally
    Reg.Free;
  end;
end;

procedure DebugOTA(const Text: string);
begin
  if DebugDisabled('MLR') then
    Exit;
  if not Assigned(OTADebugForm) then begin
    OTADebugForm := TOTADebugForm.Create(nil);
    OTADebugForm.Show;
  end;
  OTADebugForm.Memo1.Lines.Add(Text);
end;

function CurrentForm: TCustomForm;
var
  Component: TComponent;
  CurrentComponent: INTAComponent;
  FormEditor: IOTAFormEditor;
  OTAComponent: IOTAComponent;
begin
  Result := nil;
  FormEditor := GetFormEditor;
  if Assigned(FormEditor) then begin
    OTAComponent := FormEditor.GetRootComponent;
    if OTAComponent <> nil then
      if OTAComponent.QueryInterface(INTAComponent, CurrentComponent)
        = S_OK then begin
        Component := CurrentComponent.GetComponent;
        if Component is TCustomForm then
          Result := TCustomForm(Component);
        Exit;
      end;
  end;
end;

function GetBaseRegistryKey: string;
begin
  Result := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey;
end;

function GetDelphiCustomMenu(const Caption: string): TMenuItem;
var
  MainMenu: TMenuItem;
begin
  MainMenu := (BorlandIDEServices as INTAServices).MainMenu.Items;
  Result := MainMenu.Find(Caption);
  if Result = nil then begin
    Result := TMenuItem.Create(MainMenu);
    Result.Caption := Caption;
    MainMenu.Add(Result);
  end;
end;

function GetDelphiDataMenu: TMenuItem;
begin
  Result := (BorlandIDEServices as INTAServices).MainMenu.Items[7];
end;

function GetDelphiHelpMenu: TMenuItem;
var
  NTAServices: INTAServices;
begin
  Result := nil;
  NTAServices := BorlandIDEServices as INTAServices;
  if NTAServices = nil then Exit;
  if NTAServices.MainMenu.Items.Count >= 10 then
    Result := NTAServices.MainMenu.Items[9];
end;

function GetEditView: IOTAEditView;
var
  CurrMod: IOTAModule;
  i: Integer;
  Editor: IOTASourceEditor;
begin
  Result := nil;
  CurrMod := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if not Assigned(CurrMod) then Exit;
  for i := 0 to CurrMod.GetModuleFileCount - 1 do
    if CurrMod.GetModuleFileEditor(i).QueryInterface(IOTASourceEditor,
      Editor) = S_OK then begin
      if Editor.GetEditViewCount = 0 then
        continue;
      Result := Editor.EditViews[0];
      if Result <> nil then
        Exit;
    end;
end;

function GetFormEditor: IOTAFormEditor;
var
  CurrMod: IOTAModule;
  i: Integer;
  FormEditor: IOTAFormEditor;
begin
  Result := nil;
  CurrMod := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if Assigned(CurrMod) then
    for i := 0 to CurrMod.GetModuleFileCount - 1 do
      if CurrMod.GetModuleFileEditor(i).QueryInterface(IOTAFormEditor,
        FormEditor) = S_OK then begin
        Result := FormEditor;
        Exit;
      end;
end;

function GetSelectedComponent: TComponent;
var
  CurrentComponent: INTAComponent;
  Editor: IOTAFormEditor;
  OTAComponent: IOTAComponent;
begin
  Result := nil;
  Editor := GetFormEditor;
  if Assigned(Editor) then begin
    if Editor.GetSelCount >= 1 then begin
      OTAComponent := Editor.GetSelComponent(0);
      if Assigned(OTAComponent) and
        (OTAComponent.QueryInterface(INTAComponent,
        CurrentComponent) = S_OK) then
        Result := CurrentComponent.GetComponent;
    end;
  end;
end;

function GetSelectedText: string;
var
  Block: IOTAEditBlock;
  CurrMod: IOTAModule;
  i: Integer;
  Editor: IOTASourceEditor;
  View: IOTAEditView;
begin
  Result := '';
  CurrMod := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if not Assigned(CurrMod) then Exit;
  for i := 0 to CurrMod.GetModuleFileCount - 1 do
    if CurrMod.GetModuleFileEditor(i).QueryInterface(IOTASourceEditor,
      Editor) = S_OK then begin
      if Editor.GetEditViewCount = 0 then
        continue;
      View := Editor.EditViews[0];
      if View = nil then
        continue;
      Block := View.GetBlock;
      if Block = nil then
        continue;
      Result := Block.Text;
      Exit;
    end;
end;

function GetSourceEditor: IOTASourceEditor;
var
  CurrMod: IOTAModule;
  i: Integer;
  Editor: IOTASourceEditor;
begin
  Result := nil;
  CurrMod := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if not Assigned(CurrMod) then Exit;
  for i := 0 to CurrMod.GetModuleFileCount - 1 do
    if CurrMod.GetModuleFileEditor(i).QueryInterface(IOTASourceEditor,
      Editor) = S_OK then begin
      Result := Editor;
      Exit;
    end;
end;

procedure Register;
begin
  RegisterPackageWizard(TMLRActionsToMenuExpert.Create as IOTAWizard);
end;

procedure SetSelectedText(Text: string);
var
  OldAutoIndent: Boolean;
  Buffer: IOTAEditBuffer;
  Block: IOTAEditBlock;
  Editor: IOTASourceEditor;
  i: Integer;
  View: IOTAEditView;
begin
  Editor := GetSourceEditor;
  if Editor = nil then Exit;
  for i := 0 to Editor.GetEditViewCount - 1 do begin
    View := Editor.GetEditView(i);
    if View = nil then continue;
    Block := View.GetBlock;
    if Block.Text <> '' then begin
      Buffer := View.GetBuffer;
      OldAutoIndent := Buffer.BufferOptions.AutoIndent;
      try
        Buffer.BufferOptions.AutoIndent := False;
        View.Position.InsertText(Text);
        View.Paint;
      finally
        Buffer.BufferOptions.AutoIndent := OldAutoIndent;
      end;
      Exit;
    end;
  end;
end;

{ TMLRActionsToMenuExpert }

procedure TMLRActionsToMenuExpert.AfterSave;
begin

end;

procedure TMLRActionsToMenuExpert.BeforeSave;
begin

end;

constructor TMLRActionsToMenuExpert.Create;
begin
  ActionsToMenuDM := TActionsToMenuDM.Create(nil);
  WebSearchDM := TWebSearchDM.Create(nil);
end;

destructor TMLRActionsToMenuExpert.Destroy;
begin
  WebSearchDM.Free;
  ActionsToMenuDM.Free;
end;

procedure TMLRActionsToMenuExpert.Destroyed;
begin

end;

procedure TMLRActionsToMenuExpert.Execute;
begin

end;

function TMLRActionsToMenuExpert.GetIDString: string;
begin
  Result := 'XLSistemas.TMLRActionsToMenuExpert';
end;

function TMLRActionsToMenuExpert.GetName: string;
begin
  Result := 'Actions to menu.';
end;

function TMLRActionsToMenuExpert.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TMLRActionsToMenuExpert.Modified;
begin

end;

end.

