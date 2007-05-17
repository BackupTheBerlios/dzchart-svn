unit de.dummzeuch.experts.BdsIdeScriptExpert.Expert;

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
  Menus,
  ExtCtrls,
  ToolsAPI,
  de.dummzeuch.experts.BaseExpert,
  de.dummzeuch.ScriptHandler.Manager;

type
  Tdm_dzBdsIdeScriptExpert = class(Tdm_DzBaseExpert, IEditorTextAccess)
    TheTimer: TTimer;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure TheTimerTimer(Sender: TObject);
  private
    {: contains all TMenuItems created for scripts }
    FScriptItems: TList;
    {: contains the names of all script files }
    FScripts: TStringList;
    FScriptsDir: string;
    FEngines: TStringList;
    {: frees all items in the script menu and clears the FScriptItems list }
    procedure FreeScriptMenuItems;
    {: initializes the FScripts list of available script files }
    procedure InitScriptList;
    {: creates the main menu item and assigns its OnClick event,
       called in the constructor and after that periodically in the TheTimeTimer event }
    procedure InitScriptMenu;
    {: reads the configuration from the registry and loads the configured
       scripting engines }
    procedure ReadConfigAndLoadEngines;
    {: writes the configuration to the registry }
    procedure WriteConfig;
    procedure LoadEngines;
  private
    {: assigned to the OnClick handler of the main menu item,
       recreates the dropdown menu }
    procedure MenuDropDown(_Sender: TObject);
    {: assigned to all script menu items, calls the associated script }
    procedure MenuScriptClick(Sender: TObject);
    {: assigend to the configure menu item, opens the configuration dialog }
    procedure MenuConfigClick(Sender: TObject);
    procedure UnloadEngines;
  end;

implementation

{$R *.DFM}

uses
  Registry,
  de.dummzeuch.experts.BdsIdeScriptExpert.ConfigForm;

const
  MESSAGE_WINDOW_TITLE = 'BdsIdeScript';
  CUSTOM_MENU_CAPTION = 'Scripts';
  REGISTRY_BRANCH = 'dzBdsIdeScript';
  REGISTRY_SCRIPTSDIR_NAME = 'ScriptsDir';
  REGISTRY_ENGINES_BRANCH = 'Engines';

procedure Tdm_dzBdsIdeScriptExpert.DataModuleCreate(Sender: TObject);
begin
  FScriptItems := TList.Create;
  FScripts := TStringList.Create;
  FEngines := TStringList.Create;

  ReadConfigAndLoadEngines;
  InitScriptMenu;
end;

procedure Tdm_dzBdsIdeScriptExpert.DataModuleDestroy(Sender: TObject);
begin
  FreeScriptMenuItems;
  DeleteDelphiCustomMenu(CUSTOM_MENU_CAPTION);
  UnloadEngines;
  FEngines.Free;
  FScripts.Free;
  FScriptItems.Free;
end;

procedure Tdm_dzBdsIdeScriptExpert.ReadConfigAndLoadEngines;
begin
  FScriptsDir := ReadRegistryString(REGISTRY_BRANCH, REGISTRY_SCRIPTSDIR_NAME);
  ReadRegistryStringList(REGISTRY_BRANCH, REGISTRY_ENGINES_BRANCH, FEngines);
  LoadEngines;
end;

procedure Tdm_dzBdsIdeScriptExpert.WriteConfig;
begin
  WriteRegistryString(REGISTRY_BRANCH, REGISTRY_SCRIPTSDIR_NAME, FScriptsDir);
  WriteRegistryStringList(REGISTRY_BRANCH, REGISTRY_ENGINES_BRANCH, FEngines);
end;

procedure ListFiles(const _Mask: string; _Files: TStrings);
var
  Found: Integer;
  SearchRec: TSearchRec;
begin
  Found := FindFirst(_Mask, faAnyFile, SearchRec);
  try
    while Found = 0 do begin
      if (SearchRec.Attr and faDirectory) <> faDirectory then
        _Files.Add(SearchRec.Name);
      Found := FindNext(SearchRec);
    end;
  finally
    FindClose(SearchRec);
  end;
end;

procedure Tdm_dzBdsIdeScriptExpert.InitScriptList;
var
  i: Integer;
  Handler: IScriptHandler;
  HandlerIdx: Integer;
  Dir: string;
begin
  Dir := IncludeTrailingPathDelimiter(FScriptsDir);

  FScripts.Clear;
  for HandlerIdx := 0 to GetScriptManager.Count - 1 do begin
    Handler := GetScriptManager.GetHandlers(HandlerIdx);
    ListFiles(Dir + '*' + Handler.GetScriptExtension, FScripts);
    for i := 0 to FScripts.Count - 1 do
      FScripts[i] := Dir + FScripts[i];
  end;
end;

procedure Tdm_dzBdsIdeScriptExpert.MenuConfigClick(Sender: TObject);
begin
  if Tf_ConfigForm.Execute(Self, FScriptsDir, FEngines) then
    WriteConfig;
end;

procedure Tdm_dzBdsIdeScriptExpert.LoadEngines;
var
  Handle: Cardinal;
  i: Integer;
begin
  for i := FEngines.Count - 1 downto 0 do begin
    try
      if FEngines.Objects[i] = nil then begin
        Handle := LoadPackage(FEngines[i]);
        FEngines.Objects[i] := Pointer(Handle);
      end;
    except
      FEngines.Delete(i);
    end;
  end;
end;

procedure Tdm_dzBdsIdeScriptExpert.UnloadEngines;
var
  i: Integer;
begin
  for i := 0 to FEngines.Count - 1 do begin
    if FEngines.Objects[i] <> nil then
      try
        UnloadPackage(Cardinal(FEngines.Objects[i]));
        FEngines.Objects[i] := nil;
      except
        // ignore
      end;
  end;
end;

procedure Tdm_dzBdsIdeScriptExpert.TheTimerTimer(Sender: TObject);
begin
  InitScriptMenu;
end;

procedure Tdm_dzBdsIdeScriptExpert.MenuDropDown(_Sender: TObject);
var
  i: Integer;
  Item: TMenuItem;
  ScriptMenu: TMenuItem;
begin
  if not TryGetDelphiCustomMenu(CUSTOM_MENU_CAPTION, ScriptMenu) then
    exit;

  InitScriptList;
  FreeScriptMenuItems;

  for i := 0 to FScripts.Count - 1 do begin
    Item := TMenuItem.Create(nil);
    FScriptItems.Add(Item);
    Item.Caption := ChangeFileExt(ExtractFileName(FScripts[i]), '');
    Item.Tag := i;
    Item.OnClick := MenuScriptClick;
    ScriptMenu.Add(Item);
  end;

  Item := TMenuItem.Create(nil);
  FScriptItems.Add(Item);
  Item.Caption := '-';
  ScriptMenu.Add(Item);

  Item := TMenuItem.Create(nil);
  FScriptItems.Add(Item);
  Item.Caption := 'Configure ...';
  Item.OnClick := MenuConfigClick;
  ScriptMenu.Add(Item);
end;

procedure Tdm_dzBdsIdeScriptExpert.InitScriptMenu;
var
  ScriptMenu: TMenuItem;
begin
  ScriptMenu := GetDelphiCustomMenu(CUSTOM_MENU_CAPTION);
  if not Assigned(ScriptMenu) then begin
    TheTimer.Enabled := True;
    Exit;
  end;
  ScriptMenu.OnClick := MenuDropDown;
end;

procedure Tdm_dzBdsIdeScriptExpert.FreeScriptMenuItems;
var
  i: Integer;
begin
  if (not Assigned(FScriptItems)) or (FScriptItems.Count = 0) then
    Exit;
  for i := 0 to FScriptItems.Count - 1 do
    TMenuItem(FScriptItems[i]).Free;
  FScriptItems.Clear;
end;

type
  TMsgWindowWrapper = class(TInterfacedObject, IMessageOutput)
  private
    FMsgWindow: IMsgWindow;
    procedure Write(const _Msg: string; _Show: Boolean = False);
  public
    constructor Create(_MsgWindow: IMsgWindow);
  end;

procedure Tdm_dzBdsIdeScriptExpert.MenuScriptClick(Sender: TObject);
var
  Code: TStringList;
  FileName: string;
  Idx: Integer;
  Msg: IMsgWindow;
  ScriptHandler: IScriptHandler;
begin
  Msg := GetMsgWindow(MESSAGE_WINDOW_TITLE);
  Msg.Clear;

  Idx := (Sender as TComponent).Tag;
  FileName := FScripts[Idx];
  Msg.Write('Loading script ' + FileName);

  ScriptHandler := GetScriptManager.GetHandler(ExtractFileExt(FileName));
  if not Assigned(ScriptHandler) then
    raise exception.Create('No script handler registered for that script type.');

  Code := TStringList.Create;
  try
    Code.LoadFromFile(FileName);
    ScriptHandler.Execute(Self, TMsgWindowWrapper.Create(GetMsgWindow(MESSAGE_WINDOW_TITLE)), Code);
  finally
    Code.Free;
  end;
end;

{ TMsgWindowWrapper }

constructor TMsgWindowWrapper.Create(_MsgWindow: IMsgWindow);
begin
  inherited Create;
  FMsgWindow := _MsgWindow;
end;

procedure TMsgWindowWrapper.Write(const _Msg: string; _Show: Boolean);
begin
  FMsgWindow.Write(_Msg, _Show);
end;

var
  Expert: Tdm_dzBdsIdeScriptExpert;

initialization
  Expert := Tdm_dzBdsIdeScriptExpert.Create(nil);
finalization
  Expert.Free;
end.

