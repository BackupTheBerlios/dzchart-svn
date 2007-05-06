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
    procedure RunScriptSubItemClick(Sender: TObject);
    procedure RunScriptExplainClick(Sender: TObject);
    procedure TheTimerTimer(Sender: TObject);
  private
    {: contains all TMenuItems created for scripts }
    FScriptItems: TList;
    {: contains the names of all script files }
    FScripts: TStringList;
    {: frees all items in the script menu and clears the FScriptItems list }
    procedure FreeScriptMenuItems;
    {: initializes the FScripts list of available script files }
    procedure InitScriptList;
    {: recreates the script menu items, called once on startup and
       after that periodically in the TheTimeTimer event }
    procedure RefreshMenuItems;
    {: enables the refresh timer based on the setting in the registry }
    procedure SetTimerRefreshing;
//    procedure LoadScriptHandlers;
  end;

implementation

{$R *.DFM}

uses
  Registry;

const
  MESSAGE_WINDOW_TITLE = 'BdsIdeScript';
  CUSTOM_MENU_CAPTION = 'Scripts';
  REGISTRY_BRANCH = 'dzBdsIdeScript';

procedure Tdm_dzBdsIdeScriptExpert.DataModuleCreate(Sender: TObject);
begin
  FScriptItems := TList.Create;
  FScripts := TStringList.Create;

//  LoadScriptHandlers;
  RefreshMenuItems;
end;

procedure Tdm_dzBdsIdeScriptExpert.DataModuleDestroy(Sender: TObject);
begin
  FreeScriptMenuItems;
  DeleteDelphiCustomMenu(CUSTOM_MENU_CAPTION);
  FScripts.Free;
  FScriptItems.Free;
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

procedure Tdm_dzBdsIdeScriptExpert.InitScriptList;
var
  Folder: string;
  i: Integer;
  Reg: TRegistry;
  Handler: IScriptHandler;
  HandlerIdx: Integer;
begin
  Reg := TRegistry.Create;
  try
    Reg.OpenKey(GetBaseRegistryKey + '\' + REGISTRY_BRANCH, True);
    if not Reg.ValueExists('ScriptFolder') then
      { Add the IDE's folder. }
      Reg.WriteString('ScriptFolder',
        IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName)) + 'Scripts')
    else
      Folder := Reg.ReadString('ScriptFolder');
  finally
    Reg.Free;
  end;
  Folder := IncludeTrailingPathDelimiter(Folder);

  FScripts.Clear;
  for HandlerIdx := 0 to GetScriptManager.Count - 1 do begin
    Handler := GetScriptManager.GetHandlers(HandlerIdx);
    ListFiles(Folder + '*' + Handler.GetScriptExtension, FScripts);
    for i := 0 to FScripts.Count - 1 do
      FScripts[i] := Folder + FScripts[i];
  end;
end;

//procedure Tdm_dzBdsIdeScriptExpert.LoadScriptHandlers;
//var
//  Packages: TStringList;
//  Filename: string;
//  i: Integer;
//begin
//  Filename := GetModuleName(hInstance);
//  Packages := TStringList.Create;
//  try
//    ListFiles(ExtractFilePath(Filename) + 'dzBdsIdeScript_*.bpl.*', Packages);
//    for i := 0 to Packages.Count - 1 do begin
//      LoadPackage(Packages[i]);
//    end;
//  finally
//    Packages.Free;
//  end;
//end;

procedure Tdm_dzBdsIdeScriptExpert.RunScriptExplainClick(Sender: TObject);
begin
  ShowMessage(
    'Scripts should be placed in the folder pointed by '#13 +
    'the registry value at:'#13#13 +
    'HKCU\' + GetBaseRegistryKey + '\' + REGISTRY_BRANCH +
    #13#13'The value should be assigned to "ScriptFolder".' +
    #13#13'You can always refresh the script list by clicking this item.'
    );
  RefreshMenuItems;
end;

procedure Tdm_dzBdsIdeScriptExpert.TheTimerTimer(Sender: TObject);
begin
  RefreshMenuItems;
end;

procedure Tdm_dzBdsIdeScriptExpert.RefreshMenuItems;
var
  i: Integer;
  Item, ScriptMenu: TMenuItem;
begin
  ScriptMenu := GetDelphiCustomMenu(CUSTOM_MENU_CAPTION);
  if ScriptMenu = nil then begin { The menu may not exist yet. }
    TheTimer.Enabled := True;
    Exit;
  end;

  InitScriptList;
  FreeScriptMenuItems;

  for i := 0 to FScripts.Count - 1 do begin
    Item := TMenuItem.Create(nil);
    FScriptItems.Add(Item);
    Item.Caption := ChangeFileExt(ExtractFileName(FScripts[i]), '');
    Item.Tag := i;
    Item.OnClick := RunScriptSubItemClick;
    ScriptMenu.Add(Item);
  end;

  Item := TMenuItem.Create(nil);
  FScriptItems.Add(Item);
  Item.Caption := '(customize)';
  Item.OnClick := RunScriptExplainClick;
  ScriptMenu.Add(Item);
  SetTimerRefreshing;
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

procedure Tdm_dzBdsIdeScriptExpert.SetTimerRefreshing;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.OpenKey(GetBaseRegistryKey + '\' + REGISTRY_BRANCH + '\Options', True);
    if Reg.ValueExists('Refresh') then begin
      TheTimer.Enabled := Reg.ReadBool('Refresh');
    end else begin
      Reg.WriteBool('Refresh', True);
      TheTimer.Enabled := True;
    end;
  finally
    Reg.Free;
  end;
end;

type
  TMsgWindowWrapper = class(TInterfacedObject, IMessageOutput)
  private
    FMsgWindow: IMsgWindow;
    procedure Write(const _Msg: string; _Show: Boolean = False);
  public
    constructor Create(_MsgWindow: IMsgWindow);
  end;

procedure Tdm_dzBdsIdeScriptExpert.RunScriptSubItemClick(Sender: TObject);
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

