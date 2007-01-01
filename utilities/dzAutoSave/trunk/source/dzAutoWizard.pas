unit dzAutoWizard;

{$I delphiversions.inc}

interface

{ Auto save expert. Automatically save modified files and forms.
  If Delphi exits unexpectedly (due to a crash, power failure, etc.),
  you lose only the work done since the last auto-save.
  When opening a file that had been auto-saved, you can restore
  the auto-saved file, or ignore it and revert to the original file.

  The user turns auto-save on and off and can change the interval
  at which files are automatically saved.

  Copyright © 1999 Tempest Software, Inc.

  Changes copyright 2003-2006 Thomas Mueller
}

uses
  Windows,
  ActiveX,
  SysUtils,
  Classes,
  Forms,
  Dialogs,
  Controls,
  Menus,
  ExtCtrls,
  ToolsApi,
  Registry,
  dzAutoUtil,
  dzAutoConfig;

type
  TAutoSaveWizard = class;

  TAutoSaveNotifier = class(TNotifierObject)
  private
    fNotifierIndex: Integer;
    fWizard: TAutoSaveWizard;
  protected
    function RegisterNotifier: Integer; virtual;
    procedure UnregisterNotifier(_Index: Integer); overload; virtual; abstract;
  public
    constructor Create(Wizard: TAutoSaveWizard);
    destructor Destroy; override;
    procedure UnregisterNotifier; overload;
  end;

  TIdeNotifier = class(TAutoSaveNotifier, IOTAIdeNotifier)
  private
    // IOTAIdeNotifier
    procedure FileNotification(NotifyCode: TOTAFileNotification;
      const FileName: string; var Cancel: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure AfterCompile(Succeeded: Boolean);
  protected
    function RegisterNotifier: Integer; override;
    procedure UnregisterNotifier(_Index: Integer); override;
  public
  end;

{$IFDEF delphi7up}
  TMessageNotifier = class(TAutoSaveNotifier, IOTAMessageNotifier)
  private
    fShowMessageView: Boolean;
    fMessageGroup: IOTAMessageGroup;
    procedure MessageGroupAdded(const Group: IOTAMessageGroup);
    procedure MessageGroupDeleted(const Group: IOTAMessageGroup);
    procedure AutoSaveCallback(const _Filename: string);
    procedure ErrorCallback(const _Error: string);
  protected
    function RegisterNotifier: Integer; override;
    procedure UnregisterNotifier(_Index: Integer); override;
  public
    constructor Create(_Wizard: TAutoSaveWizard);
    destructor Destroy; override;
    procedure ClearMessageView(_Show: Boolean);
  end;
{$ENDIF}

  // Here is the actual wizard.
  TAutoSaveWizard = class(TNotifierObject, IOTAWizard)
  private
    fModuleList: TModuleList; // List of modules open in the IDE.
    fTimer: TTimer; // Timer for periodic auto-saving.
    fMenuItem: TMenuItem; // Tools menu item for Auto Save Properties.
    fIdeNotifier: TIdeNotifier;
{$IFDEF delphi7up}
    fMessageNotifier: TMessageNotifier;
{$ENDIF}
    fSaveOnCompile: Boolean;
    fToolPosition: Integer;
    fShowMessageView: Boolean;
    procedure RestoreAutoSave(const SavedFile, CurrentFile: string);
    procedure OnClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    function GetEnabled: Boolean;
    procedure SetEnabled(_Enabled: Boolean);
    function GetInterval: Integer;
    procedure SetInterval(Value: Integer);
    procedure SetToolPosition(const _ToolPosition: Integer);
    procedure SetSaveOnCompile(const _SaveOnCompile: Boolean);
    procedure UpdateMenuChecked;
  protected
    procedure EditProperties;
    procedure GetRegInfo;
    procedure SetRegInfo;
    procedure AutoSaveFiles;
    procedure BeforeCompile;
    function MakeMenuItem: TMenuItem;
    procedure GetOpenModules;
    procedure AddNotifiers(const FileName: string); overload;
    procedure AddNotifiers(const Module: IOTAModule); overload;
    property ModuleList: TModuleList read fModuleList;
  public
    constructor Create;
    destructor Destroy; override;
    function FileOpening(const FileName: string): Boolean;

    // IOTAWizard
    procedure Execute;
    function GetName: string;
    function GetState: TWizardState;
    function GetIDString: string;

    property Enabled: Boolean read GetEnabled write SetEnabled;
    property SaveOnCompile: Boolean read fSaveOnCompile write SetSaveOnCompile;
    property Interval: Integer read GetInterval write SetInterval;
    property Timer: TTimer read fTimer;
    property ToolPosition: Integer read fToolPosition write SetToolPosition;
    property MenuItem: TMenuItem read fMenuItem;
    property ShowMessageView: Boolean read fShowMessageView write fShowMessageView;
  end;

implementation

resourcestring
  S_CantFindTools = 'Cannot locate Tools menu';
  S_Label = '&Auto Save...';
  S_Name = 'Auto Save Expert';
  S_CantRestore = 'Cannot restore auto-saved file';
  S_LoadPrompt = 'File was auto-saved when Delphi terminated unexpectedly:' +
    #13#10'%s'#13#10'Load from the auto-saved file?';

  { TAutoSaveWizard }

  { When the expert is created,it installs a menu item on Delphi's Tools menu,
    to invoke the Auto Save Properties dialog. The constructor also
    creates the auto-save timer. }

constructor TAutoSaveWizard.Create;
begin
  inherited;
  fShowMessageView := True;

  fModuleList := TModuleList.Create;
  fTimer := TTimer.Create(nil);
  Timer.OnTimer := OnTimer;

  // Register the IDE notifier so the wizard knows when the user opens a file.
  fIdeNotifier := TIdeNotifier.Create(Self);
{$IFDEF delphi7up}
  fMessageNotifier := TMessageNotifier.Create(Self);
{$ENDIF}
  // Add the Auto Save menu item to the Tools menu.
  fMenuItem := MakeMenuItem;

  // Get the configuration data from the registry.
  GetRegInfo;

  // Add notifiers for all files currently open.
  GetOpenModules;
end;

destructor TAutoSaveWizard.Destroy;
begin
  if Assigned(fIdeNotifier) then
    fIdeNotifier.UnregisterNotifier; // Delphi frees the notifier when its ref count = 0
{$IFDEF delphi7up}
  if Assigned(fMessageNotifier) then
    fMessageNotifier.UnregisterNotifier; // Delphi frees the notifier when its ref count = 0
{$ENDIF}
  MenuItem.Free;
  fMenuItem := nil;
  Timer.Free;
  fTimer := nil;
  ModuleList.Free;
  fModuleList := nil;

  inherited;
end;

{ Store the auto-save configuration in the registry. }
const
  REG_KEY = '\Software\dummzeuch.de';
  REG_SECTION = 'Auto Save Expert';
  REG_ENABLED = 'Enabled';
  REG_INTERVAL = 'Interval';
  DEFAULT_INTERVAL = 2 * 60 * 1000; // 2 minutes, in milliseconds
  REG_SAVE_ON_COMPILE = 'SaveOnCompile';
  REG_TOOL_POSITION = 'ToolPosition';
  REG_SHOW_MESSAGE_VIEW = 'ShowMessageView';

procedure TAutoSaveWizard.GetRegInfo;
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create(REG_KEY);
  try
    Enabled := Reg.ReadBool(REG_SECTION, REG_ENABLED, True);
    Interval := Reg.ReadInteger(REG_SECTION, REG_INTERVAL, DEFAULT_INTERVAL);
    SaveOnCompile := Reg.ReadBool(REG_SECTION, REG_SAVE_ON_COMPILE, False);
    ToolPosition := Reg.ReadInteger(REG_SECTION, REG_TOOL_POSITION, 0);
    ShowMessageView := Reg.ReadBool(REG_SECTION, REG_SHOW_MESSAGE_VIEW, True);
  finally
    Reg.Free;
  end;
end;

{ Store the configuration after it changes. }

procedure TAutoSaveWizard.SetRegInfo;
var
  Reg: TRegIniFile;
begin
  Reg := TRegIniFile.Create(REG_KEY);
  try
    Reg.WriteBool(REG_SECTION, REG_ENABLED, Enabled);
    Reg.WriteInteger(REG_SECTION, REG_INTERVAL, Interval);
    Reg.WriteBool(REG_SECTION, REG_SAVE_ON_COMPILE, SaveOnCompile);
    Reg.WriteInteger(REG_SECTION, REG_TOOL_POSITION, ToolPosition);
    Reg.WriteBool(REG_SECTION, REG_SHOW_MESSAGE_VIEW, ShowMessageView);
  finally
    Reg.Free;
  end;
end;

{ Perform the automatic save by checking all the modules,
  and saving any that have been modified. }

procedure TAutoSaveWizard.AutoSaveFiles;
begin
{$IFDEF delphi7up}
  if Assigned(fMessageNotifier) then
    fMessageNotifier.ClearMessageView(ShowMessageView);
{$ENDIF}
  ModuleList.AutoSave;
end;

{ Restore an auto-saved file. Delete the original, and rename
  the auto-save file to have the name of the original. If the
  Delete succeeds and the rename fails, then the user needs to
  patch things up by hand. }

procedure TAutoSaveWizard.RestoreAutoSave(const SavedFile, CurrentFile: string);
begin
  if not DeleteFile(CurrentFile) or not RenameFile(SavedFile, CurrentFile) then
    MessageDlg(S_CantRestore, mtWarning, [mbOK], 0);
end;

{ A file notifier has been notified that a file is about to be
  opened. Before allowing Delphi to open the file, check for
  an auto-saved file. In most cases, there won't be one, so
  just return False, to allow the open to proceed normally.
  If there is an auto-save file, then ask the user whether
  he or she wants to load the auto-saved file, or stick with
  the original. The user can also choose Cancel, which returns
  True, to cancel the open action. If the user choose Yes, to
  use the auto-save file, then delete the original and rename
  the auto-saved file in its stead. }

function TAutoSaveWizard.FileOpening(const FileName: string): Boolean;
var
  SaveFile: string;
  st: TFileStream;
begin
  Result := False;
  SaveFile := MakeAutoSaveFileName(FileName);
  if not FileExists(SaveFile) then
    Exit;

  try
    st := TFileStream.Create(SaveFile, fmOpenRead);
    try
      if st.Size = 0 then begin
        FreeAndNil(st);
        DeleteFile(SaveFile);
        Exit; // don't restore empty files
      end;
    finally
      FreeAndNil(st);
    end;
  except
    Exit; // if we can't open it, it doesn't make sense to try to restore it
  end;

  MessageBeep(Mb_IconQuestion);
  case MessageDlg(Format(S_LoadPrompt, [FileName]), mtConfirmation, mbYesNoCancel, 0) of
    mrYes:
      RestoreAutoSave(SaveFile, FileName);
    mrNo:
      DeleteFile(SaveFile);
    mrCancel:
      Result := True;
  end;
end;

// Create notifiers for all the files that are open when the wizard starts.

procedure TAutoSaveWizard.GetOpenModules;
var
  i: Integer;
  Services: IOTAModuleServices;
begin
  Services := BorlandIDEServices as IOTAModuleServices;
  for i := 0 to Services.ModuleCount - 1 do
    AddNotifiers(Services.Modules[i]);
end;

{ The user has chosen the Auto Save menu item, to edit the
  auto-save properties. If the user clicks the OK button, then
  save the new configuration values, and store them in the registry. }

procedure TAutoSaveWizard.EditProperties;
const
  MillisecPerMinute = 60 * 1000;
var
  Form: TAutoSaveForm;
begin
  Form := TAutoSaveForm.Create(Application);
  try
    { Initialize the form with the current values. }
    Form.SaveInterval := Interval div MillisecPerMinute;
    Form.SaveTimed := Enabled;
    Form.SaveOnCompile := SaveOnCompile;
    Form.ToolPosition := ToolPosition;
    Form.ShowMessageView := ShowMessageView;
    ModuleList.GetFileList(Form.FileList.Items);

    if Form.ShowModal = mrOk then begin
        { Extract the new values from the form. }
      Enabled := Form.SaveTimed;
      Interval := Form.SaveInterval * MillisecPerMinute;
      SaveOnCompile := Form.SaveOnCompile;
      ToolPosition := Form.ToolPosition;
      ShowMessageView := Form.ShowMessageView;
        { Save the new values in the registry. }
      SetRegInfo;
    end;
  finally
    Form.Free;
  end;
end;

// Find a menu item whose component name is Name. Search recursively
// in all submenus, starting at Item. Return a reference to the menu item
// or nil if not found.

function FindMenuItem(Item: TMenuItem; const Name: string): TMenuItem;
var
  i: Integer;
begin
  Result := nil;
  if CompareText(Item.Name, Name) = 0 then
    Result := Item
  else
    for i := 0 to Item.Count - 1 do begin
      Result := FindMenuItem(Item.Items[i], Name);
      if Result <> nil then
        Exit;
    end;
end;

{ Create and return a menu item to invoke the configuration dialog box. }

function TAutoSaveWizard.MakeMenuItem: TMenuItem;
var
  MainMenu: TMainMenu;
  ToolsMenu: TMenuItem;
begin
  // Add a menu item to the Tools menu.
  MainMenu := (BorlandIDEServices as INTAServices).MainMenu;
  ToolsMenu := FindMenuItem(MainMenu.Items, 'ToolsMenu');
  if ToolsMenu = nil then
    raise Exception.Create(S_CantFindTools);

  { Insert as first menu item. Appending as last Tools menu
    item does not work because when the user adds tools,
    they overwrite the added item. }
  Result := NewItem(S_Label, 0, Enabled, True, OnClick, 0, 'mi_AutoSaveWizard');
  ToolsMenu.Insert(ToolPosition, Result);
end;

// After the IDE opens a file, create a notifier for each editor.

procedure TAutoSaveWizard.AddNotifiers(const FileName: string);
var
  Module: IOTAModule;
begin
  Module := (BorlandIDEServices as IOTAModuleServices).FindModule(FileName);
  if Module <> nil then
    AddNotifiers(Module);
end;

// Modules with file systems are not usually files. Even if the file
// system is file-based, the "file" might be part of an OLE structured storage
// file or anything else--without knowing what the real file is, the
// auto-save wizard can't do anything useful. Thus it is best to let the
// file system itself handle the auto-saving.

procedure TAutoSaveWizard.AddNotifiers(const Module: IOTAModule);
begin
  if Module.FileSystem = '' then
    ModuleList.Add(Module);
end;

function TAutoSaveWizard.GetEnabled: Boolean;
begin
  Result := Timer.Enabled;
end;

function TAutoSaveWizard.GetInterval: Integer;
begin
  Result := Timer.Interval;
end;

procedure TAutoSaveWizard.SetEnabled(_Enabled: Boolean);
begin
  Timer.Enabled := _Enabled;
  UpdateMenuChecked;
end;

procedure TAutoSaveWizard.SetSaveOnCompile(const _SaveOnCompile: Boolean);
begin
  fSaveOnCompile := _SaveOnCompile;
  UpdateMenuChecked;
end;

procedure TAutoSaveWizard.SetToolPosition(const _ToolPosition: Integer);
begin
  fToolPosition := _ToolPosition;
  fMenuItem.Free;
  fMenuItem := MakeMenuItem;
  UpdateMenuChecked;
end;

procedure TAutoSaveWizard.UpdateMenuChecked;
begin
  if not Assigned(fMenuItem) then
    Exit;
  { Set the menu item to reflect whether auto-saving is enabled. }
  MenuItem.Checked := Enabled or SaveOnCompile;
end;

procedure TAutoSaveWizard.SetInterval(Value: Integer);
begin
  Timer.Interval := Value;
end;

{ Implement the expert interface. }

function TAutoSaveWizard.GetName: string;
begin
  Result := S_Name;
end;

// Delphi doesn't use GetState except for menu wizard, but in case a future
// version pays attention, return something meaningful.

function TAutoSaveWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

function TAutoSaveWizard.GetIDString: string;
begin
  Result := 'Tempest.AutoSaveWizard';
end;

{ OnClick menu handler, for the menu item add-in. When the user
  clicks the Auto Save menu item, pop up the properties dialog. }

procedure TAutoSaveWizard.OnClick(Sender: TObject);
begin
  EditProperties;
end;

{ Timer event: when the timer expires, auto-save files. }

procedure TAutoSaveWizard.OnTimer(Sender: TObject);
begin
  AutoSaveFiles;
end;

procedure TAutoSaveWizard.BeforeCompile;
begin
  if SaveOnCompile then
    AutoSaveFiles;
end;

// Not called for an add-in wizard.

procedure TAutoSaveWizard.Execute;
begin
end;

{ TAutoSaveNotifier }

constructor TAutoSaveNotifier.Create(Wizard: TAutoSaveWizard);
begin
  inherited Create;
  fWizard := Wizard;
  fNotifierIndex := Self.RegisterNotifier;
end;

destructor TAutoSaveNotifier.Destroy;
begin
  if fNotifierIndex >= 0 then
    UnregisterNotifier;
  inherited;
end;

function TAutoSaveNotifier.RegisterNotifier: Integer;
begin
  Result := -1;
end;

procedure TAutoSaveNotifier.UnregisterNotifier;
var
  Idx: Integer;
begin
  Idx := fNotifierIndex;
  fNotifierIndex := -1;
  UnregisterNotifier(Idx);
end;

{ TIdeNotifier }

// Ignore compilation notification.

procedure TIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
end;

procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin
  fWizard.BeforeCompile;
end;

// If the user is about to open a file, see if an auto-save file
// exists. After opening a file, create the necessary notifiers.

procedure TIdeNotifier.FileNotification(
  NotifyCode: TOTAFileNotification; const FileName: string;
  var Cancel: Boolean);
begin
  case NotifyCode of
    ofnFileOpening:
      Cancel := fWizard.FileOpening(FileName);
    ofnFileOpened:
      fWizard.AddNotifiers(FileName);
  else
    ; { ignore other notifications }
  end;
end;

function TIdeNotifier.RegisterNotifier: Integer;
var
  Services: IOTAServices;
begin
  // Register the notifier.
  if Supports(BorlandIDEServices, IOTAServices, Services) then
    Result := Services.AddNotifier(Self)
  else
    Result := -1;
end;

procedure TIdeNotifier.UnregisterNotifier(_Index: Integer);
begin
  if _Index <> -1 then
    (BorlandIDEServices as IOTAServices).RemoveNotifier(_Index);
end;

{$IFDEF delphi7up}
{ TMessageNotifier }

constructor TMessageNotifier.Create(_Wizard: TAutoSaveWizard);
begin
  inherited Create(_Wizard);
  OnAutoSave := Self.AutoSaveCallback;
  OnError := Self.ErrorCallback;
end;

destructor TMessageNotifier.Destroy;
begin
  OnAutoSave := nil;
  OnError := nil;
  inherited;
end;

function TMessageNotifier.RegisterNotifier: Integer;
var
  MessageView: IOTAMessageServices;
begin
  if Supports(BorlandIDEServices, IOTAMessageServices, MessageView) then begin
    Result := MessageView.AddNotifier(Self);
    fMessageGroup := MessageView.AddMessageGroup('AutoSave');
  end else
    Result := -1;
end;

procedure TMessageNotifier.UnregisterNotifier(_Index: Integer);
begin
  if Assigned(fMessageGroup) then
    (BorlandIDEServices as IOTAMessageServices).RemoveMessageGroup(fMessageGroup);
  if _Index <> -1 then
    (BorlandIDEServices as IOTAMessageServices).RemoveNotifier(_Index);
end;

procedure TMessageNotifier.MessageGroupAdded(const Group: IOTAMessageGroup);
begin
  // ignore
end;

procedure TMessageNotifier.MessageGroupDeleted(const Group: IOTAMessageGroup);
begin
  if Group = fMessageGroup then begin
    fMessageGroup := nil;
  end;
end;

procedure TMessageNotifier.AutoSaveCallback(const _Filename: string);
var
  MessageView: IOTAMessageServices;
  LineRef: pointer;
begin
  if Assigned(fMessageGroup) then
    if Supports(BorlandIDEServices, IOTAMessageServices, MessageView) then begin
      MessageView.AddToolMessage(_Filename, 'autosaved', DateTimeToStr(Now),
        0, 0, nil, LineRef, fMessageGroup);
      if fShowMessageView then
        MessageView.ShowMessageView(fMessageGroup);
    end;
end;

procedure TMessageNotifier.ErrorCallback(const _Error: string);
var
  MessageView: IOTAMessageServices;
  //   LineRef: pointer;
begin
  if Assigned(fMessageGroup) then
    if Supports(BorlandIDEServices, IOTAMessageServices, MessageView) then begin
      MessageView.AddTitleMessage(_Error, fMessageGroup);
      if fShowMessageView then
        MessageView.ShowMessageView(fMessageGroup);
    end;
end;

procedure TMessageNotifier.ClearMessageView(_Show: Boolean);
var
  MessageView: IOTAMessageServices;
begin
  fShowMessageView := _Show;
  if Assigned(fMessageGroup) then
    if Supports(BorlandIDEServices, IOTAMessageServices, MessageView) then
      MessageView.ClearMessageGroup(fMessageGroup);
end;
{$ENDIF}

end.

