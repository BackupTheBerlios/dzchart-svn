unit de.dummzeuch.experts.BdsIdeScriptExpert;

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
  dws2Comp,
  dws2Exprs,
  dws2StringFunctions,
  dws2FileFunctions,
  dws2ClassesLibModule,
  de.dummzeuch.experts.BaseExpert;

type
  Tdm_dzBdsIdeScriptExpert = class(Tdm_DzBaseExpert)
    TheTimer: TTimer;
    dws2Unit: Tdws2Unit;
    dws_Main: TDelphiWebScriptII;
    dws2ClassesLib: Tdws2ClassesLib;
    dws2FileFunctions: Tdws2FileFunctions;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure RunScriptSubItemClick(Sender: TObject);
    procedure RunScriptExplainClick(Sender: TObject);
    procedure TheTimerTimer(Sender: TObject);
  private
    FScriptItems: TList;
    FScripts: TStringList;

    procedure ClearMenuItems(_List: TList);
    procedure ClearScriptMenuItems;
    procedure LoadScripts;
    procedure RefreshMenuItems;
    procedure RefreshScriptMenuItems;
    procedure SetTimerRefreshing;
  private
    function Compile(const _Source: string): TProgram;
    procedure ExecuteScript(_Code: TStrings);
    procedure OnIdeEditorGetSelectedTextEval(_Info: TProgramInfo; _ExtObject: TObject);
    procedure OnIdeEditorSetSelectedTextEval(_Info: TProgramInfo; _ExtObject: TObject);
    procedure OnIdeEditorInsertTextEval(_Info: TProgramInfo; _ExtObject: TObject);
    procedure RegisterOTAFunctions;
  end;

implementation

{$R *.DFM}

uses
  Registry,
  ShellAPI,
  dws2Symbols,
  dws2Compiler;

const
  MESSAGE_WINDOW_TITLE = 'BdsIdeScript';
  CUSTOM_MENU_CAPTION = 'Scripts';
  REGISTRY_BRANCH = 'dzBdsIdeScript';

procedure Tdm_dzBdsIdeScriptExpert.ExecuteScript(_Code: TStrings);
var
  i: Integer;
  Prog: TProgram;
  Msg: IMsgWindow;
begin
  Msg := GetMsgWindow(MESSAGE_WINDOW_TITLE);
  Msg.Write('Compiling script.');
  try
    Prog := Compile(_Code.Text);
  except
    on e: Exception do begin
      Msg.Write('Error compiling script:'#13#10 + e.Message, True);
      Exit;
    end;
  end;

  Msg.Write('Running script.');
  try
//    Prog.BeginProgram();
    Prog.Execute;
//    Prog.EndProgram;

    for i := 0 to Prog.Msgs.Count - 1 do
      Msg.Write(Prog.Msgs[i].asInfo);
    Msg.Write(Tdws2DefaultResult(Prog.Result).Text);

  finally
    Prog.Free;
  end;
  Msg.Write('Done.');
end;

function Tdm_dzBdsIdeScriptExpert.Compile(const _Source: string): TProgram;
var
  i: Integer;
  err: string;
begin
  Result := dws_Main.Compile(_Source);
  if Result.Msgs.HasCompilerErrors then begin
    err := '';
    for i := 0 to Result.Msgs.Count - 1 do begin
      if err <> '' then
        err := err + #13#10;
      err := err + Result.Msgs[i].asInfo;
    end;
    Result.Free;
    raise Exception.Create(err);
  end;
end;

procedure Tdm_dzBdsIdeScriptExpert.DataModuleCreate(Sender: TObject);
begin
  FScriptItems := TList.Create;
  FScripts := TStringList.Create;

  RefreshMenuItems;

  RegisterOTAFunctions;
end;

procedure Tdm_dzBdsIdeScriptExpert.DataModuleDestroy(Sender: TObject);
begin
  ClearScriptMenuItems;
  DeleteDelphiCustomMenu(CUSTOM_MENU_CAPTION);
  FScripts.Free;
  FScriptItems.Free;
end;

procedure Tdm_dzBdsIdeScriptExpert.OnIdeEditorGetSelectedTextEval(_Info: TProgramInfo; _ExtObject: TObject);
begin
  _Info.Result := GetSelectedText;
end;

procedure Tdm_dzBdsIdeScriptExpert.OnIdeEditorSetSelectedTextEval(_Info: TProgramInfo; _ExtObject: TObject);
begin
  SetSelectedText(_Info.Value['_Text']);
end;

procedure Tdm_dzBdsIdeScriptExpert.OnIdeEditorInsertTextEval(_Info: TProgramInfo; _ExtObject: TObject);
begin
  InsertText(_Info.Value['_Text']);
end;

procedure Tdm_dzBdsIdeScriptExpert.RegisterOTAFunctions;
var
  Item: Tdws2Class;
  Meth: Tdws2Method;
  Param: Tdws2Parameter;
begin
  Item := dws2Unit.Classes.Add as Tdws2Class;
  Item.Name := 'TIdeEditor';

  Meth := Item.Methods.Add as Tdws2Method;
  Meth.Name := 'GetSelectedText';
  Meth.Kind := mkClassFunction;
  Meth.ResultType := 'String';
  Meth.OnEval := OnIdeEditorGetSelectedTextEval;

  Meth := Item.Methods.Add as Tdws2Method;
  Meth.Name := 'SetSelectedText';
  Meth.Kind := mkClassProcedure;
  Param := Meth.Parameters.Add as Tdws2Parameter;
  Param.Name := '_Text';
  Param.DataType := 'String';
  Meth.OnEval := OnIdeEditorSetSelectedTextEval;

  Meth := Item.Methods.Add as Tdws2Method;
  Meth.Name := 'InsertText';
  Meth.Kind := mkClassProcedure;
  Param := Meth.Parameters.Add as Tdws2Parameter;
  Param.Name := '_Text';
  Param.DataType := 'String';
  Meth.OnEval := OnIdeEditorInsertTextEval;
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

procedure Tdm_dzBdsIdeScriptExpert.LoadScripts;
var
  Folder: string;
  i: Integer;
  Reg: TRegistry;
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
  ListFiles(Folder + '*.dws', FScripts);
  for i := 0 to FScripts.Count - 1 do
    FScripts[i] := Folder + FScripts[i];
end;

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
begin
  RefreshScriptMenuItems;
end;

procedure Tdm_dzBdsIdeScriptExpert.RefreshScriptMenuItems;
var
  i: Integer;
  Item, ScriptMenu: TMenuItem;
begin
  ScriptMenu := GetDelphiCustomMenu(CUSTOM_MENU_CAPTION);
  if ScriptMenu = nil then begin { The menu may not exist yet. }
    TheTimer.Enabled := True;
    Exit;
  end;

  LoadScripts;
  ClearScriptMenuItems;

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

  ScriptMenu.Visible := FScripts.Count > 0;
end;

procedure Tdm_dzBdsIdeScriptExpert.ClearMenuItems(_List: TList);
var
  i: Integer;
begin
  if (not Assigned(_List)) or (_List.Count = 0) then
    Exit;
  for i := 0 to _List.Count - 1 do
    TMenuItem(_List[i]).Free;
  _List.Clear;
end;

procedure Tdm_dzBdsIdeScriptExpert.ClearScriptMenuItems;
begin
  ClearMenuItems(FScriptItems);
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

procedure Tdm_dzBdsIdeScriptExpert.RunScriptSubItemClick(Sender: TObject);
var
  Code: TStringList;
  FileName: string;
  Idx: Integer;
  Msg: IMsgWindow;
begin
  Idx := (Sender as TComponent).Tag;
  FileName := FScripts[Idx];
  Msg := GetMsgWindow(MESSAGE_WINDOW_TITLE);
  Msg.Clear;
  Msg.Write('Loading script ' + FileName);
  Code := TStringList.Create;
  try
    Code.LoadFromFile(FileName);
    ExecuteScript(Code);
  finally
    Code.Free;
  end;
end;

var
  Expert: Tdm_dzBdsIdeScriptExpert;

initialization
  Expert := Tdm_dzBdsIdeScriptExpert.Create(nil);
finalization
  Expert.Free;
end.

