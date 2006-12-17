unit de.dummzeuch.dm_BdsIdeScriptWizard;

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
  dws2FileFunctions, dws2ClassesLibModule;

type
  Tdm_BdsIdeScript = class(TDataModule)
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
    FMessageGroup: IOTAMessageGroup;

    procedure ClearMenuItems(_List: TList);
    procedure ClearScriptMenuItems;
    procedure LoadScripts;
    procedure RefreshMenuItems;
    procedure RefreshScriptMenuItems;
    procedure SetTimerRefreshing;
    procedure RegisterOTAFunctions;
  private // Eval functions for the TIdeEditor class and IdeEditor global variable
  private
    function Compile(const _Source: string): TProgram;
    procedure ExecuteScript(_Code: TStrings);
    procedure WriteToMessages(const _Msg: string; _Show: Boolean = False);
    procedure ClearMessages;
    procedure OnIdeEditorGetSelectedTextEval(_Info: TProgramInfo; _ExtObject: TObject);
    procedure OnIdeEditorSetSelectedTextEval(_Info: TProgramInfo; _ExtObject: TObject);
    procedure OnIdeEditorInsertTextEval(_Info: TProgramInfo;
      _ExtObject: TObject);
  public
  end;

var
  dm_BdsIdeScript: Tdm_BdsIdeScript;

implementation

{$R *.DFM}

uses
  Registry,
  ShellAPI,
  dws2Symbols,
  dws2Compiler,
  de.dummzeuch.utils.IdeExpert;

procedure Tdm_BdsIdeScript.ExecuteScript(_Code: TStrings);
var
  i: Integer;
  Prog: TProgram;
begin
  WriteToMessages('Compiling script.');
  try
    Prog := Compile(_Code.Text);
  except
    on e: Exception do begin
      WriteToMessages('Error compiling script:'#13#10 + e.Message, True);
      Exit;
    end;
  end;

  WriteToMessages('Running script.');
  try
//    Prog.BeginProgram();
    Prog.Execute;
//    Prog.EndProgram;

    for i := 0 to Prog.Msgs.Count - 1 do
      WriteToMessages(Prog.Msgs[i].asInfo);
    WriteToMessages(Tdws2DefaultResult(Prog.Result).Text);

  finally
    Prog.Free;
  end;
  WriteToMessages('Done.');
end;

procedure Tdm_BdsIdeScript.ClearMessages;
var
  MessageView: IOTAMessageServices;
begin
  if Supports(BorlandIDEServices, IOTAMessageServices, MessageView) then begin
    FMessageGroup := MessageView.AddMessageGroup('BdsIdeScript');
    MessageView.ClearMessageGroup(FMessageGroup);
  end;
end;

procedure Tdm_BdsIdeScript.WriteToMessages(const _Msg: string; _Show: Boolean = False);
var
  MessageView: IOTAMessageServices;
  st: TStringList;
  i: Integer;
begin
  if Supports(BorlandIDEServices, IOTAMessageServices, MessageView) then begin
    st := TStringList.Create;
    try
      st.Text := _Msg;
      FMessageGroup := MessageView.AddMessageGroup('BdsIdeScript');
      if _Show then
        MessageView.ShowMessageView(FMessageGroup);
      for i := 0 to st.Count - 1 do
        MessageView.AddTitleMessage(st[i], FMessageGroup);
    finally
      st.Free;
    end;
  end;
end;

function Tdm_BdsIdeScript.Compile(const _Source: string): TProgram;
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

procedure Tdm_BdsIdeScript.DataModuleCreate(Sender: TObject);
begin
  FScriptItems := TList.Create;
  FScripts := TStringList.Create;

  RefreshMenuItems;

  RegisterOTAFunctions;
end;

procedure Tdm_BdsIdeScript.DataModuleDestroy(Sender: TObject);
begin
  ClearScriptMenuItems;
  FScripts.Free;
  FScriptItems.Free;
end;

procedure Tdm_BdsIdeScript.OnIdeEditorGetSelectedTextEval(_Info: TProgramInfo; _ExtObject: TObject);
begin
  _Info.Result := GetSelectedText;
end;

procedure Tdm_BdsIdeScript.OnIdeEditorSetSelectedTextEval(_Info: TProgramInfo; _ExtObject: TObject);
begin
  SetSelectedText(_Info.Value['_Text']);
end;

procedure Tdm_BdsIdeScript.OnIdeEditorInsertTextEval(_Info: TProgramInfo; _ExtObject: TObject);
begin
  InsertText(_Info.Value['_Text']);
end;

procedure Tdm_BdsIdeScript.RegisterOTAFunctions;
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

procedure Tdm_BdsIdeScript.LoadScripts;
var
  Folder: string;
  i: Integer;
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.OpenKey(GetBaseRegistryKey + '\dzBdsIdeScript', True);
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

procedure Tdm_BdsIdeScript.RunScriptExplainClick(Sender: TObject);
begin
  ShowMessage(
    'Scripts should be placed in the folder pointed by '#13 +
    'the registry value at:'#13#13 +
    'HKCU\' + GetBaseRegistryKey + '\dzBdsIdeScript' +
    #13#13'The value should be assigned to "ScriptFolder".' +
    #13#13'You can always refresh the script list by clicking this item.'
    );
  RefreshMenuItems;
end;

procedure Tdm_BdsIdeScript.TheTimerTimer(Sender: TObject);
begin
  RefreshMenuItems;
end;

procedure Tdm_BdsIdeScript.RefreshMenuItems;
begin
  RefreshScriptMenuItems;
end;

procedure Tdm_BdsIdeScript.RefreshScriptMenuItems;
var
  i: Integer;
  Item, ScriptMenu: TMenuItem;
begin
  ScriptMenu := GetDelphiCustomMenu('Scripts');
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

procedure Tdm_BdsIdeScript.ClearMenuItems(_List: TList);
var
  i: Integer;
begin
  if (not Assigned(_List)) or (_List.Count = 0) then
    Exit;
  for i := 0 to _List.Count - 1 do
    TMenuItem(_List[i]).Free;
  _List.Clear;
end;

procedure Tdm_BdsIdeScript.ClearScriptMenuItems;
begin
  ClearMenuItems(FScriptItems);
end;

procedure Tdm_BdsIdeScript.SetTimerRefreshing;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create;
  try
    Reg.OpenKey(GetBaseRegistryKey + '\dzBdsIdeScript\Options', True);
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

procedure Tdm_BdsIdeScript.RunScriptSubItemClick(Sender: TObject);
var
  Code: TStringList;
  FileName: string;
  Idx: Integer;
begin
  Idx := (Sender as TComponent).Tag;
  FileName := FScripts[Idx];
  ClearMessages;
  WriteToMessages('Loading script ' + FileName);
  Code := TStringList.Create;
  try
    Code.LoadFromFile(FileName);
    ExecuteScript(Code);
  finally
    Code.Free;
  end;
end;

end.

