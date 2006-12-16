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
  dws2FileFunctions,
  dws2ClassesLibModule,
  dws2StringResult;

type
  Tdm_BdsIdeScript = class(TDataModule)
    TheTimer: TTimer;
    dws2Unit: Tdws2Unit;
    dws_Main: TDelphiWebScriptII;
    dws2StringsUnit: Tdws2StringsUnit;
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
    FOrigEditorText: string;
    FNewEditorText: string;

    procedure ClearMenuItems(_List: TList);
    procedure ClearScriptMenuItems;
    procedure LoadScripts;
    procedure RefreshMenuItems;
    procedure RefreshScriptMenuItems;
    procedure SetTimerRefreshing;
    procedure GenerateIdeEditorClass;
  private // Eval functions for the TIdeEditor class and IdeEditor global variable
    procedure OnIdeEditorCommitEval(Info: TProgramInfo; ExtObject: TObject);
    procedure OnIdeEditorCountEval(Info: TProgramInfo; ExtObject: TObject);
    procedure OnIdeEditorGetLinesEval(Info: TProgramInfo; ExtObject: TObject);
    procedure OnIdeEditorCreateAssignExternalObject(Info: TProgramInfo; var ExtObject: TObject);
    procedure OnIdeEditorDestroy(ExternalObject: TObject);
    procedure OnIdeEditorSetLinesEval(Info: TProgramInfo; ExtObject: TObject);
    procedure OnIdeEditorAddEval(Info: TProgramInfo; ExtObject: TObject);
    procedure OnIdeEditorDeleteEval(Info: TProgramInfo; ExtObject: TObject);
    procedure OnIdeEditorClearEval(Info: TProgramInfo; ExtObject: TObject);
    procedure OnIdeEditorInsertEval(Info: TProgramInfo; ExtObject: TObject);
    procedure OnIdeEditorSetTextEval(Info: TProgramInfo; ExtObject: TObject);
    procedure OnIdeEditorGetTextEval(Info: TProgramInfo; ExtObject: TObject);
    procedure OnIdeEditorInstantiate(var ExtObject: TObject);
  private
    function Compile(const _Source: string): TProgram;
    procedure ExecuteScript(_Code: TStrings);
    procedure WriteToMessages(const _Error: string);
    procedure ClearMessages;
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
      WriteToMessages('Error compiling script:'#13#10 + e.Message);
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

procedure Tdm_BdsIdeScript.WriteToMessages(const _Error: string);
var
  MessageView: IOTAMessageServices;
  st: TStringList;
  i: Integer;
begin
  if Supports(BorlandIDEServices, IOTAMessageServices, MessageView) then begin
    st := TStringList.Create;
    try
      st.Text := _Error;
      FMessageGroup := MessageView.AddMessageGroup('BdsIdeScript');
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

  GenerateIdeEditorClass;
end;

procedure Tdm_BdsIdeScript.DataModuleDestroy(Sender: TObject);
begin
  ClearScriptMenuItems;
  FScripts.Free;
  FScriptItems.Free;
end;

procedure Tdm_BdsIdeScript.OnIdeEditorCommitEval(Info: TProgramInfo; ExtObject: TObject);
var
  st: TStringList;
begin
  st := ExtObject as TStringList;
  FNewEditorText := st.Text;
end;

procedure Tdm_BdsIdeScript.OnIdeEditorCountEval(Info: TProgramInfo; ExtObject: TObject);
var
  st: TStringList;
begin
  st := ExtObject as TStringList;
  Info.Result := st.Count;
end;

procedure Tdm_BdsIdeScript.OnIdeEditorGetLinesEval(Info: TProgramInfo; ExtObject: TObject);
var
  st: TStringList;
  Idx: Integer;
begin
  st := ExtObject as TStringList;
  Idx := Info.Value['_Idx'];
  Info.Result := st[Idx];
end;

procedure Tdm_BdsIdeScript.OnIdeEditorCreateAssignExternalObject(Info: TProgramInfo; var ExtObject: TObject);
var
  st: TStringList;
begin
  st := TStringList.Create;
  st.Text := FOrigEditorText;
  ExtObject := st;
end;

procedure Tdm_BdsIdeScript.OnIdeEditorAddEval(Info: TProgramInfo; ExtObject: TObject);
var
  st: TStringList;
  Line: string;
begin
  st := ExtObject as TStringList;
  Line := Info.Value['_Line'];
  Info.Result := st.Add(Line);
end;

procedure Tdm_BdsIdeScript.OnIdeEditorClearEval(Info: TProgramInfo; ExtObject: TObject);
var
  st: TStringList;
begin
  st := ExtObject as TStringList;
  st.Clear;
end;

procedure Tdm_BdsIdeScript.OnIdeEditorDeleteEval(Info: TProgramInfo; ExtObject: TObject);
var
  st: TStringList;
  Idx: Integer;
begin
  st := ExtObject as TStringList;
  Idx := Info.Value['_Idx'];
  st.Delete(Idx);
end;

procedure Tdm_BdsIdeScript.OnIdeEditorGetTextEval(Info: TProgramInfo; ExtObject: TObject);
var
  st: TStringList;
begin
  st := ExtObject as TStringList;
  Info.Result := st.Text;
end;

procedure Tdm_BdsIdeScript.OnIdeEditorInsertEval(Info: TProgramInfo; ExtObject: TObject);
var
  st: TStringList;
  Idx: Integer;
  Line: string;
begin
  st := ExtObject as TStringList;
  Idx := Info.Value['_Idx'];
  Line := Info.Value['_Line'];
  st.Insert(Idx, Line);
end;

procedure Tdm_BdsIdeScript.OnIdeEditorSetLinesEval(Info: TProgramInfo; ExtObject: TObject);
var
  st: TStringList;
  Idx: Integer;
begin
  st := ExtObject as TStringList;
  Idx := Info.Value['_Idx'];
  st[Idx] := Info.Value['_Line'];
end;

procedure Tdm_BdsIdeScript.OnIdeEditorSetTextEval(Info: TProgramInfo; ExtObject: TObject);
var
  st: TStringList;
begin
  st := ExtObject as TStringList;
  st.Text := Info.Value['_Text'];
end;

procedure Tdm_BdsIdeScript.OnIdeEditorDestroy(ExternalObject: TObject);
begin
  ExternalObject.Free;
end;

procedure Tdm_BdsIdeScript.OnIdeEditorInstantiate(var ExtObject: TObject);
var
  st: TStringList;
begin
  st := TStringList.Create;
  st.Text := FOrigEditorText;
  ExtObject := st;
end;

procedure Tdm_BdsIdeScript.GenerateIdeEditorClass;
var
  Item: Tdws2Class;
  Constr: Tdws2Constructor;
  Meth: Tdws2Method;
  Param: Tdws2Parameter;
  Prop: Tdws2Property;
  Inst: Tdws2Instance;
begin
  Item := dws2Unit.Classes.Add as Tdws2Class;
  Item.Name := 'TIdeEditor';
  Item.OnObjectDestroy := Self.OnIdeEditorDestroy;

  Constr := Item.Constructors.Add as Tdws2Constructor;
  Constr.Name := 'Create';
  Constr.OnAssignExternalObject := OnIdeEditorCreateAssignExternalObject;

  Meth := Item.Methods.Add as Tdws2Method;
  Meth.Name := 'Commit';
  Meth.Kind := mkProcedure;
  Meth.OnEval := OnIdeEditorCommitEval;

  Meth := Item.Methods.Add as Tdws2Method;
  Meth.Name := 'Count';
  Meth.Kind := mkFunction;
  Meth.ResultType := 'Integer';
  Meth.OnEval := OnIdeEditorCountEval;

  Meth := Item.Methods.Add as Tdws2Method;
  Meth.Name := 'GetLines';
  Meth.Kind := mkFunction;
  Param := Meth.Parameters.Add as Tdws2Parameter;
  Param.Name := '_Idx';
  Param.DataType := 'Integer';
  Meth.ResultType := 'String';
  Meth.OnEval := OnIdeEditorGetLinesEval;

  Meth := Item.Methods.Add as Tdws2Method;
  Meth.Name := 'SetLines';
  Meth.Kind := mkProcedure;
  Param := Meth.Parameters.Add as Tdws2Parameter;
  Param.Name := '_Idx';
  Param.DataType := 'Integer';
  Param := Meth.Parameters.Add as Tdws2Parameter;
  Param.Name := '_Line';
  Param.DataType := 'String';
  Meth.OnEval := OnIdeEditorSetLinesEval;

  Meth := Item.Methods.Add as Tdws2Method;
  Meth.Name := 'Add';
  Meth.Kind := mkFunction;
  Meth.ResultType := 'Integer';
  Param := Meth.Parameters.Add as Tdws2Parameter;
  Param.Name := '_Line';
  Param.DataType := 'String';
  Meth.OnEval := OnIdeEditorAddEval;

  Meth := Item.Methods.Add as Tdws2Method;
  Meth.Name := 'Delete';
  Meth.Kind := mkProcedure;
  Param := Meth.Parameters.Add as Tdws2Parameter;
  Param.Name := '_Idx';
  Param.DataType := 'Integer';
  Meth.OnEval := OnIdeEditorDeleteEval;

  Meth := Item.Methods.Add as Tdws2Method;
  Meth.Name := 'Clear';
  Meth.Kind := mkProcedure;
  Meth.OnEval := OnIdeEditorClearEval;

  Meth := Item.Methods.Add as Tdws2Method;
  Meth.Name := 'Insert';
  Meth.Kind := mkProcedure;
  Param := Meth.Parameters.Add as Tdws2Parameter;
  Param.Name := '_Idx';
  Param.DataType := 'Integer';
  Param := Meth.Parameters.Add as Tdws2Parameter;
  Param.Name := '_Line';
  Param.DataType := 'String';
  Meth.OnEval := OnIdeEditorInsertEval;

  Meth := Item.Methods.Add as Tdws2Method;
  Meth.Name := 'SetText';
  Meth.Kind := mkProcedure;
  Param := Meth.Parameters.Add as Tdws2Parameter;
  Param.Name := '_Text';
  Param.DataType := 'String';
  Meth.OnEval := OnIdeEditorSetTextEval;

  Meth := Item.Methods.Add as Tdws2Method;
  Meth.Name := 'GetText';
  Meth.Kind := mkFunction;
  Meth.ResultType := 'String';
  Meth.OnEval := OnIdeEditorGetTextEval;

  Prop := Item.Properties.Add as Tdws2Property;
  Prop.Name := 'Lines';
  Prop.DataType := 'String';
  Prop.ReadAccess := 'GetLines';
  Prop.WriteAccess := 'SetLines';
  Param := Prop.Parameters.Add as Tdws2Parameter;
  Param.Name := '_Idx';
  Param.DataType := 'Integer';
  Prop.IsDefault := True;

  Prop := Item.Properties.Add as Tdws2Property;
  Prop.Name := 'Text';
  Prop.DataType := 'String';
  Prop.ReadAccess := 'GetText';
  Prop.WriteAccess := 'SetText';

  Inst := dws2Unit.Instances.Add as Tdws2Instance;
  Inst.Name := 'IdeEditor';
  Inst.DataType := 'TIdeEditor';
  Inst.AutoDestroyExternalObject := True;
  Inst.OnInstantiate := OnIdeEditorInstantiate;
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
    FOrigEditorText := GetSelectedText;
    FNewEditorText := FOrigEditorText;
    ExecuteScript(Code);
    if FNewEditorText <> FOrigEditorText then
      SetSelectedText(FNewEditorText);
  finally
    Code.Free;
  end;
end;

end.

