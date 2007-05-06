unit de.dummzeuch.ScriptHandler.DWS2;

interface

uses
  SysUtils,
  Classes,
  dws2Comp,
  dws2Exprs,
  dws2StringFunctions,
  dws2FileFunctions,
  dws2ClassesLibModule,
  de.dummzeuch.ScriptHandler.Manager;

type
  TScriptHandlerDWS2 = class(TInterfacedObject, IScriptHandler)
  private
    dws_Main: TDelphiWebScriptII;
    dws2Unit: Tdws2Unit;
    dws2FileFunctions: Tdws2FileFunctions;
    FTextAccess: IEditorTextAccess;
    {: compiles the source and returns an executable TProgram }
    function Compile(const _Source: string): TProgram;
    {: registers the IdeEditorXxxx scripting functions for accessing the editor text }
    procedure RegisterOTAFunctions;
  private // event functions called from the script
    {: implements the script function IdeEditorGetSelectedText }
    procedure OnIdeEditorGetSelectedTextEval(_Info: TProgramInfo; _ExtObject: TObject);
    {: implements the script function IdeEditorSetSelectedText }
    procedure OnIdeEditorSetSelectedTextEval(_Info: TProgramInfo; _ExtObject: TObject);
    {: implements the script function IdeEditorInsertText }
    procedure OnIdeEditorInsertTextEval(_Info: TProgramInfo; _ExtObject: TObject);
  private // implementation of IScriptHandler
    {: @returns '.dws' }
    function GetScriptExtension: string;
    {: @returns 'Delphi Web Script II' }
    function GetName: string;
    {: executes the script stored in the TStrings }
    procedure Execute(_TextAccess: IEditorTextAccess; _Output: IMessageOutput;
      _Code: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  dws2Compiler,
  dws2Symbols;

const
  DWS_SCRIPT_EXTENSION = '.dws';
  DWS_SCRIPT_NAME = 'Delphi Web Script II';


{ TScriptHandler }

constructor TScriptHandlerDWS2.Create;
begin
  inherited Create;

  dws_Main := TDelphiWebScriptII.Create(nil);
  with dws_Main do begin
    Name := 'dws_Main';
    Config.CompilerOptions := [];
    Config.MaxDataSize := 0;
    Config.Timeout := 0;
  end;

  dws2ClassesLib := Tdws2ClassesLib.Create(nil);
  with dws2ClassesLib do
    Script := dws_Main;

  dws2Unit := Tdws2Unit.Create(nil);
  with dws2Unit do begin
    Name := 'dws2Unit';
    Dependencies.Clear;
    Dependencies.Add('Classes');
    UnitName := 'dzIdeEditor';
    StaticSymbols := False;
    Script := dws_Main;
  end;

  dws2FileFunctions := Tdws2FileFunctions.Create(nil);
  with dws2FileFunctions do begin
    Name := 'dws2FileFunctions';
  end;

  RegisterOTAFunctions;
end;

destructor TScriptHandlerDWS2.Destroy;
begin
  dws2Unit.Free;
  dws_Main.Free;
  inherited;
end;

function TScriptHandlerDWS2.Compile(const _Source: string): TProgram;
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

procedure TScriptHandlerDWS2.Execute(_TextAccess: IEditorTextAccess; _Output: IMessageOutput;
  _Code: TStrings);
var
  i: Integer;
  Prog: TProgram;
begin
  FTextAccess := _TextAccess;
//  Msg := GetMsgWindow(MESSAGE_WINDOW_TITLE);
  _Output.Write('Compiling script.');
  try
    Prog := Compile(_Code.Text);
  except
    on e: Exception do begin
      _Output.Write('Error compiling script:'#13#10 + e.Message, True);
      Exit;
    end;
  end;

  _Output.Write('Running script.');
  try
//    Prog.BeginProgram();
    Prog.Execute;
//    Prog.EndProgram;

    for i := 0 to Prog.Msgs.Count - 1 do
      _Output.Write(Prog.Msgs[i].asInfo);
    _Output.Write(Tdws2DefaultResult(Prog.Result).Text);

  finally
    Prog.Free;
  end;
  _Output.Write('Done.');
end;

function TScriptHandlerDWS2.GetName: string;
begin
  Result := DWS_SCRIPT_NAME;
end;

function TScriptHandlerDWS2.GetScriptExtension: string;
begin
  Result := DWS_SCRIPT_EXTENSION;
end;

procedure TScriptHandlerDWS2.OnIdeEditorGetSelectedTextEval(_Info: TProgramInfo; _ExtObject: TObject);
begin
  _Info.Result := FTextAccess.GetSelectedText;
end;

procedure TScriptHandlerDWS2.OnIdeEditorInsertTextEval(_Info: TProgramInfo; _ExtObject: TObject);
begin
  FTextAccess.InsertText(_Info.Value['_Text']);
end;

procedure TScriptHandlerDWS2.OnIdeEditorSetSelectedTextEval(_Info: TProgramInfo; _ExtObject: TObject);
begin
  FTextAccess.SetSelectedText(_Info.Value['_Text']);
end;

procedure TScriptHandlerDWS2.RegisterOTAFunctions;
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

initialization
  GetScriptManager.RegisterHandler(TScriptHandlerDWS2.Create);
finalization
  GetScriptManager.UnregisterHandler(DWS_SCRIPT_NAME);
end.

