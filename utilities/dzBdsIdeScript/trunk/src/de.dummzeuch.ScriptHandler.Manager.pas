unit de.dummzeuch.ScriptHandler.Manager;

interface

uses
  Windows,
  SysUtils,
  Classes;

type
  IEditorTextAccess = interface ['{FDD3CA3C-7B2C-4E7C-B688-1F1F9E8E9D80}']
    function GetSelectedText: string;
    procedure SetSelectedText(const _Text: string);
    procedure InsertText(const _Text: string);
  end;

type
  IMessageOutput = interface ['{8A5887AD-EB2F-43AC-A120-E6B93F21A280}']
    procedure Write(const _Msg: string; _Show: Boolean = False);
  end;

type
  IScriptHandler = interface ['{95602CF2-2F60-4611-87B3-6493301B9747}']
    procedure Execute(_TextAccess: IEditorTextAccess; _Output: IMessageOutput; _Script: TStrings);
    function GetScriptExtension: string;
    function GetName: string;
  end;

type
  IScriptHandlerManager = interface ['{4552B47D-AE45-4D83-A34F-1C0F944B6272}']
    procedure RegisterHandler(_ScriptHandler: IScriptHandler);
    procedure UnregisterHandler(const _Name: string);
    function GetHandler(const _Extension: string): IScriptHandler;
    function GetHandlers(_Idx: integer): IScriptHandler;
    function Count: integer;
  end;

function GetScriptManager: IScriptHandlerManager;

implementation

type
  TScriptHandlerManager = class(TInterfacedObject, IScriptHandlerManager)
  private
    procedure RegisterHandler(_ScriptHandler: IScriptHandler);
    procedure UnregisterHandler(const _Name: string);
    function GetHandler(const _Extension: string): IScriptHandler;
    function GetHandlers(_Idx: integer): IScriptHandler;
    function Count: integer;
  end;

function GetScriptManager: IScriptHandlerManager;
begin
  Result := TScriptHandlerManager.Create;
end;

var
  RegisteredHandlers: TInterfaceList;

function TScriptHandlerManager.Count: integer;
begin
  Result := RegisteredHandlers.Count;
end;

function TScriptHandlerManager.GetHandler(const _Extension: string): IScriptHandler;
var
  i: Integer;
begin
  for i := 0 to RegisteredHandlers.Count - 1 do begin
    Result := RegisteredHandlers[i] as IScriptHandler;
    if AnsiSameText(Result.GetScriptExtension, _Extension) then
      exit;
  end;
  Result := nil;
end;

function TScriptHandlerManager.GetHandlers(_Idx: integer): IScriptHandler;
begin
  Result := RegisteredHandlers[_Idx] as IScriptHandler;
end;

procedure TScriptHandlerManager.RegisterHandler(_ScriptHandler: IScriptHandler);
begin
  RegisteredHandlers.Add(_ScriptHandler);
end;

procedure TScriptHandlerManager.UnregisterHandler(const _Name: string);
var
  i: Integer;
begin
  for i := 0 to RegisteredHandlers.Count - 1 do begin
    if AnsiSameText((RegisteredHandlers[i] as IScriptHandler).GetName, _Name) then begin
      RegisteredHandlers.Delete(i);
      exit;
    end;
  end;
end;

initialization
  RegisteredHandlers := TInterfaceList.Create;
finalization
  RegisteredHandlers.Free;
end.

