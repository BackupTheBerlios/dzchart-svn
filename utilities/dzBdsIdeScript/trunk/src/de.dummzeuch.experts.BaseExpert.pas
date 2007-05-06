unit de.dummzeuch.experts.BaseExpert;

interface

uses
  SysUtils,
  Classes,
  Forms,
  Graphics,
  Menus,
  ToolsAPI;

type
  IMsgWindow = interface ['{94A55B0E-1185-4469-85F6-28A2A433A609}']
    procedure Write(const _Msg: string; _Show: Boolean = False);
    procedure Clear;
  end;

type
  Tdm_DzBaseExpert = class(TDataModule)
  protected // helper methods
    function GetMsgWindow(_Title: string): IMsgWindow;
    function GetBaseRegistryKey: string;
    function GetDelphiCustomMenu(const _Caption: string): TMenuItem;
    function TryGetDelphiCustomMenu(const _Caption: string; out _Item: TMenuItem): Boolean;
    procedure DeleteDelphiCustomMenu(const _Caption: string);
    function GetSelectedText: string;
    function GetSourceEditor: IOTASourceEditor;
    procedure InsertText(const _Text: string);
    procedure SetSelectedText(const _Text: string);
  end;

implementation

{$R *.dfm}

type
  TMsgWindow = class(TInterfacedObject, IMsgWindow)
  private
    FTitle: string;
  public
    constructor Create(const _Title: string = '');
    procedure Write(const _Msg: string; _Show: Boolean = False);
    procedure Clear;
  end;

{ Tdm_DzBaseExpert }

procedure Tdm_DzBaseExpert.DeleteDelphiCustomMenu(const _Caption: string);
var
  Item: TMenuItem;
begin
  if TryGetDelphiCustomMenu(_Caption, Item) then
    Item.Free;
end;

function Tdm_DzBaseExpert.GetBaseRegistryKey: string;
begin
  Result := (BorlandIDEServices as IOTAServices).GetBaseRegistryKey;
end;

function Tdm_DzBaseExpert.GetDelphiCustomMenu(const _Caption: string): TMenuItem;
var
  MainMenu: TMenuItem;
  NTAServices: INTAServices;
begin
  Result := nil;
  if not Supports(BorlandIDEServices, INTAServices, NTAServices) then
    Exit;

  MainMenu := NTAServices.MainMenu.Items;
  if not Assigned(MainMenu) then
    Exit;

  Result := MainMenu.Find(_Caption);
  if Result = nil then begin
    Result := TMenuItem.Create(MainMenu);
    Result.Caption := _Caption;
    MainMenu.Add(Result);
  end;
end;

function Tdm_DzBaseExpert.GetMsgWindow(_Title: string): IMsgWindow;
begin
  Result := TMsgWindow.Create(_Title);
end;

function Tdm_DzBaseExpert.GetSelectedText: string;
var
  Block: IOTAEditBlock;
  CurrMod: IOTAModule;
  i: Integer;
  Editor: IOTASourceEditor;
  View: IOTAEditView;
begin
  Result := '';
  CurrMod := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if not Assigned(CurrMod) then
    Exit;
  for i := 0 to CurrMod.GetModuleFileCount - 1 do
    if CurrMod.GetModuleFileEditor(i).QueryInterface(IOTASourceEditor, Editor) = S_OK then begin
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

function Tdm_DzBaseExpert.GetSourceEditor: IOTASourceEditor;
var
  CurrMod: IOTAModule;
  i: Integer;
begin
  Result := nil;
  CurrMod := (BorlandIDEServices as IOTAModuleServices).CurrentModule;
  if not Assigned(CurrMod) then Exit;
  for i := 0 to CurrMod.GetModuleFileCount - 1 do
    if CurrMod.GetModuleFileEditor(i).QueryInterface(IOTASourceEditor, Result) = S_OK then
      Exit;
end;

procedure Tdm_DzBaseExpert.InsertText(const _Text: string);
var
  OldAutoIndent: Boolean;
  Buffer: IOTAEditBuffer;
  Editor: IOTASourceEditor;
  View: IOTAEditView;
begin
  Editor := GetSourceEditor;
  if (Editor = nil) or (Editor.GetEditViewCount = 0) then
    Exit;

  View := Editor.GetEditView(0);
  if View = nil then
    Exit;

  Buffer := View.GetBuffer;
  if Buffer = nil then
    Exit;

  OldAutoIndent := Buffer.BufferOptions.AutoIndent;
  try
    Buffer.BufferOptions.AutoIndent := False;
    View.Position.InsertText(_Text);
    View.Paint;
  finally
    Buffer.BufferOptions.AutoIndent := OldAutoIndent;
  end;
end;

procedure Tdm_DzBaseExpert.SetSelectedText(const _Text: string);
var
  OldAutoIndent: Boolean;
  Buffer: IOTAEditBuffer;
  Block: IOTAEditBlock;
  Editor: IOTASourceEditor;
  i: Integer;
  View: IOTAEditView;
begin
  Editor := GetSourceEditor;
  if Editor = nil then
    Exit;
  for i := 0 to Editor.GetEditViewCount - 1 do begin
    View := Editor.GetEditView(i);
    if View = nil then
      continue;
    Block := View.GetBlock;
    if Block.Text <> '' then begin
      Buffer := View.GetBuffer;
      OldAutoIndent := Buffer.BufferOptions.AutoIndent;
      try
        Buffer.BufferOptions.AutoIndent := False;
        View.Position.InsertText(_Text);
        View.Paint;
      finally
        Buffer.BufferOptions.AutoIndent := OldAutoIndent;
      end;
      Exit;
    end;
  end;
end;

function Tdm_DzBaseExpert.TryGetDelphiCustomMenu(const _Caption: string; out _Item: TMenuItem): Boolean;
var
  NTAServices: INTAServices;
  MainMenu: TMenuItem;
begin
  Result := Supports(BorlandIDEServices, INTAServices, NTAServices);
  if not Result then
    Exit;

  MainMenu := NTAServices.MainMenu.Items;
  _Item := MainMenu.Find(_Caption);
  Result := Assigned(_Item);
end;

{ TMsgWindow }

procedure TMsgWindow.Clear;
var
  MessageView: IOTAMessageServices;
  MessageGroup: IOTAMessageGroup;
begin
  if Supports(BorlandIDEServices, IOTAMessageServices, MessageView) then begin
    MessageGroup := MessageView.AddMessageGroup(FTitle);
    MessageView.ClearMessageGroup(MessageGroup);
  end;
end;

constructor TMsgWindow.Create(const _Title: string);
begin
  inherited Create;
  FTitle := _Title;
end;

procedure TMsgWindow.Write(const _Msg: string; _Show: Boolean);
var
  MessageView: IOTAMessageServices;
  MessageGroup: IOTAMessageGroup;
  st: TStringList;
  i: Integer;
begin
  if Supports(BorlandIDEServices, IOTAMessageServices, MessageView) then begin
    st := TStringList.Create;
    try
      st.Text := _Msg;
      MessageGroup := MessageView.AddMessageGroup(FTitle);
      if _Show then
        MessageView.ShowMessageView(MessageGroup);
      for i := 0 to st.Count - 1 do
        MessageView.AddTitleMessage(st[i], MessageGroup);
    finally
      st.Free;
    end;
  end;
end;

end.

