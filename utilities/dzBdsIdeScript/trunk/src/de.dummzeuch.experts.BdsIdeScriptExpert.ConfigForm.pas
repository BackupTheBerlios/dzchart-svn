unit de.dummzeuch.experts.BdsIdeScriptExpert.ConfigForm;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  CheckLst,
  Mask;

type
  Tf_ConfigForm = class(TForm)
    l_ScriptsDir: TLabel;
    ed_ScriptsDir: TEdit;
    b_ScriptsDir: TButton;
    b_OK: TButton;
    b_Cancel: TButton;
    l_ScriptingEngines: TLabel;
    b_Add: TButton;
    b_Delete: TButton;
    lb_ScriptingEngines: TListBox;
    od_Select: TOpenDialog;
    procedure b_ScriptsDirClick(Sender: TObject);
    procedure b_AddClick(Sender: TObject);
    procedure b_DeleteClick(Sender: TObject);
  private
  public
    class function Execute(_Owner: TComponent; var _ScriptsDir: string; _Engines: TStrings): boolean;
  end;

implementation

{$R *.dfm}

uses
  FileCtrl;

procedure Tf_ConfigForm.b_AddClick(Sender: TObject);
var
  s: string;
  Handle: Cardinal;
begin
  if not od_Select.Execute then
    exit;
  s := od_Select.FileName;
  Handle := LoadPackage(s);
  lb_ScriptingEngines.Items.AddObject(s, pointer(Handle));
end;

procedure Tf_ConfigForm.b_DeleteClick(Sender: TObject);
var
  Idx: integer;
  Handle: Cardinal;
begin
  Idx := lb_ScriptingEngines.ItemIndex;
  if Idx = -1 then
    exit;

  Handle := Integer(lb_ScriptingEngines.Items.Objects[Idx]);
  UnloadPackage(Handle);
  lb_ScriptingEngines.Items.Delete(Idx);
end;

procedure Tf_ConfigForm.b_ScriptsDirClick(Sender: TObject);
var
  Root: WideString;
  Dir: string;
begin
  Root := '';
  Dir := ed_ScriptsDir.Text;
  if not SelectDirectory('Select Scripts Directory', Root, Dir) then
    exit;
  ed_ScriptsDir.Text := Dir;
end;

class function Tf_ConfigForm.Execute(_Owner: TComponent; var _ScriptsDir: string; _Engines: TStrings): boolean;
var
  frm: Tf_ConfigForm;
begin
  frm := Tf_ConfigForm.Create(_Owner);
  try
    frm.ed_ScriptsDir.Text := _ScriptsDir;
    frm.lb_ScriptingEngines.Items.Assign(_Engines);
    Result := (mroK = frm.ShowModal);
    if not Result then
      exit;
    _ScriptsDir := frm.ed_ScriptsDir.Text;
    _Engines.Assign(frm.lb_ScriptingEngines.Items);
  finally
    frm.Free;
  end;
end;

end.

