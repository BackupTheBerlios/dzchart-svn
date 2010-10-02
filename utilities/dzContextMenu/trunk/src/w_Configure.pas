unit w_Configure;

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
  IniFiles,
  ComCtrls,
  Grids,
  c_dzVirtualStringGrid;

type
  Tf_Configure = class(TForm)
    b_Cancel: TButton;
    b_OK: TButton;
    ed_MenuCaption: TEdit;
    l_MenuCaption: TLabel;
    lb_Sections: TListBox;
    sg_Items: TdzVirtualStringGrid;
    b_AddSection: TButton;
    b_DeleteSection: TButton;
    b_AddItem: TButton;
    b_DeleteItem: TButton;
    b_EditItem: TButton;
    b_EditSection: TButton;
    ed_Extension: TEdit;
    l_Extension: TLabel;
    l_FileTypes: TLabel;
    procedure lb_SectionsClick(Sender: TObject);
    procedure sg_ItemsGetNonfixedCellData(_Sender: TObject; _DataCol, _DataRow: Integer;
      _State: TGridDrawState; var _Text: string; var _HAlign: TAlignment;
      var _VAlign: TdzCellVertAlign; _Font: TFont; var _Color: TColor);
    procedure b_OKClick(Sender: TObject);
    procedure b_EditSectionClick(Sender: TObject);
    procedure b_DeleteSectionClick(Sender: TObject);
  private
    FIni: TCustomIniFile;
    FItems: TStringList;
    FExecutables: TStringList;
    procedure SetData(_Ini: TCustomIniFile);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    class function Execute(_Owner: TComponent; _Ini: TCustomIniFile): boolean;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils,
  w_dzDialog,
  w_EditSection;

class function Tf_Configure.Execute(_Owner: TComponent; _Ini: TCustomIniFile): boolean;
var
  frm: Tf_Configure;
begin
  frm := Tf_Configure.Create(_Owner);
  try
    frm.SetData(_Ini);
    Result := (frm.ShowModal = mrOK);
  finally
    FreeAndNil(frm);
  end;
end;

constructor Tf_Configure.Create(_Owner: TComponent);
begin
  inherited;
  FItems := TStringList.Create;
  FExecutables := TStringList.Create;
end;

destructor Tf_Configure.Destroy;
begin
  FreeAndNil(FExecutables);
  FreeAndNil(FItems);
  inherited;
end;

procedure Tf_Configure.b_DeleteSectionClick(Sender: TObject);
var
  Section: string;
begin
  if not TListBox_GetSelected(lb_Sections, Section) then
    exit;
  if mrYes <> Tf_dzDialog.ShowMessage(mtConfirmation, Format('Really delete context menu for file type %s ?', [Section]), [dbeYes, dbeCancel], Self) then
    exit;
  FIni.EraseSection(Section);
end;

procedure Tf_Configure.b_EditSectionClick(Sender: TObject);
var
  OrigSection: string;
  Section: string;
  Extension: string;
  sl: TStringList;
  s: string;
  i: Integer;
begin
  if not TListBox_GetSelected(lb_Sections, OrigSection) then
    exit;
  Section := OrigSection;
  Extension := ed_Extension.Text;
  if Tf_EditSection.Execute(Self, Section, Extension) then begin
    if Section <> OrigSection then begin
      sl := TStringList.Create;
      try
        FIni.ReadSectionValues(OrigSection, sl);
        FIni.EraseSection(OrigSection);
        FIni.WriteString(Section, 'extension', Extension);
        for i := 0 to sl.Count - 1 do begin
          s := sl.Names[i];
          FIni.WriteString(Section, s, sl.Values[s]);
        end;
      finally
        sl.Free;
      end;
    end else
      FIni.WriteString(Section, 'extension', Extension);
  end;
end;

procedure Tf_Configure.b_OKClick(Sender: TObject);
begin
  FIni.WriteString('global', 'caption', ed_MenuCaption.Text);
end;

procedure Tf_Configure.SetData(_Ini: TCustomIniFile);
var
  sl: TStringList;
  i: Integer;
begin
  FIni := _Ini;
  ed_MenuCaption.Text := FIni.ReadString('global', 'caption', 'SubMenu');
  sl := TStringList.Create;
  try
    FIni.ReadSections(sl);
    for i := 0 to sl.Count - 1 do begin
      if SameText(sl[i], 'global') then
        // do nothing
      else begin
        lb_Sections.Items.Add(sl[i]);
      end;
    end;
  finally
    FreeAndNil(sl);
  end;
end;

procedure Tf_Configure.sg_ItemsGetNonfixedCellData(_Sender: TObject; _DataCol, _DataRow: Integer;
  _State: TGridDrawState; var _Text: string; var _HAlign: TAlignment; var _VAlign: TdzCellVertAlign;
  _Font: TFont; var _Color: TColor);
begin
  case _DataCol of
    0: begin
        if not Assigned(FItems) or (FItems.Count <= _DataRow) then
          exit;
        _Text := FItems[_DataRow];
      end;
    1: begin
        if not Assigned(FExecutables) or (FExecutables.Count <= _DataRow) then
          exit;
        _Text := FExecutables[_DataRow];
      end;
  end;
end;

procedure Tf_Configure.lb_SectionsClick(Sender: TObject);
var
  Section: string;
  i: Integer;
begin
  if not TListBox_GetSelected(lb_Sections, Section) then
    exit;
  FIni.ReadSection(Section, FItems);
  ed_Extension.Text := FIni.ReadString(Section, 'extension', '');
  FItems.Delete(0);
  FExecutables.Clear;
  for i := 0 to FItems.Count - 1 do begin
    FExecutables.Add(FIni.ReadString(Section, fItems[i], ''));
  end;
  TGrid_SetRowCount(sg_Items, FItems.count + 1);
  TGrid_Resize(sg_Items, [roUseGridWidth, roIgnoreHeader, roUseAllRows]);
end;

end.

