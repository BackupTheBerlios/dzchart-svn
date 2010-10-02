unit w_EditSection;

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
  StdCtrls;

type
  Tf_EditSection = class(TForm)
    ed_Section: TEdit;
    ed_Extension: TEdit;
    b_Ok: TButton;
    b_Cancel: TButton;
    l_Section: TLabel;
    l_Extension: TLabel;
  private
    procedure SetData(const _Section, _Extension: string);
    procedure GetData(var _Section, _Extension: string);
  public
    class function Execute(_Owner: TWinControl; var _Section, _Extension: string): boolean;
  end;

implementation

{$R *.dfm}

uses
  u_dzVclUtils;

{ Tf_EditSection }

class function Tf_EditSection.Execute(_Owner: TWinControl; var _Section, _Extension: string): boolean;
var
  frm: Tf_EditSection;
begin
  frm := Tf_EditSection.Create(_Owner);
  try
    TForm_CenterOn(frm, _Owner);
    frm.SetData(_Section, _Extension);
    Result := (frm.ShowModal = mrOk);
    if Result then
      frm.GetData(_Section, _Extension);
  finally
    FreeAndNil(frm);
  end;
end;

procedure Tf_EditSection.GetData(var _Section, _Extension: string);
begin
  _Section := ed_Section.Text;
  _Extension := ed_Extension.Text;
end;

procedure Tf_EditSection.SetData(const _Section, _Extension: string);
begin
  ed_Section.Text := _Section;
  ed_Extension.Text := _Extension;
end;

end.

