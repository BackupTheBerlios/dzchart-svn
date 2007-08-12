unit w_Delphi7HelpForBds;

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
  Tf_Delphi7HelpForBds = class(TForm)
    l_Blurb: TLabel;
    ed_Helpfile: TEdit;
    b_Select: TButton;
    b_OK: TButton;
    b_Cancel: TButton;
    od_HelpFile: TOpenDialog;
    rb_Winhelp: TRadioButton;
    rb_WebUrl: TRadioButton;
    rb_DoNothing: TRadioButton;
    cmb_WebUrl: TComboBox;
    procedure cmb_WebUrlEnter(Sender: TObject);
    procedure cmb_WebUrlClick(Sender: TObject);
    procedure ed_HelpfileClick(Sender: TObject);
    procedure rb_WebUrlClick(Sender: TObject);
    procedure rb_WinhelpClick(Sender: TObject);
    procedure b_SelectClick(Sender: TObject);
  private
  public
    class function Execute(out _Url: string): boolean;
  end;

implementation

{$R *.dfm}

procedure Tf_Delphi7HelpForBds.b_SelectClick(Sender: TObject);
begin
  if od_HelpFile.Execute then
    ed_Helpfile.Text := od_HelpFile.FileName;
end;

procedure Tf_Delphi7HelpForBds.cmb_WebUrlClick(Sender: TObject);
begin
  rb_WebUrl.Checked := true;
end;

procedure Tf_Delphi7HelpForBds.cmb_WebUrlEnter(Sender: TObject);
begin
  rb_WebUrl.Checked := true;
end;

procedure Tf_Delphi7HelpForBds.ed_HelpfileClick(Sender: TObject);
begin
  rb_Winhelp.Checked := true;
end;

class function Tf_Delphi7HelpForBds.Execute(out _Url: string): boolean;
var
  frm: Tf_Delphi7HelpForBds;
begin
  frm := Tf_Delphi7HelpForBds.Create(nil);
  try
    if mrOk = frm.ShowModal then begin
      Result := true;
      if frm.rb_Winhelp.Checked then begin
        _Url := 'winhelp:' + frm.ed_Helpfile.Text;
      end else if frm.rb_WebUrl.Checked then begin
        _Url := frm.cmb_WebUrl.Text;
      end else
        _Url := '-';
    end else
      Result := false;
  finally
    frm.free
  end;
end;

procedure Tf_Delphi7HelpForBds.rb_WebUrlClick(Sender: TObject);
begin
  cmb_WebUrl.SetFocus;
end;

procedure Tf_Delphi7HelpForBds.rb_WinhelpClick(Sender: TObject);
begin
  ed_Helpfile.SetFocus;
end;

end.

