unit w_dzConfigTest;

interface

uses
  SysUtils,
  Types,
  Classes,
  Variants,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  c_dzConfigComboBox,
  c_dzConfigListBox,
  c_dzConfigRadioGroup,
  c_dzConfigCheckBox,
  c_dzConfigEdit;

type
  Tf_dzConfigTest = class(TForm)
    grp_ConfigGroup1: TGroupBox;
    l_StringSetting1: TLabel;
    ed_StringSetting1: TdzConfigEdit;
    chk_BooleanSetting1: TdzConfigCheckBox;
    chk_BooleanSetting2: TdzConfigCheckBox;
    b_OK: TButton;
    b_Cancel: TButton;
    p_ConfigGroup2: TPanel;
    rg_IntegerSetting1: TdzConfigRadioGroup;
    rg_StringSetting1: TdzConfigRadioGroup;
    pc_Invisible: TPageControl;
    ts_ConfigGroup3: TTabSheet;
    ts_ConfigGroup4: TTabSheet;
    lb_IntegerSetting2: TdzConfigListBox;
    cmb_IntegerSetting3: TdzConfigComboBox;
    lb_StringSetting2: TdzConfigListBox;
    cmb_StringSetting3: TdzConfigComboBox;
    procedure b_OKClick(Sender: TObject);
    procedure b_CancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

var
  f_dzConfigTest: Tf_dzConfigTest;

implementation

{$R *.dfm}

uses
  u_dzAutoConfigCommon,
  u_ConfigFormHandler,
  u_ConfigToConsole,
  u_ConfigToIni;

{ Tf_dzConfigTest }

procedure Tf_dzConfigTest.b_OKClick(Sender: TObject);
var
  Writer: IdzAutoConfigWriter;
  FormHandler: IdzAutoConfigFormHandler;
begin
  Writer := TConfigToConsole.Create;
  FormHandler := TConfigFormHandler.Create;
  FormHandler.FormToSettings(Writer, '', self);
  Writer := TConfigToIni.Create('test');
  FormHandler.FormToSettings(Writer, 'rootpath', self);
  ShowMessage('done');
  Close;
end;

procedure Tf_dzConfigTest.b_CancelClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_dzConfigTest.FormCreate(Sender: TObject);
var
  Reader: IdzAutoConfigReader;
  FormHandler: IdzAutoConfigFormHandler;
begin
  FormHandler := TConfigFormHandler.Create;
  Reader := TConfigToIni.Create('test');
  FormHandler.SettingsToForm(Reader, 'rootpath', self);
end;

end.

