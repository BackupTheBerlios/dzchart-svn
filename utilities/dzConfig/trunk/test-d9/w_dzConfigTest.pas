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
  u_dzConfigCommon,
  u_dzConfigFormHandler,
  u_dzConfigToConsole,
  u_dzConfigToIni;

{ Tf_AutoConfigTest }

procedure Tf_dzConfigTest.b_OKClick(Sender: TObject);
var
  // Note that both are interfaces so no destructor call is needed
  Writer: IdzConfigWriter;
  FormHandler: IdzConfigFormHandler;
begin
  // We create two different writers here:
  // * TConfigToConsole which writes to the console
  //   this is of course for demo purposes only since in a real world
  //   application who wants to write his settings to the console?
  // * TConfigToIni which writes to an ini file
  Writer := TConfigToConsole.Create;

  // a FormHandler is the "glue" between a configuration form an a ConfigWriter
  FormHandler := TdzConfigFormHandler.Create;
  FormHandler.FormToSettings(Writer, '', self);

  // write to test.ini in the default location (c:\windows / c:\winnt)
  Writer := TConfigToIni.Create('test');
  // prefix all groups with "Delphi 2005 Test", you can also just pass an empty string here
  FormHandler.FormToSettings(Writer, 'Delphi 2005 Test', self);
  ShowMessage('done');
  Close;
end;

procedure Tf_dzConfigTest.b_CancelClick(Sender: TObject);
begin
  Close;
end;

procedure Tf_dzConfigTest.FormCreate(Sender: TObject);
var
  Reader: IdzConfigReader;
  FormHandler: IdzConfigFormHandler;
begin
  // The FormHandler is the "glue" between a ConfigReader and a configuration form
  FormHandler := TdzConfigFormHandler.Create;
  // a TConfigToIni reads settings from an ini file, in this case test.ini in the default location
  Reader := TConfigToIni.Create('test');
  // prefix all groups with "Delphi 2005 Test", you can also just pass an empty string here
  FormHandler.SettingsToForm(Reader, 'Delphi 2005 Test', self);
end;

end.

