unit u_dzConfigRegister;

interface

uses
  Classes,
  DesignIntf,
{$IFDEF linux}
  QForms,
{$ENDIF linux}
  c_dzConfigEdit,
  c_dzConfigCheckbox,
  c_dzConfigRadioGroup,
  c_dzConfigListBox,
  c_dzConfigComboBox {,
  w_dzAutoConfigEditor};

{: Registers the components TdzConfigCheckBox, TdzConfigRadioGroup,
   TdzConfigListBox, TdzConfigEdit and TdzConfigComboBox with the IDE}
procedure Register;

implementation

{$R c_dzConfigListBox.dcr}
{$R c_dzConfigComboBox.dcr}
{$R c_dzConfigEdit.dcr}
{$R c_dzConfigCheckBox.dcr}
{$R c_dzConfigRadioGroup.dcr}

const
  PalettePage = 'dummzeuch.de';

procedure Register;
begin
  // The UnRegisterClass calls are necessary because otherwise
  // the ide would complain about duplicate registrations of
  // the _DZ_CONFIG_CONTROL_ class.

  RegisterComponents(PalettePage, [TdzConfigCheckBox]);
  UnRegisterClass(c_dzConfigCheckbox._DZ_CONFIG_CONTROL_);

  RegisterComponents(PalettePage, [TdzConfigRadioGroup]);
  UnRegisterClass(c_dzConfigRadioGroup._DZ_CONFIG_CONTROL_);

  RegisterComponents(PalettePage, [TdzConfigListBox]);
  UnRegisterClass(c_dzConfigListBox._DZ_CONFIG_CONTROL_);

  RegisterComponents(PalettePage, [TdzConfigEdit]);
  UnRegisterClass(c_dzConfigEdit._DZ_CONFIG_CONTROL_);

  RegisterComponents(PalettePage, [TdzConfigComboBox]);
  UnRegisterClass(c_dzConfigComboBox._DZ_CONFIG_CONTROL_);

  //  RegisterComponentEditor(TForm, TdzAutoConfigEditor);
end;

end.

