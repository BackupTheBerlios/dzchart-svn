*Title: dzAutoConfig component package
*Author: Thomas Mueller

*TOC

*Introduction

The purpose of this set of components is to simplify the task of defining
application settings and associated option dialogs.

With these components you just use the form designer to set up your option
dialog, using the TdzConfigXxxx controls. In addition you can use all the
"passive" controls like TPanel, TGroupBox and TPageControl for grouping
the controls and the associated settings.

*Example

I. Start a new application with a form and drop the following components on it:

	1. A TGroupBox, name it grp__ConfigGroup1
	2. A TdzEdit, name it ed__StringSetting1 and place it inside grp_ConfigGroup1
	3. A TdzEdit, name it ed__StringSetting2 and place it outside grp_ConfigGroup1
	4. A TButton, name it b__OK with Caption 'OK' and place it wherever you want
	5. A TButton, name it b__Cancel with Caption 'Cancel' and place it wherever you want

II. Add a FormCreate event to the form with the following code:

^<<
var
  Reader: IdzAutoConfigReader;
  FormHandler: IdzAutoConfigFormHandler;
begin
  FormHandler := TConfigFormHandler.Create;
  Reader := TConfigToIni.Create('test');
  FormHandler.SettingsToForm(Reader, 'configuration', self);
end;
^>>

III. Add a Click event to the b_Cancel button, closing the form

IV. Add a Click event to the b_Ok button with the following code:

^<<
var
  Writer: IdzAutoConfigWriter;
  FormHandler: IdzAutoConfigFormHandler;
begin
  Writer := TConfigToIni.Create('test.ini');
  FormHandler := TConfigFormHandler.Create;
  FormHandler.FormToSettings(Writer, 'configuration', self);
  Close;
end;
^>>

V. you are done. Your dialog should now look something like this:

*Image-center: example-screenshot.png

Of course you are free to add labels and other (passive) controls to
make this dialog look nicer. ;-)

VI. You have just created an options dialog defining the following
two settings:

	* ConfigGroup1\StringSetting1
	* StringSetting2

They will be stored in the INI file called 'test.ini' (by default stored in
the Windows directory, but you can easily change this by just passing
an absolute name to the TConfigToIni constructor) in the following form:

^<<
[configuration]
StringSetting2=<value of StringSetting2>

[configuration\ConfigGroup1]
StringSetting1=<value of StringSetting1>
^>>

*Additional Information

**Grouping

In the example a TGroupBox was used to group settings in a user visible way,
but you can also have an invisible logical grouping by using a TPanel (don't
forget to turn off the border). Another possibility for grouping is
a TPageControl with several TTabSheets. Each TTabSheet will be used as
a settings group (see the example program provided in the archive).

**Storage types

The StorageType property of the TdzConfigXxxx controls defines whether and
how the control's setting is stored in the permanent storage. There are four
possible values:

	* csNone - The setting is not stored at all
	* csBoolean - The setting is stored as a boolean
	* csInteger - The setting is stored as an integer
	* csString - The setting is stored as a string

Some of the TdzConfigXxxx controls support more than one storage type, all of
them support csNone, of course:

	! Controls and supported storage types                            !
	!-----------------------------------------------------------------!
	! Control             ! csNone ! csBoolean ! csInteger ! csString !
	!-----------------------------------------------------------------!
	! TdzConfigCheckbox   ! X      ! X         !           !          !
	! TdzConfigEdit       ! X      !           !           ! X        !
	! TdzConfigComboBox   ! X      !           ! X         ! X        !
	! TdzConfigListBox    ! X      !           ! X         ! X        !
	! TdzConfigRadioGroup ! X      !           ! X         ! X        !

All those that support csInteger and csString default to csInteger meaning
the ItemIndex property is stored. If set to csString, the actual item
string is stored. Beware of localization issues if you use csString!

**Reading the settings

Of course the settings are not only needed in the options dialog but also
elsewhere in the program. To read them just instantiate the appropriate
class implementing the IdzAutoConfigReader interface (eg. TConfigToIni) and
use its ReadSetting methods to access them.

*Implementation details

You might have wondered about the unusual implementation of the TdzConfigXxxx
controls, especially the part saying:

^<<
// This is the implementation part of the template
{$DEFINE AUTO_CONFIG_CONTROL_TEMPLATE}
type
  // the control's ancestor is TCustomCheckBox
  _AUTO_CONFIG_CONTROL_ANCESTOR_ = TCustomCheckBox;
const
  // It is used to display a boolean type config setting
  _AUTO_CONFIG_CONTROL_STORAGE_TYPE_ = csBoolean;
  // It supports only one type of storage: Boolean
  _AUTO_CONFIG_CONTROL_STORAGE_TYPES_: TdzConfigStorageTypes = [csBoolean];
  // It is supposed to have a name prefix 'chk_'
  _AUTO_CONFIG_CONTROL_PREFIX_ = CHECKBOX_PREFIX;
{$INCLUDE 'dzAutoConfigControlTemplate.tpl'}
^>>

Since I am a lazy guy I didn't want to implement the stuff common to all those
controls for each of them, so I generated a pseudo template containing the
common code and used that to save me some typing or rather cutting and
pasting. Incidentally I avoided cut & paste errors and the problem of having
to maintain several copies of the code.

See http://www.dummzeuch.de/delphi/object__pascal__templates/english.html
for an introduction to pseudo templates.
