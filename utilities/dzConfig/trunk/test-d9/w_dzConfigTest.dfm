object f_dzConfigTest: Tf_dzConfigTest
  Left = 452
  Top = 216
  Width = 327
  Height = 362
  HorzScrollBar.Range = 252
  VertScrollBar.Range = 302
  ActiveControl = ed_StringSetting1
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'autoconfig test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object grp_ConfigGroup1: TGroupBox
    Left = 8
    Top = 8
    Width = 305
    Height = 289
    Caption = 'Config Group 1'
    TabOrder = 0
    object l_StringSetting1: TLabel
      Left = 8
      Top = 16
      Width = 72
      Height = 13
      Caption = 'String Setting 1'
    end
    object ed_StringSetting1: TdzConfigEdit
      Tag = 1
      Left = 8
      Top = 32
      Width = 289
      Height = 21
      Cursor = crIBeam
      TabOrder = 0
      Text = 'StringSetting1Value'
      ConfigStorageType = csString
      ConfigStorageName = 'StringSetting1'
    end
    object chk_BooleanSetting2: TdzConfigCheckBox
      Tag = -1
      Left = 168
      Top = 56
      Width = 120
      Height = 20
      Caption = 'Boolean Setting 2'
      Checked = True
      State = cbChecked
      TabOrder = 2
      ConfigStorageType = csBoolean
      ConfigStorageName = 'BooleanSetting2'
    end
    object chk_BooleanSetting1: TdzConfigCheckBox
      Left = 16
      Top = 56
      Width = 110
      Height = 20
      Caption = 'Boolean Setting 1'
      TabOrder = 1
      ConfigStorageType = csBoolean
      ConfigStorageName = 'BooleanSetting1'
    end
    object p_ConfigGroup2: TPanel
      Left = 8
      Top = 80
      Width = 289
      Height = 79
      BevelOuter = bvNone
      TabOrder = 4
      object rg_IntegerSetting1: TdzConfigRadioGroup
        Tag = 2
        Left = 0
        Top = 8
        Width = 145
        Height = 68
        Items.Strings = (
          'Value1'
          'Value2'
          'Value3')
        Caption = 'Integer Setting 1'
        ItemIndex = 0
        TabOrder = 0
        TabStop = False
        ConfigStorageType = csInteger
        ConfigStorageName = 'IntegerSetting1'
      end
      object rg_StringSetting1: TdzConfigRadioGroup
        Tag = 2
        Left = 152
        Top = 8
        Width = 137
        Height = 68
        Items.Strings = (
          'Value1'
          'Value2'
          'Value3')
        Caption = 'String Setting 1'
        ItemIndex = 2
        TabOrder = 1
        TabStop = False
        ConfigStorageType = csString
        ConfigStorageName = 'StringSetting1'
      end
    end
    object pc_Invisible: TPageControl
      Left = 8
      Top = 168
      Width = 289
      Height = 113
      ActivePage = ts_ConfigGroup3
      TabOrder = 3
      object ts_ConfigGroup3: TTabSheet
        Caption = 'Config Group 3'
        object lb_IntegerSetting2: TdzConfigListBox
          Left = 0
          Top = 0
          Width = 281
          Height = 49
          ItemHeight = 13
          Items.Strings = (
            'Value1'
            'Value2'
            'Value3')
          TabOrder = 0
          ConfigStorageType = csInteger
          ConfigStorageName = 'IntegerSetting2'
        end
        object cmb_IntegerSetting3: TdzConfigComboBox
          Left = 0
          Top = 56
          Width = 281
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          Items.Strings = (
            'IntegerSetting3Value1'
            'IntegerSetting3Value2'
            'IntegerSetting3Value3')
          ItemIndex = -1
          TabOrder = 1
          ConfigStorageType = csInteger
          ConfigStorageName = 'IntegerSetting3'
        end
      end
      object ts_ConfigGroup4: TTabSheet
        Caption = 'Config Group 4'
        ImageIndex = 1
        object lb_StringSetting2: TdzConfigListBox
          Tag = 2
          Left = 0
          Top = 0
          Width = 281
          Height = 49
          ItemHeight = 13
          Items.Strings = (
            'Value1'
            'Value2'
            'Value3')
          TabOrder = 0
          ConfigStorageType = csString
          ConfigStorageName = 'StringSetting2'
        end
        object cmb_StringSetting3: TdzConfigComboBox
          Tag = 3
          Left = 0
          Top = 56
          Width = 281
          Height = 21
          ItemHeight = 13
          Items.Strings = (
            'StringSetting3Value1'
            'StringSetting3Value2'
            'StringSetting3Value3')
          ItemIndex = -1
          TabOrder = 1
          Text = 'Hallo Trallala Diedeldum'
          ConfigStorageType = csString
          ConfigStorageName = 'StringSetting3'
        end
      end
    end
  end
  object b_OK: TButton
    Left = 152
    Top = 304
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 1
    OnClick = b_OKClick
  end
  object b_Cancel: TButton
    Left = 232
    Top = 304
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = b_CancelClick
  end
end
