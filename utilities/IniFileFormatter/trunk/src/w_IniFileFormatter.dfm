object f_IniFileFormatter: Tf_IniFileFormatter
  Left = 0
  Top = 0
  Caption = 'INI File Formatter'
  ClientHeight = 569
  ClientWidth = 601
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnResize = FormResize
  DesignSize = (
    601
    569)
  PixelsPerInch = 96
  TextHeight = 13
  object l_Filename: TLabel
    Left = 8
    Top = 8
    Width = 42
    Height = 13
    Caption = 'Filename'
  end
  object l_Original: TLabel
    Left = 8
    Top = 208
    Width = 36
    Height = 13
    Caption = 'Original'
  end
  object l_Preview: TLabel
    Left = 304
    Top = 208
    Width = 38
    Height = 13
    Caption = 'Preview'
  end
  object l_Template: TLabel
    Left = 8
    Top = 160
    Width = 63
    Height = 13
    Caption = 'Template File'
  end
  object ed_Filename: TEdit
    Left = 8
    Top = 24
    Width = 553
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object b_Filename: TButton
    Left = 568
    Top = 22
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = b_FilenameClick
  end
  object m_Original: TMemo
    Left = 8
    Top = 224
    Width = 289
    Height = 297
    Anchors = [akLeft, akTop, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object m_Preview: TMemo
    Left = 304
    Top = 224
    Width = 289
    Height = 297
    Anchors = [akLeft, akTop, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 5
  end
  object gb_SortSections: TGroupBox
    Left = 8
    Top = 48
    Width = 185
    Height = 105
    Caption = 'Sort Sections'
    TabOrder = 2
    object rb_SectionsUnsorted: TRadioButton
      Left = 8
      Top = 24
      Width = 169
      Height = 17
      Caption = 'Unsorted'
      TabOrder = 0
      OnClick = SettingsChanged
    end
    object rb_SectionsAlpha: TRadioButton
      Left = 8
      Top = 48
      Width = 169
      Height = 17
      Caption = 'Alphabetically'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = SettingsChanged
    end
    object rb_SectionsByTemplate: TRadioButton
      Left = 8
      Top = 72
      Width = 169
      Height = 17
      Caption = 'By Template'
      TabOrder = 2
      OnClick = SettingsChanged
    end
  end
  object gb_SortItems: TGroupBox
    Left = 208
    Top = 48
    Width = 185
    Height = 105
    Caption = 'Sort Items'
    TabOrder = 3
    object rb_ItemsUnsorted: TRadioButton
      Left = 8
      Top = 24
      Width = 169
      Height = 17
      Caption = 'Unsorted'
      TabOrder = 0
      OnClick = SettingsChanged
    end
    object rb_ItemsAlpha: TRadioButton
      Left = 8
      Top = 48
      Width = 169
      Height = 17
      Caption = 'Alphabetically'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = SettingsChanged
    end
    object rb_ItemsByTemplate: TRadioButton
      Left = 8
      Top = 72
      Width = 169
      Height = 17
      Caption = 'By Template'
      TabOrder = 2
      OnClick = SettingsChanged
    end
  end
  object b_Close: TButton
    Left = 520
    Top = 536
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    TabOrder = 7
    OnClick = b_CloseClick
  end
  object b_SaveAs: TButton
    Left = 438
    Top = 536
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Save As ...'
    TabOrder = 6
    OnClick = b_SaveAsClick
  end
  object ed_Template: TEdit
    Left = 8
    Top = 176
    Width = 553
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
  end
  object b_Template: TButton
    Left = 568
    Top = 174
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 9
    OnClick = b_TemplateClick
  end
  object od_Filename: TOpenDialog
    Filter = 'All files (*.*)|*.*'
    Title = 'Select file'
    Left = 152
  end
  object sd_Filename: TSaveDialog
    DefaultExt = '.ini'
    Filter = 'All files (*.*)|*.*'
    Title = 'Save File As'
    Left = 368
    Top = 520
  end
end
