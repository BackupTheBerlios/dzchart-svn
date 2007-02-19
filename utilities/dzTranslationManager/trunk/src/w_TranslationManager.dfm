object f_TranslationManager: Tf_TranslationManager
  Left = 395
  Top = 210
  Caption = 'Translation manager'
  ClientHeight = 298
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    417
    298)
  PixelsPerInch = 96
  TextHeight = 13
  object l_Template: TLabel
    Left = 8
    Top = 8
    Width = 102
    Height = 13
    Caption = 'Template file (empty)'
  end
  object l_OutputFile: TLabel
    Left = 8
    Top = 208
    Width = 53
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Output File'
  end
  object l_TranslationFiles: TLabel
    Left = 8
    Top = 56
    Width = 247
    Height = 13
    Caption = 'Translation files to use (first non-fuzzy match used)'
  end
  object ed_Template: TEdit
    Left = 8
    Top = 24
    Width = 369
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object clb_TranslationFiles: TCheckListBox
    Left = 8
    Top = 72
    Width = 369
    Height = 121
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object ed_OutputFile: TEdit
    Left = 8
    Top = 224
    Width = 369
    Height = 21
    Anchors = [akLeft, akRight, akBottom]
    TabOrder = 7
  end
  object b_SelectTemplate: TButton
    Left = 384
    Top = 22
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = b_SelectTemplateClick
  end
  object b_AddTranslationFile: TButton
    Left = 384
    Top = 72
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '+'
    TabOrder = 3
    OnClick = b_AddTranslationFileClick
  end
  object b_OutputFile: TButton
    Left = 384
    Top = 222
    Width = 25
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '...'
    TabOrder = 8
    OnClick = b_OutputFileClick
  end
  object b_DeleteTranslationFile: TButton
    Left = 384
    Top = 104
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '-'
    TabOrder = 4
    OnClick = b_DeleteTranslationFileClick
  end
  object b_Translate: TButton
    Left = 256
    Top = 264
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Translate'
    Default = True
    TabOrder = 9
    OnClick = b_TranslateClick
  end
  object b_Exit: TButton
    Left = 336
    Top = 264
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Exit'
    TabOrder = 10
    OnClick = b_ExitClick
  end
  object b_Up: TButton
    Left = 384
    Top = 136
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Up'
    TabOrder = 5
    OnClick = b_UpClick
  end
  object b_Down: TButton
    Left = 384
    Top = 168
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Dn'
    TabOrder = 6
    OnClick = b_DownClick
  end
  object od_OutputFile: TOpenDialog
    DefaultExt = 'po'
    Filter = 'po files (*.po)|*.po|all files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 320
    Top = 224
  end
  object od_TemplateFile: TOpenDialog
    DefaultExt = 'po'
    Filter = 'po files (*.po)|*.po|all files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 312
    Top = 8
  end
  object od_AddTranslation: TOpenDialog
    DefaultExt = 'po'
    Filter = 'po files (*.po)|*.po|all files (*.*)|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 312
    Top = 72
  end
end
