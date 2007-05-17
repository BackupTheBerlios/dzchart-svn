object f_ConfigForm: Tf_ConfigForm
  Left = 0
  Top = 0
  Caption = 'Configure BDS IDE Script Expert'
  ClientHeight = 218
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    417
    218)
  PixelsPerInch = 96
  TextHeight = 13
  object l_ScriptsDir: TLabel
    Left = 8
    Top = 8
    Width = 79
    Height = 13
    Caption = 'Scripts Directory'
  end
  object l_ScriptingEngines: TLabel
    Left = 8
    Top = 56
    Width = 81
    Height = 13
    Caption = 'Scripting Engines'
  end
  object ed_ScriptsDir: TEdit
    Left = 8
    Top = 24
    Width = 369
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
  end
  object b_ScriptsDir: TButton
    Left = 384
    Top = 22
    Width = 27
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&...'
    TabOrder = 1
    OnClick = b_ScriptsDirClick
  end
  object b_OK: TButton
    Left = 256
    Top = 184
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 5
  end
  object b_Cancel: TButton
    Left = 336
    Top = 184
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 6
  end
  object b_Add: TButton
    Left = 336
    Top = 72
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Add'
    TabOrder = 3
    OnClick = b_AddClick
  end
  object b_Delete: TButton
    Left = 336
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Delete'
    TabOrder = 4
    OnClick = b_DeleteClick
  end
  object lb_ScriptingEngines: TListBox
    Left = 8
    Top = 72
    Width = 321
    Height = 97
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
  object od_Select: TOpenDialog
    Filter = 
      'Scripting engine packages (*.bpl.script)|*.bpl.script|All Files ' +
      '(*.*)|*.*'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Left = 352
    Top = 136
  end
end
