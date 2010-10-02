object f_EditSection: Tf_EditSection
  Left = 0
  Top = 0
  Caption = 'File Type'
  ClientHeight = 89
  ClientWidth = 273
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object l_Section: TLabel
    Left = 8
    Top = 8
    Width = 43
    Height = 13
    Caption = 'File Type'
  end
  object l_Extension: TLabel
    Left = 144
    Top = 8
    Width = 47
    Height = 13
    Caption = 'Extension'
  end
  object ed_Section: TEdit
    Left = 8
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object ed_Extension: TEdit
    Left = 144
    Top = 24
    Width = 121
    Height = 21
    TabOrder = 1
  end
  object b_Ok: TButton
    Left = 112
    Top = 56
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object b_Cancel: TButton
    Left = 192
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
