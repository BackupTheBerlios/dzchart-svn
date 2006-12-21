object f_dzPolynomEdit: Tf_dzPolynomEdit
  Left = 449
  Top = 203
  ActiveControl = b_Ok
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Polynom Editor'
  ClientHeight = 225
  ClientWidth = 251
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  DesignSize = (
    251
    225)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 13
    Width = 26
    Height = 13
    Caption = 'Order'
  end
  object b_Ok: TButton
    Left = 169
    Top = 160
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = b_OkClick
  end
  object sed_Order: TSpinEdit
    Left = 48
    Top = 8
    Width = 73
    Height = 22
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = 11
    Font.Name = 'MS Sans Serif'
    Font.Pitch = fpVariable
    Font.Style = []
    MaxValue = 10
    MinValue = 1
    ParentFont = False
    TabOrder = 1
    Value = 0
  end
  object ed_Polynom: TEdit
    Left = 8
    Top = 40
    Width = 234
    Height = 21
    Cursor = crIBeam
    Anchors = [akLeft, akTop, akRight]
    ReadOnly = True
    TabOrder = 2
  end
  object b_Cancel: TButton
    Left = 169
    Top = 192
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object scr_Coefficients: TScrollBox
    Left = 8
    Top = 72
    Width = 154
    Height = 145
    HorzScrollBar.Range = 113
    VertScrollBar.Range = 34
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoScroll = False
    TabOrder = 3
    object l_Placeholder: TLabel
      Left = 8
      Top = 12
      Width = 15
      Height = 13
      Caption = 'a ='
    end
    object ed_Placeholder: TEdit
      Left = 32
      Top = 8
      Width = 81
      Height = 21
      Cursor = crIBeam
      TabOrder = 0
    end
  end
end
