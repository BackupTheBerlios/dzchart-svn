object f_FeedEditForm: Tf_FeedEditForm
  Left = 421
  Top = 168
  ActiveControl = ed_FeedName
  Caption = 'Edit Feed'
  ClientHeight = 137
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    393
    137)
  PixelsPerInch = 96
  TextHeight = 13
  object l_Name: TLabel
    Left = 8
    Top = 8
    Width = 76
    Height = 13
    Caption = 'Name (optional)'
  end
  object l_FeedUrl: TLabel
    Left = 8
    Top = 56
    Width = 19
    Height = 13
    Caption = 'URL'
  end
  object b_OK: TButton
    Left = 232
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = b_OKClick
  end
  object b_Cancel: TButton
    Left = 312
    Top = 104
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object ed_FeedName: TEdit
    Left = 8
    Top = 24
    Width = 377
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object ed_FeedUrl: TEdit
    Left = 8
    Top = 72
    Width = 377
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
end
