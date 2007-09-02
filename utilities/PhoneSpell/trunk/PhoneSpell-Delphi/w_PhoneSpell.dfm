object f_PhoneSpell: Tf_PhoneSpell
  Left = 0
  Top = 0
  Caption = 'PhoneSpell'
  ClientHeight = 273
  ClientWidth = 209
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    209
    273)
  PixelsPerInch = 96
  TextHeight = 13
  object l_What: TLabel
    Left = 8
    Top = 8
    Width = 131
    Height = 13
    Caption = 'What do you want to spell?'
  end
  object l_Alphabet: TLabel
    Left = 8
    Top = 64
    Width = 162
    Height = 13
    Caption = 'Whiche phonetic alphabet to use?'
  end
  object ed_What: TEdit
    Left = 8
    Top = 24
    Width = 193
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'type it here ...'
    OnChange = ed_WhatChange
  end
  object cmb_Which: TComboBox
    Left = 8
    Top = 80
    Width = 193
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
    OnChange = cmb_WhichChange
  end
  object lb_Result: TListBox
    Left = 8
    Top = 112
    Width = 193
    Height = 153
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
end
