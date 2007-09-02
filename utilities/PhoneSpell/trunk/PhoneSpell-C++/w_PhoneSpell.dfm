object f_PhoneSpell: Tf_PhoneSpell
  Left = 515
  Top = 367
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'PhoneSpell'
  ClientHeight = 336
  ClientWidth = 202
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    202
    336)
  PixelsPerInch = 96
  TextHeight = 13
  object l_What: TLabel
    Left = 8
    Top = 8
    Width = 131
    Height = 13
    Caption = 'What do you want to spell?'
  end
  object l_Which: TLabel
    Left = 8
    Top = 56
    Width = 156
    Height = 13
    Caption = 'Which phonetic alphabet to use?'
  end
  object ed_What: TEdit
    Left = 8
    Top = 24
    Width = 185
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'type it here ...'
    OnChange = ed_WhatChange
  end
  object cmb_Which: TComboBox
    Left = 8
    Top = 72
    Width = 185
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 1
    OnChange = cmb_WhichChange
  end
  object lb_Result: TListBox
    Left = 8
    Top = 104
    Width = 185
    Height = 225
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 2
  end
end
