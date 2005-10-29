object f_TrafficLight: Tf_TrafficLight
  Left = 416
  Top = 108
  Width = 139
  Height = 109
  Caption = 'Traffic Light'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object sb_Red: TSpeedButton
    Left = 8
    Top = 8
    Width = 41
    Height = 22
    Caption = 'Red'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object sb_Yellow: TSpeedButton
    Left = 8
    Top = 32
    Width = 41
    Height = 22
    Caption = 'Yellow'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clYellow
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object sb_Green: TSpeedButton
    Left = 8
    Top = 56
    Width = 41
    Height = 22
    Caption = 'Green'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clGreen
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object b_Close: TButton
    Left = 56
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 0
  end
end
