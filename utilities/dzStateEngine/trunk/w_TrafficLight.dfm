object f_TrafficLight: Tf_TrafficLight
  Left = 416
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Traffic Light'
  ClientHeight = 168
  ClientWidth = 139
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object b_Close: TButton
    Left = 56
    Top = 136
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 4
    OnClick = b_CloseClick
  end
  object p_Red: TPanel
    Left = 0
    Top = 0
    Width = 49
    Height = 49
    BevelOuter = bvLowered
    BiDiMode = bdLeftToRight
    Caption = 'Red'
    Color = clRed
    ParentBiDiMode = False
    TabOrder = 0
  end
  object p_Yellow: TPanel
    Left = 0
    Top = 56
    Width = 49
    Height = 49
    BevelOuter = bvLowered
    BiDiMode = bdLeftToRight
    Caption = 'Yellow'
    Color = clYellow
    ParentBiDiMode = False
    TabOrder = 1
  end
  object p_Green: TPanel
    Left = 0
    Top = 112
    Width = 49
    Height = 49
    BevelOuter = bvLowered
    BiDiMode = bdLeftToRight
    Caption = 'Green'
    Color = clLime
    ParentBiDiMode = False
    TabOrder = 2
  end
  object b_Switch: TButton
    Left = 56
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Switch'
    TabOrder = 3
    OnClick = b_SwitchClick
  end
end
