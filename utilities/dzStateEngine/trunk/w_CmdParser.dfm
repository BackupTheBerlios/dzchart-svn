object f_CmdParser: Tf_CmdParser
  Left = 518
  Top = 225
  Caption = 'Commandline parser test'
  ClientHeight = 287
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object l_Commandline: TLabel
    Left = 8
    Top = 8
    Width = 63
    Height = 13
    Caption = 'Commandline'
  end
  object l_Result: TLabel
    Left = 8
    Top = 64
    Width = 30
    Height = 13
    Caption = 'Result'
  end
  object l_ErrorDisplay: TLabel
    Left = 8
    Top = 83
    Width = 111
    Height = 13
    Caption = 'Error display goes here'
  end
  object ed_Commandline: TEdit
    Left = 8
    Top = 24
    Width = 329
    Height = 21
    TabOrder = 0
    Text = '-a --abc -b 5 --bcd=5 --def 5'
  end
  object b_Parse: TButton
    Left = 343
    Top = 22
    Width = 75
    Height = 25
    Caption = 'Parse'
    Default = True
    TabOrder = 1
    OnClick = b_ParseClick
  end
  object sg_Result: TStringGrid
    Left = 8
    Top = 83
    Width = 410
    Height = 196
    ColCount = 2
    DefaultRowHeight = 20
    FixedCols = 0
    RowCount = 2
    TabOrder = 2
    ColWidths = (
      143
      245)
  end
end
