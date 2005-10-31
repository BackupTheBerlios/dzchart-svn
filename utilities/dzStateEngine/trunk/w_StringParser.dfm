object f_StringParser: Tf_StringParser
  Left = 448
  Top = 166
  BorderStyle = bsDialog
  Caption = 'String Parser'
  ClientHeight = 88
  ClientWidth = 379
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
  object led_ParseString: TLabeledEdit
    Left = 8
    Top = 24
    Width = 361
    Height = 21
    EditLabel.Width = 90
    EditLabel.Height = 13
    EditLabel.Caption = 'Parse String Literal'
    TabOrder = 0
    Text = #39'asdf'#39#39'jkl;+-0-9082354^ '#39'^a#5#$2f'#39'hallo'#39
  end
  object b_Parse: TButton
    Left = 216
    Top = 56
    Width = 75
    Height = 25
    Caption = 'Parse'
    Default = True
    TabOrder = 1
    OnClick = b_ParseClick
  end
  object b_Close: TButton
    Left = 296
    Top = 56
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    TabOrder = 2
    OnClick = b_CloseClick
  end
end
