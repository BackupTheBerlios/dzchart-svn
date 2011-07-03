object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 348
  ClientWidth = 633
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    633
    348)
  PixelsPerInch = 96
  TextHeight = 13
  object b_Start: TButton
    Left = 552
    Top = 320
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Start'
    TabOrder = 0
    OnClick = b_StartClick
  end
  object lv_Events: TListView
    Left = 8
    Top = 8
    Width = 617
    Height = 297
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Start'
        Width = 100
      end
      item
        Caption = 'End'
        Width = 100
      end
      item
        Caption = 'Duration'
        Width = 100
      end
      item
        Caption = 'Summary'
        Width = 100
      end
      item
        Caption = 'Description'
        Width = 200
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    ExplicitWidth = 521
  end
end
