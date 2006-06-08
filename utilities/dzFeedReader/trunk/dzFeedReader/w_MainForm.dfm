object f_MainForm: Tf_MainForm
  Left = 439
  Top = 151
  Width = 588
  Height = 264
  AutoScroll = True
  Caption = 'Simple RSS Reader'
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMenu = pm_Main
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pm_Main: TPopupMenu
    Left = 280
    Top = 128
    object mi_AddFeed: TMenuItem
      Caption = 'Add Feed ...'
      OnClick = mi_AddFeedClick
    end
  end
end
