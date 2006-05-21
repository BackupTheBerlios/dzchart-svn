object f_MainForm: Tf_MainForm
  Left = 439
  Top = 151
  Width = 582
  Height = 279
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
  object p_Description: TPanel
    Left = 24
    Top = 64
    Width = 305
    Height = 129
    BevelOuter = bvNone
    Color = clInfoBk
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    Visible = False
    object l_Description: TLabel
      Left = 0
      Top = 21
      Width = 85
      Height = 26
      Caption = 'the content goes here'
      WordWrap = True
    end
    object p_Title: TPanel
      Left = 0
      Top = 0
      Width = 305
      Height = 17
      Align = alTop
      BevelOuter = bvNone
      Color = clActiveCaption
      TabOrder = 0
      object l_Title: TLabel
        Left = 0
        Top = 2
        Width = 100
        Height = 13
        Caption = 'DescriptionGoesHere'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clCaptionText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
    end
    object p_Link: TPanel
      Left = 0
      Top = 112
      Width = 305
      Height = 17
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object l_Link: TLabel
        Left = 0
        Top = 2
        Width = 65
        Height = 13
        Caption = 'LinkGoesHere'
      end
    end
  end
  object pm_Main: TPopupMenu
    Left = 280
    Top = 128
    object mi_AddFeed: TMenuItem
      Caption = 'Add Feed ...'
      OnClick = mi_AddFeedClick
    end
  end
end
