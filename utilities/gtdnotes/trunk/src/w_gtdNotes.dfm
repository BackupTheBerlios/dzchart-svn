object f_gtdNotes: Tf_gtdNotes
  Left = 410
  Top = 184
  BiDiMode = bdLeftToRight
  Caption = 'GTD Notes'
  ClientHeight = 449
  ClientWidth = 532
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ParentBiDiMode = False
  PixelsPerInch = 96
  TextHeight = 13
  object VST: TVirtualStringTree
    Left = 0
    Top = 19
    Width = 532
    Height = 430
    Align = alClient
    BiDiMode = bdLeftToRight
    Colors.BorderColor = clWindowText
    Colors.HotColor = clBlack
    Header.AutoSizeIndex = 0
    Header.DefaultHeight = 17
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.MainColumn = -1
    HintAnimation = hatNone
    IncrementalSearch = isAll
    ParentBiDiMode = False
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoTristateTracking]
    TreeOptions.MiscOptions = [toCheckSupport, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowButtons, toShowRoot, toShowTreeLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect, toCenterScrollIntoView]
    OnChange = VSTChange
    OnChecked = VSTChecked
    OnDblClick = VSTDblClick
    OnFreeNode = VSTFreeNode
    OnGetText = VSTGetText
    OnPaintText = VSTPaintText
    OnInitNode = VSTInitNode
    OnKeyPress = VSTKeyPress
    ExplicitWidth = 456
    Columns = <>
  end
  object tb_Main: TToolBar
    Left = 0
    Top = 0
    Width = 532
    Height = 19
    ButtonHeight = 19
    ButtonWidth = 86
    Caption = 'GTD Notes Toolbar'
    List = True
    ShowCaptions = True
    AllowTextButtons = True
    TabOrder = 1
    ExplicitWidth = 456
    object tb_Load: TToolButton
      Left = 0
      Top = 0
      Action = act_Load
      Style = tbsTextButton
    end
    object tb_Save: TToolButton
      Left = 35
      Top = 0
      Action = act_Save
      Style = tbsTextButton
    end
    object tb_SaveAs: TToolButton
      Left = 71
      Top = 0
      Action = act_SaveAs
      Style = tbsTextButton
    end
    object ToolButton3: TToolButton
      Left = 133
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 2
      Style = tbsSeparator
    end
    object tb_SetNextAction: TToolButton
      Left = 141
      Top = 0
      Action = act_SetNextAction
      Style = tbsTextButton
    end
    object tb_AddAction: TToolButton
      Left = 226
      Top = 0
      Action = act_AddAction
      Style = tbsTextButton
    end
    object tb_DeleteAction: TToolButton
      Left = 289
      Top = 0
      Action = act_DeleteAction
      Style = tbsTextButton
    end
    object ToolButton7: TToolButton
      Left = 364
      Top = 0
      Width = 8
      Caption = 'ToolButton7'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object tb_ShowDone: TToolButton
      Left = 372
      Top = 0
      Action = act_ShowDone
      AllowAllUp = True
      Grouped = True
      Style = tbsTextButton
    end
  end
  object TheActionList: TActionList
    Left = 224
    Top = 88
    object act_Load: TAction
      Caption = 'Load'
      OnExecute = act_LoadExecute
    end
    object act_Save: TAction
      Caption = 'Save'
      OnExecute = act_SaveExecute
    end
    object act_SaveAs: TAction
      Caption = 'Save as ...'
      OnExecute = act_SaveAsExecute
    end
    object act_ShowDone: TAction
      Caption = 'Show done'
      GroupIndex = 1
      OnExecute = act_ShowDoneExecute
    end
    object act_SetNextAction: TAction
      Caption = 'Set Next Action'
      OnExecute = act_SetNextActionExecute
    end
    object act_AddAction: TAction
      Caption = 'Add Action'
    end
    object act_DeleteAction: TAction
      Caption = 'Delete Action'
    end
  end
end
