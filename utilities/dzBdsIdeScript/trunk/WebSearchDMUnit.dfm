object WebSearchDM: TWebSearchDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 479
  Width = 741
  object ActionList1: TActionList
    Left = 32
    Top = 16
    object WebSearchAction: TAction
      Caption = '&Web search...'
    end
    object ScriptRunAction: TAction
      Caption = 'Run Sc&ript...'
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 32
    Top = 72
    object N1: TMenuItem
      Caption = '-'
    end
    object WebSearchItem: TMenuItem
      Caption = '&Web Search'
    end
    object ScriptRunItem: TMenuItem
      Caption = 'Run Sc&ript...'
    end
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = Timer1Timer
    Left = 32
    Top = 120
  end
end
