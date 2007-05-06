inherited dm_dzBdsIdeScriptExpert: Tdm_dzBdsIdeScriptExpert
  OldCreateOrder = True
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 219
  Width = 217
  object TheTimer: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = TheTimerTimer
    Left = 24
    Top = 16
  end
end
