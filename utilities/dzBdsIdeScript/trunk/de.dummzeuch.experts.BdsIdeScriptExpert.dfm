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
  object dws2Unit: Tdws2Unit
    Script = dws_Main
    Arrays = <>
    Classes = <>
    Constants = <>
    Dependencies.Strings = (
      'Classes')
    Enumerations = <>
    Forwards = <>
    Functions = <>
    Instances = <>
    Records = <>
    Synonyms = <>
    UnitName = 'dzIdeEditor'
    Variables = <>
    StaticSymbols = False
    Left = 112
    Top = 88
  end
  object dws_Main: TDelphiWebScriptII
    Config.CompilerOptions = []
    Config.MaxDataSize = 0
    Config.Timeout = 0
    Left = 24
    Top = 88
  end
  inline dws2ClassesLib: Tdws2ClassesLib
    OldCreateOrder = False
    Script = dws_Main
    Left = 24
    Top = 144
    Height = 0
    Width = 0
  end
  object dws2FileFunctions: Tdws2FileFunctions
    Left = 112
    Top = 144
  end
end
