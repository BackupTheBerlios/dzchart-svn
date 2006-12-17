object dm_BdsIdeScript: Tdm_BdsIdeScript
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 479
  Width = 741
  object TheTimer: TTimer
    Enabled = False
    Interval = 5000
    OnTimer = TheTimerTimer
    Left = 32
    Top = 120
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
    Left = 152
    Top = 280
  end
  object dws_Main: TDelphiWebScriptII
    Config.CompilerOptions = []
    Config.MaxDataSize = 0
    Config.Timeout = 0
    Left = 88
    Top = 280
  end
  object dws2ClassesLib: Tdws2ClassesLib
    OldCreateOrder = False
    Script = dws_Main
    Left = 232
    Top = 344
    Height = 0
    Width = 0
  end
  object dws2FileFunctions: Tdws2FileFunctions
    Left = 368
    Top = 312
  end
end
