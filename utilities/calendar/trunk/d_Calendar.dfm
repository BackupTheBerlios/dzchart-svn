object dm_Calendar: Tdm_Calendar
  OldCreateOrder = False
  Left = 468
  Top = 264
  Height = 150
  Width = 215
  object hs_HttpServer: TIdHTTPServer
    Active = True
    Bindings = <>
    DefaultPort = 4711
    AutoStartSession = True
    OnSessionStart = hs_HttpServerSessionStart
    OnSessionEnd = hs_HttpServerSessionEnd
    ServerSoftware = 'Opera Calendar'
    SessionState = True
    SessionTimeOut = 600000
    Left = 40
    Top = 16
  end
end
