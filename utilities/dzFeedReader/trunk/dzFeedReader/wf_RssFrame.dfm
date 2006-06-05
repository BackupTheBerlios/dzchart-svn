inherited fr_RssFrame: Tfr_RssFrame
  Width = 289
  Height = 217
  ExplicitWidth = 289
  ExplicitHeight = 217
  inherited p_Frame: TPanel
    Width = 289
    Height = 217
    ExplicitWidth = 289
    ExplicitHeight = 217
    inherited p_Left: TPanel
      Height = 209
      ExplicitHeight = 209
    end
    inherited p_Bottom: TPanel
      Top = 213
      Width = 285
      ExplicitTop = 213
      ExplicitWidth = 285
      inherited p_BottomRight: TPanel
        Left = 283
        ExplicitLeft = 283
      end
    end
    inherited p_Top: TPanel
      Width = 285
      ExplicitWidth = 285
      inherited p_TopRight: TPanel
        Left = 283
        ExplicitLeft = 283
      end
    end
    inherited p_Right: TPanel
      Left = 285
      Height = 209
      ExplicitLeft = 285
      ExplicitHeight = 209
    end
    inherited p_Client: TPanel
      Width = 281
      Height = 209
      ExplicitWidth = 281
      ExplicitHeight = 209
      inherited p_Title: TPanel
        Width = 277
        ExplicitWidth = 277
        inherited p_TitleRight: TPanel
          Left = 268
          ExplicitLeft = 268
        end
        inherited p_TitleCaption: TPanel
          Width = 259
          PopupMenu = pm_Title
          ExplicitWidth = 259
        end
      end
      object lb_RssFeed: TListBox
        Left = 2
        Top = 26
        Width = 277
        Height = 181
        Align = alClient
        ItemHeight = 13
        Items.Strings = (
          'Loading ...')
        ParentShowHint = False
        ShowHint = False
        TabOrder = 1
        OnMouseLeave = lb_RssFeedMouseLeave
        OnMouseMove = lb_RssFeedMouseMove
        ExplicitLeft = 0
        ExplicitTop = -13
        ExplicitWidth = 249
        ExplicitHeight = 193
      end
    end
  end
  object SimpleRSS: TSimpleRSS
    Channel.Required.Title = 'Slashdot'
    Channel.Required.Link = 'http://rss.slashdot.org/Slashdot/slashdotScience'
    Channel.Required.Desc = 'Slashdot'
    Channel.Optional.Language = langEN
    Channel.Optional.PubDate.DateTime = 38857.449251053240000000
    Channel.Optional.PubDate.TimeZone = 'GMT'
    Channel.Optional.LastBuildDate.DateTime = 38857.449251053240000000
    Channel.Optional.LastBuildDate.TimeZone = 'GMT'
    Channel.Optional.Categories = <>
    Channel.Optional.Generator = 'SimpleRSS ver 0.4 (BlueHippo) Release 1'
    Channel.Optional.Docs = 'http://blogs.law.harvard.edu/tech/rss'
    Channel.Optional.Cloud.Port = 1
    Channel.Optional.TTL = 60
    Channel.Optional.Image.Include = False
    Channel.Optional.Image.Required.URL = 'URL Required'
    Channel.Optional.Image.Required.Title = 'Title Required'
    Channel.Optional.Image.Required.Link = 'Link Required'
    Channel.Optional.Image.Optional.Width = 0
    Channel.Optional.Image.Optional.Height = 0
    Channel.Optional.SkipDays.Monday = False
    Channel.Optional.SkipDays.Tuesday = False
    Channel.Optional.SkipDays.Wednesday = False
    Channel.Optional.SkipDays.Thursday = False
    Channel.Optional.SkipDays.Friday = False
    Channel.Optional.SkipDays.Saturday = False
    Channel.Optional.SkipDays.Sunday = False
    Channel.iTunes.Block = False
    Channel.iTunes.Explict = False
    Channel.iTunes.KeyWords = <>
    Items = <>
    Version = '2.0'
    XMLType = xtRSS
    IndyHTTP = IdHTTP
    OnParseXML = SimpleRSSParseXML
    Left = 56
    Top = 312
  end
  object IdHTTP: TIdHTTP
    AllowCookies = True
    ProxyParams.BasicAuthentication = False
    ProxyParams.ProxyPort = 0
    Request.ContentLength = -1
    Request.Accept = 'text/html, */*'
    Request.BasicAuthentication = False
    Request.UserAgent = 'Mozilla/3.0 (compatible; Indy Library)'
    HTTPOptions = [hoForceEncodeParams]
    Left = 120
    Top = 320
  end
  object pm_Title: TPopupMenu
    Left = 24
    Top = 64
    object mi_Properties: TMenuItem
      Caption = 'Properties'
    end
    object mi_Saveas: TMenuItem
      Caption = 'Save as ...'
    end
    object mi_Remove: TMenuItem
      Caption = 'Remove'
      OnClick = mi_RemoveClick
    end
  end
end
