object fmDictContxt: TfmDictContxt
  Left = 192
  Top = 107
  Width = 769
  Height = 635
  Caption = 'Symbol Dictionary and Context Map Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 573
    Top = 0
    Width = 3
    Height = 520
    Cursor = crHSplit
    Align = alRight
    Beveled = True
  end
  object Editor: TSynEdit
    Left = 0
    Top = 0
    Width = 573
    Height = 520
    Cursor = crIBeam
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    ParentShowHint = False
    ShowHint = False
    TabOrder = 0
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Terminal'
    Gutter.Font.Style = []
    Highlighter = SynPasSyn1
    Keystrokes = <
      item
        Command = ecUp
        ShortCut = 38
      end
      item
        Command = ecSelUp
        ShortCut = 8230
      end
      item
        Command = ecScrollUp
        ShortCut = 16422
      end
      item
        Command = ecDown
        ShortCut = 40
      end
      item
        Command = ecSelDown
        ShortCut = 8232
      end
      item
        Command = ecScrollDown
        ShortCut = 16424
      end
      item
        Command = ecLeft
        ShortCut = 37
      end
      item
        Command = ecSelLeft
        ShortCut = 8229
      end
      item
        Command = ecWordLeft
        ShortCut = 16421
      end
      item
        Command = ecSelWordLeft
        ShortCut = 24613
      end
      item
        Command = ecRight
        ShortCut = 39
      end
      item
        Command = ecSelRight
        ShortCut = 8231
      end
      item
        Command = ecWordRight
        ShortCut = 16423
      end
      item
        Command = ecSelWordRight
        ShortCut = 24615
      end
      item
        Command = ecPageDown
        ShortCut = 34
      end
      item
        Command = ecSelPageDown
        ShortCut = 8226
      end
      item
        Command = ecPageBottom
        ShortCut = 16418
      end
      item
        Command = ecSelPageBottom
        ShortCut = 24610
      end
      item
        Command = ecPageUp
        ShortCut = 33
      end
      item
        Command = ecSelPageUp
        ShortCut = 8225
      end
      item
        Command = ecPageTop
        ShortCut = 16417
      end
      item
        Command = ecSelPageTop
        ShortCut = 24609
      end
      item
        Command = ecLineStart
        ShortCut = 36
      end
      item
        Command = ecSelLineStart
        ShortCut = 8228
      end
      item
        Command = ecEditorTop
        ShortCut = 16420
      end
      item
        Command = ecSelEditorTop
        ShortCut = 24612
      end
      item
        Command = ecLineEnd
        ShortCut = 35
      end
      item
        Command = ecSelLineEnd
        ShortCut = 8227
      end
      item
        Command = ecEditorBottom
        ShortCut = 16419
      end
      item
        Command = ecSelEditorBottom
        ShortCut = 24611
      end
      item
        Command = ecToggleMode
        ShortCut = 45
      end
      item
        Command = ecCopy
        ShortCut = 16429
      end
      item
        Command = ecCut
        ShortCut = 8238
      end
      item
        Command = ecPaste
        ShortCut = 8237
      end
      item
        Command = ecDeleteChar
        ShortCut = 46
      end
      item
        Command = ecDeleteLastChar
        ShortCut = 8
      end
      item
        Command = ecDeleteLastChar
        ShortCut = 8200
      end
      item
        Command = ecDeleteLastWord
        ShortCut = 16392
      end
      item
        Command = ecUndo
        ShortCut = 32776
      end
      item
        Command = ecRedo
        ShortCut = 40968
      end
      item
        Command = ecLineBreak
        ShortCut = 13
      end
      item
        Command = ecLineBreak
        ShortCut = 8205
      end
      item
        Command = ecTab
        ShortCut = 9
      end
      item
        Command = ecShiftTab
        ShortCut = 8201
      end
      item
        Command = ecContextHelp
        ShortCut = 16496
      end
      item
        Command = ecSelectAll
        ShortCut = 16449
      end
      item
        Command = ecCopy
        ShortCut = 16451
      end
      item
        Command = ecPaste
        ShortCut = 16470
      end
      item
        Command = ecCut
        ShortCut = 16472
      end
      item
        Command = ecBlockIndent
        ShortCut = 24649
      end
      item
        Command = ecBlockUnindent
        ShortCut = 24661
      end
      item
        Command = ecLineBreak
        ShortCut = 16461
      end
      item
        Command = ecInsertLine
        ShortCut = 16462
      end
      item
        Command = ecDeleteWord
        ShortCut = 16468
      end
      item
        Command = ecDeleteLine
        ShortCut = 16473
      end
      item
        Command = ecDeleteEOL
        ShortCut = 24665
      end
      item
        Command = ecUndo
        ShortCut = 16474
      end
      item
        Command = ecRedo
        ShortCut = 24666
      end
      item
        Command = ecGotoMarker0
        ShortCut = 16432
      end
      item
        Command = ecGotoMarker1
        ShortCut = 16433
      end
      item
        Command = ecGotoMarker2
        ShortCut = 16434
      end
      item
        Command = ecGotoMarker3
        ShortCut = 16435
      end
      item
        Command = ecGotoMarker4
        ShortCut = 16436
      end
      item
        Command = ecGotoMarker5
        ShortCut = 16437
      end
      item
        Command = ecGotoMarker6
        ShortCut = 16438
      end
      item
        Command = ecGotoMarker7
        ShortCut = 16439
      end
      item
        Command = ecGotoMarker8
        ShortCut = 16440
      end
      item
        Command = ecGotoMarker9
        ShortCut = 16441
      end
      item
        Command = ecSetMarker0
        ShortCut = 24624
      end
      item
        Command = ecSetMarker1
        ShortCut = 24625
      end
      item
        Command = ecSetMarker2
        ShortCut = 24626
      end
      item
        Command = ecSetMarker3
        ShortCut = 24627
      end
      item
        Command = ecSetMarker4
        ShortCut = 24628
      end
      item
        Command = ecSetMarker5
        ShortCut = 24629
      end
      item
        Command = ecSetMarker6
        ShortCut = 24630
      end
      item
        Command = ecSetMarker7
        ShortCut = 24631
      end
      item
        Command = ecSetMarker8
        ShortCut = 24632
      end
      item
        Command = ecSetMarker9
        ShortCut = 24633
      end
      item
        Command = ecNormalSelect
        ShortCut = 24654
      end
      item
        Command = ecColumnSelect
        ShortCut = 24643
      end
      item
        Command = ecLineSelect
        ShortCut = 24652
      end
      item
        Command = ecMatchBracket
        ShortCut = 24642
      end>
    Lines.Strings = (
      'type'
      '  TPoint = record'
      '    x : Integer;'
      '    y : Integer;'
      '  end;'
      ''
      'type'
      '  TMyClass = class(TObject)'
      '    FSnuffy : Boolean;'
      '    FVal : Integer;'
      '    function GetMyValue : Integer;'
      '    procedure SetSomeThing(AValue : Integer);'
      '    property Value: Integer read GetMyValue;'
      '  end;'
      ''
      '{$INCLUDE '#39'SampleInclude.dws'#39'}'
      ''
      'function TMyClass.GetMyValue : Integer;'
      'begin'
      '  Result := FVal;'
      'end;'
      ''
      'procedure TMyClass.SetSomeThing(AValue : Integer);'
      'begin'
      '  var i : Integer;'
      '  if AValue = 0 then'
      '  begin'
      '    for i := 0 to 100 do'
      '    begin'
      '      FVal := FVal + AValue;'
      '    end;'
      '  end'
      '  else'
      '  begin'
      '    FVal := Value;'
      '  end;'
      'end;'
      ''
      'procedure This;'
      'var i : Integer;'
      'begin'
      '  // doesn'#39't do anything.'
      'end;'
      ''
      'var Mine : TMyClass;'
      ''
      'Mine := TMyClass.Create;'
      'try'
      '  mine.SetSomeThing(100);'
      'finally'
      '  //Mine.Free;'
      'end;')
    OnChange = EditorChange
    OnStatusChange = EditorStatusChange
  end
  object sbStatus: TStatusBar
    Left = 0
    Top = 589
    Width = 761
    Height = 19
    Panels = <>
    SimplePanel = True
  end
  object pFooter: TPanel
    Left = 0
    Top = 520
    Width = 761
    Height = 69
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object mMessages: TMemo
      Left = 0
      Top = 0
      Width = 761
      Height = 69
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object pDictionary: TPanel
    Left = 576
    Top = 0
    Width = 185
    Height = 520
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 3
    object Splitter2: TSplitter
      Left = 0
      Top = 265
      Width = 185
      Height = 3
      Cursor = crVSplit
      Align = alTop
      Beveled = True
    end
    object Splitter3: TSplitter
      Left = 0
      Top = 365
      Width = 185
      Height = 3
      Cursor = crVSplit
      Align = alBottom
    end
    object lbSymNames: TListBox
      Left = 0
      Top = 82
      Width = 185
      Height = 183
      Align = alTop
      ItemHeight = 13
      TabOrder = 1
      OnClick = lbSymNamesClick
    end
    object Panel1: TPanel
      Left = 0
      Top = 65
      Width = 185
      Height = 17
      Align = alTop
      BevelOuter = bvLowered
      Caption = 'Symbol Dictionary'
      TabOrder = 2
    end
    object lbSymPositions: TListBox
      Left = 0
      Top = 268
      Width = 185
      Height = 97
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbSymPositionsClick
    end
    object pContext: TPanel
      Left = 0
      Top = 368
      Width = 185
      Height = 152
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 3
      object pContextHdr: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 17
        Align = alTop
        BevelOuter = bvLowered
        Caption = 'Context Map'
        TabOrder = 0
      end
      object tvContextMap: TTreeView
        Left = 0
        Top = 17
        Width = 185
        Height = 135
        Align = alClient
        HideSelection = False
        Indent = 19
        ReadOnly = True
        TabOrder = 1
        OnClick = tvContextMapClick
      end
    end
    object grpListControl: TGroupBox
      Left = 0
      Top = 0
      Width = 185
      Height = 65
      Align = alTop
      Caption = 'List Control'
      TabOrder = 4
      object chkAutoUpdate: TCheckBox
        Left = 7
        Top = 17
        Width = 90
        Height = 17
        Caption = 'Auto Update'
        TabOrder = 0
      end
      object btnForceUpdate: TButton
        Left = 8
        Top = 34
        Width = 89
        Height = 22
        Caption = '&Force Update'
        TabOrder = 1
        OnClick = btnForceUpdateClick
      end
    end
  end
  object DelphiWebScriptII1: TDelphiWebScriptII
    Config.CompilerOptions = [coSymbolDictionary, coContextMap]
    Config.MaxDataSize = 0
    Config.Timeout = 0
    Left = 96
    Top = 192
  end
  object SynPasSyn1: TSynPasSyn
    CommentAttri.Background = clSilver
    CommentAttri.Foreground = clGray
    DirectiveAttri.Foreground = clGreen
    KeyAttri.Foreground = clPurple
    StringAttri.Foreground = clRed
    Left = 72
    Top = 64
  end
end
