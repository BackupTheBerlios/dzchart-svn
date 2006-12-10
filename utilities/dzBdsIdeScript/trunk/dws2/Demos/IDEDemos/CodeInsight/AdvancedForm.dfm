object fmAdvanced: TfmAdvanced
  Left = 192
  Top = 107
  Width = 696
  Height = 480
  Caption = 'Intermediate/Advanced example'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object SynEdit1: TSynEdit
    Left = 0
    Top = 0
    Width = 688
    Height = 384
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
    PopupMenu = pMenu
    ShowHint = False
    TabOrder = 0
    OnMouseMove = SynEdit1MouseMove
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
      '{ CTRL+SPACE for code completion '
      '  CTRL+SHIFT+SPACE for parameter completion'
      '  Hover moust over a symbol for a pop-up hint.'
      ''
      
        '  NOTICE the graphics that are displayed for different symbol ty' +
        'pes.'
      
        '  This is controlled with the "Include Glyphs" CheckBox option. ' +
        '}'
      ''
      'var MyTest: TMyGetSetClass;'
      ''
      '// Uncomment "MyTest" below.'
      '// Perform CodeComplete (CTRL+SHIFT) after '#39'.'#39' below.'
      '// Experiment with the CheckBox option "Include Get/Set methods"'
      ''
      '//MyTest.'
      ''
      'function DoSomething(var a: string): boolean;'
      'begin'
      '  // Press CTRL+SHIFT in this block. Notice the variable scope.'
      '  // (be sure no errors exist before trying)'
      ''
      '  Result := (a = '#39'something'#39');'
      '  a := a + '#39'howdy'#39';'
      'end;'
      ''
      '// Demo of Class Completion below. Uncomment class to test.'
      '// With cursor inside class definition,'
      '// press CTRL+SHIFT+C (or right-click for menu option)'
      '//type'
      '//  TTest = class'
      '//    property A: string read GetA write SetA;'
      '//  end;'
      ''
      'type'
      '  TTestClass1 = class'
      '    function GetName: string;'
      '    procedure SetName(Name: string);'
      '    property Name: string read GetName write SetName;'
      '    class procedure DoThat(AThis: string);'
      '  end;'
      ''
      'function TTestClass1.GetName: String;'
      'begin'
      '  '
      'end;'
      ''
      'procedure TTestClass1.SetName(Name: String);'
      'begin'
      ''
      'end;'
      ''
      'class procedure TTestClass1.DoThat(AThis: String);'
      'begin'
      ''
      'end;')
    OnChange = SynEdit1Change
  end
  object pOptions: TPanel
    Left = 0
    Top = 384
    Width = 688
    Height = 69
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object chkIncludeGraphics: TCheckBox
      Left = 8
      Top = 8
      Width = 97
      Height = 17
      Hint = 'Include glyphs in CodeInsight displays.'
      Caption = 'Include Glyphs'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = chkIncludeGraphicsClick
    end
    object lbMessages: TListBox
      Left = 168
      Top = 0
      Width = 520
      Height = 69
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 1
    end
    object chkIncludeReadWrite: TCheckBox
      Left = 8
      Top = 24
      Width = 158
      Height = 17
      Hint = 'Include method accessors in CodeComplete list.'
      Caption = 'Include Read/Write methods'
      TabOrder = 2
    end
  end
  object CodeProposal: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion]
    NbLinesInWindow = 16
    Width = 262
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <
      item
        BiggestWord = 'ww'
      end
      item
        BiggestWord = 'constructor'
      end>
    ItemHeight = 16
    Images = ilSymbolImages
    OnExecute = ProposalExecute
    ShortCut = 16416
    Editor = SynEdit1
    Left = 304
    Top = 64
  end
  object SynPasSyn1: TSynPasSyn
    CommentAttri.Foreground = clGreen
    KeyAttri.Foreground = clPurple
    StringAttri.Foreground = clRed
    Left = 72
    Top = 64
  end
  object DelphiWebScriptII1: TDelphiWebScriptII
    Config.CompilerOptions = []
    Config.MaxDataSize = 0
    Config.Timeout = 0
    Left = 96
    Top = 192
  end
  object dws2Unit1: Tdws2Unit
    Script = DelphiWebScriptII1
    Arrays = <
      item
        Name = 'MyArray'
        DataType = 'Boolean'
        LowBound = 0
        HighBound = 2
      end>
    Classes = <
      item
        Name = 'TMyClass'
        Constructors = <>
        Fields = <
          item
            Name = 'FSnuffy'
            DataType = 'Boolean'
          end>
        Methods = <
          item
            Name = 'GetMyValue'
            Parameters = <>
            ResultType = 'Integer'
            Kind = mkFunction
          end
          item
            Name = 'SetSomeThing'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Integer'
                IsWritable = False
              end>
            Kind = mkProcedure
          end>
        Properties = <
          item
            Name = 'Value'
            DataType = 'Integer'
            ReadAccess = 'GetMyValue'
            Parameters = <>
            IsDefault = False
          end>
      end
      item
        Name = 'TMyClass2'
        Ancestor = 'TMyClass'
        Constructors = <>
        Fields = <>
        Methods = <
          item
            Name = 'GetThatSpecialNumber'
            Parameters = <>
            ResultType = 'Integer'
            Kind = mkFunction
          end>
        Properties = <>
      end
      item
        Name = 'TMyGetSetClass'
        Constructors = <>
        Fields = <>
        Methods = <
          item
            Name = 'SetCreateDate'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'DateTime'
                IsWritable = False
              end>
            Kind = mkProcedure
          end
          item
            Name = 'GetCreateDate'
            Parameters = <>
            ResultType = 'DateTime'
            Kind = mkFunction
          end
          item
            Name = 'SetAge'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Float'
                IsWritable = False
              end>
            Kind = mkProcedure
          end
          item
            Name = 'GetAge'
            Parameters = <>
            ResultType = 'Float'
            Kind = mkFunction
          end
          item
            Name = 'SetCaption'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
                IsWritable = False
              end>
            Kind = mkProcedure
          end
          item
            Name = 'GetCaption'
            Parameters = <>
            ResultType = 'String'
            Kind = mkFunction
          end
          item
            Name = 'SetLeft'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Integer'
                IsWritable = False
              end>
            Kind = mkProcedure
          end
          item
            Name = 'GetLeft'
            Parameters = <>
            ResultType = 'Integer'
            Kind = mkFunction
          end
          item
            Name = 'SetTop'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Integer'
                IsWritable = False
              end>
            Kind = mkProcedure
          end
          item
            Name = 'GetTop'
            Parameters = <>
            ResultType = 'Integer'
            Kind = mkFunction
          end
          item
            Name = 'SetTag'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'Integer'
                IsWritable = False
              end>
            Kind = mkProcedure
          end
          item
            Name = 'GetTag'
            Parameters = <>
            ResultType = 'Integer'
            Kind = mkFunction
          end
          item
            Name = 'SetName'
            Parameters = <
              item
                Name = 'Value'
                DataType = 'String'
                IsWritable = False
              end>
            Kind = mkProcedure
          end
          item
            Name = 'GetName'
            Parameters = <>
            ResultType = 'String'
            Kind = mkFunction
          end>
        Properties = <
          item
            Name = 'Name'
            DataType = 'String'
            ReadAccess = 'GetName'
            WriteAccess = 'SetName'
            Parameters = <>
            IsDefault = False
          end
          item
            Name = 'Tag'
            DataType = 'Integer'
            ReadAccess = 'GetTag'
            WriteAccess = 'SetTag'
            Parameters = <>
            IsDefault = False
          end
          item
            Name = 'Top'
            DataType = 'Integer'
            ReadAccess = 'GetTop'
            WriteAccess = 'SetTop'
            Parameters = <>
            IsDefault = False
          end
          item
            Name = 'Left'
            DataType = 'Integer'
            ReadAccess = 'GetLeft'
            WriteAccess = 'SetLeft'
            Parameters = <>
            IsDefault = False
          end
          item
            Name = 'Caption'
            DataType = 'String'
            ReadAccess = 'GetCaption'
            WriteAccess = 'SetCaption'
            Parameters = <>
            IsDefault = False
          end
          item
            Name = 'Age'
            DataType = 'Float'
            ReadAccess = 'GetAge'
            WriteAccess = 'SetAge'
            Parameters = <>
            IsDefault = False
          end
          item
            Name = 'CreateDate'
            DataType = 'DateTime'
            ReadAccess = 'GetCreateDate'
            WriteAccess = 'SetCreateDate'
            Parameters = <>
            IsDefault = False
          end>
      end>
    Constants = <
      item
        Name = 'Lovely'
        DataType = 'String'
        Value = 'Lady'
      end>
    Enumerations = <>
    Forwards = <
      item
        Name = 'TMyClass'
      end>
    Functions = <
      item
        Name = 'Money'
        Parameters = <>
        ResultType = 'Float'
      end>
    Instances = <>
    Records = <
      item
        Name = 'TMyRec'
        Members = <
          item
            Name = 'Name'
            DataType = 'String'
          end
          item
            Name = 'Value'
            DataType = 'Variant'
          end>
      end>
    Synonyms = <>
    UnitName = 'MyTestUnit'
    Variables = <
      item
        Name = 'MyBDay'
        DataType = 'DateTime'
      end>
    Left = 176
    Top = 192
  end
  object dws2FileFunctions1: Tdws2FileFunctions
    Left = 96
    Top = 248
  end
  object dws2GUIFunctions1: Tdws2GUIFunctions
    Left = 96
    Top = 312
  end
  object ParamProposal: TSynCompletionProposal
    DefaultType = ctParams
    Options = [scoLimitToMatchedText, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion]
    ClBackground = clInfoBk
    Width = 262
    EndOfTokenChr = '()[]. '
    TriggerChars = '('
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <>
    ItemHeight = 16
    Images = ilSymbolImages
    OnExecute = ProposalExecute
    ShortCut = 24608
    Editor = SynEdit1
    TimerInterval = 500
    Left = 384
    Top = 64
  end
  object ScriptHint: TSynCompletionProposal
    DefaultType = ctHint
    Options = [scoAnsiStrings, scoLimitToMatchedText, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoConsiderWordBreakChars]
    Width = 262
    EndOfTokenChr = '()[]. '
    TriggerChars = ' '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = [fsBold]
    Columns = <
      item
        BiggestWord = 'CONSTRUCTOR'
      end>
    ItemHeight = 16
    Images = ilSymbolImages
    OnExecute = ProposalExecute
    ShortCut = 0
    Editor = SynEdit1
    TimerInterval = 500
    Left = 464
    Top = 64
  end
  object alActions: TActionList
    Left = 416
    Top = 144
    object actCompleteClass: TAction
      Caption = 'Complete class at cursor'
      OnExecute = actCompleteClassExecute
    end
    object actCompleteAllClasses: TAction
      Caption = 'Complete all classes'
      ShortCut = 24643
      OnExecute = actCompleteAllClassesExecute
    end
  end
  object pMenu: TPopupMenu
    Left = 480
    Top = 144
    object Completeallclasses1: TMenuItem
      Action = actCompleteAllClasses
    end
    object actCompleteClass1: TMenuItem
      Action = actCompleteClass
    end
  end
  object ilSymbolImages: TImageList
    Left = 304
    Top = 16
    Bitmap = {
      494C010108000900040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000003000000001002000000000000030
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF00BDBDBD0000FFFF00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF007B7B
      7B0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD0000FFFF00BDBDBD0000FF
      FF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD007B7B
      7B0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF00BDBDBD0000FFFF00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF007B7B
      7B0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD0000FFFF00BDBDBD0000FF
      FF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD007B7B
      7B0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF00BDBDBD0000FFFF00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF007B7B
      7B0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDBDBD0000FFFF00BDBDBD0000FF
      FF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD007B7B
      7B0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF00BDBDBD0000FFFF00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF007B7B
      7B0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF007B7B
      7B0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF00BDBDBD0000FFFF00BDBD
      BD0000FFFF00BDBDBD007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B007B7B
      7B007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BDBDBD00BDBDBD007B7B7B007B7B7B00000000000000
      00007B7B7B007B7B7B007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B007B7B
      7B00000000000000000000000000000000000000000000000000000000000000
      0000BDBDBD00BDBDBD00BDBDBD00BDBDBD007B7B7B007B7B7B007B7B7B007B7B
      7B0000000000000000007B7B7B007B7B7B000000000000000000000000000000
      0000000000007B7B7B007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7B7B007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B000000000000000000000000000000000000000000BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B00000000007B7B7B00000000000000000000000000000000000000
      00000000000000000000000000007B7B7B007B7B7B00000000007B7B7B007B7B
      7B007B7B7B007B7B7B0000000000000000000000000000000000000000000000
      00000000000000000000000000007B7B7B007B7B7B00000000007B7B7B007B7B
      7B007B7B7B007B7B7B0000000000000000000000000000000000000000000000
      00007B7B7B000000000000000000000000007B7B7B00007B7B00007B7B000000
      00007B7B7B000000000000000000000000007B7B7B00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD00BDBDBD00FFFFFF00FFFFFF007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF00007B7B00000000007B7B7B0000FFFF0000FFFF0000FF
      FF0000FFFF007B7B7B0000000000000000000000000000000000000000000000
      000000FF000000FF0000007B0000000000007B7B7B0000FF000000FF000000FF
      000000FF00007B7B7B00000000000000000000000000000000000000FF000000
      00007B7B7B007B7B7B007B7B7B00007B7B00007B7B00007B7B00007B7B00007B
      7B00000000000000000000000000000000007B7B7B00BDBDBD00BDBDBD00BDBD
      BD00FFFFFF00FFFFFF007B7B7B007B7B7B00FFFFFF00FFFFFF007B7B7B007B7B
      7B007B7B7B0000000000000000000000000000000000000000000000000000FF
      FF0000FFFF0000FFFF00007B7B00000000007B7B7B00007B7B0000FFFF0000FF
      FF0000FFFF007B7B7B00000000000000000000000000000000000000000000FF
      000000FF000000FF0000007B0000000000007B7B7B00007B000000FF000000FF
      000000FF00007B7B7B00000000000000000000000000000000000000FF000000
      FF00000000007B7B7B007B7B7B00007B7B0000FFFF00007B7B00007B7B00007B
      7B00007B7B000000000000000000000000007B7B7B00BDBDBD00FFFFFF00FFFF
      FF007B7B7B007B7B7B00000000007B7B7B007B7B7B0000000000FFFFFF00FFFF
      FF007B7B7B00000000000000000000000000000000007B7B7B0000FFFF0000FF
      FF0000FFFF007B7B7B00007B7B0000000000000000000000000000FFFF0000FF
      FF0000FFFF007B7B7B000000000000000000000000007B7B7B0000FF000000FF
      000000FF00007B7B7B00007B000000000000000000000000000000FF000000FF
      000000FF00007B7B7B00000000000000000000000000000000000000FF000000
      FF000000FF000000000000000000007B7B0000FFFF0000FFFF00007B7B00007B
      7B00007B7B000000000000000000000000007B7B7B00FFFFFF007B7B7B007B7B
      7B000000FF000000FF00007B7B00007B7B00007B7B00007B7B00000000000000
      0000FFFFFF0000000000000000000000000000000000000000007B7B7B0000FF
      FF007B7B7B007B7B7B00007B7B00007B7B00007B7B0000FFFF0000FFFF0000FF
      FF0000FFFF007B7B7B00000000000000000000000000000000007B7B7B0000FF
      00007B7B7B007B7B7B00007B0000007B0000007B000000FF000000FF000000FF
      000000FF00007B7B7B00000000000000000000000000000000000000FF000000
      FF0000007B0000007B0000007B000000000000000000007B7B00007B7B000000
      000000000000000000000000000000000000FFFFFF007B7B7B007B7B7B000000
      FF000000FF000000FF00007B7B0000FFFF00007B7B00007B7B00007B7B000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B00000000007B7B7B00007B7B00007B7B0000FFFF0000FFFF0000FFFF000000
      000000FFFF000000000000000000000000000000000000000000000000007B7B
      7B00000000007B7B7B00007B0000007B000000FF000000FF000000FF00000000
      000000FF000000000000000000000000000000000000000000000000FF000000
      FF0000007B0000000000000000007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B00000000000000000000000000000000000000000000000000FFFFFF000000
      FF000000FF0000007B00007B7B007B7B7B007B7B7B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7B7B00007B7B0000FFFF0000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7B7B00007B000000FF000000FF000000FF0000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      7B0000000000000000007B7B7B00FF0000007B7B0000FF000000000000007B7B
      7B00000000000000000000000000000000000000000000000000000000000000
      FF0000007B0000007B000000000000000000000000007B7B7B007B7B7B007B7B
      7B007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000007B7B7B0000FFFF0000FFFF0000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7B7B0000FF000000FF000000FF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007B7B00007B7B0000FF000000000000007B7B
      7B00000000000000000000000000000000000000000000000000000000000000
      7B0000007B00000000007B7B7B00BDBDBD00BDBDBD007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B0000000000000000000000000000000000000000000000
      000000000000000000007B7B7B0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000007B7B7B0000FF00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FF000000FF000000000000007B7B
      7B00000000000000000000000000000000000000000000000000000000007B7B
      7B00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000007B7B00007B7B0000FF0000007B0000000000
      000000000000000000000000000000000000000000007B7B7B00FFFFFF00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD007B7B7B007B7B7B00BDBDBD00BDBDBD00BDBD
      BD00BDBDBD000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007B7B7B00FFFF
      FF00BDBDBD007B7B7B007B7B7B00BDBDBD00BDBDBD00BDBDBD00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B00BDBDBD00BDBDBD00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B007B7B7B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B0000000000000000007B7B7B0000000000000000007B7B7B00000000000000
      00007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000007B7B7B007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B0000000000000000007B7B7B0000000000000000007B7B7B007B7B7B007B7B
      7B007B7B7B000000000000000000000000000000000000000000000000000000
      00007B7B7B00000000000000000000000000000000007B7B7B00007B7B00007B
      7B00007B7B00000000007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000007B7B7B0000000000000000000000
      000000000000000000000000000000000000000000007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B0000000000000000007B7B7B007B7B7B000000
      00007B7B7B007B7B7B00000000007B7B7B007B7B7B0000000000000000007B7B
      7B007B7B7B007B7B7B007B7B7B00000000000000000000000000000000007B7B
      7B007B7B7B007B7B7B00000000000000000000000000007B7B00007B7B00007B
      7B00007B7B00007B7B00000000007B7B7B000000000000000000000000000000
      0000000000007B7B7B007B7B7B007B7B7B00BDBDBD007B7B7B00000000000000
      000000000000000000000000000000000000000000007B7B7B00FFFFFF0000FF
      FF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FF
      FF00BDBDBD0000FFFF007B7B7B00000000000000000000000000000000007B7B
      7B007B7B7B00000000007B7B7B00000000007B7B7B00007B7B00007B7B000000
      00007B7B7B00000000000000000000000000000000000000FF00000000000000
      00007B7B7B007B7B7B007B7B7B007B7B7B00007B7B00007B7B00007B7B00007B
      7B00007B7B00007B7B00000000007B7B7B000000000000000000000000007B7B
      7B007B7B7B00FFFFFF00BDBDBD00FFFFFF00BDBDBD007B7B7B007B7B7B000000
      000000000000000000000000000000000000000000007B7B7B00FFFFFF00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBD
      BD0000FFFF00BDBDBD007B7B7B000000000000000000000000000000FF000000
      00007B7B7B007B7B7B007B7B7B00007B7B00007B7B00007B7B00007B7B00007B
      7B0000000000000000000000000000000000000000000000FF000000FF000000
      FF00000000007B7B7B007B7B7B007B7B7B00007B7B0000FFFF00007B7B00007B
      7B00007B7B00007B7B00007B7B000000000000000000000000007B7B7B00BDBD
      BD00FFFFFF00BDBDBD00FFFFFF00BDBDBD00BDBDBD007B7B7B007B7B7B007B7B
      7B0000000000000000000000000000000000000000007B7B7B00FFFFFF0000FF
      FF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FF
      FF00BDBDBD0000FFFF007B7B7B0000000000000000007B7B7B000000FF000000
      FF00000000007B7B7B007B7B7B00007B7B0000FFFF00007B7B00007B7B00007B
      7B00007B7B007B7B7B007B7B7B0000000000000000000000FF000000FF000000
      FF000000FF0000000000000000007B7B7B00007B7B0000FFFF0000FFFF00007B
      7B00007B7B00007B7B00007B7B000000000000000000000000007B7B7B00FFFF
      FF00BDBDBD00FFFFFF00BDBDBD00FFFFFF00BDBDBD007B7B7B007B7B7B007B7B
      7B007B7B7B00000000000000000000000000000000007B7B7B00FFFFFF00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBD
      BD0000FFFF00BDBDBD007B7B7B000000000000000000000000000000FF000000
      FF000000FF000000000000000000007B7B0000FFFF0000FFFF00007B7B00007B
      7B00007B7B00000000000000000000000000000000000000FF000000FF000000
      FF0000007B0000007B0000007B0000000000000000007B7B7B0000FFFF0000FF
      FF00007B7B00007B7B00000000000000000000000000000000007B7B7B00BDBD
      BD00FFFFFF00FFFFFF00BDBDBD00BDBDBD00BDBDBD007B7B7B007B7B7B007B7B
      7B007B7B7B00000000000000000000000000000000007B7B7B00FFFFFF0000FF
      FF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FF
      FF00BDBDBD0000FFFF007B7B7B000000000000000000000000000000FF000000
      FF0000007B0000007B0000007B000000000000000000007B7B00007B7B000000
      00007B7B7B00000000000000000000000000000000000000FF000000FF000000
      7B0000007B0000007B0000007B00000000000000000000000000007B7B00007B
      7B00007B7B0000000000000000000000000000000000000000007B7B7B00FFFF
      FF00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD007B7B7B007B7B
      7B007B7B7B00000000000000000000000000000000007B7B7B00FFFFFF00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBD
      BD0000FFFF00BDBDBD007B7B7B0000000000000000007B7B7B000000FF000000
      FF0000007B007B7B7B00000000007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B00000000007B7B7B007B7B7B0000000000000000000000FF000000FF000000
      7B0000007B0000000000000000007B7B7B007B7B7B007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B00000000000000000000000000000000007B7B7B00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD007B7B
      7B007B7B7B00000000000000000000000000000000007B7B7B00FFFFFF0000FF
      FF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD0000FF
      FF00BDBDBD0000FFFF007B7B7B000000000000000000000000000000FF000000
      7B0000000000000000007B7B7B00FF0000007B7B0000FF000000000000007B7B
      7B007B7B7B00000000000000000000000000000000000000FF0000007B000000
      000000000000000000007B7B7B00FFFFFF00FF0000007B7B0000FF000000FF00
      0000000000007B7B7B00000000000000000000000000000000007B7B7B00BDBD
      BD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD007B7B
      7B007B7B7B00000000000000000000000000000000007B7B7B00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF007B7B7B00000000000000000000000000000000007B7B
      7B0000000000000000007B7B7B007B7B00007B7B0000FF000000000000007B7B
      7B007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFFFF00FF000000FF000000FF00
      0000000000007B7B7B0000000000000000000000000000000000000000007B7B
      7B00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBD
      BD007B7B7B007B7B7B000000000000000000000000007B7B7B00BDBDBD0000FF
      FF00BDBDBD0000FFFF00BDBDBD0000FFFF00BDBDBD007B7B7B007B7B7B007B7B
      7B007B7B7B007B7B7B007B7B7B0000000000000000007B7B7B007B7B7B000000
      00007B7B7B007B7B7B00000000007B7B7B00FF000000FF000000000000007B7B
      7B00000000007B7B7B007B7B7B00000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B00007B7B0000FF000000FF00
      0000000000007B7B7B0000000000000000000000000000000000000000000000
      00007B7B7B00BDBDBD00BDBDBD00BDBDBD00BDBDBD00BDBDBD007B7B7B007B7B
      7B000000000000000000000000000000000000000000000000007B7B7B00BDBD
      BD0000FFFF00BDBDBD0000FFFF00BDBDBD007B7B7B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B0000000000000000007B7B7B007B7B00007B7B0000FF0000007B0000000000
      00007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF000000000000FF000000FF000000FF00
      0000000000007B7B7B0000000000000000000000000000000000000000000000
      0000000000007B7B7B00BDBDBD00BDBDBD007B7B7B007B7B7B00000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B007B7B7B007B7B7B007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007B7B
      7B0000000000000000007B7B7B0000000000000000007B7B7B00000000000000
      00007B7B7B000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000007B7B00007B7B0000FF000000FF00
      00007B0000000000000000000000000000000000000000000000000000000000
      000000000000000000007B7B7B007B7B7B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000300000000100010000000000800100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFC07FFFFFFFFFFFFF001
      FFFFFFFFFFCFC000F8FFF8FFFF870001F043F043F7070003E003E003C0070003
      C003C003C007000380038003C0070003C003C003C19F0007E817E817C60FC003
      F83FF83FCC0FE001F87FF87FFE0FE001FCFFFCFFFE0FC003FFFFFFFFFE1F8007
      FFFFFFFFFFFFC03FFFFFFFFFFFFFE1FFFFFFFFFFFFE3FFFFFFFFEDB7FFC1FFFF
      C000ED87F780FF3F80008001E380F81F8000E5078000E00F8000C0078001C007
      800080018001C0038000C0078083C0038000C19781C7C003800080018603C003
      8000CC079C03C0038000EC07FE03E00380018001FF03F00FC07FEC17FE83F83F
      E0FFEDB7FF07FCFFFFFFFFFFFE07FFFF00000000000000000000000000000000
      000000000000}
  end
end
