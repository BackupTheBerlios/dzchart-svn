object frmEditor: TfrmEditor
  Left = 143
  Top = 0
  Width = 856
  Height = 762
  Caption = 'Script Editor'
  Color = clBtnFace
  UseDockManager = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00CCCC
    CCCCCC000013337CCCCCCCCCCCC0CCCCCCCCCC00003B737CCCCCCCCCCCC0CCCC
    CCCCCC873B8387CCCCCCCCCCCCC0CCC77CCCCCCC30B388CCCCCCCCCCCCC0CC78
    7777CCCC00B38CCCCCCCCCCCCCC0CC7FFF8777CC30B38CCCCCCCCCCCCCC0CC7F
    FFFF887700B38CCCCCCCCCCCCCC0CC7F88FFFFF800B38CCCCCCCCCCCCCC0CC7F
    87F878F810B3837CCCCCCCCCCCC0CC7F87F077F800B3888777CCCCCCCCC0CC7F
    77F0F78F00B388FF87777CCCCCC0CC7F88F7878F00B388FFFFF8777CCCC0CC7F
    8FFF708800B388F88FFFF877CCC0CC78078FFFF800B388F77F88FF87CCC0CC77
    887F88F800B388F77F007F87CCC0CC77880F38F800B388F37F0F7887CCC0CC78
    707F78F800B388F78F087887CCC0CC7FF8FF078710B888FFFF800F87CCC0CC7F
    88FF801137B873707FF88F87CCC0CC7F87FF88001333991878F8FF87CCC0CC7F
    87F307F87017783887F08F87CCC0CC7F77F0878F788FFF8307F08F87CCC0CC7F
    87F3878F0FF78FF878F08F87CCC0CCC777F8008F0F8078FFFFF78F87CCC0CCCC
    C77788F80F77F3F78FFFFF87CCC0CCCCCCCC77788F8780F77F008F87CCC0CCCC
    CCCCCC7778F808F78F083F87CCC0CCCCCCCCCCCCC7778FF78F0F7887CCC0CCCC
    CCCCCCCCCCCC77788F770887CCC0CCCCCCCCCCCCCCCCCCC777F88F87CCC0CCCC
    CCCCCCCCCCCCCCCCCC777F87CCC0CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC00000
    0001000000010000000100000001000000010000000100000001000000010000
    0001000000010000000100000001000000010000000100000001000000010000
    0001000000010000000100000001000000010000000100000001000000010000
    000100000001000000010000000100000001000000010000000100000001}
  Menu = mMain
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object SplitterMessages: TSplitter
    Left = 0
    Top = 567
    Width = 848
    Height = 3
    Cursor = crVSplit
    Align = alBottom
  end
  object SplitterToolWindows: TSplitter
    Left = 177
    Top = 52
    Width = 3
    Height = 515
    Cursor = crHSplit
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 697
    Width = 848
    Height = 19
    Panels = <
      item
        Width = 80
      end
      item
        Width = 80
      end
      item
        Width = 80
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object PnlMessages: TPanel
    Left = 0
    Top = 570
    Width = 848
    Height = 127
    Align = alBottom
    Caption = 'PnlMessages'
    TabOrder = 1
    object pcMessages: TPageControl
      Left = 13
      Top = 1
      Width = 834
      Height = 125
      ActivePage = tsInputOutput
      Align = alClient
      TabOrder = 0
      TabPosition = tpBottom
      object tsCompiler: TTabSheet
        Caption = 'Compiler/Runtime Messages'
        object pnlCompilerMessages: TPanel
          Left = 0
          Top = 0
          Width = 826
          Height = 97
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object ListBoxCompilerMessages: TListBox
            Left = 0
            Top = 0
            Width = 826
            Height = 97
            Align = alClient
            ItemHeight = 13
            TabOrder = 0
            OnClick = ListBoxCompilerMessagesClick
          end
        end
      end
      object tsInputOutput: TTabSheet
        Caption = 'Input/Output Console'
        ImageIndex = 1
        object PnInputOutput: TPanel
          Left = 0
          Top = 0
          Width = 826
          Height = 97
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Label1: TLabel
            Left = 0
            Top = 4
            Width = 75
            Height = 13
            Caption = 'Immediate Eval:'
            Layout = tlCenter
          end
          object MemoInputOutput: TMemo
            Left = 0
            Top = 22
            Width = 826
            Height = 75
            Align = alBottom
            Anchors = [akLeft, akTop, akRight, akBottom]
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
            OnChange = MemoInputOutputChange
          end
          object CBImmediate: TComboBox
            Left = 80
            Top = 0
            Width = 748
            Height = 21
            Hint = 
              'Write here the command to launch.'#13#10'Use '#39'?'#39'  prefix to evaluate a' +
              'n expression'
            Style = csSimple
            Anchors = [akLeft, akTop, akRight]
            Color = 16776176
            Ctl3D = False
            ItemHeight = 13
            ParentCtl3D = False
            TabOrder = 1
            OnKeyPress = CBImmediateKeyPress
          end
        end
      end
    end
    object PnlCaptionMessages: TPanel
      Left = 1
      Top = 1
      Width = 12
      Height = 125
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object SBCloseMessages: TSpeedButton
        Left = 0
        Top = 0
        Width = 11
        Height = 11
        Caption = 'r'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -5
        Font.Name = 'Marlett'
        Font.Style = []
        Layout = blGlyphBottom
        ParentFont = False
        OnClick = SBCloseMessagesClick
      end
      object Bevel1: TBevel
        Left = 2
        Top = 13
        Width = 3
        Height = 111
        Anchors = [akLeft, akTop, akBottom]
        Style = bsRaised
      end
      object Bevel2: TBevel
        Left = 5
        Top = 13
        Width = 3
        Height = 111
        Anchors = [akLeft, akTop, akBottom]
        Style = bsRaised
      end
    end
  end
  object pCodeExplorer: TPanel
    Left = 0
    Top = 52
    Width = 177
    Height = 515
    Align = alLeft
    BevelOuter = bvNone
    Constraints.MinWidth = 2
    TabOrder = 2
    object CodeExplorerTree: TTreeView
      Left = 0
      Top = 12
      Width = 177
      Height = 503
      Align = alClient
      Images = ilSymbolImages
      Indent = 19
      ReadOnly = True
      SortType = stText
      TabOrder = 0
      OnDblClick = CodeExplorerTreeDblClick
      OnKeyPress = CodeExplorerTreeKeyPress
    end
    object PCaptionCodeTree: TPanel
      Left = 0
      Top = 0
      Width = 177
      Height = 12
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object SBCloseCodeTree: TSpeedButton
        Left = 166
        Top = 0
        Width = 11
        Height = 11
        Hint = 'CodeTree Window'
        Anchors = [akTop, akRight]
        Caption = 'r'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -5
        Font.Name = 'Marlett'
        Font.Style = []
        Layout = blGlyphBottom
        ParentFont = False
        OnClick = ActViewCodeTreeExecute
      end
      object Bevel3: TBevel
        Left = 0
        Top = 2
        Width = 166
        Height = 3
        Anchors = [akLeft, akTop, akRight]
        Style = bsRaised
      end
      object Bevel4: TBevel
        Left = 0
        Top = 5
        Width = 166
        Height = 3
        Anchors = [akLeft, akTop, akRight]
        Style = bsRaised
      end
    end
  end
  object SynEdit: TSynEdit
    Left = 180
    Top = 52
    Width = 668
    Height = 515
    Align = alClient
    Ctl3D = False
    ParentCtl3D = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentColor = False
    ParentFont = False
    PopupMenu = pmEditorPopupMenu
    TabOrder = 3
    OnMouseMove = SynEditMouseMove
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Terminal'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Gutter.Width = 16
    Options = [eoAutoIndent, eoDragDropEditing, eoScrollPastEol, eoShowScrollHint, eoTabsToSpaces, eoTrimTrailingSpaces]
    OverwriteCaret = ctVerticalLine
    SearchEngine = SynEditSearch
    TabWidth = 2
    WantTabs = True
    OnChange = SynEditChange
    OnGutterClick = SynEditGutterClick
    OnReplaceText = SynEditReplaceText
    OnSpecialLineColors = SynEditSpecialLineColors
    OnStatusChange = SynEditStatusChange
    RemovedKeystrokes = <
      item
        Command = ecContextHelp
        ShortCut = 112
      end>
    AddedKeystrokes = <
      item
        Command = ecContextHelp
        ShortCut = 16496
      end>
  end
  object ControlBar1: TControlBar
    Left = 0
    Top = 0
    Width = 848
    Height = 52
    Align = alTop
    AutoSize = True
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 4
    object ToolBar6: TToolBar
      Left = 11
      Top = 28
      Width = 137
      Height = 22
      AutoSize = True
      Caption = 'ToolBar2'
      EdgeBorders = []
      Flat = True
      Images = ButtonsImages
      TabOrder = 0
      object ToolButton7: TToolButton
        Left = 0
        Top = 0
        Action = ActFileOpen
      end
      object ToolButton9: TToolButton
        Left = 23
        Top = 0
        Width = 8
        Caption = 'ToolButton27'
        ImageIndex = 4
        Style = tbsSeparator
      end
      object ToolButton10: TToolButton
        Left = 31
        Top = 0
        Action = ActFileSave
      end
      object ToolButton22: TToolButton
        Left = 54
        Top = 0
        Action = ActFileSaveAs
      end
      object ToolButton23: TToolButton
        Left = 77
        Top = 0
        Width = 8
        Caption = 'ToolButton5'
        ImageIndex = 18
        Style = tbsSeparator
      end
      object ToolButton25: TToolButton
        Left = 85
        Top = 0
        Action = ActViewCodeTree
      end
      object ToolButton26: TToolButton
        Left = 108
        Top = 0
        Action = ActViewMessages
      end
    end
    object ToolBar7: TToolBar
      Left = 11
      Top = 2
      Width = 227
      Height = 22
      AutoSize = True
      Caption = 'ToolBar3'
      EdgeBorders = []
      Flat = True
      Images = ButtonsImages
      TabOrder = 1
      object ToolButton37: TToolButton
        Left = 0
        Top = 0
        Action = ActEditCut
      end
      object ToolButton36: TToolButton
        Left = 23
        Top = 0
        Action = ActEditCopy
      end
      object ToolButton38: TToolButton
        Left = 46
        Top = 0
        Action = ActEditPaste
      end
      object ToolButton39: TToolButton
        Left = 69
        Top = 0
        Width = 8
        Caption = 'ToolButton18'
        ImageIndex = 30
        Style = tbsSeparator
      end
      object ToolButton5: TToolButton
        Left = 77
        Top = 0
        Action = ActEditUndo
      end
      object ToolButton8: TToolButton
        Left = 100
        Top = 0
        Action = ActEditRedo
      end
      object ToolButton11: TToolButton
        Left = 123
        Top = 0
        Width = 7
        Caption = 'ToolButton11'
        ImageIndex = 36
        Style = tbsSeparator
      end
      object ToolButton41: TToolButton
        Left = 130
        Top = 0
        Action = ActEditFind
      end
      object ToolButton42: TToolButton
        Left = 153
        Top = 0
        Action = ActEditFindNext
      end
      object ToolButton43: TToolButton
        Left = 176
        Top = 0
        Action = ActEditFindPrev
      end
      object ToolButton40: TToolButton
        Left = 199
        Top = 0
        Action = ActEditReplace
      end
    end
    object ToolBar1: TToolBar
      Left = 161
      Top = 28
      Width = 137
      Height = 22
      Caption = 'ToolBar1'
      EdgeBorders = []
      Flat = True
      Images = ButtonsImages
      TabOrder = 2
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Action = ActRunCompile
      end
      object ToolButton2: TToolButton
        Left = 23
        Top = 0
        Action = ActRunExecute
      end
      object ToolButton3: TToolButton
        Left = 46
        Top = 0
        Width = 8
        Caption = 'ToolButton3'
        ImageIndex = 44
        Style = tbsSeparator
      end
      object ToolButton4: TToolButton
        Left = 54
        Top = 0
        Action = ActRunStepInto
      end
      object ToolButton6: TToolButton
        Left = 77
        Top = 0
        Action = ActRunStepOver
      end
      object ToolButton12: TToolButton
        Left = 100
        Top = 0
        Width = 8
        Caption = 'ToolButton12'
        ImageIndex = 30
        Style = tbsSeparator
      end
      object ToolButton13: TToolButton
        Left = 108
        Top = 0
        Action = ActRunProgramReset
      end
    end
  end
  object ActionList: TActionList
    Images = ButtonsImages
    Left = 224
    Top = 64
    object ActFileNew: TAction
      Category = 'File'
      Caption = 'New'
      Hint = 'New'
      ImageIndex = 7
      OnExecute = ActFileNewExecute
      OnUpdate = ActFileNewUpdate
    end
    object ActFileOpen: TAction
      Category = 'File'
      Caption = 'Open...'
      Hint = 'Open'
      ImageIndex = 14
      ShortCut = 16463
      OnExecute = ActFileOpenExecute
      OnUpdate = ActFileOpenUpdate
    end
    object ActFileSave: TAction
      Category = 'File'
      Caption = 'Save'
      Hint = 'Save'
      ImageIndex = 16
      ShortCut = 16467
      OnExecute = ActFileSaveExecute
      OnUpdate = ActFileSaveUpdate
    end
    object ActFileSaveAs: TAction
      Category = 'File'
      Caption = 'Save As...'
      Hint = 'Save As..'
      ImageIndex = 17
      OnExecute = ActFileSaveAsExecute
      OnUpdate = ActFileSaveAsUpdate
    end
    object ActFileExit: TAction
      Category = 'File'
      Caption = 'Exit'
      Hint = 'Exit'
      ImageIndex = 0
      OnExecute = ActFileExitExecute
    end
    object ActEditCopy: TAction
      Category = 'Edit'
      Caption = 'Copy'
      Hint = 'Copy'
      ImageIndex = 24
      ShortCut = 16451
      OnExecute = ActEditCopyExecute
      OnUpdate = ActEditCopyUpdate
    end
    object ActEditCut: TAction
      Category = 'Edit'
      Caption = 'Cut'
      Hint = 'Cut'
      ImageIndex = 23
      ShortCut = 16472
      OnExecute = ActEditCutExecute
      OnUpdate = ActEditCutUpdate
    end
    object ActEditDelete: TAction
      Category = 'Edit'
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 5
      OnExecute = ActEditDeleteExecute
      OnUpdate = ActEditDeleteUpdate
    end
    object ActEditPaste: TAction
      Category = 'Edit'
      Caption = 'Paste'
      Hint = 'Paste'
      ImageIndex = 25
      ShortCut = 16470
      OnExecute = ActEditPasteExecute
      OnUpdate = ActEditPasteUpdate
    end
    object ActEditSelectAll: TAction
      Category = 'Edit'
      Caption = 'Select all'
      Hint = 'Select All'
      ImageIndex = 36
      OnExecute = ActEditSelectAllExecute
    end
    object ActEditUndo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      Hint = 'Undo'
      ImageIndex = 26
      ShortCut = 16474
      OnExecute = ActEditUndoExecute
      OnUpdate = ActEditUndoUpdate
    end
    object ActRunCompile: TAction
      Category = 'Run'
      Caption = 'Compile'
      Hint = 'Compile'
      ImageIndex = 4
      ShortCut = 16504
      OnExecute = ActRunCompileExecute
    end
    object ActRunExecute: TAction
      Category = 'Run'
      Caption = 'Execute'
      Hint = 'Execute'
      ImageIndex = 43
      ShortCut = 120
      OnExecute = ActRunExecuteExecute
      OnUpdate = ActRunExecuteUpdate
    end
    object ActEditRedo: TAction
      Category = 'Edit'
      Caption = 'Redo'
      Hint = 'Redo'
      ImageIndex = 27
      ShortCut = 24666
      OnExecute = ActEditRedoExecute
      OnUpdate = ActEditRedoUpdate
    end
    object ActEditFind: TAction
      Category = 'Edit'
      Caption = 'Find...'
      Hint = 'Find'
      ImageIndex = 10
      ShortCut = 16454
      OnExecute = ActEditFindExecute
    end
    object ActEditReplace: TAction
      Category = 'Edit'
      Caption = 'Replace...'
      Hint = 'Replace'
      ImageIndex = 35
      ShortCut = 16466
      OnExecute = ActEditReplaceExecute
      OnUpdate = ActEditReplaceUpdate
    end
    object ActEditFindNext: TAction
      Category = 'Edit'
      Caption = 'Find next'
      Hint = 'Find Next'
      ImageIndex = 30
      ShortCut = 114
      OnExecute = ActEditFindNextExecute
      OnUpdate = ActEditFindNextUpdate
    end
    object ActEditFindPrev: TAction
      Category = 'Edit'
      Caption = 'Find previous'
      Hint = 'Find Previous'
      ImageIndex = 29
      ShortCut = 8306
      OnExecute = ActEditFindPrevExecute
      OnUpdate = ActEditFindPrevUpdate
    end
    object ActRunStepInto: TAction
      Category = 'Run'
      Caption = 'Step into'
      Hint = 'Step Into'
      ImageIndex = 30
      ShortCut = 118
      OnExecute = ActRunStepIntoExecute
      OnUpdate = ActRunStepIntoUpdate
    end
    object ActRunStepOver: TAction
      Category = 'Run'
      Caption = 'Step over'
      Hint = 'Step Over'
      ImageIndex = 29
      ShortCut = 119
      OnExecute = ActRunStepOverExecute
      OnUpdate = ActRunStepOverUpdate
    end
    object ActRunRunToCursor: TAction
      Category = 'Run'
      Caption = 'Run to cursor'
      Hint = 'Run to cursor'
      ImageIndex = 2
      ShortCut = 115
      OnExecute = ActRunRunToCursorExecute
      OnUpdate = ActRunRunToCursorUpdate
    end
    object ActRunRunUntilReturn: TAction
      Category = 'Run'
      Caption = 'Run until return'
      Hint = 'Run until return'
      ImageIndex = 12
      ShortCut = 8311
      OnExecute = ActRunRunUntilReturnExecute
      OnUpdate = ActRunRunUntilReturnUpdate
    end
    object ActRunShowExecutionPoint: TAction
      Category = 'Run'
      Caption = 'Show execution point'
      OnExecute = ActRunShowExecutionPointExecute
      OnUpdate = ActRunShowExecutionPointUpdate
    end
    object ActRunProgramPause: TAction
      Category = 'Run'
      Caption = 'Program pause'
      OnExecute = ActRunProgramPauseExecute
      OnUpdate = ActRunProgramPauseUpdate
    end
    object ActRunProgramReset: TAction
      Category = 'Run'
      Caption = 'Program reset'
      Hint = 'Program reset'
      ImageIndex = 3
      ShortCut = 16497
      OnExecute = ActRunProgramResetExecute
      OnUpdate = ActRunProgramResetUpdate
    end
    object ActRunToggleBreakpoint: TAction
      Category = 'Run'
      Caption = 'Toggle breakpoint'
      ShortCut = 116
      OnExecute = ActRunToggleBreakpointExecute
    end
    object ActRunClearAllBreakpoints: TAction
      Category = 'Run'
      Caption = 'Clear all breakpoints'
      OnExecute = ActRunClearAllBreakpointsExecute
    end
    object ActRunEvaluate: TAction
      Category = 'Run'
      Caption = 'Evaluate...'
      ShortCut = 16502
      OnExecute = ActRunEvaluateExecute
      OnUpdate = ActRunEvaluateUpdate
    end
    object ActRunCallStack: TAction
      Category = 'Run'
      Caption = 'Call stack...'
      ShortCut = 49235
      OnExecute = ActRunCallStackExecute
      OnUpdate = ActRunCallStackUpdate
    end
    object ActViewMessages: TAction
      Category = 'View'
      Caption = 'View Messages Window'
      Hint = 'View Messages Window'
      ImageIndex = 8
      OnExecute = ActViewMessagesExecute
    end
    object AFonts: TAction
      Category = 'Edit'
      Caption = 'Editor Fonts'
      Hint = 'Editor Fonts'
      ImageIndex = 37
      OnExecute = AFontsExecute
    end
    object ActViewCodeTree: TAction
      Category = 'View'
      Caption = 'CodeTree Window'
      Hint = 'CodeTree Window'
      ImageIndex = 41
      OnExecute = ActViewCodeTreeExecute
    end
    object ActHelp: TAction
      Category = 'Help'
      Caption = 'Help'
      Hint = 'Help'
      ImageIndex = 46
      OnExecute = ActHelpExecute
    end
    object ActAbout: TAction
      Category = 'Help'
      Caption = 'About...'
      Hint = 'About'
      ImageIndex = 45
      OnExecute = ActAboutExecute
    end
    object acnClose: TAction
      Category = 'File'
      Caption = 'Close (Hide)'
      Hint = 'Hide window'
      OnExecute = acnCloseExecute
    end
    object ActCompleteClassAtCursor: TAction
      Category = 'Edit'
      Caption = 'Complete class at cursor'
      OnExecute = ActCompleteClassAtCursorExecute
    end
    object ActCompleteAllClasses: TAction
      Category = 'Edit'
      Caption = 'Complete all classes'
      ShortCut = 24643
      OnExecute = ActCompleteAllClassesExecute
    end
    object ActToggleImplDeclPos: TAction
      Category = 'Edit'
      Caption = 'ActToggleImplDeclPos'
      Hint = 'Jump from function implementation to declaration and back'
      ShortCut = 24614
      OnExecute = ActToggleImplDeclPosExecute
    end
  end
  object pmEditorPopupMenu: TPopupMenu
    Images = ButtonsImages
    Left = 552
    Top = 88
    object miCompleteClasses: TMenuItem
      Action = ActCompleteAllClasses
    end
    object miCompleteCursorClass: TMenuItem
      Action = ActCompleteClassAtCursor
    end
    object N12: TMenuItem
      Caption = '-'
    end
    object PopupMnuEditUndo: TMenuItem
      Action = ActEditUndo
    end
    object PopupMnuEditRedo: TMenuItem
      Action = ActEditRedo
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object PopupMnuEditCut: TMenuItem
      Action = ActEditCut
    end
    object PopupMnuEditCopy: TMenuItem
      Action = ActEditCopy
    end
    object PopupMnuEditPaste: TMenuItem
      Action = ActEditPaste
    end
    object PopupMnuEditDelete: TMenuItem
      Action = ActEditDelete
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object PopupMnuEditSelectAll: TMenuItem
      Action = ActEditSelectAll
    end
  end
  object ImgGutterGlyphs: TImageList
    Height = 14
    Width = 11
    Left = 216
    Top = 228
    Bitmap = {
      494C01010600090004000B000E00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      00000000000036000000280000002C0000002A0000000100200000000000E01C
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF000000FF000000FF000000
      FF0000000000000000000000000000000000000000000000000084848400C6C6
      C60084848400C6C6C60084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF0000FF
      FF000000FF000000FF000000FF0000FFFF000000FF0000000000000000000000
      00000000000084848400C6C6C60084848400C6C6C60084848400C6C6C6008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF0000FFFF0000FFFF000000FF0000FFFF0000FF
      FF000000FF0000000000000000000000000000000000C6C6C60084848400C6C6
      C60084848400C6C6C60084848400C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF000000
      FF0000FFFF0000FFFF0000FFFF000000FF000000FF0000000000000000000000
      00000000000084848400C6C6C60084848400C6C6C60084848400C6C6C6008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF0000FFFF0000FFFF000000FF0000FFFF0000FF
      FF000000FF0000000000000000000000000000000000C6C6C60084848400C6C6
      C60084848400C6C6C60084848400C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000FF0000FF
      FF000000FF000000FF000000FF0000FFFF000000FF0000000000000000000000
      00000000000084848400C6C6C60084848400C6C6C60084848400C6C6C6008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF000000FF000000FF000000FF000000
      FF0000000000000000000000000000000000000000000000000084848400C6C6
      C60084848400C6C6C60084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000000000000000000000000
      0000000000000000000000000000000000000000000084848400840000008400
      0000848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000FF0000008400000000000000000000000000000000000000000000000000
      0000848484000000FF0084000000FF000000840000000000FF00848484000000
      0000000000000000000000000000000000000000FF0000FF00000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084000000840000008400000084000000FF000000FF000000840000000000
      000000000000000000000000000084000000840000008400000084000000FF00
      0000FF000000840000000000FF00848484000000000000000000000000000000
      FF0000FF000000FF000000FF00000000FF000000FF000000FF00000000000000
      0000000000000000000000000000000000000000000084000000840000000000
      00000000000000000000000000000000000084000000FF000000FF000000FF00
      0000FF000000FF000000FF000000840000000000000000000000000000008400
      0000FF000000FF000000FF000000FF000000FF000000FF000000840000008484
      840000000000000000000000000000FF000000FF00000000FF0000FF00000000
      FF000000FF000000FF0000000000000000000000000000000000000000000000
      000084000000FF000000FF000000840000000000000000000000000000000000
      000084000000FF000000FF000000FF000000FF000000FF000000FF000000FF00
      000084000000000000000000000084000000FF000000FF000000FF000000FF00
      0000FF000000FF000000FF000000840000000000000000000000000000000000
      FF000000FF000000FF0000FF000000FF00000000FF000000FF00000000000000
      000000000000000000000000000000000000FFFF0000FF000000FF0000008400
      0000000000000000000000000000000000008400000084840000FFFF00008484
      0000FFFF0000FF000000FF000000840000000000000000000000000000008400
      000084840000FFFF000084840000FFFF0000FF000000FF000000840000008484
      84000000000000000000000000000000FF000000FF000000FF000000FF0000FF
      00000000FF000000FF0000000000000000000000000000000000000000000000
      000000000000FFFF000084000000000000000000000000000000000000000000
      00008400000084000000840000008400000084840000FFFF0000840000000000
      0000000000000000000000000000840000008400000084000000840000008484
      0000FFFF0000840000000000FF00848484000000000000000000000000000000
      FF000000FF000000FF000000FF0000FF000000FF00000000FF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000FFFF00008400000000000000000000000000000000000000000000000000
      0000848484000000FF0084000000FFFF0000840000000000FF00848484000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF0000FF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000000000000000000000000
      0000000000000000000000000000000000000000000084848400840000008400
      0000848484008484840000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FF000000FF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      280000002C0000002A0000000100010000000000500100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      FFFFFC0000000000FFFFFC0000000000E0FC1C0000000000C0780C0000000000
      8030040000000000803004000000000080300400000000008030040000000000
      8030040000000000C0780C0000000000E0FC1C0000000000FFFFFC0000000000
      FFFFFC0000000000FFFFFC0000000000FFFFFFFFFFF00000FFFEFFDFFFF00000
      FFFE7F83F0700000FFFE3F01E0300000FFF01E00C0100000F9F00E00C0100000
      F0F00600C0100000F0F00E00C0100000F9F01E00C0100000FFFE3F01E0300000
      FFFE7F83F0100000FFFEFFDFFF300000FFFFFFFFFFF00000FFFFFFFFFFF00000
      00000000000000000000000000000000000000000000}
  end
  object ButtonsImages: TImageList
    Left = 216
    Top = 272
    Bitmap = {
      494C01012F003100040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      000000000000360000002800000040000000D0000000010020000000000000D0
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF8C5A000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF8C5A00FFBDAD000000000000000000000000000000
      000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      0000000000008080800000FFFF0000FFFF0000FFFF0000808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF8C5A00FFBDAD00000000000000000000000000000000000000
      000000000000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      0000000000008080800000FFFF0000FFFF0000FFFF0000808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FF8C5A00FF8C5A0000000000000000000000
      0000FF8C5A00FFBDAD000000000000000000000000000000000000000000FFFF
      0000FFFF0000FFFF000000008000FFFFFF000000FF0000000000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000808000008080000080800000808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF8C5A00FFAD7300FFAD7300FF8C5A00FF8C5A00FF8C
      5A00FFA58400000000000000000000000000000000000000000000000000FFFF
      0000FFFF000000008000FFFFFF00FFFFFF00FFFFFF000000FF0000000000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      0000000000000080800000FFFF0000FFFF0000FFFF0000808000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF8C5A00FFAD7300FFB57B00FFBD8C00FFC69C00FFA5
      84000000000000000000000000000000000000000000FFFF0000FFFF0000FFFF
      000000008000FFFFFF00000080000000800000008000000000000000FF000000
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      0000000000000080800000FFFF0000FFFF0000FFFF0000808000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FF8C5A00FFB57B00FFBD8C00FFC69C00FFCEAD00FFA5
      84000000000000000000000000000000000000000000FFFF0000FFFF00000000
      8000FFFFFF00FFFFFF0000008000000080000000800000000000FFFFFF000000
      FF0000000000FFFF000000000000000000000000000000000000000000000000
      0000000000000080800000FFFF0000FFFF0000FFFF0000808000008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FF8C5A00FFAD7300FFB58400FFBD9400FFCEAD00FFE7DE00FFEF
      DE00FFA5840000000000000000000000000000000000FFFF000000008000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000FF000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000FFFF0000FFFF0000FFFF00008080000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FF8C5A00FFAD7300FFB58400FFBD8C00FFCEAD00FFEFDE00FFF7E700FFF7
      EF00FFA584000000000000000000000000000000000000008000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000080000000FF0000000000FFFFFF00FFFF
      FF00FFFFFF000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF0000FFFF0000FFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF8C5A00FF8C5A00FF8C
      5A00FFB57B00FFB57B00FFBD8C00FFCEAD00FFE7D600FFA58400FFA58400FFA5
      8400000000000000000000000000000000000000FF00FFFFFF00FFFFFF000000
      80000000800000008000FFFFFF00FFFFFF00FFFFFF00000080000000FF000000
      000000000000FFFFFF000000FF00000000000000000000000000FFFFFF0000FF
      FF0000FFFF0000000000000000000000000000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFA58C00FFB57B00FFBD8C00FFBD
      8C00FFA58400FFBD8C00FFCEAD00FFE7D600FFA58400FFDED600000000000000
      0000000000000000000000000000000000000000FF00FFFFFF00FF00FF000000
      FF000000800000008000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF00FF000000
      FF0000008000FFFFFF0000008000000000000000000000000000FFFFFF0000FF
      FF0000FFFF0000808000000000000000000000FFFF0000FFFF0000FFFF0000FF
      FF00008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFA58C00FFB58C00FFC69C00FFCE
      AD00FFBD8C00FFA58400FFE7D600FFA58400FFDED60000000000000000000000
      0000000000000000000000000000000000000000FF0000008000FFFFFF000000
      800000008000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000080000000
      FF0000008000FFFFFF0000008000000000000000000000000000FFFFFF0000FF
      FF0000FFFF0000FFFF00008080000000000000FFFF0000FFFF0000FFFF0000FF
      FF00008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFA58C00FFBD9400FFCEA500FFD6
      BD00FFDECE00FFE7D600FFA58400FFDED6000000000000000000000000000000
      0000000000000000000000000000000000000000000000008000FFFFFF00FFFF
      FF00FF00FF000000FF000000FF000000FF000000FF000000FF00000080000000
      8000FFFFFF00000080000000FF000000000000000000000000000080800000FF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFA58400FFCEAD00FFE7
      D600FFEFE700FFEFE700FFA58400000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000FF0000008000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00000080000000000000000000000000000000000000000000FFFF
      FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00C0C0
      C000C0C0C0000080800000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFBDAD00FFA58400FFDE
      CE00FFEFDE00FFEFE700FFA58400000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF00000000FF000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00C0C0C000C0C0
      C000008080000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFBDAD00FFA5
      8400FFA58400FFA5840000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000808000C0C0C000C0C0C000FFFFFF00FFFFFF00C0C0C0000080
      8000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000DE6B0000DE6B0000DE6B
      0000DE6B0000DE6B0000DE6B0000DE6B0000DE6B0000DE6B0000DE6B0000DE6B
      0000DE6B0000DE6B0000DE6B0000DE6B00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DE6B0000F7D6B500F7D6B500F7D6
      B500F7D6B500F7D6B500F7D6B500F7D6B500F7D6B500F7D6B500F7D6B500F7D6
      B500F7D6B500F7D6B500F7D6B500DE6B00000000000063A563000084000063A5
      630000000000CEBD8C00B59C5200B59C5200B59C5200B59C5200B59C5200B59C
      5200B59C5200B59C5200B59C5200CEBD8C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000DE6B0000F7DEC600009C0000009C
      0000009C0000F7DEC600F7DEC600F7DEC600DECEC6005A73EF00DECEC600F7D6
      BD00F7D6B500F7D6B500F7D6B500DE6B0000000000000084000031CE6B000084
      000000000000B59C5200FFFFFF00F7EFE700DED6B500D6C69C00CEBD8400C6B5
      7300BDAD6300B5A55A00B59C5200B59C52000000000000000000000000002942
      B5000021AD000021AD000021AD000021AD000021AD000021AD000021AD000021
      AD001839AD000000000000000000000000000000000000840000000000000000
      00000000000000840000000000000000000000000000007B0000000000000000
      000000000000000000000000000000000000DE6B0000F7E7CE00009C0000009C
      0000009C0000F7E7CE00F7E7CE00DEDEDE00315AEF000031EF009CA5EF00F7DE
      C600F7D6BD00F7D6B500F7D6B500DE6B00000000000063A563000084000063A5
      630000000000CEBD8C00B59C5200B59C5200B59C5200B59C5200B59C5200B59C
      5200B59C5200B59C5200B59C5200CEBD8C0000000000000000002942CE001031
      CE001839CE002142CE002142CE001842D6002142D6002142D6001039D6000031
      D6000029BD002139AD0000000000000000000084000021B53900008400000000
      00000084000021B539000084000000000000007B000018B53900007B000084B5
      840000000000000000000000000000000000DE6B0000FFEFDE00009C0000009C
      0000009C0000FFEFDE00E7E7E700315AEF001842F7001842F700738CEF00EFE7
      CE00F7DEC600F7D6BD00F7D6B500DE6B00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000001031D600294A
      DE003152DE00395AE700395AEF00395AEF00315AEF002152EF001852EF001042
      EF000031D6000021AD0000000000000000000084000021BD4200008400000000
      00000084000021BD42000084000000000000007B000021B5390018B53900007B
      000084B58400000000000000000000000000DE6B0000FFF7EF00FFF7EF00FFF7
      EF00FFF7EF00FFF7EF0052A5FF001842F700B5C6FF0052A5FF001842F700DEDE
      DE00F7DEC600F7D6BD00F7D6B500DE6B00000000000063946B0000529C00639C
      6B0000000000CEBD8C00B59C5200B59C5200B59C5200B59C5200B59C5200B59C
      5200B59C5200CEBD8C00000000000000000000000000000000001039DE003152
      DE004263E7004A63E7004263E7003963E7003163E700295AE7002152E700104A
      EF001039D6000021AD0000000000000000000084000029BD5200008400000000
      00000084000029BD52000084000000000000007B000021BD420021BD420018B5
      3900007B000084B584000000000000000000DE6B0000FFFFEF00000000000000
      000000000000FFFFF700B5C6FF00B5C6FF00FFFFEF00CEDEEF000031EF00B5C6
      FF00F7E7CE00F7DEC600F7D6B500DE6B0000000000000839CE000031FF000063
      630000000000B59C5200FFFFFF00F7EFE700DED6B500D6C69C00CEBD8400C6B5
      7300B59C5200B59C520000000000000000000000000000000000214ADE00395A
      E700526BE7004A6BE7004A6BE7004263E7003163E700295AE7002152E700104A
      EF00214AD6000021AD0000000000000000000084000031C66300008400000000
      00000084000031C663000084000000000000007B000029C6520021BD420018B5
      390018B53900007B000084B5840000000000DE6B0000FFFFFF0000000000CE6B
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00F7F7F7005A73EF00738C
      EF00F7E7D600F7DEC600F7D6B500DE6B00002142C6000039F7000042CE001052
      B50000000000CEBD8C00B59C5200B59C5200B59C5200B59C5200B59C5200B59C
      5200B59C5200CEBD8C00000000000000000000000000000000003952E7004A63
      E7005A6BE700526BE7004A63E7004263E700315AE7002952EF002152EF00184A
      EF00294AD6000021AD0000000000000000000084000031CE6B00008400000000
      00000084000031CE6B000084000000000000007B000031C6630029BD520029BD
      520029BD520021BD4200007B000084B58400DE6B0000FFFFFF00000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700B5C6FF001842
      F700CECEE700F7DEC600F7D6B500DE6B00000839FF00000000009CB5FF001042
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004A6BE7005A73
      E7005A73E700526BE7004A63E700425AEF00315AE7002952E700214AE700214A
      E700294AD6000021AD0000000000000000000084000052C66B00008400000000
      00000084000031CE6B000084000000000000007B00006BCE6B0073CE6B0063CE
      6B0063CE6B0063CE6B00007B000084B58400DE6B0000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700CEDEEF001842
      F700738CEF00F7DEC600F7D6B500DE6B00000000000063946B0000529C00004A
      84000031FF00CEBD8C00B59C5200B59C5200B59C5200B59C5200B59C5200B59C
      5200B59C5200B59C5200B59C5200CEBD8C000000000000000000526BE700637B
      E7006373E700526BE7004A63EF00425AE7003152E7002952DE00214AE700214A
      E700294AD6000021AD0000000000000000000084000052C66B00008400000000
      00000084000052C66B000084000000000000007B000073D6730084D6840084D6
      7B0063CE6B00007B000084B5840000000000DE6B0000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFF7EF00B5C6
      FF00B5C6FF00F7DEC600F7D6B500DE6B0000000000000839CE000031FF000063
      630000000000B59C5200FFFFFF00F7EFE700DED6B500D6C69C00CEBD8400C6B5
      7300BDAD6300B5A55A00B59C5200B59C520000000000000000005A73E700738C
      EF006B7BEF005A6BE7004A63EF00425AE700395AE7003152DE002952E7002952
      E700294AD6000021AD000000000000000000008400007BCE7B00008400000000
      0000008400007BCE7B000084000000000000007B000084D6840094DE940094DE
      9400007B000084B584000000000000000000DE6B0000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFEF00FFF7EF00FFEF
      DE00F7E7CE00F7DEC600F7D6B500DE6B00002142C6000039F7000042CE001052
      B50000000000CEBD8C00B59C5200B59C5200B59C5200B59C5200B59C5200B59C
      5200B59C5200B59C5200B59C5200CEBD8C000000000000000000637BE7008C9C
      F700738CEF00637BEF005A73EF00526BE7004A63E700425AE700395AE700395A
      E700294AD6001831AD00000000000000000000840000ADD6A500008400000000
      000000840000ADD6A5000084000000000000007B00007BD67B00A5DEA500007B
      000084B58400000000000000000000000000DE6B0000CE7B2100CE7B2100CE7B
      2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B
      2100CE7B2100CE7B2100CE7B2100DE6B00000839FF00000000009CB5FF001042
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000738CEF009CAD
      EF008494F700738CEF006B84EF00637BE700637BE7005A73EF00526BE7004263
      EF00294AD600314AB500000000000000000000840000ADD6A500008400000000
      000000840000ADD6A5000084000000000000007B000052BD5200007B000084B5
      840000000000000000000000000000000000CE7B2100DE6B0000DE6B0000DE6B
      0000DE6B0000DE6B0000DE6B0000DE6B0000DE6B0000DE6B0000DE6B0000DE6B
      0000DE6B0000DE6B0000DE6B0000CE7B21000000000000000000DEE7FF000031
      FF005273FF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000738C
      EF00637BE700526BE7004A6BE7004A63E700425AE700425AE700395ADE003152
      DE003152CE000000000000000000000000000000000000840000000000000000
      00000000000000840000000000000000000000000000007B0000000000000000
      000000000000000000000000000000000000F7DEBD00CE7B2100DE6B0000DE6B
      0000DE6B0000DE6B0000DE6B0000DE6B0000DE6B0000DE6B0000DE6B0000DE6B
      0000DE6B0000DE6B0000CE7B2100F7D6BD000000000000000000000000009CB5
      FF000031FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D66B0000D66B0000D66B
      0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B
      0000D66B0000D66B0000D66B00000000000000000000D6520800D6520800D652
      0800D6520800D6520800D6520800D6520800D6520800D6520800D6520800D652
      0800D6520800D6520800CE5A1800F7D6BD000000000000000000B57B7300B57B
      7300B57B7300B57B7300B57B7300B57B7300B57B7300B57B7300B57B7300B57B
      7300B57B730000000000000000000000000000000000AD737300AD737300AD73
      7300AD737300AD737300AD737300AD737300AD737300AD737300000000000000
      000000000000000000000000000000000000D66B0000F7D6B500F7D6B500F7D6
      B500F7D6B500F7D6B500F7D6B500009C2900F7D6B500F7D6B500F7D6B500F7D6
      B500F7D6AD00F7CEAD00F7CEAD00D66B0000EF632100F7631800F76B2900F76B
      2900EF6B3100F76B2900F7732900F77B2900F7842900FFA55A00FFFFFF00FFFF
      FF00FFB57300F77B0800E7630000C652180000000000C69C9400FFF7EF00FFEF
      DE00FFEFD600FFEFCE00FFE7C600FFE7C600FFDEBD00FFDEBD00FFDEB500FFDE
      BD00B57B730000000000000000000000000084840000FFF7E700FFEFD600FFEF
      CE00FFEFC600FFDEBD00FFDEBD00FFDEB500FFDEB500AD737300000000000000
      000000000000000000000000000000000000D66B0000F7DEBD00F7DEBD00F7DE
      BD00F7DEBD0052FFAD00009C2900009C2900F7DEBD00F7DEBD00F7D6BD00F7D6
      B500F7D6B500F7D6B500F7CEAD00D66B0000FF6B1800FF6B2900FF733900FF7B
      4200FF7B4200FF843900FF8C4200FF944200FF9C3900FFFFFF00FFBD6B00FFA5
      2100FFFFFF00FFFFFF00F7841800D652080000000000C69C9400FFF7E700FFEF
      D600FFE7CE00FFE7C600FFDEBD00FFDEBD00FFDEB500FFDEB500FFD6AD00FFDE
      B500B57B730000000000000000000000000084840000FFF7E700FFF7CE00F7E7
      CE00DECEC600F7DEC600FFDEB500FFDEB500FFDEB500AD737300AD737300AD73
      730000000000000000000000000000000000D66B0000F7E7CE00F7E7CE00F7E7
      CE0052FFAD00009C2900009C2900009C2900009C2900009C2900009C2900009C
      2900009C290052FFAD00F7D6AD00D66B0000FF6B2100FF733100FF7B4200FF84
      4A00FF844A00FF8C4A00FF944A00FF9C4A00FFA54A00FFFFFF00FFBD5A00FFAD
      2900FFA52100FFBD6300FFFFFF00D652080000000000C69C9400FFF7EF00FFEF
      DE00FFEFD600CECED600BDBDCE00EFDEBD00FFDEBD00FFDEB500FFDEB500FFDE
      BD00B57B7300000000000000000000000000C69C9400FFFFEF00CECEE7001842
      F7001842F700BDBDCE00FFE7BD00FFDEB500FFDEBD00AD737300FFDEB500AD73
      730000000000000000000000000000000000D66B0000FFEFDE00FFEFDE00FFEF
      DE00009C2900009C2900009C2900009C2900009C2900009C2900009C2900009C
      2900009C2900009C290052FFAD00D66B0000FF733100FF7B3900FF844A00FF8C
      5200FF8C5200FF8C5200FFB57300FFFFCE00FFFFCE00FFFFFF00FFFFFF00FFBD
      5200FFEFCE00FF9C2100FF841800D652080000000000C6AD9C00FFFFEF00E7E7
      E7009CA5EF001842F7000031EF009CA5EF00FFE7BD00FFDEBD00FFDEB500FFDE
      BD00B57B7300000000000000000000000000C6AD9C00EFEFEF001842F700315A
      EF001842F700315AEF00EFDEBD00FFE7BD00FFE7C600AD737300FFDEB500AD73
      7300AD737300AD7373000000000000000000D66B0000FFF7EF00FFF7EF00FFF7
      EF0052FFAD0000BD520000BD520000BD520000BD520000BD520000BD520000BD
      520000BD2900009C2900009C2900D66B0000FF733900FF844200FF8C4A00FF8C
      5A00FF8C5200FF8C5200FFCE9400FFFFCE00FFBD6B00FFFFCE00FFFFFF00FFFF
      FF00FFFFFF00FFAD4200F7842100D652080000000000C6AD9C00FFFFFF009CA5
      EF000031EF00315AEF001842F7001842F700E7CECE00FFE7BD00FFDEBD00FFE7
      C600B57B7300000000000000000000000000C6AD9C00FFFFFF00CEDEEF00FFF7
      DE00DEDEDE001842F700738CEF00FFEFC600FFE7CE00AD737300FFDEBD00AD73
      7300FFDEB500AD7373000000000000000000D66B0000FFF7F700FFFFF700FFFF
      F700FFFFF70052FFAD0000BD520000BD520052FFAD0000FF520000DE520000BD
      520000BD520000BD2900009C2900D66B0000FF7B3900FF8C4A00FF8C5A00FF94
      5A00FF945A00FF945200FFCE9400FFFFCE00FFBD6B00FFC66B00FFFFCE00FFD6
      9400FFFFFF00FFAD5200F7842100D652080000000000CEB5AD00FFFFFF00B5C6
      FF009CA5EF00FFF7DE00B5C6FF000031EF00738CEF00FFEFC600FFDEBD00FFE7
      CE00B57B7300000000000000000000000000C6AD9C00FFFFFF00FFFFEF00FFFF
      E700FFFFE7009CA5EF001842F700BDBDCE00FFF7CE00AD737300FFE7C600AD73
      7300FFDEB500AD7373000000000000000000D66B0000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0052FFAD0000BD5200FFFFFF00FFF7F700FFF7EF00ADFF
      D60000BD520000BD290000BD2900D66B0000FF844200FF8C5A00FF946300FF94
      6300FF945A00FF945A00FFCE8C00FFFFCE00FFFFCE00FFFFCE00FFFFCE00FF94
      3100FF942900FF942900F7843100D652080000000000DEB5A500FFFFFF00FFFF
      EF00FFFFEF00FFFFE700FFF7DE00738CEF001842F70073ADF700FFE7CE00FFEF
      CE00B57B7300000000000000000000000000DEB5A500FFFFFF00FFFFFF00FFFF
      F700FFF7EF00F7EFE7009CA5EF00CECEE700FFEFDE00AD7373008CB5F700AD73
      7300FFDEBD00AD7373000000000000000000D66B0000FFFFFF0000BD520052FF
      AD00FFFFFF00FFFFFF00FFFFFF0052FFAD00FFFFFF00FFFFF700FFF7EF00ADFF
      D60000BD520000BD290000BD2900D66B0000FF844A00FFADA500FFBDD600FFAD
      9C00FF9C6B00FF945A00FFCE8C00FFFFCE00FF8C4200FFB58400FFFFCE00FFA5
      4A00FF8C3100FF8C3100F77B3100D652080000000000DEB5A500FFFFFF00FFFF
      FF00FFFFEF00FFF7EF00FFFFE700DEDEDE001842F70052A5FF0084B5F700FFEF
      DE00B57B7300000000000000000000000000E7BDB500FFFFFF00FFFFFF00FFFF
      FF00FFFFEF00FFFFEF00FFF7DE00F7D6BD00FFB5B500AD73730052A5FF006B94
      D600FFE7C600AD7373000000000000000000D66B0000FFFFFF0052FFAD0000BD
      520000BD520052DE7B00ADFFD600ADFFD600ADFFD600ADFFD600ADFFD60052DE
      7B0000BD290000BD290052DE7B00D66B0000FFA59C00FFCEFF00FFCEFF00FFB5
      B500FFCEFF00FF9C7B00FFBD8400FFFFCE00FFFFCE00FFFFCE00FFFFCE00FF8C
      4200FF7B3100FF7B3100F77B3100D652080000000000E7BDB5007BB5FF0052A5
      FF0052A5FF00FFFFEF00FFF7EF00FFF7E70052A5FF00ADFFFF0052A5FF008CB5
      F700B57B7300000000000000000000000000EFC6B500FFFFFF00FFFFFF007BB5
      FF0052A5FF0052A5FF00E7CECE00C6947B00DE9C4A0052A5FF00ADFFFF0052A5
      FF008CB5EF006B94D6000000000000000000D66B0000FFFFFF00FFFFFF0052FF
      AD0000BD520000BD520000BD290000BD290000BD290000BD290000BD290000BD
      290000BD290052DE7B00F7D6B500D66B0000FF9C7300FFCEFF00FFBDC600FFA5
      6B00FFA58400FFCEFF00FF9C7300FFA56B00FFA56300FFDEF700FFCEF700FF7B
      3100FF733100FF7B3100F7733100D652080000000000E7BDB5007BB5FF0052A5
      FF00ADFFFF0052A5FF00C6DEF70052A5FF00ADFFFF0052A5FF00ADFFFF0052A5
      FF0052A5FF008CBDFF00FFA5AD0000000000F7D6B500FFFFFF00FFFFFF007BB5
      FF0052A5FF00ADFFFF0052A5FF00C6DEFF0052A5FF00ADFFFF0052A5FF00ADFF
      FF0052A5FF0052A5FF008CBDFF0000000000D66B0000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0052DE7B0052FFAD0052DE7B0052DE7B0052DE7B0052DE7B0052FF
      AD00F7E7CE00F7DEBD00F7D6B500D66B0000FF945A00FFB59C00FFCEFF00FFCE
      FF00FFCEFF00FFCEFF00FFCEFF00FFCEFF00FFCEFF00FFCEFF00FFB5CE00FF73
      3100FF733100FF7B3100F7733100D652080000000000EFC6B500FFFFFF0084B5
      FF0052A5FF00ADFFFF0052A5FF00ADFFFF0052A5FF00ADFFFF0052A5FF00ADFF
      FF00ADFFFF0052A5FF00FFA5AD0000000000EFC69C00F7B59400F7B59400F7B5
      94007BA5E70052A5FF00ADFFFF0052A5FF00ADFFFF0052A5FF00ADFFFF0052A5
      FF00ADFFFF00ADFFFF0052A5FF0000000000D66B0000CE7B2100CE7B2100CE7B
      2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B
      2100CE7B2100CE7B2100CE7B2100D66B0000FF946B00FFAD8C00FFAD8C00FFAD
      9400FFAD9400FFA58C00FFB5C600FFCEFF00FF9C7B00FFCEFF00FFB5BD00FF7B
      3900FF7B3900FF7B3900F7733100D652080000000000EFC6B500FFFFFF00FFFF
      FF00ADD6FF0052A5FF009CEFFF0052A5FF00ADFFFF0052A5FF00ADFFFF00ADFF
      FF00ADFFFF00ADFFFF00FFA5AD00000000000000000000000000F7D6B500FFFF
      FF00FFFFFF00ADD6FF0052A5FF009CEFFF0052A5FF00ADFFFF0052A5FF00ADFF
      FF00ADFFFF00ADFFFF00ADFFFF00FFA5AD00CE7B2100D66B0000D66B0000D66B
      0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B
      0000D66B0000D66B0000D66B0000CE7B2100FF9C7300FFBD9C00FFBD9C00FFB5
      8C00FFAD8400FFA57B00FFA57B00FFCEFF00FFC6E700FFCEFF00FFADAD00FF8C
      5200FF844A00FF7B4200F7733100D652080000000000F7D6B500FFFFFF00FFFF
      FF00FFFFFF008CBDFF0052A5FF00ADFFFF0052A5FF00ADFFFF00ADFFFF00ADFF
      FF00ADFFFF00ADFFFF00FFA5AD00FFD6CE000000000000000000EFC69C00F7B5
      9400F7B59400F7B5940084A5DE0052A5FF00ADFFFF0052A5FF00ADFFFF00ADFF
      FF00ADFFFF00ADFFFF00ADFFFF00FFA5AD00F7DEBD00CE7B2100D66B0000D66B
      0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B
      0000D66B0000D66B0000CE7B2100F7D6BD00FFA57300FFBD9C00FFBD9C00FFAD
      8C00FFAD8400FFAD8C00FFBDC600FFBDD600FFCEFF00FFCEFF00FFA58400FF8C
      5200FF8C4A00FF7B4200F76B3100D652080000000000EFC69C00EFC69C00EFC6
      9C00EFC69C00EFC69C00ADADC60052A5FF00ADFFFF00ADFFFF00ADFFFF00ADFF
      FF00ADFFFF0052A5FF00FFA5AD00FFD6CE000000000000000000000000000000
      0000F7D6B500FFFFFF00FFFFFF00C6DEFF0052A5FF00ADFFFF00ADFFFF00ADFF
      FF00ADFFFF00ADFFFF0052A5FF00FFA5AD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7D6BD00FFA57300FF9C7300FF9C
      6B00FF9C6B00FF845200FF947B00FFA59400FFADAD00FFA59400FF845200FF84
      5200FF845200FF845200EF6B3100F7DEBD000000000000000000000000000000
      00000000000000000000000000008CBDFF0052A5FF0052A5FF0052A5FF0052A5
      FF0052A5FF0000000000FFA5AD00FFA5AD000000000000000000000000000000
      0000EFC69C00F7B59400F7B59400F7B594008CA5DE0052A5FF0052A5FF0052A5
      FF0052A5FF0052A5FF0000000000FFA5AD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000C6A5AD008C849C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000D66B0000D66B0000D66B
      0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B
      0000D66B0000D66B0000D66B0000D66B000000000000D66B0000D66B0000D66B
      0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B
      0000D66B0000D66B0000D66B0000D66B00000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000C6A5AD008C849C0052BDFF0084ADCE004A7BC600847BA500CEA5
      AD00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D66B0000F7D6B500F7D6B500F7D6
      B500F7D6B500F7D6B500F7D6B500F7D6B500F7D6B500F7D6B500F7D6B500F7D6
      B500F7D6AD00F7CEAD00F7CEAD00D66B0000D66B0000F7D6B500F7D6B500F7D6
      B500F7D6B500F7D6B500F7D6B500009C2900F7D6B500F7D6B500F7D6B500F7D6
      B500F7D6AD00F7CEAD00F7CEAD00D66B0000000000000000000021ADDE0021AD
      DE0021ADDE0052BDDE007BCEE700C6E7F7000000000000000000000000000000
      0000C6A5AD008C849C0052BDFF007BC6FF009CD6FF004AB5FF004A7BC600847B
      A500CEA5AD000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D66B0000F7DEBD00F7DEBD00F7DE
      BD00F7DEBD00F7DEBD00F7DEBD00F7DEBD00F7DEBD00F7DEBD00F7D6BD0000BD
      2900F7D6B500F7D6B500F7CEAD00D66B0000D66B0000F7DEBD00F7DEBD00F7DE
      BD00F7DEBD0052FFAD00009C2900009C2900F7DEBD00F7DEBD00F7D6BD00F7D6
      B500F7D6B500F7D6B500F7CEAD00D66B0000000000000000000021ADDE008CD6
      EF006BD6F7004AC6EF0029B5DE0010A5D600009CCE0031ADD60063BDDE00A59C
      AD008C849C0052BDFF007BC6FF00000000000000000094D6FF0052B5FF004A7B
      C600847BA500CEA5AD000000000000000000BDDEBD00BDDEBD00000000000000
      000000000000000000000000000000000000D66B0000F7E7CE00F7E7CE00F7E7
      CE00F7E7CE00F7E7CE00F7E7CE00F7E7CE00F7E7CE00F7DECE00F7DEC600C6D6
      A50000BD2900E7CEAD00F7D6AD00D66B0000D66B0000F7E7CE00F7E7CE00F7E7
      CE0052FFAD00009C2900009C2900009C2900009C2900009C2900009C2900009C
      2900009C290052FFAD00F7D6AD00D66B000021A5D60052BDDE0021ADDE004AB5
      DE00BDF7FF0084E7FF0084E7FF00D6B59C00D6AD9400D6AD9400D6B59C008C84
      9C0052BDFF007BC6FF000000000000000000000000000000000094D6FF0052B5
      FF004A7BCE00847BA500DEC6CE007BB57B001073100000630000007B080063A5
      630000000000000000000000000000000000BD73000052BD7B0052BD7B0052BD
      7B0052BD7B0052BD7B0052BD7B0052BD7B0052BD7B0052BD7B0052BD7B0052BD
      7B0029B5420000BD2900B5C68C00D66B0000D66B0000FFEFDE00FFEFDE00FFEF
      DE00009C2900009C2900009C2900009C2900009C2900009C2900009C2900009C
      2900009C2900009C290052FFAD00D66B000021A5D600B5E7F70021ADDE004ABD
      EF00CEF7FF008CF7FF00DEB5A500F7E7CE00FFEFC600FFE7BD00EFD6AD00CE9C
      8400BDADA50094D6EF0000000000000000000000000000000000000000008CCE
      FF0052B5FF00528CD6004A844A00739463004A7B310039732900108C2100108C
      210052844200FFFFFF000000000000000000AD73080000BD290000BD290000BD
      290000BD290000BD290000BD290000BD290000BD290000BD290000BD290000BD
      290000BD290000BD29004AB55200D66B0000D66B0000FFF7EF00FFF7EF00FFF7
      EF0052FFAD0000BD520000BD520000BD520000BD520000BD520000BD520000BD
      520000BD2900009C2900009C2900D66B000021ADDE00A5DEEF0021ADDE0073CE
      F7008CD6EF00D6B59C00F7E7D600FFF7EF00FFE7C600FFE7BD00FFFFC600F7DE
      B500D6AD940039B5DE0000000000000000000000000000000000000000000000
      0000A5DEFF00CED6D600BD8C8400D6AD8C00EFD6AD00FFF7CE002984290031CE
      6300108418009C9C7300FFFFFF0000000000D66B0000FFF7F700FFFFF700FFFF
      F700FFFFF700FFFFF700FFFFF700FFFFF700FFF7F700FFF7EF00FFEFE700A5D6
      9C0000BD29008CC67B00F7D6B500D66B0000D66B0000FFF7F700FFFFF700FFFF
      F700FFFFF7000000AD0000BD520000BD52000000AD0000FF520000DE520000BD
      520000BD520000BD2900009C2900D66B000021ADDE0063CEFF0021ADDE007BD6
      FF0039B5DE00D6AD9400FFF7CE00FFEFC600FFFFC600FFFFDE00FFFFDE00FFFF
      DE00D6AD940031ADDE0094D6EF00000000000000000000000000000000007BB5
      7B007BB57B00DEBDB500DEAD8C00FFF7C600FFF7C600FFFFDE007BB5730031BD
      520031BD52006B8C5200CEADA50000000000D66B0000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFF7EF0000BD
      290039B55200E7D6B500F7D6B500D66B0000D66B0000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000AD000000AD00FFFFFF00FFF7F7000000AD000000
      AD000000AD000000AD0000BD2900D66B000021ADDE0063CEFF0021ADDE0094E7
      FF007BE7FF00D6AD9400FFE7BD00FFE7BD00FFFFDE00FFFFE700FFFFEF00FFFF
      E700D6AD940084D6EF0039B5DE0000000000000000000000000073AD73002984
      3100087318005A843900EFD6AD00FFE7B50000630000006300001084180042D6
      6B0042DE7300108C21000063000000630000BD73000052BD7B0052BD7B0052BD
      7B0052BD7B0052BD7B0052BD7B0052BD7B0052BD7B0052BD7B0052BD7B0052BD
      7B0000BD290008AD3100B5CE9400D66B0000D66B0000FFFFFF0000BD520052FF
      AD00FFFFFF00FFFFFF000000AD000000AD00FFFFFF00FFFFF700FFF7EF00ADFF
      D60000BD520000BD290000BD2900D66B000021ADDE0063CEFF0021ADDE00ADFF
      FF0094FFFF00D6B59C00EFD6AD00FFFFC600FFFFDE00FFFFEF00FFFFFF00F7E7
      D600D6B59C00B5E7F70021ADDE00000000000000000000000000318C4A0039C6
      630039C65A00217B29006BA55200FFDEAD0073A55200399C630029AD42005AF7
      8C0052EF8400219C31005A7B390000000000AD73080000BD290000BD290000BD
      290000BD290000BD290000BD290000BD290000BD290000BD290000BD290000BD
      290000BD290000BD29004AB55200D66B0000D66B0000FFFFFF0052FFAD0000BD
      520000BD520052DE7B000000AD000000AD00ADFFD600ADFFD6000000AD000000
      AD000000AD000000AD0052DE7B00D66B000021ADDE007BDEFF0021ADDE00C6FF
      FF009CFFFF009CFFFF00D6AD9400F7DEB500FFFFDE00FFFFE700F7E7DE00DEB5
      A50063BDDE0021ADDE0021ADDE00000000000000000073AD7300219C310052EF
      84005AF78C0029AD4200399C6300739C5200FFF7C60073AD5A00217B290039C6
      5A0039C6630029843900CE9C840000000000D66B0000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFF7EF00EFE7
      D60000BD29008CC67B00F7D6B500D66B0000D66B0000FFFFFF00FFFFFF0052FF
      AD0000BD52000000AD0000BD290000BD29000000AD0000BD290000BD290000BD
      290000BD290052DE7B00F7D6B500D66B000021ADDE0094FFFF0039B5DE009CDE
      EF00A5E7F700DEFFFF009CDEEF00C6AD9C00D6AD9400D6AD9400CEA5940021AD
      DE00ADDEEF000000000000000000000000000063000000630000108C210042DE
      730042D66B00108418000063000000630000FFE7BD00FFEFBD0073A552000873
      1800298431006B944A00CE9C840000000000D66B0000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7F700FFF7EF0000BD
      2900CED6AD00F7DEBD00F7D6B500D66B0000D66B0000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0052DE7B0052FFAD0052DE7B0052DE7B0052DE7B0052DE7B0052FF
      AD00F7E7CE00F7DEBD00F7D6B500D66B000021ADDE009CFFFF009CFFFF0021AD
      DE0021ADDE0021ADDE0021ADDE009CFFFF009CFFFF00009CCE00000000000000
      00000000000000000000000000000000000000000000000000007BB57B0031BD
      520031BD52006B945A00D6B59400FFFFFF00FFFFEF00FFF7CE00FFDEA5007BA5
      5A007BAD6300D6AD8C00D6B5AD0000000000D66B0000CE7B2100CE7B2100CE7B
      2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B
      2100CE7B2100CE7B2100CE7B2100D66B0000D66B0000CE7B2100CE7B2100CE7B
      2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B2100CE7B
      2100CE7B2100CE7B2100CE7B2100D66B000021ADDE00D6FFFF00C6FFFF00D6FF
      FF0021ADDE0021ADDE0021ADDE0021ADDE0021ADDE0021ADDE00000000000000
      0000000000000000000000000000000000000000000000000000BDDEBD001084
      180031CE630031843100D6AD9C00D6AD9C00EFDEBD00FFF7CE00FFEFC600EFD6
      AD00D6A58C00CEA59400FFFFFF0000000000CE7B2100D66B0000D66B0000D66B
      0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B
      0000D66B0000D66B0000D66B0000CE7B2100CE7B2100D66B0000D66B0000D66B
      0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B
      0000D66B0000D66B0000D66B0000CE7B2100ADDEEF0021ADDE0021ADDE0021AD
      DE00CEEFF7000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000063A5
      6300108C2100108C210052945200528C4A0073844A00527B3100CE9C8400D6A5
      9400DEC6BD00FFFFFF000000000000000000F7DEBD00CE7B2100D66B0000D66B
      0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B
      0000D66B0000D66B0000CE7B2100F7D6BD00F7D6BD00CE7B2100D66B0000D66B
      0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B0000D66B
      0000D66B0000D66B0000CE7B2100F7D6BD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000063A56300007B080000630000107310007BB57B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008C849400CEA5AD000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008C849400CEA5AD000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008C849400CEA5AD000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008C849400CEA5AD000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084ADCE004A7BC600847BA500CEA5
      AD00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084ADCE004A7BC600847BA500CEA5
      AD00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084ADCE004A7BC600847BA500CEA5
      AD00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084ADCE004A7BC600847BA500CEA5
      AD00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009CD6FF004AB5FF004A7BC600847B
      A500CEA5AD000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009CD6FF004AB5FF004A7BC600847B
      A500CEA5AD000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009CD6FF004AB5FF004A7BC600847B
      A500CEA5AD000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009CD6FF004AB5FF004A7BC600847B
      A500CEA5AD000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000094D6FF0052B5FF004A7B
      C600847BA500CEA5AD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000094D6FF0052B5FF004A7B
      C600847BA500CEA5AD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000094D6FF0052B5FF004A7B
      C600847BA500CEA5AD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000094D6FF0052B5FF004A7B
      C600847BA500CEA5AD0000000000000000000000000000000000000000000000
      0000EFFFFF00000000000000000000000000000000000000000094D6FF0052B5
      FF004A7BCE00847BA500DEC6CE00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000094D6FF0052B5
      FF004A7BCE00847BA500DEC6CE00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000094D6FF0052B5
      FF004A7BCE00847BA500DEC6CE00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000094D6FF0052B5
      FF004A7BCE00847BA500DEC6CE0000000000000000000000000000000000EFFF
      FF00EFFFFF00EFFFFF0000000000000000000000000000000000000000008CCE
      FF0052B5FF00528CD600ADADB500D6B5AD00CE9C8400CE9C8400CE9C8400D6A5
      9400D6B5AD00FFFFFF0000000000000000000000000000000000000000008CCE
      FF0052B5FF00528CD600ADADB5009CA584009C946300CE9C8400CE9C8400D6A5
      9400D6B5AD00FFFFFF0000000000000000000000000000000000000000008CCE
      FF0052B5FF00528CD600ADADB500D6B5AD00CE9C8400CE9C8400CE9C8400D6A5
      9400D6B5AD00FFFFFF0000000000000000000000000000000000000000008CCE
      FF0052B5FF00528CD600ADADB500D6B5AD00CE9C8400CE9C8400EFFFFF00EFFF
      FF00EFFFFF00EFFFFF00EFFFFF00000000000000000000000000000000000000
      0000A5DEFF00CED6D600BD8C8400D6AD8C00EFD6AD00FFF7CE00FFF7D600EFDE
      BD00DEB5A500CEAD9C00FFFFFF00000000000000000000000000000000000000
      0000A5DEFF00639C6B0010731000007B080000630000106B08007BAD6B00EFDE
      BD00DEB5A500CEAD9C00FFFFFF00000000000000000000000000000000000000
      0000A5DEFF00639C6B005A7B4200D6AD8C00EFD6AD00FFF7CE00FFF7D600EFDE
      BD00DEB5A500CEAD9C00FFFFFF00000000000000000000000000000000000000
      0000A5DEFF00CED6D600BD8C8400D6AD8C00EFD6AD00EFFFFF00EFFFFF00EFFF
      FF00EFFFFF00EFFFFF00EFFFFF00000000000000000000000000000000000000
      000000000000DEBDB500DEAD8C00FFF7C600FFF7C600FFFFDE00FFFFEF00FFFF
      FF00FFFFFF00DEB5A500CEADA500000000000000000000000000000000000000
      000063A56300108C2100108C210052943900ADCE8400BDDEA5009CC69400FFFF
      FF00FFFFFF00DEB5A500CEADA500000000000000000000000000000000000000
      000073AD7300298431000873180073A55200FFF7C600FFFFDE00FFFFEF00FFFF
      FF00FFFFFF00DEB5A500CEADA500000000000000000000000000000000000000
      000000000000DEBDB500DEAD8C00FFF7C600EFFFFF00EFFFFF00EFFFFF00EFFF
      FF00EFFFFF00EFFFFF00CEADA500000000000000000000000000000000000000
      000000000000D6A59400EFD6AD00FFE7B500FFFFC600FFFFDE00FFFFEF00FFFF
      FF00FFFFF700EFDEBD00CE9C8C0000000000000000000000000000000000BDDE
      BD001084180031CE6300297B2100FFE7B500FFFFC600FFFFDE00FFFFEF00FFFF
      FF00FFFFF700EFDEBD00CE9C8C00000000000000000000000000000000000000
      0000318C4A0039C6630039C65A00217B290073A55200FFFFDE00FFFFEF00FFFF
      FF00FFFFF700EFDEBD00CE9C8C00000000000000000000000000000000000000
      0000C6D6FF009CA5EF009CA5EF009CA5EF009CA5EF00FFADFF00EFFFFF00EFFF
      FF00EFFFFF00EFDEBD00CE9C8C00000000000000000000000000000000000000
      000000000000CE9C8400FFEFC600FFDEAD00FFF7BD00FFFFD600FFFFE700FFFF
      EF00FFFFE700FFF7D600CE9C8400000000000000000000000000000000007BB5
      7B0031BD520031BD52007BAD6300FFDEAD00FFF7BD00FFFFD600FFFFE700FFFF
      EF00FFFFE700FFF7D600CE9C84000000000000000000000000000000000073AD
      7300219C310052EF84005AF78C0029AD4200399C630073AD5A00FFFFE700FFFF
      EF00FFFFE700FFF7D600CE9C840000000000000000000000000000000000A5AD
      EF009CA5EF00B5C6FF00B5C6FF00B5C6FF00B5C6FF009CA5EF00EFFFFF00EFFF
      FF00FFFFE700FFF7D600CE9C8400000000000000000000000000000000000000
      000000000000CE9C8400FFEFC600FFE7BD00FFF7C600FFFFCE00FFFFD600FFFF
      D600FFFFD600FFF7CE00CE9C840000000000000000000063000000630000108C
      210042D66B0042DE7300108418000063000000630000FFFFCE00FFFFD600FFFF
      D600FFFFD600FFF7CE00CE9C8400000000000000000000000000006300000063
      0000108C210042DE730042D66B00108418000063000000630000FFFFD600FFFF
      D600FFFFD600FFF7CE00CE9C8400000000000000000000000000A5ADEF00AD84
      FF009CA5EF00B5C6FF00B5C6FF00FFCEFF00FFCEFF00FFA5FF00EFFFFF00FFFF
      D600FFFFD600FFF7CE00CE9C8400000000000000000000000000000000000000
      000000000000D6A59400EFDEB500FFF7DE00FFE7BD00FFEFBD00FFF7BD00FFF7
      C600FFF7C600EFD6AD00CE9C840000000000000000000000000073AD7300219C
      31005AF78C0052EF840029AD4200399C6300FFE7BD00FFEFBD00FFF7BD00FFF7
      C600FFF7C600EFD6AD00CE9C8400000000000000000000000000000000000000
      00007BB57B0031BD520031BD52007BAD6B00FFE7BD00FFEFBD00FFF7BD00FFF7
      C600FFF7C600EFD6AD00CE9C84000000000000000000A5ADEF00AD84FF00AD84
      FF00B5C6FF009CA5EF009CA5EF009CA5EF00FFA5FF00FFADFF00FFF7BD00FFF7
      C600FFF7C600EFD6AD00CE9C8400000000000000000000000000000000000000
      000000000000E7C6BD00D6B59400FFFFFF00FFFFEF00FFF7CE00FFDEA500FFE7
      B500FFF7C600D6AD8C00D6B5AD0000000000000000000000000000000000318C
      4A0039C65A0039C66300217B2900FFFFFF00FFFFEF00FFF7CE00FFDEA500FFE7
      B500FFF7C600D6AD8C00D6B5AD00000000000000000000000000000000000000
      0000BDDEBD001084180031CE630031843100FFFFEF00FFF7CE00FFDEA500FFE7
      B500FFF7C600D6AD8C00D6B5AD0000000000A5ADEF00AD84FF00AD84FF009CA5
      EF00FFCEFF00FFCEFF00FFCEFF00FFFFFF00FFADFF00FFF7CE00FFDEA500FFE7
      B500FFF7C600D6AD8C00D6B5AD00000000000000000000000000000000000000
      000000000000FFFFFF00D6AD9C00D6AD9C00EFDEBD00FFF7CE00FFEFC600EFD6
      AD00D6A58C00CEA59400FFFFFF000000000000000000000000000000000073AD
      73002984310008731800D6AD9C00D6AD9C00EFDEBD00FFF7CE00FFEFC600EFD6
      AD00D6A58C00CEA59400FFFFFF00000000000000000000000000000000000000
      00000000000063A56300108C2100108C21004A8C3900639C4A008CB56B006394
      4A00D6A58C00CEA59400FFFFFF0000000000A5ADEF00AD84FF009CA5EF00FFCE
      FF00FFCEFF00FFCEFF00FFFFFF00FFADFF00EFDEBD00FFF7CE00FFEFC600EFD6
      AD00D6A58C00CEA59400FFFFFF00000000000000000000000000000000000000
      00000000000000000000FFFFFF00DEC6BD00CEA58C00CE9C8400CE9C8400D6A5
      9400DEC6BD00FFFFFF0000000000000000000000000000000000000000000000
      00007BB57B007BB57B00FFFFFF00DEC6BD00CEA58C00CE9C8400CE9C8400D6A5
      9400DEC6BD00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000063A56300007B080000630000086B080063844200D6A5
      9400DEC6BD00FFFFFF000000000000000000A5ADEF009CA5EF00FFCEFF00FFCE
      FF00FFCEFF00FFFFFF00FFA5FF00DEC6BD00CEA58C00CE9C8400CE9C8400D6A5
      9400DEC6BD00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000BDDEBD00BDDEBD00000000000000
      000000000000000000000000000000000000D6DEFF00A5ADEF00A5ADEF00A5AD
      EF00A5ADEF00A5ADEF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A57B7300A57B7300A57B7300A57B7300A57B7300A57B7300A57B
      7300A57B7300A57B7300B57B7300000000000000000000000000000000000000
      00000000000000000000AD7B7300AD7B7300AD7B7300AD7B7300AD7B7300AD7B
      7300AD7B7300AD7B7300AD7B7300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AD847B00FFD6C600FFD6CE00FFD6CE00FFD6C600FFD6C600FFD6
      C600FFD6C600FFD6B500B57B7300000000000000000000000000BDE7F700009C
      CE00009CCE00009CCE00AD847B00FFD6A500FFD6A500FFD6A500FFD6A500FFCE
      9C00FFCE9C00FFCEB500AD7B7300000000000000000000000000000000000000
      000000000000000000000000000000000000FFF7EF00B56B4200DEBDAD000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFF7EF00B5634200DEBDAD000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000AD847B00FFE7C600FFD6AD00FFD6AD00FFD6AD00FFD6A500FFD6
      A500FFD6A500FFC6C600B57B73000000000000000000DEF7FF00009CCE006BDE
      F7006BDEF7006BDEF700B5847B00FFF7E700FFF7E700FFF7E700FFF7E700FFF7
      E700FFF7E700FFD6BD00AD7B7300000000000000000000000000000000000000
      000000000000000000000000000000000000BD735200C65A0000AD420000DEB5
      9C00000000000000000000000000000000000000000000000000000000000000
      0000FFF7EF00B5633100CE630000B5521000EFDECE0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000BD8C8400FFE7C600FFE7C600FFE7C600FFE7C600FFE7C600FFE7
      C600FFE7C600FFC6C600B57B7300000000000000000042B5DE008CF7FF0073D6
      FF0073D6FF0073D6FF00B58C7B00FFF7E700FFF7E700FFF7E700FFF7E700FFF7
      E700FFF7E700FFD6BD00AD7B7300000000000000000000000000000000000000
      000000000000000000000000000000000000AD5A3100C65A0000CE630000B552
      1000EFDECE00000000000000000000000000000000000000000000000000FFF7
      EF00B5633100CE630000CE630000B5521000EFDECE0000000000000000000000
      00000000000000000000000000000000000000000000AD847B00A57B7300A57B
      7300A57B7300BD8C8400FFE7CE00FFD6AD00FFD6AD00FFD6AD00FFD6AD00FFD6
      AD00FFD6A500FFC6C600B57B7300000000000000000042B5DE008CF7FF007BE7
      FF007BE7FF007BE7FF00C6948400FFF7E700FFD6AD00FFD6AD00FFD6A500FFD6
      A500FFD6A500FFD6C600AD7B7300000000000000000000000000000000000000
      000000000000000000000000000000000000FFF7EF00AD5A3100CE630000CE63
      0000AD522100FFF7EF000000000000000000000000000000000000000000CE94
      7300C65A0000CE630000B5521000EFDECE000000000000000000000000000000
      00000000000000000000000000000000000000000000AD847B00FFD6CE00FFD6
      CE00FFD6CE00C6947B00FFEFD600FFEFD600FFEFD600FFEFD600FFEFD600FFEF
      D600FFEFD600FFC6C600B57B7300000000000000000042B5DE008CF7FF0084EF
      FF0084EFFF0084EFFF00C6948400FFF7E700FFF7E700FFF7E700FFF7E700FFF7
      E700FFF7E700FFD6C600AD7B7300000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFF7EF00AD522100CE63
      0000C6630000C684630000000000000000000000000000000000E7CEBD00B552
      0000CE630000B54A0000E7CEBD00000000000000000000000000000000000000
      00000000000000000000000000000000000000000000BD8C8400FFE7C600FFD6
      AD00FFD6AD00C6947B00FFEFDE00FFD6AD00FFD6AD00FFD6AD00FFD6AD00FFD6
      AD00FFD6AD00FFC6C600B57B7300000000000000000042B5DE0094F7FF008CF7
      FF008CF7FF008CF7FF00CE9C8400FFF7E700FFDEB500FFD6AD00FFD6AD00FFD6
      AD00FFD6AD00FFD6C600AD7B730000000000AD420000BD5A0000BD5A0000BD5A
      0000BD5A0000BD5A0000BD5A0000BD520000C684630000000000EFDECE00AD4A
      0000CE630000B54A0000F7E7DE00000000000000000000000000AD522100CE63
      0000BD5A0000CE9C7B0000000000E7CEBD00B54A0000BD5A0000BD5A0000BD5A
      0000BD5A0000BD5A0000BD5A0000BD5A000000000000BD8C8400FFE7CE00FFE7
      CE00FFE7CE00CE9C8400FFF7E700FFF7E700FFF7E700FFF7E700FFF7E700FFF7
      E700FFF7E700FFC6C600B57B7300000000000000000042B5DE00A5F7FF0094FF
      FF0094FFFF0094FFFF00CE9C8400FFF7E700FFF7E700FFF7E700FFF7E700FFF7
      E700FFD6CE00FFADA500AD7B730000000000BD520000D66B0000CE630000CE63
      0000CE630000CE630000CE630000CE630000BD6B42000000000000000000CE9C
      7B00C6630000CE630000C68463000000000000000000E7CEBD00C65A0000D66B
      0000B55A21000000000000000000E7CEBD00B54A0000BD5A0000BD5A0000BD5A
      0000C6630000CE630000CE630000CE63000000000000C6947B00FFEFD600FFD6
      AD00FFD6AD00CE9C8400FFF7EF00FFFFFF00FFFFFF00FFFFEF00FFF7EF00FFF7
      E700FFE7D600FFC6C600B57B7300000000000000000042B5DE00B5FFFF009CFF
      FF009CFFFF009CFFFF00D6A58400FFFFF700FFFFFF00FFFFFF00FFFFF700FFF7
      EF00AD7B7300AD7B7300AD7B730000000000BD5A0000DE7B0000DE730000CE63
      0000A5421000CE9C7B00CE9C7B00CE9C7B00F7E7DE000000000000000000FFF7
      EF00AD4A0000CE630000B55210000000000000000000C68C7300EF840000D66B
      0000DEB59C00000000000000000000000000EFDECE00E7CEBD00E7CEBD00BD73
      5200BD520000CE630000CE630000CE63000000000000CE9C8400FFEFDE00FFEF
      DE00FFEFDE00D6A58C00FFFFF700FFFFFF00FFFFFF00FFFFFF00FFFFEF00FFF7
      EF00B5847300B5847300B5847300000000000000000042B5DE00C6FFFF00A5FF
      FF00A5FFFF00A5FFFF00DEAD8C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFF7
      EF00AD7B7300E7C6B5000000000000000000C65A0000EF840000CE630000DE73
      0000CE630000C684630000000000000000000000000000000000000000000000
      0000BD734200CE630000B54A0000FFF7EF0000000000CE7B4200FF940000CE63
      0000F7E7DE000000000000000000000000000000000000000000BD846300BD52
      0000CE630000BD520000C65A0000CE63000000000000CE9C8400FFF7E700FFD6
      AD00FFD6AD00DEAD8C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      EF00B5847300DEAD8400C6C6C600000000000000000042B5DE00D6FFFF00BDFF
      FF00BDFFFF00C6FFF700DEAD8C00DEAD8400DEAD8400DEAD8400DEAD8400DEAD
      8400AD7B7300000000000000000000000000CE630000F7940000B5521000B552
      0000E77B0000CE6B0000BD735200FFF7EF000000000000000000000000000000
      0000BD734200CE630000B54A0000FFF7EF0000000000BD734200FF9C0000DE73
      0000EFDECE0000000000000000000000000000000000C6846300BD520000CE63
      0000BD520000BD846300BD5A0000CE63000000000000D6A58C00FFF7EF00FFF7
      EF00FFF7EF00E7B59400DEAD8C00DEAD8C00DEAD8C00DEAD8C00DEAD8C00DEAD
      8C00B5847300C6C6C60000000000000000000000000042B5DE00DEFFFF00D6FF
      FF009CE7F7009CE7F7009CE7F7009CE7F7009CE7F7009CE7F700B5F7FF00D6FF
      FF00009CCE00000000000000000000000000CE6B0800FF9C0800CE844200DEB5
      9C00B5520000E7840000D66B0000BD633100EFDECE000000000000000000EFDE
      CE00B54A0000CE630000B55210000000000000000000D6A58C00F79C2100FF9C
      1000BD7B5200FFF7EF0000000000E7CEBD00BD6B3100CE630000D66B0000BD52
      0000DEB59C00E7CEBD00BD5A0000CE63000000000000DEAD8C00FFFFF700FFFF
      FF00FFFFFF00FFFFEF00FFFFEF00FFEFDE00F7B59400B5847300000000000000
      0000000000000000000000000000000000000000000042B5DE00EFFFFF00D6E7
      DE00A58C8400A58C8400A58C8400A58C8400A58C8400A58C840084ADAD00CEF7
      FF00009CCE00000000000000000000000000CE732100FFAD3100CE8442000000
      0000DEB59C00BD631000EF840000E77B0000BD520000C67B5200C68C7300AD52
      1000CE630000CE630000C68463000000000000000000FFF7EF00BD6B2900FFB5
      4200EF942100C66B1800BD632100D6730000EF8C0000E7840000B54A0000DEB5
      9C0000000000E7CEBD00BD5A0000CE63000000000000E7B59400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFEF00B5847300B5847300B5847300000000000000
      00000000000000000000000000000000000000000000000000004AC6DE0063D6
      E700739C9C00CEBDBD00DED6D600DED6D600DED6D60084949400299CBD0021A5
      D600ADDEEF00000000000000000000000000CE7B3900FFBD6300C67B4A000000
      000000000000EFDECE00B55A2100E77B0000E7840000E77B0000DE730000D66B
      0000CE630000AD4A1000FFF7EF00000000000000000000000000DEB59C00BD63
      2900FFBD5A00FFAD3900FFA51800FF9C0000E77B0000B55A2100EFDECE000000
      000000000000EFDECE00B5520000C65A000000000000E7B59400FFFFFF00FFFF
      FF00FFFFFF00F7F7F700EFEFEF00B5847300EFC69C00C6C6C600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A58C8400AD9C8C00AD9C8C00AD9C8C00CEC6BD00000000000000
      000000000000000000000000000000000000CE947B00C6845A00DEBDAD000000
      00000000000000000000FFF7EF00CE9C7B00C6631000CE6B0000D66B0000BD5A
      0000BD6B4200EFDECE000000000000000000000000000000000000000000EFDE
      CE00BD7B5200CE7B4200C66B2900C67B4200DEB59C0000000000000000000000
      00000000000000000000DEB59C00D6A58C0000000000E7B59400DEAD8C00DEAD
      8C00DEAD8C00DEAD8C00DEAD8C00B5847300C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000E7CEBD00E7CEBD00F7E7
      DE00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A573
      7300A5737300A5737300A5737300A5737300A5737300A5737300A5737300A573
      7300A57373008C5A5200000000000000000000000000CE630000CE630000CE63
      0000CE630000CE630000CE630000CE630000CE630000CE630000CE630000CE63
      0000CE6300000000000000000000000000000000000000000000000000000000
      0000CE630000CE630000CE630000CE630000CE630000CE630000CE630000CE63
      0000CE6300000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000ADA5B500A5737300F7D6
      CE00FFDECE00FFDECE00FFD6C600FFD6C600FFD6C600FFD6BD00FFD6BD00FFD6
      AD00FFCEAD008C5A5A000000000000000000CE630000FFDEBD00FFDEBD00FFDE
      BD00FFDEBD00FFDEBD00FFDEBD00FFDEBD00FFDEBD00FFDEBD00FFDEBD00FFDE
      BD00CE6300000000000000000000000000000000000000000000000000000000
      0000CE630000FFDEBD00FFDEBD00FFDEBD00FFDEBD00FFDEBD00FFDEBD00FFDE
      BD00CE6300000000000000000000000000000000000000000000000000000000
      0000C68C7300BD734200C68C7300000000000000000000000000C68C7300BD73
      4200C68C7300000000000000000000000000B5D6E7004A84D6008C7BA500E7C6
      BD00FFEFD600FFE7CE00FFE7C600FFDEBD00FFDEBD00FFDEB500FFD6AD00FFD6
      AD00FFD6A5008C5A5A000000000000000000CE630000FFE7CE00FFE7CE009CA5
      A5009CA5A5009CA5A5009CA5A500FFE7CE00FFE7CE00FFE7CE00FFE7CE00FFDE
      BD00CE6300000000000000000000000000000000000000000000000000000000
      0000CE630000FFFFDE00FFFFDE00FFFFDE00FFFFDE00FFFFDE00FFFFDE00FFDE
      BD00CE630000000000000000000000000000000000000000000000000000EFDE
      CE00AD420000BD633100A5420000CE9C7B0000000000CE9C7B00A5390000BD63
      3100B54A0000EFDECE000000000000000000F7FFFF0073CEFF004A84CE008C7B
      A500E7CEC600FFEFD600FFE7CE00FFE7C600FFDEBD00FFDEBD00FFDEB500FFD6
      B500FFD6AD00946363000000000000000000CE630000FFEFD600FFEFD600FFEF
      D600FFEFD600FFEFD600FFEFD600FFEFD600FFEFD600FFEFD600FFE7CE00FFDE
      BD00CE6300000000000000000000000000000000000000000000000000000000
      0000CE630000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFDE00FFDE
      BD00CE630000000000000000000000000000000000000000000000000000E7CE
      BD00A542000000000000C68C7300BD6B310000000000AD5A3100CE9C7B00F7E7
      DE00A5420000E7CEBD00000000000000000000000000F7FFFF006BB5E7004A7B
      C6009484A500FFE7D600F7DEC600EFCEBD00FFDEC600FFDEBD00FFDEBD00FFDE
      B500FFD6B500946363000000000000000000CE630000FFFFEF00FFFFEF009CA5
      A5009CA5A5009CA5A5009CA5A5009CA5A5009CA5A5009CA5A500FFE7CE00FFDE
      BD00CE6300000000000000000000000000000000000000000000CE630000CE63
      0000CE630000DE9C4A00DE9C4A00DE9C4A00DE9C4A00DE9C4A00DE9C4A00DE9C
      4A00CE630000000000000000000000000000000000000000000000000000F7E7
      DE00AD420000CE9C7B00BD846300BD5A000000000000A5420000C68C7300BD84
      6300AD4A0000F7E7DE0000000000000000000000000000000000C68C7B0073C6
      F700848CA500D6AD9400D6AD8C00D6A58400DEAD9400DEB59C00FFDEBD00FFDE
      BD00FFDEB5009C6B6B000000000000000000CE630000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFEF00FFEFD600FFE7CE00FFDE
      BD00CE6300000000000000000000000000000000000000000000CE630000FFDE
      BD00DE9C4A00CE630000CE630000CE630000CE630000CE630000CE630000CE63
      0000DE9C4A0000000000000000003939BD000000000000000000000000000000
      0000CE947300B54A0000BD520000B54A0000E7CEBD00AD4A0000BD5A0000B552
      0000CE9473000000000000000000000000000000000000000000C68C7B00FFFF
      F700DEB5A500E7BD9C00F7E7B500FFFFDE00F7E7D600E7BDAD00D6AD9C00FFE7
      C600FFDEBD009C736B000000000000000000CE630000FFFFFF00FFFFFF009CA5
      A5009CA5A5009CA5A5009CA5A5009CA5A5009CA5A5009CA5A500FFE7CE00FFDE
      BD00CE6300000000000000000000000000000000000000000000CE630000FFFF
      DE00EFE7CE00DE9C4A00CE630000CE630000CE630000CE630000CE630000DE9C
      4A00F7D6BD0000000000000000000000AD000000000000000000000000000000
      000000000000DEBDAD00CE9C7B009C42180094634A009C4A1800C68C7300DEBD
      AD00000000000000000000000000000000000000000000000000CE947B00FFFF
      F700DEAD9400F7D6A500FFFFCE00FFFFE700FFFFF700F7E7C600D6AD8C00FFE7
      C600FFDEBD00A57373000000000000000000CE630000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFEF00FFEFD600FFE7CE00FFDE
      BD00CE6300000000000000000000000000000000000000000000CE630000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFDE00FFDEBD00CE6300000000
      0000000000003939BD00000000000000AD000000000000000000000000000000
      00000000000000000000000000008C736300C6B5B5008C736B00000000000000
      0000000000000000000000000000000000000000000000000000D69C8400FFFF
      FF00D6A58400FFE7B500FFFFC600FFFFDE00FFFFDE00FFFFD600D69C8400EFDE
      D600FFE7C600A57B73000000000000000000CE630000FF844A00FF844A00FF84
      4A00FF844A00FF844A00FF844A00FF844A00FF844A00FF844A00FF844A00FF84
      4A00CE630000000000000000000000000000CE630000CE630000CE630000DE9C
      4A00DE9C4A00DE9C4A00DE9C4A00DE9C4A00DE9C4A00DE9C4A00CE6300000000
      0000000000000000AD000000AD00000000000000000000000000000000000000
      00000000000000000000BDADA500B5A59C009C847300A5948400BDADA5000000
      0000000000000000000000000000000000000000000000000000DEA58400FFFF
      FF00DEAD9400F7DECE00FFE7BD00FFF7C600FFF7C600F7DEB500D6A58C00DED6
      DE00FFE7CE00A57B73000000000000000000DE9C4A00CE630000CE630000CE63
      0000CE630000CE630000CE630000CE630000CE630000CE630000CE630000CE63
      0000DE9C4A00000000000000000000000000CE630000FFDEBD00DE9C4A00CE63
      0000CE630000CE630000CE630000CE630000CE630000CE630000DE9C4A000000
      00000000000000009C000000AD003939BD000000000000000000000000000000
      000000000000000000009C8C8400A59C9400A58C8400AD9C94009C8C8400F7F7
      F700000000000000000000000000000000000000000000000000E7AD8C00FFFF
      FF00E7C6B500DEBDAD00F7E7CE00FFEFBD00F7DEAD00DEAD8C00DEBDAD00FFE7
      D600FFDECE00AD7B7B000000000000000000F7D6BD00DE9C4A00CE630000CE63
      0000CE630000CE630000CE630000CE630000CE630000CE630000CE630000DE9C
      4A00F7D6BD00000000003939BD0000000000CE630000FFFFDE00EFE7CE00DE9C
      4A00CE630000CE630000CE630000CE630000CE630000DE9C4A00F7DEBD003939
      BD00000000006B6BCE0000000000000000000000000000000000000000000000
      000000000000CEC6BD00BDB5AD00A5948C00000000009C8C7B00B5ADA500CEC6
      BD00000000000000000000000000000000000000000000000000E7B58C00FFFF
      FF00FFFFFF00E7C6BD00D6AD9400D6A58400DEAD9400E7C6B500F7DECE00F7BD
      B500F7A59C00AD847B0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000AD0000000000CE630000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFDE00FFDEBD00CE63000000000000000000000000
      AD000000AD000000000000000000000000000000000000000000000000000000
      000000000000AD9C9400AD9C9400EFEFEF0000000000EFEFEF00BDADA500A594
      8C00000000000000000000000000000000000000000000000000EFBD9400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFFFF700B57B7300B57B
      7300B57B7300B57B730000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003939
      BD00000000000000AD000000000000000000CE630000DE9C4A00DE9C4A00DE9C
      4A00DE9C4A00DE9C4A00DE9C4A00DE9C4A00CE63000000000000000000000000
      9C000000AD003939BD0000000000000000000000000000000000000000000000
      0000F7F7F7008C736300BDADA500000000000000000000000000BDADA500846B
      5A00F7F7F7000000000000000000000000000000000000000000F7BD9400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700B57B7300F7CE
      9C00DEB59C000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      AD000000AD00000000000000000000000000DE9C4A00CE630000CE630000CE63
      0000CE630000CE630000CE630000CE630000DE9C4A003939BD00000000003939
      BD00000000000000000000000000000000000000000000000000000000000000
      0000DED6D6008C736300F7F7F700000000000000000000000000000000008C73
      6300DED6D6000000000000000000000000000000000000000000F7C69400DEAD
      8400DEAD8400DEAD8400DEAD8400DEAD8400DEAD8400DEAD8400B57B7300E7BD
      9C00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      9C000000AD003939BD000000000000000000F7D6BD00DE9C4A00CE630000CE63
      0000CE630000CE630000CE630000DE9C4A00F7D6BD000000AD000000AD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6CEC600D6CEC6000000000000000000000000000000000000000000DED6
      D600CEC6BD000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000009C000000AD003939
      BD00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000009C4A4A009C4A4A009C4A
      4A00C6C6C600C6C6C600C6C6C600C6C6C6009C4A4A009C4A4A009C4A4A000000
      0000000000000000000000000000000000000000000000000000A5737300A573
      7300A5737300A5737300A5737300A5737300A5737300A5737300A5737300A573
      7300A573730094635A0000000000000000000000000000000000000000000000
      00000000000000000000BDB5BD00CECED600BDB5BD00BDB5BD00000000000000
      00000000000000000000000000000000000000000000DEB59C00844A4A009C4A
      4A009C424200B5B5B500B5B5B500B5B5B500B5B5B500B5B5B500B5B5B5009C31
      31009C3131009C4A4A009C4A4A0000000000B58473009C4A4A00CE636B00944A
      4A00C69C9400DEDEDE00FFFFFF00DEDEDE0094313100B55A5A00CE636B00944A
      4A000000000000000000000000000000000000000000A57B7300F7E7D600F7E7
      D600FFEFD600EFDEBD00FFE7C600FFDEBD00FFDEBD00FFDEB500FFD6AD00FFD6
      A500FFD6A50094635A0000000000000000000000000000000000000000000000
      0000ADADAD00ADADAD00C6C6C600ADADAD009C8C8400CEB5AD00BDB5BD00BDB5
      BD00BDB5BD0000000000000000000000000000000000BD847300CE636300CE63
      63009C4A4A00AD9C9C00C6737300DEB5B500FFFFFF00F7F7F700E7E7E7009C31
      31009C313100CE6363009C4A4A0000000000B5847300CE636B00CE636B009C4A
      5200FF425200C69C9400DEDEDE00FFFFFF0094313100B55A5A00CE636B00944A
      4A009C4A4A0000000000000000000000000000000000AD7B7300F7E7DE00F7E7
      DE00009C0000ADDEA50073C66B0042AD290084BD5A00EFD6A500FFD6AD00FFD6
      A500FFD6A5009463630000000000000000000000000000000000ADADAD00ADAD
      AD00F7F7F700EFEFEF00BDB5BD00BDB5BD00525252006B6B6B009CA5A500C6C6
      C600CECED600BDB5BD00BDB5BD000000000000000000BD847300CE636300CE63
      63009C4A4A00CEB5B500B5525200BD8C8C00DEDEDE00FFFFFF00F7F7F7009C31
      31009C313100CE6363009C4A4A0000000000B5847300CE636B00CE636B009C4A
      520094313100AD847B00FFD6CE00DEDEDE0094313100B55A5A00CE636B00944A
      4A00CE636B00944A4A00000000000000000000000000B5847B00F7EFDE00F7EF
      DE00009C0000009C0000009C0000009C0000009C000042AD2900FFDEB500FFD6
      AD00FFD6AD009C6B6300000000000000000000000000ADADAD00FFFFFF00F7F7
      F700EFEFEF00E7E7E700BDB5BD00ADADAD009CA5A5009CA5A5006B6B6B009CA5
      A500ADADAD00C6C6C6009CA5A5000000000000000000BD847300CE636300CE63
      63009C4A4A00E7C6C600AD424200AD737300B5B5B500DEDEDE00FFFFFF009C31
      31009C313100CE6363009C4A4A0000000000B5847300CE636B00CE636B009C4A
      5200BD6B6B00CE8C9400C67B8400BD6B6B00AD424200CE636300CE636B00944A
      4A00CE636B00944A4A009C4A4A000000000000000000B58C7B00F7EFE700F7EF
      E700009C0000009C0000109C0800CED6A500FFE7CE0021A51800FFDEB500FFD6
      B500FFD6B5009C6B6300000000000000000000000000ADADAD00F7F7F700E7E7
      E700BDB5BD009CA5A5009CA5A5009CA5A5009CA5A500ADADAD00BDB5BD008CBD
      94008CBD94009CA5A500000000000000000000000000BD847300CE636300CE63
      63009C4A4A00DECECE00CE949400CEB5B500ADADAD00B5B5B500D6CECE009C31
      31009C313100CE6363009C4A4A0000000000B5847300CE636B00CE636B00CE63
      6B00CE636B00CE636B00CE636B00CE636B00CE636B00CE636B00C6636B00944A
      4A00CE636B00944A4A00CE636B00944A4A0000000000BD8C7B00F7EFEF00F7EF
      EF00009C0000009C0000009C0000009C0000FFE7CE00EFDEBD00FFDEB500FFD6
      B500FFDEB500A56B6B00000000000000000000000000ADADAD00ADADAD00ADAD
      AD00CECED600CECED600DEDEDE00BDB5BD00ADADAD009CA5A5009CA5A5009CA5
      A500ADADAD009CA5A500000000000000000000000000BD847300CE636300CE63
      6300CE636300CE636300CE636300CE636300CE636300CE636300CE636300CE63
      6300CE636300CE6363009C4A4A0000000000B5847300CE636B00FF638400E7CE
      CE00E7CECE00E7CECE00E7CECE00E7CECE00FF638400B55A5A00C6636B00944A
      4A00CE636B00944A4A00CE636B00944A4A0000000000C6948400FFF7EF00FFF7
      EF00FFF7EF00FFF7EF00FFF7EF00FFF7EF00EFE7C600EFE7C600FFE7CE00FFDE
      BD00FFDEBD00A5736B00000000000000000000000000ADADAD00DEDEDE00DEDE
      DE00CECED600DEDEDE00BDB5BD00BDB5BD00BDB5BD00DEDEDE00CECED600C6C6
      C600ADADAD009CA5A500000000000000000000000000BD847300CE636300CE63
      6300BD8C8C00BD8C8C00BD8C8C00BD8C8C00BD8C8C00BD8C8C00BD8C8C00BD8C
      8C00CE636300CE6363009C4A4A0000000000B5847300CE636B00AD424200FFFF
      FF00BDB5BD00BDB5BD00BDB5BD00FFFFFF00CEA5A500B55A5A00C6636B00944A
      4A00CE636B00944A4A00CE636B00944A4A0000000000CE9C8400FFF7F700FFFF
      FF00FFF7EF00FFF7EF00FFF7EF00009C0000009C0000009C0000FFEFD600FFE7
      C600FFE7C600AD736B00000000000000000000000000ADADAD00DEDEDE00CECE
      D600CECED600BDB5BD00E7E7E700EFEFEF00E7E7E700BDB5BD00BDB5BD00BDB5
      BD00C6C6C6009CA5A500000000000000000000000000BD847300CE636300BD8C
      8C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00BD8C8C00CE6363009C4A4A0000000000B5847300CE636B00AD424200FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CEA5A500B55A5A00C6636B00944A
      4A00C6636B00944A4A00CE636B00944A4A0000000000D6A58400FFFFFF00FFFF
      FF00109C0800ADDEA500FFF7EF00ADDEA500009C0000009C0000FFEFD600FFE7
      CE00FFE7C600AD7B6B0000000000000000000000000000000000BDB5BD00BDB5
      BD00BDB5BD00CECED600E7E7E700EFEFEF00EFEFEF00EFEFEF00E7E7E700E7E7
      E700BDB5BD0000000000000000000000000000000000BD847300CE636300BD8C
      8C00FFFFFF00C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600FFFF
      FF00BD8C8C00CE6363009C4A4A0000000000B5847300CE636B00AD424200FFFF
      FF00BDB5BD00BDB5BD00BDB5BD00FFFFFF00CEA5A500B55A5A00C6636B00944A
      4A00C6636B00944A4A00CE636B00944A4A0000000000DEAD8C00FFFFFF00FFFF
      FF0073C66B00009C0000009C0000009C0000009C0000009C0000FFEFD600FFEF
      D600FFE7CE00AD7B730000000000000000000000000000000000000000000000
      0000DEAD8C00C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600CECED600BDB5
      BD00BDB5BD0000000000000000000000000000000000BD847300CE636300BD8C
      8C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00BD8C8C00CE6363009C4A4A0000000000B5847300CE636B00AD424200FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CEA5A500B55A5A00C6636B00944A
      4A00C6636B00944A4A00C6636B00944A4A0000000000DEAD8C00FFFFFF00FFFF
      FF00FFFFFF0073C66B00009C0000009C0000ADDEA500009C0000FFEFD600FFCE
      C600FFB5B500B57B730000000000000000000000000000000000000000000000
      0000DEAD8C00FFDEB500FFE7C600FFEFD600F7E7CE00DEAD8C00BDB5BD000000
      00000000000000000000000000000000000000000000BD847300CE636300BD8C
      8C00FFFFFF00BDBDBD00C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600FFFF
      FF00BD8C8C00CE6363009C4A4A0000000000B5847300944A4A0094313100C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600FF63840094313100944A4A00DEDE
      DE00C6636B00944A4A00C6636B00944A4A0000000000E7B58C00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFFFF700FFF7EF00F7DECE00B57B
      7300B57B7300B57B73000000000000000000000000000000000000000000DEAD
      8C00FFDEBD00FFDEBD00FFDEBD00FFDEBD00FFDEBD00DEAD8C00000000000000
      00000000000000000000000000000000000000000000BD847300CE636300BD8C
      8C00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00BD8C8C00CE6363009C4A4A00000000000000000000000000B5847300CE63
      6B00AD424200FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00CEA5A500B55A
      5A00C6636B00944A4A00C6636B00944A4A0000000000EFBD9400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFFFF700F7E7D600B57B
      7300EFA55200D6AD94000000000000000000000000000000000000000000DEAD
      8C00FFEFD600FFEFD600FFEFD600FFEFD600FFEFD600DEAD8C00000000000000
      00000000000000000000000000000000000000000000BD8473009C4A4A00BD8C
      8C00C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600BD8C8C009C4A4A00DEB59C00000000000000000000000000B5847300944A
      4A0094313100C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600FF6384009431
      3100944A4A00DEDEDE00C6636B00944A4A0000000000EFBD9400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00F7F7F700F7F7F700F7F7F700F7EFEF00EFDED600B57B
      7300DEB59C00FFFFF7000000000000000000000000000000000000000000DEAD
      8C00FFF7E700FFF7E700FFF7E700FFF7E700FFF7E700DEAD8C00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B5847300CE636B00AD424200FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00CEA5A500B55A5A00C6636B00944A4A0000000000F7BD9400DEAD8400DEAD
      8400DEAD8400DEAD8400DEAD8400DEAD8400DEAD8400DEAD8400DEAD8400B57B
      7300FFFFF7000000000000000000000000000000000000000000DEAD8C00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00DEAD8C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B5847300944A4A0094313100C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600FF63840094313100944A4A00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000DEAD8C00DEAD
      8C00DEAD8C00DEAD8C00DEAD8C00DEAD8C000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000A573
      7300A5737300A5737300A5737300A5737300A5737300A5737300A5737300A573
      7300A573730094635A00000000000000000000000000109CCE00109CCE0052BD
      DE008CD6EF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000109CCE00109CCE0052BD
      DE008CD6EF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A57B7300FFCE
      C600FFCEC600FFCEC600FFCEC600FFCEC600FFCEC600FFCEC600FFCEC600FFCE
      C600FFCEC60094635A0000000000000000004ABDE700A5DEEF0063D6FF004AC6
      EF0029B5DE0008A5D600109CCE0052BDDE008CD6EF0000000000000000000000
      0000000000000000000000000000000000004ABDE700A5DEEF0063D6FF004AC6
      EF0029B5DE0008A5D600109CCE0052BDDE008CD6EF0000000000000000000000
      00000000000000000000000000000000000000000000000000008C949400525A
      630094949C00DEDEDE0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000AD7B7300F7E7
      D600FFEFD600FFE7CE00FFE7C600FFE7C600FFDEBD00FFDEB500FFD6B500FFD6
      AD00FFD6A5009463630000000000000000004ABDE70063C6E700ADEFFF0084E7
      FF0084E7FF0084E7FF0073DEFF0052CEEF0031B5DE0018A5DE0073CEE7000000
      0000000000000000000000000000000000004ABDE70063C6E700ADEFFF0084E7
      FF0084E7FF0084E7FF0073DEFF0052CEEF0031B5DE0018A5DE0073CEE7000000
      0000000000000000000000000000000000000000000000000000AD5A21009C31
      00004A291800525252006B737B00C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000B5847B00F7E7
      DE00FFEFDE00FFEFD600FFE7CE00FFE7C600FFE7C600FFDEBD00FFDEB500FFD6
      B500FFD6AD009C6B630000000000000000004ABDE7004ABDE700CEFFFF008CF7
      FF008CF7FF008CF7FF008CF7FF008CF7FF008CF7FF0073D6FF0039BDE7000000
      0000000000000000000000000000000000004ABDE7004ABDE700CEFFFF008CF7
      FF008CF7FF008CF7FF008CF7FF008CF7FF008CF7FF0073D6FF0039BDE7000000
      0000000000000000000000000000000000000000000000000000B5732100A539
      1000FFEFD600EFB58400D68452009C3100004A29210052525A00848C8C000000
      0000000000000000000000000000000000000000000000000000B58C7B00F7EF
      E700FFEFDE00FFEFDE00FFEFD600FFE7CE00FFE7C600FFE7C600FFDEBD00FFDE
      B500FFD6B5009C6B630000000000000000004ABDE7004ABDEF00C6EFF70094FF
      FF0094FFFF0094FFFF0094FFFF0094FFFF0094FFFF0073DEFF008CE7FF009CDE
      EF00000000000000000000000000000000004ABDE7004ABDEF00C6EFF70094FF
      FF0094FFFF0094FFFF0094FFFF0094FFFF0094FFFF0073DEFF008CE7FF009CDE
      EF00000000000000000000000000000000000000000000000000AD631000AD5A
      2100FFEFD600FFDEBD00FFE7C600FFE7B500FFCE9C00CE631800423939000000
      0000000000000000000000000000000000000000000000000000BD8C7B00F7EF
      E700FFF7E700FFEFDE00FFEFDE00FFEFD600FFE7CE00FFE7C600FFE7C600FFDE
      BD00FFDEB500A56B6B0000000000000000004ABDE70073CEFF0084CEE700C6FF
      FF00A5FFFF00A5FFFF00A5FFFF00A5FFFF00A5FFFF0073DEFF00B5F7FF0052BD
      E700000000000000000000000000000000004ABDE70073CEFF0084CEE700C6FF
      FF00A5FFFF00A5FFFF00A5FFFF00A5FFFF00A5FFFF0073DEFF00B5F7FF0052BD
      E700000000000000000000000000000000000000000000000000B5732100A54A
      2100FFEFDE00FFE7C600B5CEC600FFDEC600FFBD4A00C65A1800424239000000
      0000000000000000000000000000000000000000000000000000C6948400FFF7
      EF00FFF7EF00FFF7E700FFEFDE00FFEFDE00FFEFD600FFE7CE00FFE7C600FFE7
      C600FFDEBD00A5736B0000000000000000004ABDE7008CDEFF004AC6EF0073C6
      E7008CD6EF00B5E7F700E7FFFF00C6FFFF00C6FFFF007BDEFF00A5DEDE008CD6
      DE00BDE7F7000000000000000000000000004ABDE7008CDEFF004AC6EF0073C6
      E7008CD6EF00B5E7F700E7FFFF00C6FFFF00C6FFFF007BDEFF00A5DEDE008CD6
      DE00BDE7F7000000000000000000000000000000000000000000BD631800B56B
      3100FFF7E70018A5D600189CD600FFE7C600BDBDA500C65A080042424A000000
      0000000000000000000000000000000000000000000000000000CE9C8400FFF7
      F700FFFFF700FFF7EF00FFF7E700FFEFDE00FFEFDE00FFEFD600FFE7CE00FFE7
      C600FFE7C600AD736B0000000000000000004ABDE7009CEFFF0084EFFF0084EF
      FF0073E7FF0052D6EF0039BDD600DEF7FF00F7FFFF00ADDEEF00087318002984
      310063BDD6000000000000000000000000004ABDE7009CEFFF0084EFFF0084EF
      FF0073E7FF0052D6EF0039BDD600DEF7FF00F7FFFF00ADDEEF00087318002984
      310063BDD6000000000000000000000000000000000000000000C66B2900C67B
      420084C6D600FFF7DE00DEE7DE00FFE7D600CE8C6300FFF7DE00FFDEAD00B563
      18006B6B63009CA59C0000000000000000000000000000000000D6A58400FFFF
      FF00FFFFF700FFFFF700FFF7EF00FFF7E700FFEFDE00FFEFDE00FFEFD600FFE7
      CE00FFE7C600AD7B6B0000000000000000004ABDE70094FFFF0094FFFF0094FF
      FF0094FFFF0094FFFF0084F7FF0052D6EF0039BDD600217B290039C6630039C6
      5A00318C4A000000000000000000000000004ABDE70094FFFF0094FFFF0094FF
      FF0094FFFF0094FFFF0084F7FF0052D6EF0039BDD600217B290039C6630039C6
      5A00318C4A000000000000000000000000000000000000000000C6731800CE8C
      6B00FFFFF700FFF7EF00FFF7E70031ADD600FFEFD600B54A08007B523900EFA5
      5A00DEDEDE004252CE005A636B00000000000000000000000000DEAD8C00FFFF
      FF00FFFFFF00FFFFF700FFFFF700FFF7EF00FFF7E700FFEFDE00FFEFDE00FFEF
      D600FFE7CE00AD7B730000000000000000004ABDE700BDFFFF009CFFFF009CFF
      FF00CEF7FF00A5E7F700C6FFFF00CEFFFF00399C630029AD420052EF84005AF7
      8C00219C310073AD730000000000000000004ABDE700BDFFFF009CFFFF00087B
      1000CEF7FF00A5E7F700C6FFFF00CEFFFF00399C630029AD420052EF84005AF7
      8C00219C310073AD730000000000000000000000000000000000CE731800CE94
      7300FFFFFF00FFF7EF00FFFFEF0008A5CE00FFEFD600B5520800424A52000000
      0000B5A59C002939BD009494C600000000000000000000000000DEAD8C00FFFF
      FF00FFFFFF00FFFFFF00FFFFF700FFFFF700FFF7EF00FFF7E700FFEFDE00FFD6
      CE00FFB5B500B57B730000000000000000004ABDE7009CDEEF00ADE7F700DEFF
      FF0073C6E7004ABDE7004ABDE70000630000006300001084180042DE730042D6
      6B00108C21000063000000630000000000004ABDE7009CDEEF00297B290000A5
      2900087B10004ABDE7004ABDE70000630000006300001084180042DE730042D6
      6B00108C21000063000000630000000000000000000000000000CE630800CEB5
      A500FFFFFF00FFFFF700FFF7F700DEEFE700E7EFE700B5520800525A63000000
      0000000000000000000000000000000000000000000000000000E7B58C00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFFFF700FFF7EF00F7DECE00B57B
      7300B57B7300B57B73000000000000000000EFFFFF004ABDE7004ABDE7004ABD
      E700CEEFF700000000000000000000000000000000007BB57B0031BD520031BD
      52007BB57B00000000000000000000000000EFFFFF00297B290031C65A0052DE
      7B0000A52900087B10000000000000000000000000007BB57B0031BD520031BD
      52007BB57B000000000000000000000000000000000000000000D6731000BD52
      0000CE733100CEA58C00E7CECE00FFFFFF00FFFFFF00B55208005A5A63000000
      0000000000000000000000000000000000000000000000000000EFBD9400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFFFF700F7E7D600B57B
      7300EFA55200D6AD940000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000003184310031CE63001084
      180000000000000000000000000000000000398C390031C65A0052DE7B0052DE
      7B0052DE7B0000A52900087B100000000000000000003184310031CE63001084
      1800000000000000000000000000000000000000000000000000E7842900C65A
      0000C6520000C6520000BD520000B55A1800D69C7300AD4A10005A6363000000
      0000000000000000000000000000000000000000000000000000EFBD9400FFFF
      FF00FFFFFF00FFFFFF00F7F7F700F7F7F700F7F7F700F7EFEF00EFDED600B57B
      7300DEB59C00FFFFF70000000000000000000000000000000000000000000000
      0000000000000000000000000000ADCEAD0052945200108C2100108C210063A5
      6300000000000000000000000000000000000000000000000000398C390000A5
      2900087B100008841800000000000000000052945200108C2100108C210063A5
      6300000000000000000000000000000000000000000000000000D6D6D600ADAD
      AD008C8C8C00BDA59400D6BD8C00DE9C6300DE841800A55210006B737B000000
      0000000000000000000000000000000000000000000000000000F7BD9400DEAD
      8400DEAD8400DEAD8400DEAD8400DEAD8400DEAD8400DEAD8400DEAD8400B57B
      7300FFFFF7000000000000000000000000000000000000000000000000000000
      0000000000007BB57B001073100000630000007B0800107B18007BB57B000000
      000000000000000000000000000000000000000000000000000000000000398C
      390000A52900087B100000A5290010842100007B0800107B18007BB57B000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000EFF7F700E7EFEF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000BDDEBD00BDDEBD0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000398C3900398C3900398C3900398C3900BDDEBD0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008C849400CEA5AD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000009C633900BDA5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000009C9C9C006B6B6B00525252005252520073737300000000000000
      00000000000000000000000000000000000084ADCE004A7BC600847BA500CEA5
      AD00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000634A3900DEA57300BDB5A500000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000E7D6CE00EFEFE700F7F7F700F7F7F700EFEFEF00F7E7DE00CEBDBD006363
      6300000000000000000000000000000000009CD6FF004AB5FF004A7BC600847B
      A500CEA5AD000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000D6CECE007B634A00634A3900D6AD8C00DEA573006B180800946B52000000
      000000000000000000000000000000000000000000000000000000000000EFDE
      D600F7F7F700EFDEDE00AD8C6B00B5734A00D6AD8C00E7E7E700EFEFEF00F7E7
      DE00848484000000000000000000000000000000000094D6FF0052B5FF004A7B
      C600847BA500CEA5AD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000E7732900C66B5A00CE6B5200D66B5200D6736B00C66339005A4A
      4A0000000000000000000000000000000000000000000000000000000000946B
      7300BDA59400D6AD8C00D6AD8C00E7C6A500DEBDA500D6AD8C00AD8C6B004231
      3100D6D6D6000000000000000000000000000000000000000000F7EFEF00F7F7
      FF00CE5A2900CE6B3100B5734A00F7FFF700CE6B3100B5633100CE5A2900EFEF
      EF00EFDED600737373000000000000000000000000000000000094D6FF0052B5
      FF004A7BCE00847BA500DEC6CE00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000B5843100C6632900B5522900B5522100B5522100C65A2900C66B6B00BD5A
      29006B4A31000000000000000000000000000000000000000000BDA59400E7C6
      A500FFDEBD00D694730094290000CE5A290094290000FFE7CE00FFDEAD00D6AD
      8C006331310000000000000000000000000000000000E7D6CE00F7FFF700D694
      7300CE6B3100CE6B3100CE6B3100E7CEBD00CE6B3100CE6B3100CE6B3100B563
      3100F7FFF7007B6B6B0000000000000000000000000000000000000000008CCE
      FF0052B5FF00528CD600ADADB500D6B5AD00CE9C8400CE9C8400CE9C8400D6A5
      9400D6B5AD00FFFFFF000000000000000000000000000000000000000000BD4A
      1000942900009C420800B5420000217B08008C520000A54200009C390000BD5A
      2900C66352006B524200000000000000000000000000D6AD8C00CEBDA500FFEF
      DE00FFE7CE00FFF7CE009C63390094290000FFFFDE00FFE7CE00FFDEBD00E7C6
      A500CEBDA50094847B00000000000000000000000000F7E7DE00F7F7F700CE5A
      2900CE6B3100CE6B3100CE6B3100E7C6A500CE6B3100CE6B3100CE6B3100CE6B
      3100F7F7FF00EFDED60000000000000000000000000000000000000000000000
      0000A5DEFF00CED6D600BD8C8400D6AD8C00EFD6AD00FFF7CE00FFF7D600EFDE
      BD00DEB5A500CEAD9C00FFFFFF00000000000000000000000000B5944200B54A
      0000B54A0000C65A0000CE6B0000087B0000087B0000D65A0000B54A0000AD4A
      0000845A4200A5634200000000000000000000000000CEBDA500EFD6BD00FFE7
      CE00FFEFDE00FFEFDE009C63390094290000FFFFDE00FFE7CE00FFE7CE00FFE7
      CE00E7C6A5006B180800000000000000000000000000FFF7F700E7C6A500CE6B
      3100CE6B3100CE6B3100CE6B3100FFFFFF00CE5A2900CE6B3100CE6B3100CE6B
      3100EFE7E700F7E7DE0084848400000000000000000000000000000000000000
      000000000000DEBDB500DEAD8C00FFF7C600FFF7C600FFFFDE00FFFFEF00FFFF
      FF00FFFFFF00DEB5A500CEADA500000000000000000000000000D66B1800CE63
      0000CE6B0000DE7B00007B84000008840000397B0800DE730800947300006B5A
      0000107B08007B6B5200847B7B000000000000000000DECEB500FFF7EF00FFEF
      DE00FFEFDE00FFEFDE009C63390094290000FFFFDE00FFE7CE00FFE7CE00FFE7
      CE00FFE7CE0063313100000000000000000000000000FFFFFF00D6AD8C00D684
      5200CE6B3100CE6B3100CE6B3100CE6B3100FFFFFF00D6845200CE6B3100CE6B
      3100EFDED600F7EFEF0084848400000000000000000000000000000000000000
      000000000000D6A59400EFD6AD00FFE7B500FFFFC600FFFFDE00FFFFEF00FFFF
      FF00FFFFF700EFDEBD00CE9C8C00000000000000000000000000CE5A0800D673
      0000D6730000E78400009C9C08007B9C0800DE940800E77B0000087B0000007B
      0000087B0000636331008C7363000000000000000000DECEB500FFF7EF00FFF7
      EF00FFF7EF00FFFFF7008C42210094290000FFFFF700FFEFDE00FFEFDE00FFEF
      DE00FFE7CE0063313100000000000000000000000000FFF7F700FFDEBD00D684
      5200CE6B3100CE5A2900CE6B3100CE6B3100EFE7E700FFFFFF00CE6B3100CE6B
      3100FFF7F700F7E7DE009C9C9C00000000000000000000000000000000000000
      000000000000CE9C8400FFEFC600FFDEAD00FFF7BD00FFFFD600FFFFE700FFFF
      EF00FFFFE700FFF7D600CE9C8400000000000000000000000000D6630000F77B
      08006B94000031AD29006BAD2100FFA51800FFA51000FF940000AD8C0800087B
      0000317B0000526B2100947352000000000000000000D6D6D600FFFFF700FFF7
      EF00FFF7EF00AD8C6B008C4221006B180800FFFFFF00FFEFDE00FFEFDE00FFF7
      EF00FFEFDE0094847B00000000000000000000000000F7E7DE00FFFFFF00FF8C
      6300D6845200F7F7FF00E7C6A500CE5A2900EFDEDE00FFFFFF00CE6B3100DE84
      2900FFFFFF00EFDED60000000000000000000000000000000000000000000000
      000000000000CE9C8400FFEFC600FFE7BD00FFF7C600FFFFCE00FFFFD600FFFF
      D600FFFFD600FFF7CE00CE9C8400000000000000000000000000F7942100A594
      100021BD390029C64A0031D66B00BDDE8400FFD67B00FFB53100EF940000009C
      0800088C0000007B000000000000000000000000000000000000EFE7E7000000
      0000FFFFF700FFFFF700CE948C00E7945200FFFFF700FFF7EF00FFF7EF00FFF7
      EF00FFF7EF0000000000000000000000000000000000EFDED600FFFFFF00FFEF
      DE00FF8C6300D6CECE00FFFFFF00FFF7F700FFFFFF00E7CEBD00E7945200FF8C
      6300FFFFFF009494940000000000000000000000000000000000000000000000
      000000000000D6A59400EFDEB500FFF7DE00FFE7BD00FFEFBD00FFF7BD00FFF7
      C600FFF7C600EFD6AD00CE9C8400000000000000000000000000B5BD840010BD
      390031CE63005ADE8400DEEFAD00BDEFB500FFF7CE00FFCE6300FF9400008484
      0000007B00004A7B000000000000000000000000000000000000F7E7DE00FFF7
      F70000000000000000004200080094290000000000000000000000000000FFFF
      FF00D6C6BD000000000000000000000000000000000000000000EFDED600FFFF
      FF00FFFFF700FFEFB500FFCE9C00F7BD8400F7BD8400F7BD8400FFFFF700FFFF
      FF00ADADAD000000000000000000000000000000000000000000000000000000
      000000000000E7C6BD00D6B59400FFFFFF00FFFFEF00FFF7CE00FFDEA500FFE7
      B500FFF7C600D6AD8C00D6B5AD000000000000000000000000000000000010B5
      310031CE630084E79400CEEFBD00E7FFD600FFF7C600EFC6520084A52100C68C
      000010940000D684210000000000000000000000000000000000000000000000
      0000DED6D600F7FFF7000000000000000000F7FFF700FFFFFF00CEBDBD000000
      000000000000000000000000000000000000000000000000000000000000EFDE
      DE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00EFDE
      D600000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00D6AD9C00D6AD9C00EFDEBD00FFF7CE00FFEFC600EFD6
      AD00D6A58C00CEA59400FFFFFF000000000000000000000000000000000039A5
      8C00E7C6420084E79400E7EFAD00E7E7A5009CE78C0021C65200FF940000E77B
      0000FF8C00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000EFDEDE00F7E7DE00F7EFEF00FFFFFF00F7EFEF00EFDED600D6CECE000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00DEC6BD00CEA58C00CE9C8400CE9C8400D6A5
      9400DEC6BD00FFFFFF0000000000000000000000000000000000000000000000
      000000000000E7DEA50094DE84006BD67B0073C6630021BD3900FFB53100C6CE
      C600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000004A73000063940063636300000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      000000000000A57B7300A57B7300A57B7300A57B7300A57B7300A57B7300A57B
      7300A57B730000008400B57B7300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A5A5A500007373006BC6FF009494940000000000000000000000
      0000000000000000000000000000000000000000000000008400000084000000
      000000000000AD847B00FFD6C600FFD6CE00FFD6CE00FFD6C600FFD6C600FFD6
      C6000000840000008400B57B7300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      840000000000000000003131310000DEDE009C9C9C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      840000000000AD847B00FFE7C600FFD6AD00FFD6AD00FFD6AD00FFD6A5000000
      840000008400FFC6C600B57B7300000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000A57B7300A57B
      7300A57B7300A57B7300A57B7300A57B7300A57B7300A57B7300A57B73008C5A
      5A00000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00CECECE006B6B6B003131310000DEDE009C9C9C0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      840000008400BD8C8400FFE7C600FFE7C600FFE7C600FFE7C600FFE7C6000000
      8400FFE7C600FFC6C600B57B730000000000000000000000000000000000CE63
      0000CE630000CE630000CE630000CE630000CE630000CE630000CE630000CE63
      0000CE630000CE630000CE630000000000000000000000000000B5847300F7EF
      E700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE7008C5A
      5A00000000000000000000000000000000000000000084848400A5A5A500FFFF
      FF0084848400FFFFFF000031520000DEDE009494940084848400000000000000
      00000000000000000000000000000000000000000000AD847B00A57B73000000
      840000008400BD8C8400FFE7CE00FFD6AD00FFD6AD00FFD6AD00000084000000
      8400FFD6A500FFC6C600B57B730000000000000000000000000000000000FFFF
      FF00FFFFFF00FFF7EF00FFF7E700FFEFD600FFE7CE00FFE7CE00FFE7CE00FFDE
      BD00FFD6AD00FFD6AD00CE630000000000000000000000000000B5847300F7EF
      E700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE7008C5A
      5A000000000000000000000000000000000000000000848484006B6B6B00FFFF
      FF00E7E7E7009C9C9C003131310000DEDE0094949400FFFFFF00B5B5B5006363
      63008484840000000000000000000000000000000000AD847B00FFD6CE00FFD6
      CE000000840000008400FFEFD600FFEFD600FFEFD60000008400000084000000
      8400FFEFD600FFC6C600B57B730000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFF700FFFFEF00F7EFE700F7E7D600FFEFD600FFE7
      CE00FFDEBD00FFDEBD00CE630000000000000000000000000000BD8C8400F7EF
      E700FFCE9C00FFCE9C00FFCE9C00FFCE9C00FFCE9C00FFCE9C00F7EFE7008C5A
      5A00000000000000000000000000000000000000000084848400CECECE00FFFF
      FF0063636300CECECE003131310000DEDE0094949400FFFFFF00CECECE00FFFF
      FF00FFFFFF0084848400000000000000000000000000BD8C8400FFE7C600FFD6
      AD00FFD6AD00000084000000840000008400000084000000840000008400FFD6
      AD00FFD6AD00FFC6C600B57B730000000000000000000000000000000000FFFF
      FF00FFFFFF006384F700FFFFFF00FFFFF700A5390800A5390800FFEFD600009C
      CE00009CCE00FFDEBD00CE630000000000000000000000000000BD8C8400F7EF
      E700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE7009C6B
      6300000000000000000000000000000000000000000084848400949494006B6B
      6B0094949400FFFFFF003131310000DEDE0094949400FFFFFF00848484003131
      31006B6B6B00C6C6C600000000000000000000000000BD8C8400FFE7CE00FFE7
      CE00FFE7CE00CE9C84000000840000008400000084000000840000008400FFF7
      E700FFF7E700FFC6C600B57B730000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFF7EF00FFF7E700FFEF
      D600FFE7CE00FFE7CE00CE630000000000000000000000000000CE9C8400F7EF
      E700FFCE9C00FFCE9C00FFCE9C00FFCE9C00FFCE9C00FFCE9C00F7EFE7009C6B
      6300000000000000000000000000000000000000000084848400636363006363
      630063636300FFFFFF003131310000DEDE0094949400FFFFFF009C9C9C003131
      310063636300C6C6C600000000000000000000000000C6947B00FFEFD600FFD6
      AD00FFD6AD00CE9C840000008400000084000000840000008400FFF7EF00FFF7
      E700FFE7D600FFC6C600B57B730000000000000000000000000000000000FFFF
      FF00FFFFFF00B5B5B500FFFFFF00FFFFFF00B5B5B500ADADAD00FFF7EF00ADAD
      AD00BDADA500FFE7CE00CE630000000000000000000000000000CE9C8400F7EF
      E700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700F7EFE700A57B
      7300000000000000000000000000000000000000000084848400CECECE00FFFF
      FF00A5A5A500180052004A73FF0000DEDE006B8CFF006363630084848400FFFF
      FF00CECECE00B5B5B500000000000000000000000000CE9C8400FFEFDE00FFEF
      DE00FFEFDE00D6A58C000000840000008400000084000000840000008400FFF7
      EF00B5847300B5847300B584730000000000000000000000000000000000FFFF
      FF00FFFFFF00CE9C9C00FFFFFF00FFFFFF00E77B0000E77B0000FFFFF700009C
      0000009C0000FFEFD600CE630000000000000000000000000000DEAD8400FFF7
      F700FFCE9C00FFCE9C00FFCE9C00FFCE9C00FFCE9C00FFCE9C00F7EFE700A57B
      73000000000000000000000000000000000000000000848484009C9C9C00FFFF
      FF0031313100F7F7F7006B6B6B00004A73006B6B6B005252520094949400FFFF
      FF00C6C6C600B5B5B500000000000000000000000000CE9C8400FFF7E700FFD6
      AD00FFD6AD000000840000008400000084000000840000008400000084000000
      840000008400DEAD8400C6C6C60000000000000000000000000000000000FFFF
      FF00FFFFFF00CE9C9C00FFFFFF00FFFFFF00E77B0000E77B0000FFFFFF00009C
      0000009C0000FFF7E700CE630000000000000000000000000000DEAD8400FFFF
      FF00FFFFFF00FFFFFF00FFFFF700FFF7EF00FFF7EF00FFF7E700F7EFE700A57B
      730000000000000000000000000000000000000000008484840094949400FFFF
      FF00CECECE009494940031313100F7F7F7009C9C9C00FFFFFF006B6B6B00E7E7
      E700C6C6C600B5B5B500000000000000000000000000D6A58C00FFF7EF00FFF7
      EF00000084000000840000008400DEAD8C00DEAD8C00DEAD8C00000084000000
      840000008400000084000000840000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      F700FFF7EF00FFF7E700CE630000000000000000000000000000E7B58C00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFF700FFFFEF00B5847300B5847300B584
      7300000000000000000000000000000000000000000000000000000000008484
      8400B5B5B500FFFFFF00080808006B6B6B00F7F7F700FFFFFF0094949400FFFF
      FF00FFFFFF00B5B5B500000000000000000000000000DEAD8C00FFFFF7000000
      8400000084000000840000008400FFEFDE00F7B59400B5847300000000000000
      000000008400000084000000840000008400000000000000000000000000CE63
      0000CE630000CE630000CE630000CE630000CE630000CE630000CE630000CE63
      0000CE630000CE630000CE630000000000000000000000000000EFBD9400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFF700B5847300FFC67B00DEC6
      B500000000000000000000000000000000000000000000000000000000000000
      0000000000008484840084848400F7F7F70031313100FFFFFF00949494000808
      080052525200C6C6C60000000000000000000000000000008400000084000000
      84000000840000008400FFFFEF00B5847300B5847300B5847300000000000000
      0000000000000000840000008400000084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000EFBD9400DEAD
      8400DEAD8400DEAD8400DEAD8400DEAD8400DEAD8400B5847300E7CEBD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400CECECE008484
      840031313100C6C6C60000000000000000000000000000008400000084000000
      840000008400F7F7F700EFEFEF00B5847300EFC69C00C6C6C600000000000000
      0000000000000000840000008400000084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      840084848400B5B5B500000000000000000000000000E7B59400000084000000
      8400DEAD8C00DEAD8C00DEAD8C00B5847300C6C6C60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000052527B002929630021215A003129420063635A000000
      000000000000000000000000000000000000008484004AADC60000849C001873
      840000848400008C8400008C8400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000031398C0000008C0000009C0000009C000000A50000009C0000007B000000
      2900847B6B000000000000000000000000000084840052C6EF008CE7FF0073DE
      FF007BDEFF004AC6EF0029B5E700217BA5000084840000848400008484000000
      0000000000000000000000000000000000000000000000000000EFEFFF00216B
      AD00216BAD00216BAD00216BAD00216BAD00216BAD00216BAD00216BAD00216B
      AD003194C6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000031DE000031DE0000000000000000000000000000000000000000002121
      9C000000940000009C0000009C000000AD000000A50000009C00000094000000
      A50000003100949484000000000000000000008484007BCEEF00ADEFFF0084E7
      FF0084E7FF008CE7FF008CEFFF0073DEFF0039BDE7004A849C00427B8C000084
      8400000000000000000000000000000000000000000000000000089CC6001094
      EF003194C6003194C6003194C600089CC6001094EF001094EF001094EF00089C
      C600089CC600FFFFFF00000000000000000000000000000000000031DE000031
      DE00CED6FF0000000000000000000000000000000000000000009CADEF000031
      DE000031DE0000000000000000000000000000000000000000006B6BC6000000
      9C004A4ABD00B5B5EF000000B5000000BD000000B5002929B500C6C6EF000000
      A5000000A5000000210000000000000000000084840073D6FF00DEEFF7009CF7
      FF008CEFFF008CEFFF008CEFFF008CEFFF008CEFFF008CF7FF0094F7FF002994
      B500000000000000000000000000000000000000000000000000089CC6003194
      C600DEEFFF00DEE7FF00DEE7F700DEE7FF0031ADDE0010B5DE0010B5DE001094
      EF00089CC600FFFFFF0000000000000000000000000000000000000000000031
      DE000031DE000031DE000000000000000000000000006384F7000031DE000031
      DE000000000000000000000000000000000000000000000000000000AD000000
      B500B5B5B50000000000ADADEF000000B5002929BD00FFFFF700FFFFFF005252
      B5000000A50000007300A5A5940000000000008484008CD6EF0031B5E700FFFF
      FF0094F7FF0094F7FF0094F7FF009CFFFF00B5CED6008CEFFF0094FFFF009CF7
      FF000084840000000000000000000000000000000000000000001094EF003194
      C600FFFFFF0039ADE700ADE7F700DEE7FF00ADE7F70010B5DE0010B5DE001094
      EF00089CC600FFFFFF0000000000000000000000000000000000000000006384
      F7000031DE000031DE009CADEF0000000000000000000031DE000031DE00CED6
      FF000000000000000000000000000000000000000000BDBDF7000000AD000000
      CE000000BD00ADA5AD0000000000B5B5EF00EFEFFF00FFFFF7005252AD000000
      BD000000BD000000A5006B6B6B00000000000084840063D6F70042BDEF00D6EF
      F7009CFFFF00A5FFFF00A5FFFF008CF7FF00FFD69C00ADDEE7009CFFFF00CEFF
      FF001873840000848400000000000000000000000000000000001094EF0039AD
      E70094BDF700EFF7FF0052ADE700DEE7FF00DEE7FF00D6EFFF0042BDEF0010B5
      DE00089CC600FFFFFF0000000000000000000000000000000000000000000000
      0000CED6FF00295AF7000031DE009CADEF000031DE000031DE00000000000000
      000000000000000000000000000000000000000000009494E7000000C6000808
      D6000000D6000000C600C6C6D60000000000FFFFFF006363C6000000CE000000
      CE000000BD000000BD006363730000000000008484008CDEFF006BD6FF005AC6
      EF00E7F7FF00F7FFFF00DEFFFF00FFD6AD00FFEFB500FFE7B500F7D6AD00EFFF
      FF0042ADD600526B6300000000000000000000000000000000003194C60052AD
      E70052ADE70094BDF700EFF7FF0039ADE70063A5EF0031ADDE00EFF7FF0094BD
      F700216BAD00FFFFFF0000000000000000000000000000000000000000000000
      000000000000000000006384F7000031DE000031DE0000000000000000000000
      000000000000000000000000000000000000000000009494E7000000D6001818
      EF000000E7002121D600EFEFEF00FFFFFF0000000000A5A5EF000000CE000000
      D6000808D6000000BD0073737B0000000000008484009CE7FF008CEFFF008CEF
      FF0094F7FF0084CEE700D6B59400A5D6F700F7E7D600FFFFD600FFFFDE00FFEF
      D600FFFFFF0018ADD600008C8C000000000000000000000000003194C60052AD
      E70052ADE70052ADE700DEE7F70052ADE70031ADDE001094EF001094EF00FFFF
      FF00216BAD00FFFFFF0000000000000000000000000000000000000000000000
      0000000000009CADEF000031FF000031FF000031FF006384F700000000000000
      00000000000000000000000000000000000000000000BDC6FF000000E7002121
      FF003131E700FFFFEF00FFFFF7006363CE00B5B5BD0000000000B5B5F7000000
      EF000808D6000000C600ADADAD000000000000848400C6FFFF009CFFFF00A5FF
      FF008CE7F700FFCEA500FFDEB500FFDEA500DECEB500D6CEB500C6CEC600ADCE
      D60084BDDE009CD6E700008484000000000000000000000000003194C60063A5
      EF0063A5EF0052ADE700DEE7F7003194C60031ADDE00FFFFFF001094EF00FFFF
      FF00216BAD00FFFFFF0000000000000000000000000000000000000000000000
      00000031FF000031DE000031FF0000000000000000000031FF006384F7000000
      00000000000000000000000000000000000000000000000000001818F7003131
      FF00CECEC600FFFFE7005252C6000000F7000000D600A5A5A500FFFFEF006363
      D6000808E7000008B500000000000000000000848400CEFFFF009CFFFF009CFF
      FF00B5BDB500FFDEB500FFD6AD00FFD6AD00FFD6A500FFDEB500FFCEA500F7E7
      D600B59C7B00948C94000084840000000000000000000000000039ADE70063BD
      F70063A5EF0052ADE70063BDF700DEE7FF003194C6003194C60052ADE700EFF7
      FF00216BAD00FFFFFF0000000000000000000000000000000000000000000031
      FF000031FF000031FF00000000000000000000000000000000004273FF000031
      FF00000000000000000000000000000000000000000000000000737BFF004242
      FF006363EF006B6BBD000808FF000800F7000000FF001818E7006B6BAD002929
      FF000808F7009494BD000000000000000000008484008CCEE700B5F7FF00BDE7
      DE00FFEFDE00FFF7E700FFE7CE00FFD6B500FFDEB500FFDEAD00FFD6A500FFF7
      E700A5ADA500000000000000000000000000000000000000000052ADE7009CDE
      F70094BDF70063A5EF0063A5EF00DEE7F700FFFFFF00FFFFFF00FFFFFF003194
      C600216BAD00FFFFFF0000000000000000000000000000000000295AF7000031
      FF000031FF00CED6FF0000000000000000000000000000000000000000009CAD
      EF00CED6FF000000000000000000000000000000000000000000000000004242
      FF006B6BFF009CA5FF009CA5FF007B7BFF007373FF007373FF005A5AFF002929
      FF005A63D6000000000000000000000000000000000000848400299C9C000894
      9C00FFEFDE00FFFFF700FFF7EF00FFDEC600FFDEBD00FFDEB500FFF7E700A5A5
      940000848400000000000000000000000000000000000000000063BDF70094BD
      F70063BDF70063A5EF0063A5EF0052ADE70052ADE70052ADE70039ADE7003194
      C600216BAD00EFF7FF0000000000000000000000000000000000000000000031
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00007B84F7005A5AFF009C9CFF00A5ADFF009494FF005A5AFF003939FF009CA5
      F700000000000000000000000000000000000000000000000000008484000084
      8400A5B5A500FFFFFF00FFFFF700FFFFF700FFDEAD00FFF7E700FFFFF7000084
      840000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00EFF7FF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000BDBDFF00ADADFF00ADADFF00CED6F700000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFF7DE00FFFFFF00FFDEAD00FFFFFF00FFD6C6008CADAD000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000004000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000008484006BB5A500EFD6AD00FFDEBD0000848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000D00000000100010000000000800600000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFEF801FC7F0000FFFCF801F83F0000
      FFF9E001F81F0000FE73E001FC1F0000FC078001F83F0000FC0F8001F81F0000
      FC0F8001F81F0000F8078001FC1F0000F0078001FE0F0000800F0001C3070000
      003F0001C1030000007F0001C003000000FF8001C003000081FF8003E0030000
      81FF8007F0070000C3FF801FF80F00008000FFFFFFFFFFFF00008800FFFFFFFF
      00008800E007BBBF00008800C003110F0000FFFFC003110700008803C0031103
      38008803C003110128000803C003110038004FFFC003110000008000C0031101
      00008800C003110300000800C003110700004FFFC003110F0000C7FFE007BBBF
      0000E7FFFFFFFFFFFFFFFFFFFFFFFFFF80018000C007803F000000008007003F
      000000008007000F000000008007000F00000000800700030000000080070003
      0000000080070003000000008007000300000000800700030000000080070003
      00000000800100010000000080010001000000008001C000000000008000C000
      000000008000F000FFFF0000FE04F002FFFCFFFF80008000FFF80FFF00000000
      C0F007FF00000000C001833F000000000003C00F000000000003E00300000000
      0003F001000000000001E001000000000001C000000000000001C00100000000
      00018001000000000007000100000000003FC00100000000003FC00100000000
      07FFE00300000000FFFFF07FFFFFFFFF9FFF9FFF9FFF9FFF0FFF0FFF0FFF0FFF
      07FF07FF07FF07FF83FF83FF83FF83F7C1FFC1FFC1FFC1E3E003E003E003E001
      F001F001F001F001F801F001F001F801F801E001F001F001F801E001E001E001
      F8018001C001C001F801C001F0018001F801E001F0010001F801E001F8010001
      FC03F003FC030003FFFFFFFFFF3F03FFF801FC01FFFFFFFFF801C001FF1FF8FF
      F8018001FF0FF07FF8018001FF07E07F80018001FF03E0FF80018001FF83C1FF
      800180010041C200800180010061860080018001006187008001800303F087C0
      8001800700F087808003800700618200803F800710018008803FC0071801C018
      803FF83F1C03E07C807FFFFFFF8FFFFFE0038007F007FFFF80030007F007F1C7
      00030007F007E08300030007F007E48380030007C007E083C0030007C006F007
      C0030007C006F80FC0030007C01AFE3FC00300070019FC1FC00300070018FC0F
      C0030005000BF88FC003FFFD0067F88FC003FFEB0063F1C7C007FFE7002FF1E7
      C00FFFE3001FF3E7FFFFFFFFFF8FFFFFFFFF801FC003FC3F8001000F8003F007
      800100078003C001800100038003800180010001800380038001000080038003
      80010000800380038001000080038003800100008003C007800100008003F007
      800100008003F01F800100008003E03F8001C0008003E03F8001C0008003E03F
      FFFFF0008007C07FFFFFF001FFFFC0FFFFFFE00387FF87FFFFFFC003007F007F
      C3FFC003001F001FC0FFC003001F001FC01FC003000F000FC01FC003000F000F
      C01FC00300070007C01FC00300070007C003C00300070007C001C00300030003
      C011C00300010001C01FC00307870387C01FC003FF8F018FC01FC003FE0FC30F
      C01FC007F81FE01FFF3FFFFFFE7FF07FFFFFFFFF9FFFFFFFFF3FF83F0FFFFFFF
      FE3FF00F07FFFFFFF01FE00783FFF80FE007C003C1FFF007C0078003E003E003
      80038003F001C00380038001F801C00180038001F801C00180038001F801C001
      80038003F801C003D0078003F801C003CCE7C007F801E003F31FE00FF801E007
      FFFFF01FFC03F80FFFFFFFFFFFFFFFFFF83FB801FFFFFFFFF87F9801FFFFFFFF
      8C7FC801FFFFC00F807FE001E001C00F803F8001E001C00F80078001E001C00F
      80038001E001C00F80038001E001C00F80038001E001C00F80038001E001C00F
      80038001E001C00F80038001E001C00FE0038030E001C00FF8038038FFFFC01F
      FF038038FFFFFFFFFFE3807FFFFFFFFFFFFFFFFFFFFFFFFFFC1F01FFFFFFFFFF
      F007001FC007FFF3E003000FC003C7C7C003000FC003E38FC4010007C003E18F
      82010003C003F03F81010003C003FC7F80810001C003F83F80410001C003F19F
      C0030001C003E3CFC0030007C003C3E7E0078007C003EFFFF00FC00FE007FFFF
      FC3FF81FFFFFFFFFFFFFF83FFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object FontDialog1: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 224
    Top = 128
  end
  object mMain: TMainMenu
    Images = ButtonsImages
    Left = 312
    Top = 64
    object miFile: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Action = ActFileNew
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object Open1: TMenuItem
        Action = ActFileOpen
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Save1: TMenuItem
        Action = ActFileSave
      end
      object SaveAs1: TMenuItem
        Action = ActFileSaveAs
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = ActFileExit
      end
    end
    object miEdit: TMenuItem
      Caption = '&Edit'
      object Undo1: TMenuItem
        Action = ActEditUndo
      end
      object Redo1: TMenuItem
        Action = ActEditRedo
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Cut1: TMenuItem
        Action = ActEditCut
      end
      object Copy1: TMenuItem
        Action = ActEditCopy
      end
      object Paste1: TMenuItem
        Action = ActEditPaste
      end
      object Delete1: TMenuItem
        Action = ActEditDelete
      end
      object Selectall1: TMenuItem
        Action = ActEditSelectAll
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object Find1: TMenuItem
        Action = ActEditFind
      end
      object Replace1: TMenuItem
        Action = ActEditReplace
      end
      object Findnext1: TMenuItem
        Action = ActEditFindNext
      end
      object Findprevious1: TMenuItem
        Action = ActEditFindPrev
      end
    end
    object miView: TMenuItem
      Caption = '&View'
      object miViewViewMessagesWindow: TMenuItem
        Action = ActViewMessages
      end
      object miViewCodeTreeWindow: TMenuItem
        Action = ActViewCodeTree
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object miViewEditorFonts: TMenuItem
        Action = AFonts
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object miViewRefreshCodeTree: TMenuItem
        Caption = 'Refresh Code Tree'
        Hint = 'Refresh Code Tree'
        ImageIndex = 36
      end
    end
    object miRun: TMenuItem
      Caption = '&Run'
      object Compile1: TMenuItem
        Action = ActRunCompile
      end
      object Execute1: TMenuItem
        Action = ActRunExecute
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Stepinto1: TMenuItem
        Action = ActRunStepInto
      end
      object Stepover1: TMenuItem
        Action = ActRunStepOver
      end
      object Runtocursor1: TMenuItem
        Action = ActRunRunToCursor
      end
      object Rununtilreturn1: TMenuItem
        Action = ActRunRunUntilReturn
      end
      object Showexecutionpoint1: TMenuItem
        Action = ActRunShowExecutionPoint
      end
      object Programpause1: TMenuItem
        Action = ActRunProgramPause
      end
      object Programreset1: TMenuItem
        Action = ActRunProgramReset
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object Togglebreakpoint1: TMenuItem
        Action = ActRunToggleBreakpoint
      end
      object Clearallbreakpoints1: TMenuItem
        Action = ActRunClearAllBreakpoints
      end
      object Evaluate1: TMenuItem
        Action = ActRunEvaluate
      end
      object Callstack1: TMenuItem
        Action = ActRunCallStack
      end
    end
    object miHelp: TMenuItem
      Caption = '&Help'
      object Help1: TMenuItem
        Action = ActHelp
      end
      object About1: TMenuItem
        Action = ActAbout
      end
    end
  end
  object ilSymbolImages: TImageList
    Left = 480
    Top = 272
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
  object SynEditSearch: TSynEditSearch
    Left = 304
    Top = 128
  end
  object synCodeProposal: TSynCompletionProposal
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
    Editor = SynEdit
    Left = 480
    Top = 352
  end
  object synParamProposal: TSynCompletionProposal
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
    Editor = SynEdit
    TimerInterval = 500
    Left = 576
    Top = 352
  end
  object synSymbolHint: TSynCompletionProposal
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
    Editor = SynEdit
    TimerInterval = 500
    Left = 664
    Top = 352
  end
  object tInsightCompileTimer: TTimer
    Enabled = False
    OnTimer = tInsightCompileTimerTimer
    Left = 448
    Top = 88
  end
end
