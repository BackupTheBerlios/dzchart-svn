object AutoSaveForm: TAutoSaveForm
  Left = 322
  Top = 214
  AutoScroll = False
  BorderIcons = [biSystemMenu]
  Caption = 'Auto Save Properties'
  ClientHeight = 248
  ClientWidth = 349
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object b_Cancel: TButton
    Left = 272
    Top = 216
    Width = 66
    Height = 26
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object PageControl1: TPageControl
    Left = 7
    Top = 7
    Width = 332
    Height = 195
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'General'
      object Label2: TLabel
        Left = 91
        Top = 90
        Width = 36
        Height = 13
        Caption = 'minutes'
      end
      object Icon: TImage
        Left = 13
        Top = 13
        Width = 26
        Height = 26
        Picture.Data = {
          055449636F6E0000010001002020100000000000E80200001600000028000000
          2000000040000000010004000000000080020000000000000000000000000000
          0000000000000000000080000080000000808000800000008000800080800000
          C0C0C000808080000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000
          FFFFFF0000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000008000000
          08000000000000000000000CC077777FF0C00000000000000000000CC077777F
          F0C00000000000000000000CC077777FF0C00000000000000000000CC0000000
          00C00000000000000000000CCCCCCCCCCCC00000000000000000000C00000000
          0CC00000000000000000000C0FFFFFFFF0C00000000000000FF00F0C0FFFFFFF
          F0C0000000000000FFF00F0C0FFFFFFFF0C00000000000FF0FF00F0C0FFFFFFF
          F0C0000000000FFFFFFFFF0C0FFFFFFFF0000000000008FFFFFFFF0C0FFFFFFF
          F07000000000FF8FFFFFFF0000000000000000000000FFFFFFFFFFFFFFFF0000
          000000000000000FFFF00FFFF0000000000000000000000FFFF00FFFF0000000
          000000000000FFFFFF0F00FFFFFF0000000000000000FFFFF0FFF00FFFFF0000
          0000000000000F8F0FFFFF00F8F000000000000000000777FFFFFFFFFF800000
          00000000000000008FF00FF8FF0000000000000000000FFFFFF00FFF80000000
          00000000000000000FF00FF00000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFE000FFFFC000FFFFC000FFFFC000F
          FFFC000FFFFC000FFFFC000FFFFC000FFF80000FFE00000FFC00000FF800000F
          F800000FF000000FF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF8001FFF
          F8001FFFF8003FFFF8007FFFFF81FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFF}
      end
      object Label3: TLabel
        Left = 65
        Top = 12
        Width = 157
        Height = 16
        AutoSize = False
        Caption = 'Auto Save Wizard'
      end
      object Bevel1: TBevel
        Left = 7
        Top = 52
        Width = 312
        Height = 7
        Anchors = [akLeft, akTop, akRight]
        Shape = bsTopLine
      end
      object Label1: TLabel
        Left = 184
        Top = 64
        Width = 107
        Height = 13
        Caption = 'Position in Tools Menu'
      end
      object l_Version: TLabel
        Left = 64
        Top = 26
        Width = 69
        Height = 13
        Caption = '0.00.000.0000'
      end
      object ud_Interval: TUpDown
        Left = 72
        Top = 88
        Width = 12
        Height = 21
        Associate = ed_Interval
        Min = 1
        Max = 60
        Position = 5
        TabOrder = 2
      end
      object ed_Interval: TEdit
        Left = 32
        Top = 88
        Width = 40
        Height = 21
        TabOrder = 1
        Text = '5'
      end
      object chk_SaveTimed: TCheckBox
        Left = 16
        Top = 64
        Width = 153
        Height = 16
        Caption = '&Save Files Every'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object chk_SaveOnCompile: TCheckBox
        Left = 16
        Top = 128
        Width = 161
        Height = 17
        Caption = 'Save Before &Compile'
        TabOrder = 3
      end
      object ud_ToolPosition: TUpDown
        Left = 224
        Top = 88
        Width = 11
        Height = 21
        Associate = ed_ToolPosition
        Min = 1
        Max = 5
        Position = 1
        TabOrder = 4
      end
      object ed_ToolPosition: TEdit
        Left = 184
        Top = 88
        Width = 40
        Height = 21
        TabOrder = 5
        Text = '1'
      end
      object chk_ShowMessage: TCheckBox
        Left = 184
        Top = 128
        Width = 137
        Height = 17
        Caption = 'Show Message View'
        TabOrder = 6
        Visible = False
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Help'
      object Memo1: TMemo
        Left = 7
        Top = 7
        Width = 312
        Height = 156
        TabStop = False
        BorderStyle = bsNone
        Color = clBtnFace
        Ctl3D = False
        Lines.Strings = (
          'The Auto Save Wizard can save your files at regular intervals, '
          
            'helping to ensure that you do not lose information if Delphi or ' +
            'your '
          'system were to crash.'
          ''
          
            'At regular intervals, every modified file is saved into a tempor' +
            'ary '
          
            'file, whose name is formed by inserting a tilde (~) at the start' +
            ' of the '
          
            'file name and which is marked *hidden* so it won'#39't show up in th' +
            'e '
          'file listing.'
          ''
          'When you open a project, if there are any auto-saved files, you '
          
            'are asked if you want to restore the files as soon as you open t' +
            'he'
          'file in the editor.')
        ParentCtl3D = False
        ReadOnly = True
        TabOrder = 0
        WantReturns = False
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Files'
      ImageIndex = 2
      object FileList: TListBox
        Left = 0
        Top = 0
        Width = 324
        Height = 167
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnDblClick = FileListDblClick
      end
    end
  end
  object b_Ok: TButton
    Left = 200
    Top = 216
    Width = 66
    Height = 26
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
end
