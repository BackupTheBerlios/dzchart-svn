object f_Delphi7HelpForBds: Tf_Delphi7HelpForBds
  Left = 354
  Top = 151
  Caption = 'Delphi 7 help for BDS Wizard'
  ClientHeight = 234
  ClientWidth = 444
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    444
    234)
  PixelsPerInch = 96
  TextHeight = 13
  object l_Blurb: TLabel
    Left = 8
    Top = 8
    Width = 428
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = 
      'This is the first time you have called the Delphi7 help for BDS ' +
      'wizard. Please specify below  the action you want to execute wit' +
      'h this shortcut.'
    WordWrap = True
  end
  object ed_Helpfile: TEdit
    Left = 24
    Top = 72
    Width = 381
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnClick = ed_HelpfileClick
  end
  object b_Select: TButton
    Left = 411
    Top = 70
    Width = 25
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = b_SelectClick
  end
  object b_OK: TButton
    Left = 280
    Top = 200
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object b_Cancel: TButton
    Left = 360
    Top = 200
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object rb_Winhelp: TRadioButton
    Left = 8
    Top = 48
    Width = 397
    Height = 17
    Caption = 'Open a *.hlp file'
    Checked = True
    TabOrder = 5
    TabStop = True
    OnClick = rb_WinhelpClick
  end
  object rb_WebUrl: TRadioButton
    Left = 8
    Top = 112
    Width = 397
    Height = 17
    Caption = 
      'Open a Web URL (the current word from the editor will be appende' +
      'd)'
    TabOrder = 6
    OnClick = rb_WebUrlClick
  end
  object rb_DoNothing: TRadioButton
    Left = 8
    Top = 176
    Width = 153
    Height = 17
    Caption = 'Do nothing'
    TabOrder = 4
    TabStop = True
  end
  object cmb_WebUrl: TComboBox
    Left = 24
    Top = 136
    Width = 381
    Height = 21
    ItemHeight = 13
    TabOrder = 7
    Text = 'http://www.google.com/search?q=delphi+'
    OnClick = cmb_WebUrlClick
    OnEnter = cmb_WebUrlEnter
    Items.Strings = (
      'http://www.google.com/search?q=delphi+'
      'http://groups.google.com/groups/search?q=delphi+')
  end
  object od_HelpFile: TOpenDialog
    DefaultExt = 'hlp'
    Filter = 'help files (*.hlp)|*.hlp|all files|*.*'
    FilterIndex = 0
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 168
    Top = 80
  end
end
