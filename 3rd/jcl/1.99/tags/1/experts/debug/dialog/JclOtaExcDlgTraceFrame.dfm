inherited JclOtaExcDlgTracePage: TJclOtaExcDlgTracePage
  object LabelPreview: TLabel
    Left = 303
    Top = 7
    Width = 50
    Height = 13
    Caption = 'RsPreview'
  end
  object CheckBoxRawData: TCheckBox
    Left = 56
    Top = 57
    Width = 233
    Height = 17
    Caption = 'RsRawData'
    TabOrder = 0
    OnClick = CheckBoxClick
  end
  object CheckBoxModuleName: TCheckBox
    Left = 56
    Top = 95
    Width = 233
    Height = 17
    Caption = 'RsModuleName'
    TabOrder = 1
    OnClick = CheckBoxClick
  end
  object CheckBoxCodeDetails: TCheckBox
    Left = 56
    Top = 175
    Width = 233
    Height = 17
    Caption = 'RsCodeDetails'
    TabOrder = 2
    OnClick = CheckBoxClick
  end
  object CheckBoxVirtualAddress: TCheckBox
    Left = 56
    Top = 214
    Width = 233
    Height = 17
    Caption = 'RsVirtualAddress'
    TabOrder = 3
    OnClick = CheckBoxClick
  end
  object CheckBoxModuleOffset: TCheckBox
    Left = 56
    Top = 136
    Width = 233
    Height = 17
    Caption = 'RsModuleOffset'
    TabOrder = 4
    OnClick = CheckBoxClick
  end
  object MemoStack: TMemo
    Left = 303
    Top = 26
    Width = 313
    Height = 263
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 6
    WordWrap = False
  end
  object CheckBoxStackList: TCheckBox
    Left = 32
    Top = 26
    Width = 257
    Height = 17
    Caption = 'RsStackList'
    TabOrder = 5
    OnClick = CheckBoxStackListClick
  end
end
