object f_Configure: Tf_Configure
  Left = 0
  Top = 0
  Anchors = [akLeft, akBottom]
  Caption = 'Configure dzContextMenu'
  ClientHeight = 353
  ClientWidth = 729
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    729
    353)
  PixelsPerInch = 96
  TextHeight = 13
  object l_MenuCaption: TLabel
    Left = 8
    Top = 8
    Width = 66
    Height = 13
    Caption = 'Menu Caption'
  end
  object l_Extension: TLabel
    Left = 256
    Top = 56
    Width = 47
    Height = 13
    Caption = 'Extension'
  end
  object l_FileTypes: TLabel
    Left = 8
    Top = 56
    Width = 48
    Height = 13
    Caption = 'File Types'
  end
  object b_Cancel: TButton
    Left = 647
    Top = 320
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 11
  end
  object b_OK: TButton
    Left = 567
    Top = 320
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 10
    OnClick = b_OKClick
  end
  object ed_MenuCaption: TEdit
    Left = 8
    Top = 24
    Width = 233
    Height = 21
    TabOrder = 0
  end
  object lb_Sections: TListBox
    Left = 8
    Top = 72
    Width = 233
    Height = 209
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 1
    OnClick = lb_SectionsClick
  end
  object sg_Items: TdzVirtualStringGrid
    Left = 256
    Top = 104
    Width = 465
    Height = 177
    ColumnHeaders.Strings = (
      'Caption'
      'Executable')
    OnGetNonfixedCellData = sg_ItemsGetNonfixedCellData
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 2
    TabOrder = 6
    ColWidths = (
      145
      300)
  end
  object b_AddSection: TButton
    Left = 8
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Add ...'
    TabOrder = 2
  end
  object b_DeleteSection: TButton
    Left = 168
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 4
    OnClick = b_DeleteSectionClick
  end
  object b_AddItem: TButton
    Left = 256
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Add ...'
    TabOrder = 7
  end
  object b_DeleteItem: TButton
    Left = 416
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Delete'
    TabOrder = 9
  end
  object b_EditItem: TButton
    Left = 336
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Edit ...'
    TabOrder = 8
  end
  object b_EditSection: TButton
    Left = 88
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Edit ...'
    TabOrder = 3
    OnClick = b_EditSectionClick
  end
  object ed_Extension: TEdit
    Left = 256
    Top = 72
    Width = 121
    Height = 21
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 5
  end
end
