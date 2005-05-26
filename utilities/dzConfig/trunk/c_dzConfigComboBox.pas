{: Declares TdzCustomConfigComboBox and TdzConfigComboBox based on TCustomComboBox
   but implementing the IdzConfigControl interface. The implementation
   is based on the dzConfigControlTemplate pseudo template. }
unit c_dzConfigComboBox;

interface

uses
  SysUtils,
  Classes,
{$IFDEF linux}
  QControls,
  QStdCtrls,
  QGraphics,
{$ENDIF linux}
{$IFDEF mswindows}
  Controls,
  StdCtrls,
  Graphics,
{$ENDIF mswindows}
  u_dzConfigCommon;

// This is the implementation part of the template
{$DEFINE DZ_CONFIG_CONTROL_TEMPLATE}
type
  // the control's ancestor is TCustomComboBox
  _DZ_CONFIG_CONTROL_ANCESTOR_ = TCustomComboBox;
const
  // It is used to display an integer type config setting by default
  _DZ_CONFIG_CONTROL_STORAGE_TYPE_ = csInteger;
  // It supports two types of storage: Integer and String
  _DZ_CONFIG_CONTROL_STORAGE_TYPES_: TdzConfigStorageTypes = [csInteger, csString];
  // It is supposed to have a name prefix 'cmb_'
  _DZ_CONFIG_CONTROL_PREFIX_ = COMBOBOX_PREFIX;
{$INCLUDE 'dzConfigControlTemplate.tpl'}

type
  {: A TCustomComboBox descendant implementing the IdzConfigControl interface }
  TdzCustomConfigComboBox = class(_DZ_CONFIG_CONTROL_, IdzConfigControl)
  protected
    {: Reads an integer or string setting from the config reader. (depending on the StorageType property)
       @param(Path) is the path to be used for the setting
       @param(Reader) is a IdzConfigReader interface for doing the actual
                      reading. }
    procedure ReadSetting(const _Path: string; const _Reader: IdzConfigReader); override;
    {: Writes an intger or string setting to the config writer. (depending on the StorageType property)
       @param(Path) is the path to be used for the setting
       @param(Reader) is a IdzConfigReader interface for doing the actual
                      reading. }
    procedure WriteSetting(const _Path: string; const _Writer: IdzConfigWriter); override;
  public
  end;

  {: a descendant of TdzCustomConfigComboBox publishing the usual properties of
     TComboBox and the additional properties ConfigStorageType and ConfigStorageName }
  TdzConfigComboBox = class(TdzCustomConfigComboBox)
  published // properties
{$IFDEF linux}
    property Duplicates;
    property InsertMode;
    property MaxItems;
{$ENDIF linux}
    property Style; { Must be published before Items }
    property Anchors;
    property AutoComplete;
    property CharCase;
    property Color{$IFDEF linux}default clBase{$ENDIF linux};
    property Constraints;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property Items;
    property ItemIndex;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
  published // events
{$IFDEF linux}
    property OnHighlighted;
    property OnKeyString;
{$ENDIF linux}
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDrag;
  published // new properties
    property ConfigStorageType;
    property ConfigStorageName;
  end;

implementation

// The implementation part of the template
{$INCLUDE 'dzConfigControlTemplate.tpl'}

{ TdzCustomConfigComboBox }

procedure TdzCustomConfigComboBox.ReadSetting(const _Path: string; const _Reader: IdzConfigReader);
var
  s: string;
  Idx: integer;
begin
  case ConfigStorageType of
    csString:
      begin
        _Reader.GetSetting(_Path, ConfigStorageName, s);
        Text := s;
      end;
    csInteger:
      begin
        _Reader.GetSetting(_Path, ConfigStorageName, Idx);
        ItemIndex := Idx;
      end;
  end;
end;

procedure TdzCustomConfigComboBox.WriteSetting(const _Path: string; const _Writer: IdzConfigWriter);
begin
  case ConfigStorageType of
    csString:
      _Writer.StoreSetting(_Path, ConfigStorageName, Text);
    csInteger:
      _Writer.StoreSetting(_Path, ConfigStorageName, ItemIndex);
  end;
end;

end.

