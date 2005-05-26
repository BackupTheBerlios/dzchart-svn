{: Declares TdzCustomConfigListBox and TdzConfigListBox based on TCustomListBox
   but implementing the IdzConfigControl interface. The implementation
   is based on the dzConfigControlTemplate pseudo template. }
unit c_dzConfigListBox;

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
  _DZ_CONFIG_CONTROL_ANCESTOR_ = TCustomListBox;
const
  // It is used to display an integer type config setting by default
  _DZ_CONFIG_CONTROL_STORAGE_TYPE_ = csInteger;
  // It supports two types of storage: Integer and String
  _DZ_CONFIG_CONTROL_STORAGE_TYPES_: TdzConfigStorageTypes = [csInteger, csString];
  // It is supposed to have a name prefix 'lb_'
  _DZ_CONFIG_CONTROL_PREFIX_ = LISTBOX_PREFIX;
{$INCLUDE 'dzConfigControlTemplate.tpl'}

type
  {: A TCustomListBox descendant implementing the IdzConfigControl interface }
  TdzCustomConfigListBox = class(_DZ_CONFIG_CONTROL_, IdzConfigControl)
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
  end;

  {: a descendant of TdzCustomConfigListBox publishing the usual properties of
     TComboBox and the additional properties ConfigStorageType and ConfigStorageName }
  TdzConfigListBox = class(TdzCustomConfigListBox)
  published
{$IFDEF linux}
    property RowLayout; { Must be published before Rows }
    property ColumnLayout; { Must be published before Columns }
    property Rows;
{$ENDIF linux}
    property Style; { Must be published before Items }
    property Align;
    property Anchors;
    property BorderStyle;
    property Color{$IFDEF linux}default clBase{$ENDIF};
    property Columns;
    property Constraints;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property ItemHeight;
    property Items;
    property MultiSelect;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Visible;
  published // events
{$IFDEF linux}
    property OnKeyString;
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnDrawItem;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    // new properties
    property ConfigStorageType;
    property ConfigStorageName;
  end;

implementation

// The implementation part of the template
{$INCLUDE 'dzConfigControlTemplate.tpl'}

{ TdzCustomConfigListBox }

procedure TdzCustomConfigListBox.ReadSetting(const _Path: string; const _Reader: IdzConfigReader);
var
  s: string;
  Idx: integer;
begin
  case ConfigStorageType of
    csString:
      begin
        _Reader.GetSetting(_Path, ConfigStorageName, s);
        for Idx := 0 to Items.Count - 1 do
          if Items[Idx] = s then
            begin
              ItemIndex := Idx;
              exit;
            end;
        ItemIndex := -1;
      end;
    csInteger:
      begin
        _Reader.GetSetting(_Path, ConfigStorageName, Idx);
        ItemIndex := Idx;
      end;
  end;
end;

procedure TdzCustomConfigListBox.WriteSetting(const _Path: string; const _Writer: IdzConfigWriter);
var
  Idx: integer;
begin
  case ConfigStorageType of
    csString:
      begin
        Idx := ItemIndex;
        if idx <> -1 then
          _Writer.StoreSetting(_Path, ConfigStorageName, Items[Idx])
        else
          _Writer.StoreSetting(_Path, ConfigStorageName, '');
      end;
    csInteger:
      _Writer.StoreSetting(_Path, ConfigStorageName, ItemIndex);
  end;
end;

end.

