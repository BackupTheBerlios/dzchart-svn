{: Declares TdzCustomConfigRadioGroup and TdzConfigRadioGroup based on TCustomRadioGroup
   but implementing the IdzConfigControl interface. The implementation
   is based on the dzConfigControlTemplate pseudo template. }
unit c_dzConfigRadioGroup;

interface

uses
  SysUtils,
  Classes,
{$IFDEF linux}
  QControls,
  QStdCtrls,
  QExtCtrls,
{$ENDIF linux}
{$IFDEF mswindows}
  Controls,
  StdCtrls,
  ExtCtrls,
{$ENDIF mswindows}
  u_dzConfigCommon;

// This is the implementation part of the template
{$DEFINE DZ_CONFIG_CONTROL_TEMPLATE}
type
  // the control's ancestor is TCustomRadioGroup
  _DZ_CONFIG_CONTROL_ANCESTOR_ = TCustomRadioGroup;
const
  // It is used to display an integer type config setting by default
  _DZ_CONFIG_CONTROL_STORAGE_TYPE_ = csInteger;
  // It supports two types of storage: Integer and String
  _DZ_CONFIG_CONTROL_STORAGE_TYPES_: TdzConfigStorageTypes = [csInteger, csString];
  // It is supposed to have a name prefix 'rg_'
  _DZ_CONFIG_CONTROL_PREFIX_ = RADIOGROUP_PREFIX;
{$INCLUDE 'dzConfigControlTemplate.tpl'}

type
  {: A TCustomRadioGroup descendant implementing the IdzConfigControl interface }
  TdzCustomConfigRadioGroup = class(_DZ_CONFIG_CONTROL_, IdzConfigControl)
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

  {: a descendant of TdzCustomConfigRadioGroup publishing the usual properties of
     TRadioGroup and the additional properties ConfigStorageType and ConfigStorageName }
  TdzConfigRadioGroup = class(TdzCustomConfigRadioGroup)
  published // properties
{$IFDEF linux}
    property Alignment default taLeftJustify;
    property Bitmap;
    property ColumnLayout;
    property Masked default False;
{$ENDIF linux}
    property Items;
    property Align default alNone;
    property Anchors;
    property Caption;
    property Color;
    property Columns;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ItemIndex;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint default False;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnStartDrag;
    // new properties
    property ConfigStorageType;
    property ConfigStorageName;
  end;

implementation

// The implementation part of the template
{$INCLUDE 'dzConfigControlTemplate.tpl'}

{ TdzCustomConfigRadioGroup }

procedure TdzCustomConfigRadioGroup.ReadSetting(const _Path: string; const _Reader: IdzConfigReader);
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

procedure TdzCustomConfigRadioGroup.WriteSetting(const _Path: string; const _Writer: IdzConfigWriter);
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

