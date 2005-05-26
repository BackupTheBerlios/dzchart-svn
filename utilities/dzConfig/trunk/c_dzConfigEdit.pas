{: Declares TdzCustomConfigEdit and TdzConfigEdit based on TCustomCheckbox
   but implementing the IdzConfigControl interface. The implementation
   is based on the dzConfigControlTemplate pseudo template. }
unit c_dzConfigEdit;

interface

uses
  SysUtils,
  Classes,
{$IFDEF linux}
  QControls,
  QStdCtrls,
{$ENDIF linux}
{$IFDEF mswindows}
  Controls,
  StdCtrls,
{$ENDIF}
  u_dzConfigCommon;

// This is the implementation part of the template
{$DEFINE DZ_CONFIG_CONTROL_TEMPLATE}
type
  // the control's ancestor is TCustomEdit
  _DZ_CONFIG_CONTROL_ANCESTOR_ = TCustomEdit;
const
  // It is used to display a string type config setting
  _DZ_CONFIG_CONTROL_STORAGE_TYPE_ = csString;
  // It supports only one type of storage: String
  _DZ_CONFIG_CONTROL_STORAGE_TYPES_: TdzConfigStorageTypes = [csString];
  // It is supposed to have a name prefix 'ed_'
  _DZ_CONFIG_CONTROL_PREFIX_ = EDIT_PREFIX;
{$INCLUDE 'dzConfigControlTemplate.tpl'}

type
  {: A TCustomEdit descendant implementing the IdzConfigControl interface }
  TdzCustomConfigEdit = class(_DZ_CONFIG_CONTROL_, IdzConfigControl)
  protected
    {: Reads a string setting from the config reader.
       @param(Path) is the path to be used for the setting
       @param(Reader) is a IdzConfigReader interface for doing the actual
                      reading. }
    procedure ReadSetting(const _Path: string; const _Reader: IdzConfigReader); override;
    {: Writes a string setting to the config writer.
       @param(Path) is the path to be used for the setting
       @param(Reader) is a IdzConfigReader interface for doing the actual
                      reading. }
    procedure WriteSetting(const _Path: string; const _Writer: IdzConfigWriter); override;
  end;

type
  {: a descendant of TdzCustomConfigEdit publishing the usual properties of
     TEdit and the additional properties ConfigStorageType and ConfigStorageName }
  TdzConfigEdit = class(TdzCustomConfigEdit)
  public
{$IFDEF linux}
    property CursorPos;
  published
    property Alignment;
    property EchoMode;
{$ENDIF linux}
  published // properties
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
  published // events
{$IFDEF linux}
    property OnKeyString;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnReturnPressed;
{$ENDIF linux}
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
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

{ TdzCustomConfigEdit }

procedure TdzCustomConfigEdit.ReadSetting(const _Path: string; const _Reader: IdzConfigReader);
var
  s: string;
begin
  case ConfigStorageType of
    csString:
      begin
        _Reader.GetSetting(_Path, ConfigStorageName, s);
        Text := s;
      end;
  end;
end;

procedure TdzCustomConfigEdit.WriteSetting(const _Path: string; const _Writer: IdzConfigWriter);
begin
  case ConfigStorageType of
    csString:
      _Writer.StoreSetting(_Path, ConfigStorageName, Text);
  end;
end;

end.

