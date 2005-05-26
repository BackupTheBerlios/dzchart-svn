{: Declares TdzCustomConfigCheckbox and TdzConfigCheckbox based on TCustomCheckbox
   but implementing the IdzConfigControl interface. The implementation
   is based on the dzConfigControlTemplate pseudo template. }
unit c_dzConfigCheckBox;

interface

uses
  SysUtils,
  Classes,
{$IFDEF linux}
  QControls,
  QStdCtrls,
{$ENDIF Linux}
{$IFDEF mswindows}
  Controls,
  StdCtrls,
{$ENDIF mswindows}
  u_dzConfigCommon;

// This is the implementation part of the template
{$DEFINE DZ_CONFIG_CONTROL_TEMPLATE}
type
  // the control's ancestor is TCustomCheckBox
  _DZ_CONFIG_CONTROL_ANCESTOR_ = TCustomCheckBox;
const
  // It is used to display a boolean type config setting
  _DZ_CONFIG_CONTROL_STORAGE_TYPE_ = csBoolean;
  // It supports only one type of storage: Boolean
  _DZ_CONFIG_CONTROL_STORAGE_TYPES_: TdzConfigStorageTypes = [csBoolean];
  // It is supposed to have a name prefix 'chk_'
  _DZ_CONFIG_CONTROL_PREFIX_ = CHECKBOX_PREFIX;
{$INCLUDE 'dzConfigControlTemplate.tpl'}

type
  {: A TCustomCheckbox descendant implementing the IdzConfigControl interface }
  TdzCustomConfigCheckBox = class(_DZ_CONFIG_CONTROL_, IdzConfigControl)
  protected
    {: Reads a boolean setting from the config reader.
       @param(Path) is the path to be used for the setting
       @param(Reader) is a IdzConfigReader interface for doing the actual
                      reading. }
    procedure ReadSetting(const _Path: string; const _Reader: IdzConfigReader); override;
    {: Writes a boolean setting to the config writer.
       @param(Path) is the path to be used for the setting
       @param(Reader) is a IdzConfigReader interface for doing the actual
                      reading. }
    procedure WriteSetting(const _Path: string; const _Writer: IdzConfigWriter); override;
  end;

  {: a descendant of TdzCustomConfigCheckBox publishing the usual properties of
     TCheckBox and the additional properties ConfigStorageType and ConfigStorageName }
  TdzConfigCheckBox = class(TdzCustomConfigCheckBox)
  published // properties
{$IFDEF linux}
    property Bitmap;
    property Masked default False;
{$ENDIF linux}
    property Action;
    property AllowGrayed;
    property Anchors;
    property Caption;
    property Checked;
    property Color;
    property Constraints;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor default True;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop;
    property Visible;
  published // events
{$IFDEF linux}
    property OnKeyString;
    property OnMouseEnter;
    property OnMouseLeave;
{$ENDIF linux}
    property OnClick;
    property OnContextPopup;
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

{ TdzCustomConfigCheckBox }
(*
constructor TdzCustomConfigCheckBox.Create(_Owner: TComponent);
begin
  inherited;
  fConfigStorageType := csBoolean;
end;

function TdzCustomConfigCheckBox.GetConfigStorageName: string;
begin
  Result := fConfigStorageName;
  if Result = '' then
    begin
      Result := Name;
      if Copy(Result, 1, 4) = 'chk_' then
        Result := Copy(Result, 5, 255);
    end;
end;

procedure TdzCustomConfigCheckBox.SetConfigStorageName(const _ConfigStorageName: string);
begin
  fConfigStorageName := _ConfigStorageName;
end;

function TdzCustomConfigCheckBox.GetConfigStorageType: TdzConfigStorageType;
begin
  Result := fConfigStorageType;
end;

procedure TdzCustomConfigCheckBox.SetConfigStorageType(const _ConfigStorageType: TdzConfigStorageType);
begin
  if _ConfigStorageType in [csNone, csBoolean] then
    fConfigStorageType := _ConfigStorageType;
end;
*)

procedure TdzCustomConfigCheckBox.ReadSetting(const _Path: string; const _Reader: IdzConfigReader);
var
  b: boolean;
begin
  case ConfigStorageType of
    csBoolean:
      begin
        _Reader.GetSetting(_Path, ConfigStorageName, b);
        Checked := b;
      end;
  end;
end;

procedure TdzCustomConfigCheckBox.WriteSetting(const _Path: string; const _Writer: IdzConfigWriter);
begin
  case ConfigStorageType of
    csBoolean:
      _Writer.StoreSetting(_Path, ConfigStorageName, Checked);
  end;
end;

end.

