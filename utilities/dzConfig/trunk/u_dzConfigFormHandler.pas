unit u_dzConfigFormHandler;

interface

uses
  SysUtils,
  Classes,
{$IFDEF linux}
  QForms,
{$ENDIF}
{$IFDEF mswindows}
  Forms,
{$ENDIF mswindows}
  u_dzConfigCommon;

type
  TdzConfigFormHandler = class(TInterfacedObject, IdzConfigFormHandler)
  protected // IdzConfigFormHandler interface
    procedure SettingsToForm(const _Reader: IdzConfigReader; const _RootPath: string; _Form: TForm);
    procedure FormToSettings(const _Writer: IdzConfigWriter; const _RootPath: string; _Form: TForm);
  public
  end;

implementation

uses
{$IFDEF linux}
  QControls,
  QComCtrls,
  QStdCtrls,
  QExtCtrls;
{$ENDIF}
{$IFDEF mswindows}
  Controls,
  ComCtrls,
  StdCtrls,
  ExtCtrls;
{$ENDIF mswindows}

procedure TdzConfigFormHandler.FormToSettings(const _Writer: IdzConfigWriter; const _RootPath: string; _Form: TForm);

  procedure HandleSubComponents(const _Path: string; _Parent: TControl);
  var
    SettingName: string;
    i: integer;
    comp: TComponent;
    ctrl: TControl;
    ConfigCtrl: IdzConfigControl;
  begin
    for i := 0 to _Form.ComponentCount - 1 do
      begin
        Comp := _Form.Components[i];
        if not (Comp is TControl) then
          Continue;

        Ctrl := Comp as TControl;
        if not (Ctrl.Parent = _Parent) then
          Continue;

        SettingName := Ctrl.Name;
        if Supports(Comp, IdzConfigControl, ConfigCtrl)
          and (ConfigCtrl.ConfigStorageType <> csNone) then
          ConfigCtrl.WriteSetting(_Path, _Writer)
        else
          begin
            if Ctrl is TPageControl then
              SettingName := ''
            else if Ctrl is TTabSheet then
              SettingName := RemoveCtrlPrefix(TABSHEET_PREFIX, SettingName)
            else if Ctrl is TGroupBox then
              SettingName := RemoveCtrlPrefix(GROUPBOX_PREFIX, SettingName)
            else if Ctrl is TPanel then
              SettingName := RemoveCtrlPrefix(PANEL_PREFIX, SettingName);

            if (_Path <> '') and (SettingName <> '') then
              SettingName := _Path + '\' + SettingName
            else if _Path <> '' then
              SettingName := _Path;
            HandleSubComponents(SettingName, Ctrl);
          end;
      end;
  end;

begin
  HandleSubComponents(_RootPath, _Form);
end;

procedure TdzConfigFormHandler.SettingsToForm(const _Reader: IdzConfigReader;
  const _RootPath: string; _Form: TForm);

  procedure HandleSubComponents(const _Path: string; _Parent: TControl);
  var
    SettingName: string;
    i: integer;
    comp: TComponent;
    ctrl: TControl;
    ConfigCtrl: IdzConfigControl;
  begin
    for i := 0 to _Form.ComponentCount - 1 do
      begin
        Comp := _Form.Components[i];
        if Comp is TControl then
          begin
            Ctrl := Comp as TControl;
            if (Ctrl.Parent = _Parent) then
              begin
                SettingName := Ctrl.Name;
                if Supports(Comp, IdzConfigControl, ConfigCtrl)
                  and (ConfigCtrl.ConfigStorageType <> csNone) then
                  begin
                    ConfigCtrl.ReadSetting(_Path, _Reader);
                  end
                else
                  begin
                    if Ctrl is TPageControl then
                      SettingName := ''
                    else if Ctrl is TTabSheet then
                      SettingName := RemoveCtrlPrefix(TABSHEET_PREFIX, SettingName)
                    else if Ctrl is TGroupBox then
                      SettingName := RemoveCtrlPrefix(GROUPBOX_PREFIX, SettingName)
                    else if Ctrl is TPanel then
                      SettingName := RemoveCtrlPrefix(PANEL_PREFIX, SettingName);

                    if (_Path <> '') and (SettingName <> '') then
                      SettingName := _Path + '\' + SettingName
                    else if _Path <> '' then
                      SettingName := _Path;
                    HandleSubComponents(SettingName, Ctrl);
                  end;
              end;
          end;
      end;
  end;

begin
  HandleSubComponents(_RootPath, _Form);
end;

end.

