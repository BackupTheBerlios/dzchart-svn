{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is JclOtaExcDlgSystemFrame.pas.                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{         <outchy att users dott sourceforge dott net>                                             }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet. All rights reserved.     }
{                                                                                                  }
{ Contributors:                                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2009-07-30 13:23:44 +0200 (jeu., 30 juil. 2009)                         $ }
{ Revision:      $Rev:: 122                                                                      $ }
{ Author:        $Author:: outch                                                                 $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaExcDlgSystemFrame;

interface

{$I jcl.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JclOtaExcDlgRepository, JclOtaWizardFrame;

type
  TJclOtaExcDlgSystemPage = class(TJclWizardFrame)
    CheckBoxDelayed: TCheckBox;
    CheckBoxHookDll: TCheckBox;
    CheckBoxModuleList: TCheckBox;
    CheckBoxOSInfo: TCheckBox;
    CheckBoxActiveControls: TCheckBox;
    CheckBoxCatchMainThread: TCheckBox;
    CheckBoxUnitVersioning: TCheckBox;
    CheckBoxDisableIfDebuggerAttached: TCheckBox;
    procedure CheckBoxModuleListClick(Sender: TObject);
  private
    FParams: TJclOtaExcDlgParams;
  public
    constructor Create(AOwner: TComponent; AParams: TJclOtaExcDlgParams); reintroduce;

    procedure PageActivated(Direction: TJclWizardDirection); override;
    procedure PageDesactivated(Direction: TJclWizardDirection); override;

    property Params: TJclOtaExcDlgParams read FParams write FParams;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/trunk/jcl/experts/repository/JclOtaExcDlgSystemFrame.pas $';
    Revision: '$Revision: 122 $';
    Date: '$Date: 2009-07-30 13:23:44 +0200 (jeu., 30 juil. 2009) $';
    LogPath: 'JCL\experts\repository';
    Extra: '';
    Data: nil
    );
{$ENDIF UNITVERSIONING}

implementation

{$R *.dfm}

uses
  JclOtaResources;

//=== { TJclOtaExcDlgSystemPage } ============================================

procedure TJclOtaExcDlgSystemPage.CheckBoxModuleListClick(Sender: TObject);
begin
  CheckBoxUnitVersioning.Enabled := CheckBoxModuleList.Checked;
end;

constructor TJclOtaExcDlgSystemPage.Create(AOwner: TComponent;
  AParams: TJclOtaExcDlgParams);
begin
  FParams := AParams;
  inherited Create(AOwner);

  Caption := RsExcDlgSystemOptions;
  CheckBoxDelayed.Caption := RsDelayedStackTrace;
  CheckBoxHookDll.Caption := RsHookDll;
  CheckBoxModuleList.Caption := RsModuleList;
  CheckBoxUnitVersioning.Caption := RsUnitVersioning;
  CheckBoxOSInfo.Caption := RsOSInfo;
  CheckBoxActiveControls.Caption := RsActiveControls;
  CheckBoxCatchMainThread.Caption := RsCatchMainThread;
  CheckBoxDisableIfDebuggerAttached.Caption := RsDisableIfDebuggerAttached;
end;

procedure TJclOtaExcDlgSystemPage.PageActivated(Direction: TJclWizardDirection);
begin
  inherited PageActivated(Direction);

  CheckBoxDelayed.Checked := Params.DelayedTrace;
  CheckBoxHookDll.Checked := Params.HookDll;
  CheckBoxModuleList.Checked := Params.ModuleList;
  CheckBoxUnitVersioning.Checked := Params.UnitVersioning;
  CheckBoxOSInfo.Checked := Params.OSInfo;
  CheckBoxActiveControls.Checked := Params.ActiveControls;
  CheckBoxCatchMainThread.Checked := Params.CatchMainThread;
  CheckBoxDisableIfDebuggerAttached.Checked := Params.DisableIfDebuggerAttached;
end;

procedure TJclOtaExcDlgSystemPage.PageDesactivated(
  Direction: TJclWizardDirection);
begin
  inherited PageDesactivated(Direction);

  Params.DelayedTrace := CheckBoxDelayed.Checked;
  Params.HookDll := CheckBoxHookDll.Checked;
  Params.ModuleList := CheckBoxModuleList.Checked;
  Params.UnitVersioning := CheckBoxUnitVersioning.Checked;
  Params.OSInfo := CheckBoxOSInfo.Checked;
  Params.ActiveControls := CheckBoxActiveControls.Checked;
  Params.CatchMainThread := CheckBoxCatchMainThread.Checked;
  Params.DisableIfDebuggerAttached := CheckBoxDisableIfDebuggerAttached.Checked;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
