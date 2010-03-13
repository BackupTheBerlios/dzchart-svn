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
{ The Original Code is JclOtExceptionForm.pas.                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Florent Ouchet                                     }
{  [outchy att users dott sourceforge dott net]                                                    }
{ Portions created by Florent Ouchet are Copyright (C) of Florent Ouchet.                          }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Unit owner: Florent Ouchet                                                                       }
{ Last modified: $Date: 2006-05-30 00:02:45 +0200 (mar., 30 mai 2006) $                                                      }
{                                                                                                  }
{**************************************************************************************************}

unit JclOtaExceptionForm;

interface

{$I jcl.inc}
{$I crossplatform.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  JclOtaUtils;

type
  TJclExpertExceptionForm = class(TForm)
    MemoDetails: TMemo;
    LabelURL: TLabel;
    MemoCallStack: TMemo;
    ButtonClose: TButton;
    procedure LabelURLClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure ShowException(AExceptionObj: TObject);
    function Execute: Boolean;
  end;

implementation

{$R *.dfm}

uses
  TypInfo, ShellApi,
{$IFDEF MSWINDOWS}
  JclDebug,
{$ENDIF MSWINDOWS}
  JclOtaResources;

procedure TJclExpertExceptionForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  // Fixing the Window Ghosting "bug"
  Params.Style := params.Style or WS_POPUP;
  if Assigned(Screen.ActiveForm) then
    Params.WndParent := Screen.ActiveForm.Handle
  else if Assigned (Application.MainForm) then
    Params.WndParent := Application.MainForm.Handle
  else
    Params.WndParent := Application.Handle;
end;

function TJclExpertExceptionForm.Execute: Boolean;
begin
  Result := ShowModal = mrOk;
end;

procedure TJclExpertExceptionForm.FormCreate(Sender: TObject);
begin
  Caption := RsReportFormCaption;
  MemoDetails.Lines.Text := RsExceptionDetails;
  LabelURL.Caption := RsReportCaption;
  ButtonClose.Caption := RsReportClose;
end;

procedure TJclExpertExceptionForm.LabelURLClick(Sender: TObject);
begin
  ShellExecute(Handle, 'open', PChar(RsReportURL), '', '', SW_SHOW);    // do not localize
end;

procedure TJclExpertExceptionForm.ShowException(AExceptionObj: TObject);
var
  AStackInfoList: TJclStackInfoList;
begin
  MemoCallStack.Lines.Clear;

  try
    if Assigned(AExceptionObj) then
      MemoCallStack.Lines.Add(RsDetailsExceptionName + AExceptionObj.ClassName);

    if AExceptionObj is Exception then
    begin
      MemoCallStack.Lines.Add(RsDetailsExceptionMessage + Exception(AExceptionObj).Message);
{$IFDEF MSWINDOWS}
      if (AExceptionObj is EJclExpertException) then
        with EJclExpertException(AExceptionObj) do
          if Assigned(StackInfo) then
      begin
        StackInfo.AddToStrings(MemoCallStack.Lines, True, True, True, True);
        Exit;
      end;
{$ENDIF MSWINDOWS}
    end;

{$IFDEF MSWINDOWS}
    AStackInfoList := JclCreateStackList(False, 0, nil);
    try
      AStackInfoList.AddToStrings(MemoCallStack.Lines, True, True, True, True);
    finally
      AStackInfoList.Free;
    end;
{$ENDIF MSWINDOWS}
  except
    MemoCallStack.Lines.Add(RsErrorWhileFormatting);
  end;
end;

end.
