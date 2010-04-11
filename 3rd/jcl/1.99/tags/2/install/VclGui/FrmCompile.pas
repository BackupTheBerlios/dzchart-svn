{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: FrmCompile.pas, released on 2004-12-13.

The Initial Developer of the Original Code is Andreas Hausladen
(Andreas dott Hausladen att gmx dott de)
Portions created by Andreas Hausladen are Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s): -
  Florent Ouchet (outchy) - New installer core

You may retrieve the latest version of this file at the Project JEDI's JVCL
home page, located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id: FrmCompile.pas 1748 2006-09-03 17:53:21Z outchy $

unit FrmCompile;

{$I jedi.inc}

interface

uses
  Windows, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls;

type
  ICompileMessages = interface
    ['{C932390B-8DB6-4CAE-89D0-7BAB8A2E640B}']
    procedure Clear;

    procedure AddHint(const Text: string);
    procedure AddWarning(const Text: string);
    procedure AddError(const Text: string);
    procedure AddFatal(const Text: string);
    procedure AddText(const Msg: string);

      { Text is the line that the compiler outputs. The ICompileMessages
        implementor must parse the line itself. }
  end;

  TFormCompile = class(TForm)
    PanelClient: TPanel;
    BtnOk: TButton;
    BevelProject: TBevel;
    BevelStatus: TBevel;
    BevelCurrentLine: TBevel;
    BevelHints: TBevel;
    LblProject: TLabel;
    LblStatusCaption: TLabel;
    BevelTotalLines: TBevel;
    LblCurrentLineCaption: TLabel;
    LblCurrentLine: TLabel;
    LblTotalLinesCaption: TLabel;
    LblTotalLines: TLabel;
    BevelWarnings: TBevel;
    BevelErrors: TBevel;
    LblHintsCaption: TLabel;
    LblHints: TLabel;
    LblWarningsCaption: TLabel;
    LblWarnings: TLabel;
    LblErrorsCaption: TLabel;
    LblErrors: TLabel;
    LblProjectCaption: TLabel;
    LblStatus: TLabel;
    LblErrorReason: TLabel;
    procedure BtnOkClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FHints: Cardinal;
    FWarnings: Cardinal;
    FErrors: Cardinal;
    FCurrentLine: Cardinal;
    FTotalLines: Cardinal;
    FCurFilename: string;
    FCompileMessages: ICompileMessages;
    FAutoClearCompileMessages: Boolean;
    procedure SetCurrentLine(Line: Cardinal);
  public
    procedure Init(const ProjectName: string; Clear: Boolean = True);
    procedure Compiling(const Filename: string);
    procedure Linking(const Filename: string);
    procedure Done(const ErrorReason: string = '');

    procedure AddHint(const Line: string);
    procedure AddWarning(const Line: string);
    procedure AddError(const Line: string);
    procedure AddFatal(const Line: string);
    procedure AddText(const Line: string);
    procedure CompilationProgress(const FileName: string; LineNumber: Integer);

    property Hints: Cardinal read FHints;
    property Warnings: Cardinal read FWarnings;
    property Errors: Cardinal read FErrors;
    property CurrentLine: Cardinal read FCurrentLine write SetCurrentLine;

    property AutoClearCompileMessages: Boolean read FAutoClearCompileMessages write FAutoClearCompileMessages default False;
    property CompileMessages: ICompileMessages read FCompileMessages write FCompileMessages;
  end;

implementation

{$IFDEF MSWINDOWS}
{$I windowsonly.inc}
uses
  FileCtrl;
{$ENDIF MSWINDOWS}

{$R *.dfm}

resourcestring
  RsPreparing = 'Preparing...';
  RsCompiling = 'Compiling';
  RsLinking = 'Linking';
  RsDone = 'Done';
  RsThereAreErrors = 'There are errors.';
  RsThereAreWarnings = 'There are warnings.';
  RsThereAreHints = 'There are hints.';
  RsCompiled = 'compiled.';

{ TFormCompile }

procedure TFormCompile.BtnOkClick(Sender: TObject);
begin
  Tag := 1;
  Close;
end;

procedure TFormCompile.Init(const ProjectName: string; Clear: Boolean);
begin
  Tag := 0;
  LblProject.Caption := MinimizeName(ProjectName, LblProject.Canvas, LblProject.ClientWidth);

  LblStatusCaption.Font.Style := [];
  LblStatus.Font.Style := [];

  if Clear then
  begin
    if Assigned(FCompileMessages) and AutoClearCompileMessages then
      FCompileMessages.Clear;
    FHints := 0;
    FErrors := 0;
    FWarnings := 0;
    FTotalLines := 0;
  end;
  FCurrentLine := 0;
  FCurFilename := '';

  LblHints.Caption := IntToStr(FHints);
  LblWarnings.Caption := IntToStr(FWarnings);
  LblErrors.Caption := IntToStr(FErrors);
  LblCurrentLine.Caption := IntToStr(FCurrentLine);
  LblTotalLines.Caption := IntToStr(FTotalLines);
  LblStatusCaption.Caption := RsPreparing;
  LblStatus.Caption := '';

  BtnOk.Enabled := False;
  Show;
end;

procedure TFormCompile.Compiling(const Filename: string);
begin
  if Filename <> FCurFilename then
  begin
    FCurFilename := Filename;
    FTotalLines := FTotalLines + FCurrentLine;
    CurrentLine := 0; // updates total lines and current lines
    LblStatusCaption.Font.Style := [];
    LblStatus.Font.Style := [];
    LblStatusCaption.Caption := RsCompiling + ':';
    LblStatus.Caption := ExtractFileName(Filename);
    Application.ProcessMessages;
  end;
end;

procedure TFormCompile.Linking(const Filename: string);
begin
  FTotalLines := FTotalLines + FCurrentLine;
  CurrentLine := 0;

  LblStatusCaption.Font.Style := [];
  LblStatus.Font.Style := [];
  LblStatusCaption.Caption := RsLinking + ':';
  LblStatus.Caption := ExtractFileName(Filename);
  Application.ProcessMessages;
end;

procedure TFormCompile.Done(const ErrorReason: string);
begin
  FCurFilename := '';
  FTotalLines := FTotalLines + FCurrentLine;
  CurrentLine := 0;

  LblErrorReason.Caption := ErrorReason;
  LblErrorReason.Visible := ErrorReason <> '';
  LblStatusCaption.Font.Style := [fsBold];
  LblStatus.Font.Style := [fsBold];
  LblStatusCaption.Caption := RsDone + ':';

  if FErrors > 0 then
    LblStatus.Caption := RsThereAreErrors
  else if FWarnings > 0 then
    LblStatus.Caption := RsThereAreWarnings
  else if FHints > 0 then
    LblStatus.Caption := RsThereAreHints
  else
    LblStatus.Caption := RsCompiled;
  BtnOk.Enabled := ErrorReason <> '';
  if ErrorReason <> '' then
  begin
    Hide;
    ShowModal;
  end;
end;

procedure TFormCompile.AddError(const Line: string);
begin
  Inc(FErrors);
  LblErrors.Caption := IntToStr(FErrors);
  if Assigned(FCompileMessages) then
    FCompileMessages.AddError(Line);
  Application.ProcessMessages;
end;

procedure TFormCompile.AddHint(const Line: string);
begin
  Inc(FHints);
  LblHints.Caption := IntToStr(FHints);
  if Assigned(FCompileMessages) then
    FCompileMessages.AddHint(Line);
  Application.ProcessMessages;
end;

procedure TFormCompile.AddWarning(const Line: string);
begin
  Inc(FWarnings);
  LblWarnings.Caption := IntToStr(FWarnings);
  if Assigned(FCompileMessages) then
    FCompileMessages.AddWarning(Line);
  Application.ProcessMessages;
end;

procedure TFormCompile.AddFatal(const Line: string);
begin
  Inc(FErrors);
  LblErrors.Caption := IntToStr(FErrors);
  if Assigned(FCompileMessages) then
    FCompileMessages.AddFatal(Line);
  Application.ProcessMessages;
end;

procedure TFormCompile.AddText(const Line: string);
begin
  if Assigned(FCompileMessages) then
    FCompileMessages.AddText(Line);
end;

procedure TFormCompile.CompilationProgress(const FileName: string;
  LineNumber: Integer);
begin
  Compiling(FileName);
  CurrentLine := LineNumber;
end;

procedure TFormCompile.SetCurrentLine(Line: Cardinal);
begin
  FCurrentLine := Line;
  LblCurrentLine.Caption := IntToStr(Line);
  LblTotalLines.Caption := IntToStr(FTotalLines + FCurrentLine);
  Application.ProcessMessages;
end;

procedure TFormCompile.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := Tag = 1;
end;

end.
