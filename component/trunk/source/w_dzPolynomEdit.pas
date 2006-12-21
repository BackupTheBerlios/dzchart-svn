(*****************************************************************************
 *        TdzPolynomialDataseries Componenteditor
 *
 *        This unit is part of dummzeuch.de Charts
 *
 *                (c) 2003 Thomas Mueller
 *                 http://www.dummzeuch.de
 *
 *  Based on Paul Warren's homegrown chart components
 *        http://users.uniserve.com/~hg_soft
 *
 *****************************************************************************
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is w_dzPolynomEdit.
 *
 * The Initial Developer of the Original Code is
 * Thomas Mueller.
 * Portions created by the Initial Developer are Copyright (C) 2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * * Paul Warren
 *****************************************************************************)

unit w_dzPolynomEdit;

interface

uses
  SysUtils,
  Variants,
  Classes,
{$IFDEF MSWINDOWS}
  Windows,
  Messages,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  Grids,
  Spin,
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  Qt,
  QGraphics,
  QControls,
  QForms,
  QDialogs,
  QStdCtrls,
  QGrids,
  QComCtrls,
{$ENDIF LINUX}
  DesignEditors,
  DesignIntf,
  u_dzPolynomialDataseries;

type
  TdzPolynomEditor = class(TPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    function GetValue: string; override;
  end;

{$IFDEF MSWINDOWS}
type
  TOnSpinEditChanged = procedure(_Sender: TObject; _NewValue: integer) of object;

type
  TSpinEdit = class(Spin.TSpinEdit)
  private
    fOnChanged: TOnSpinEditChanged;
    function GetValue: LongInt;
    procedure SetValue(const _Value: LongInt);
    procedure Changed(_NewValue: integer);
  protected
    procedure UpClick (Sender: TObject); override;
    procedure DownClick (Sender: TObject); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  published
    property Value: LongInt read GetValue write SetValue;
    property OnChanged: TOnSpinEditChanged read fOnChanged write fOnChanged;
  end;
{$ENDIF MSWINDOWS}

type
  Tf_dzPolynomEdit = class(TForm)
    b_Ok: TButton;
    sed_Order: TSpinEdit;
    Label1: TLabel;
    ed_Polynom: TEdit;
    b_Cancel: TButton;
    scr_Coefficients: TScrollBox;
    l_Placeholder: TLabel;
    ed_Placeholder: TEdit;
    procedure b_OkClick(Sender: TObject);
    procedure sed_OrderChanged(Sender: TObject; NewValue: Integer);
  private
    fLabels: array of TLabel;
    fEdits: array of TEdit;
    fComponent: TdzPolynomialDataseries;
    procedure SetComponent(const _Component: TdzPolynomialDataseries);
    procedure BuildPolynom(_Order: integer);
    procedure UpdateCoefficientsEditor(_OldOrder, _NewOrder: integer);
  public
    constructor Create(_Owner: TComponent); override;
    property Component: TdzPolynomialDataseries read fComponent write SetComponent;
  end;

implementation

{$IFDEF MSWINDOWS}
{$R *.dfm}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R *.xfm}
{$ENDIF LINUX}

{ TdzPolynomEditor }

procedure TdzPolynomEditor.Edit;
var
  frm: Tf_dzPolynomEdit;
begin
  frm := Tf_dzPolynomEdit.Create(nil);
  try
    try
      frm.Component := self.GetComponent(0) as TdzPolynomialDataseries;
    except
      on e: exception do
        MessageDlg(Format('%s: %s', [e.ClassName, e.Message]), mtError, [mbOK], 0);
    end;
    if mrOK = frm.ShowModal then
      Modified;
  finally
    frm.Free;
  end;
end;

function TdzPolynomEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TdzPolynomEditor.GetValue: string;
var
  Component: TdzPolynomialDataseries;
begin
  Component := self.GetComponent(0) as TdzPolynomialDataseries;
  Result := Component.Polynom.GetFunction;
end;

{ Tf_dzPolynomEdit }

constructor Tf_dzPolynomEdit.Create(_Owner: TComponent);
begin
  inherited;
  // free placeholder components
  l_Placeholder.Free;
  ed_Placeholder.Free;
  sed_Order.OnChanged := sed_OrderChanged;
end;

procedure Tf_dzPolynomEdit.SetComponent(const _Component: TdzPolynomialDataseries);
var
  o: integer;
  i: integer;
begin
  fComponent := _Component;
  o := fComponent.Polynom.Order;
  sed_Order.Value := o; // -> calls UpdateCoefficientsEditor(-1, o);
  for i := 0 to o do
    fEdits[i].Text := Format('%g', [fComponent.Polynom.Coefficients[o - i]]);
  BuildPolynom(o);
end;

procedure Tf_dzPolynomEdit.UpdateCoefficientsEditor(_OldOrder, _NewOrder: integer);
var
  i: integer;
  ParamName: string;
  lbl: TLabel;
  ed: TEdit;
begin
  for i := _OldOrder downto _NewOrder + 1 do
    begin
      fLabels[i].Free;
      fEdits[i].Free;
    end;
  SetLength(fLabels, _NewOrder + 1);
  SetLength(fEdits, _NewOrder + 1);
  for i := _OldOrder + 1 to _NewOrder do
    begin
      lbl := TLabel.Create(self);
      lbl.Parent := scr_Coefficients;
      lbl.Left := 8;
      lbl.Top := 12 + i * 32;
      ParamName := Chr(Ord('a') + i);
      lbl.Caption := ParamName + ' =';
      lbl.Name := 'l_' + ParamName;
      fLabels[i] := lbl;

      ed := TEdit.Create(self);
      ed.Parent := scr_Coefficients;
      ed.Left := 32;
      ed.Top := 8 + i * 32;
      ed.Width := 81;
      ed.Name := 'ed_' + ParamName;
      ed.Text := '0';
      fEdits[i] := ed;
    end;
end;

procedure Tf_dzPolynomEdit.BuildPolynom(_Order: integer);
var
  i: integer;
  o: integer;
  s: string;
begin
  s := '';
  o := _Order;
  for i := 0 to _Order do
    begin
      if s <> '' then
        s := s + ' + ';
      s := s + Chr(Ord('a') + i);
      if o > 0 then
        s := s + 'x';
      if o > 1 then
        s := s + '^' + IntToStr(o);
      Dec(o);
    end;
  ed_Polynom.Text := 'y = ' + s;
end;

procedure Tf_dzPolynomEdit.sed_OrderChanged(Sender: TObject; NewValue: Integer);
var
  OldValue: integer;
begin
  OldValue := Length(fLabels) - 1;
  BuildPolynom(NewValue);
  UpdateCoefficientsEditor(OldValue, NewValue);
end;

procedure Tf_dzPolynomEdit.b_OkClick(Sender: TObject);
var
  i: integer;
  o: integer;
begin
  o := sed_Order.Value;
  fComponent.Polynom.Order := o;
  for i := 0 to o do
    fComponent.Polynom.Coefficients[o - i] := StrToFloatDef(fEdits[i].Text, 0);
end;

{$IFDEF MSWINDOWS}

{ TSpinEdit }

procedure TSpinEdit.Changed(_NewValue: integer);
begin
  if Assigned(fOnChanged) then
    fOnChanged(Self, _NewValue);
end;

function TSpinEdit.GetValue: LongInt;
begin
  Result := inherited Value;
end;

procedure TSpinEdit.DownClick(Sender: TObject);
begin
  inherited;
  Changed(Value);
end;

procedure TSpinEdit.UpClick(Sender: TObject);
begin
  inherited;
  Changed(Value);
end;

procedure TSpinEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  Changed(Value);
end;

procedure TSpinEdit.KeyPress(var Key: Char);
begin
  inherited;
  Changed(Value);
end;

procedure TSpinEdit.SetValue(const _Value: LongInt);
begin
  inherited Value := _Value;
  Changed(_Value);
end;

{$ENDIF MSWINDOWS}

end.



