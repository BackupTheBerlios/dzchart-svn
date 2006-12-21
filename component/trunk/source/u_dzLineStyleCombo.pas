(*****************************************************************************
 *                 TdzCustomChart Component
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
 * The Original Code is u_dzCustomCharts.
 *
 * The Initial Developer of the Original Code is
 * Thomas Mueller.
 * Portions created by the Initial Developer are Copyright (C) 2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * * Paul Warren
 *****************************************************************************)
unit u_dzLineStyleCombo;

interface

uses
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  Graphics;

type
  TdzLineStyleCombo = class(TCustomComboBox)
  private
    FNeedToPopulate: boolean;
    FLineStyle: TPenStyle;
    procedure PopulateList;
    function GetLineStyle: TPenStyle;
    procedure SetLineStyle(const Value: TPenStyle);
  protected
  public
    constructor Create(_Owner: TComponent); override;
    procedure CreateWnd; override;
  published
    property LineStyle: TPenStyle read GetLineStyle write SetLineStyle;
  published
    property AutoComplete default True;
    property AutoDropDown default False;
{$ifdef Delphi7_up}
    property AutoCloseUp default False;
{$endif}
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    //    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    //    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;

    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses
  TypInfo,
  u_dzCustomCharts;

{ TdzLineStyleCombo }

constructor TdzLineStyleCombo.Create(_Owner: TComponent);
begin
  inherited;
  Style := csDropDownList;
  FLineStyle := psSolid;
  PopulateList;
end;

procedure TdzLineStyleCombo.CreateWnd;
begin
  inherited CreateWnd;
  if FNeedToPopulate then
    PopulateList;
end;

function TdzLineStyleCombo.GetLineStyle: TPenStyle;
begin
  if ItemIndex = -1 then
    Result := psSolid
  else
    Result := TPenStyle(ItemIndex);
end;

procedure TdzLineStyleCombo.SetLineStyle(const Value: TPenStyle);
begin
  ItemIndex := Ord(Value);
end;

procedure TdzLineStyleCombo.PopulateList;
var
  ps: TPenStyle;
begin
  if HandleAllocated then
    begin
      Items.BeginUpdate;
      try
        for ps := Low(TPenStyle) to High(TPenStyle) do
          begin
            Items.Add(Copy(GetEnumName(TypeInfo(TPenStyle), Ord(ps)), 3, 255));
          end;
      finally
        Items.EndUpdate;
        FNeedToPopulate := False;
      end;
    end
  else
    FNeedToPopulate := True;
end;

end.

