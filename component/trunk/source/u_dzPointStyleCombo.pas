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
unit u_dzPointStyleCombo;

interface

uses
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  Graphics,
  u_dzDataSeries;

type
  TdzPointStyleCombo = class(TCustomComboBox)
  private
    FNeedToPopulate: boolean;
    FPointStyle: TPointStyle;
    procedure PopulateList;
    function GetPointStyle: TPointStyle;
    procedure SetPointStyle(const Value: TPointStyle);
  protected
  public
    constructor Create(_Owner: TComponent); override;
    procedure CreateWnd; override;
  published
    property PointStyle: TPointStyle read GetPointStyle write SetPointStyle;
  published
    property AutoComplete default True;
    property AutoDropDown default False;
    property AutoCloseUp default False;
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

{ TdzPointStyleCombo }

constructor TdzPointStyleCombo.Create(_Owner: TComponent);
begin
  inherited;
  Style := csDropDownList;
  FPointStyle := psNone;
  PopulateList;
end;

procedure TdzPointStyleCombo.CreateWnd;
begin
  inherited CreateWnd;
  if FNeedToPopulate then
    PopulateList;
end;

function TdzPointStyleCombo.GetPointStyle: TPointStyle;
begin
  if ItemIndex = -1 then
    Result := psNone
  else
    Result := TPointStyle(ItemIndex);
end;

procedure TdzPointStyleCombo.SetPointStyle(const Value: TPointStyle);
begin
  ItemIndex := Ord(Value);
end;

procedure TdzPointStyleCombo.PopulateList;
var
  ps: TPointStyle;
begin
  if HandleAllocated then
    begin
      Items.BeginUpdate;
      try
        for ps := Low(TPointStyle) to High(TPointStyle) do
          begin
            Items.Add(Copy(GetEnumName(TypeInfo(TPointStyle), Ord(ps)), 3, 255));
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

