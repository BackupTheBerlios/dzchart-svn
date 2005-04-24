(*****************************************************************************
 *   Designtime Registration Unit for dummzeuch.de charts
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
 * The Original Code is u_dzRegister.
 *
 * The Initial Developer of the Original Code is
 * Thomas Mueller.
 * Portions created by the Initial Developer are Copyright (C) 2003
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 * * Paul Warren
 *****************************************************************************)

unit u_dzRegister;

interface

procedure Register;

implementation

uses
  Classes,
  DesignIntf,
  u_dzCustomCharts,
  u_dzXYChart,
  u_dzPieChart,
  u_dzLineStyleCombo,
  u_dzPointStyleCombo,
  u_dzPolynomialDataseries,
  w_dzPolynomEdit;

{$R XYChart.dcr}
{$R PieChart.dcr}
{$R PolynomialDataseries.dcr}

procedure Register;
begin
  // register components on dummzeuch.de page
  RegisterComponents('dummzeuch.de', [TdzXYChart]);
  RegisterComponents('dummzeuch.de', [TdzPieChart]);
  RegisterComponents('dummzeuch.de', [TdzPolynomialDataseries]);
  RegisterComponents('dummzeuch.de', [TdzLineStyleCombo]);
  RegisterComponents('dummzeuch.de', [TdzPointStyleCombo]);

  // hide Tag property for all subcomponents
  RegisterPropertyEditor(TypeInfo(LongInt), TdzXYChartLegend, 'Tag', nil);
  RegisterPropertyEditor(TypeInfo(LongInt), TdzBottomChartAxis, 'Tag', nil);
  RegisterPropertyEditor(TypeInfo(LongInt), TdzLeftChartAxis, 'Tag', nil);
  RegisterPropertyEditor(TypeInfo(LongInt), TdzChartFrame, 'Tag', nil);
  RegisterPropertyEditor(TypeInfo(LongInt), TdzTitle, 'Tag', nil);
  RegisterPropertyEditor(TypeInfo(LongInt), TdzAxisScale, 'Tag', nil);
  RegisterPropertyEditor(TypeInfo(LongInt), TdzChartGrid, 'Tag', nil);

  RegisterPropertyEditor(TypeInfo(TdzPolynomDescriptor), TdzPolynomialDataseries, '', TdzPolynomEditor);
end;

end.

