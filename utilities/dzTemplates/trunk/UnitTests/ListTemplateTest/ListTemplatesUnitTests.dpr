program ListTemplatesUnitTests;
{

  Delphi DUnit Test Project
  -------------------------
  This project contains the DUnit test framework and the GUI/Console test runners.
  Add "CONSOLE_TESTRUNNER" to the conditional defines entry in the project options 
  to use the console test runner.  Otherwise the GUI test runner will be used by 
  default.

}

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

{%File '..\..\Templates\t_dzSortedListTemplate.tpl'}
{%File '..\..\Templates\t_dzListTemplate.tpl'}
{%File '..\..\Templates\t_dzListInterfaceTemplate.tpl'}
{%File '..\..\Templates\t_dzIntegerSortedListTemplate.tpl'}
{%File '..\..\Templates\t_dzIntegerSortedObjectListTemplate.tpl'}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  Testu_dzListTemplateTest in 'Testu_dzListTemplateTest.pas',
  u_dzQuicksort in '..\..\Units\u_dzQuicksort.pas',
  i_MyItemSortedList in '..\..\Examples\i_MyItemSortedList.pas',
  u_MyItemList in '..\..\Examples\u_MyItemList.pas',
  t_dzSortedListInterfaceTemplate in '..\..\Templates\t_dzSortedListInterfaceTemplate.tpl',
  u_MyItemSortedList in '..\..\Examples\u_MyItemSortedList.pas',
  u_MyItem in '..\..\Examples\u_MyItem.pas',
  i_MyItemList in '..\..\Examples\i_MyItemList.pas',
  u_MyItemIntList in '..\..\Examples\u_MyItemIntList.pas',
  t_dzObjectListTemplate in '..\..\Templates\t_dzObjectListTemplate.tpl';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

