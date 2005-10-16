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

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  Testu_dzListTemplateTest in 'Testu_dzListTemplateTest.pas',
  u_dzQuicksort in '..\..\Units\u_dzQuicksort.pas',
  u_dzSortedListTest in '..\..\Examples\u_dzSortedListTest.pas',
  u_dzListTest in '..\..\Examples\u_dzListTest.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

