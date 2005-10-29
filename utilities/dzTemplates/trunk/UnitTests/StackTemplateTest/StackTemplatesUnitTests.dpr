program StackTemplatesUnitTests;
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

{%File '..\..\Templates\t_dzStackTemplate.tpl'}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  Testu_dzStackTemplateTest in 'Testu_dzStackTemplateTest.pas',
  u_MyItemStack in '..\..\Examples\u_MyItemStack.pas',
  u_MyItem in '..\..\Examples\u_MyItem.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

