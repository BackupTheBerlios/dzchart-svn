{.GXFormatter.config=twm}
///<summary> Add this unit to ignore some JVCL classes and properties from translation </summary>
unit u_dzTranslatorJVCL;

interface

uses
  SysUtils,
  Classes;

implementation

uses
  u_dzTranslator,
  JvFormPlacement;

initialization

  // Ignore TFormStorage properties
  TP_TryGlobalIgnoreClass(TJvFormStorage);
end.

