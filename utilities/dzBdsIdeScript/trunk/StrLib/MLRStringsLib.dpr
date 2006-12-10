library MLRStringsLib;

uses
  ComServ,
  MLRStringsLib_TLB in 'MLRStringsLib_TLB.pas',
  MLRAutoStringsUnit in 'MLRAutoStringsUnit.pas' {MLRAutoStrings: CoClass};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.TLB}

{$R *.RES}

begin
end.
