unit dzAutoSaveReg;

interface

uses
  Forms,
  ToolsAPI,
  dzAutoWizard;

procedure Register;

implementation

// Register the wizard in a package.

procedure Register;
begin
  RegisterPackageWizard(TAutoSaveWizard.Create);
end;

// Register the wizard in a DLL.

function InitWizard(Services: IBorlandIDEServices;
  RegisterProc: TWizardRegisterProc;
  var Terminate: TWizardTerminateProc): Boolean; stdcall;
begin
  Application.Handle := (Services as IOTAServices).GetParentHandle;
  RegisterProc(TAutoSaveWizard.Create);
  Result := True;
end;

exports
  InitWizard name WizardEntryPoint;

end.

