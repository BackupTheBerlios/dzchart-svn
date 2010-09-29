library dzcontextmenu;

uses
  ComServ,
  u_ContextMenuHandler in 'u_ContextMenuHandler.pas',
  d_ContextMenu in 'd_ContextMenu.pas' {dm_ContextMenu: TDataModule};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin

end.

