library dzcontextmenu;

uses
  ComServ,
  u_ContextMenuHandler in 'u_ContextMenuHandler.pas',
  d_ContextMenu in 'd_ContextMenu.pas' {dm_ContextMenu: TDataModule},
  u_dzConfigUtils in '..\libs\dzLib\src\u_dzConfigUtils.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *_version.RES}

begin

end.

