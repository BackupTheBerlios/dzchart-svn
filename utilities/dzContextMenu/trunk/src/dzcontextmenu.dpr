library dzcontextmenu;

uses
  ComServ,
  u_ContextMenuHandler in 'u_ContextMenuHandler.pas',
  d_ContextMenu in 'd_ContextMenu.pas' {dm_ContextMenu: TDataModule},
  u_dzConfigUtils in '..\libs\dzLib\src\u_dzConfigUtils.pas',
  w_Configure in 'w_Configure.pas' {f_Configure},
  w_EditSection in 'w_EditSection.pas' {f_EditSection},
  w_dzDialog in '..\libs\dzLib\forms\w_dzDialog.pas' {f_dzDialog};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *_version.RES}

begin

end.

