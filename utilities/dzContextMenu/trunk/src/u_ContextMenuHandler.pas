unit u_ContextMenuHandler;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Menus,
  ActiveX,
  ComObj,
  ShlObj,
  d_ContextMenu;

type
  TContextMenu = class(TComObject, IShellExtInit, IContextMenu)
  private
  {(*}
  const
    GUID: TGUID = '{99D8B139-0855-4C5D-95E7-BC8EC6254B3D}';
  {*)}
  private
    FCmdCount: LongWord;
    FDm: Tdm_ContextMenu;
  protected
    function IShellExtInit.Initialize = IShellExtInit_Initialize;
    function IShellExtInit_Initialize(_pidlFolder: PItemIDList; _lpdobj: IDataObject;
      _HKeyProgID: HKEY): HResult; stdcall;
    function QueryContextMenu(_Menu: HMENU; _indexMenu, _idCmdFirst, _idCmdLast, _UFlags: UINT): HResult; stdcall;
    function InvokeCommand(var _ici: TCMInvokeCommandInfo): HResult; stdcall;
    function GetCommandString(_idCmd, _uType: UINT; _pwReserved: PUINT; _PszName: LPSTR; _cchMax: UINT): HResult; stdcall;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses
  ComServ,
  Shellapi,
  Registry,
  IniFiles;

procedure TContextMenu.Initialize;
begin
  inherited;
  FCmdCount := $FFFFFFFF;
  FDm := Tdm_ContextMenu.Create(nil);
end;

destructor TContextMenu.Destroy;
begin
  FreeAndNil(FDm);
  inherited;
end;

function TContextMenu.IShellExtInit_Initialize(_pidlFolder: PItemIDList; _lpdobj: IDataObject; _HKeyProgID: HKEY): HResult;
var
  StgMedium: TStgMedium;
  FormatEtc: TFormatEtc;
  Count: integer;
  i: Integer;
  Files: TStringList;
  FileName: array[0..MAX_PATH] of Char;
begin
  Result := E_INVALIDARG;
  try
  // if lpdobj is nil, fail
    if (_lpdobj = nil) then begin
      Exit;
    end;

    Files := FDm.Files;
    Files.Clear;

  // initialize clipboard format
    FormatEtc.CfFormat := CF_HDROP;
    FormatEtc.Ptd := nil;
    FormatEtc.DwAspect := DVASPECT_CONTENT;
    FormatEtc.Lindex := 1;
    FormatEtc.Tymed := TYMED_HGLOBAL;
    Result := _lpdobj.GetData(FormatEtc, StgMedium);
    if Succeeded(Result) then begin
      try
      // get selected files count
        Count := DragQueryFile(StgMedium.hGlobal, $FFFFFFFF, nil, 0);
      // get selected files
        for i := 0 to Count - 1 do begin
          DragQueryFile(StgMedium.hGlobal, i, FileName, SizeOf(FileName));
          Files.Add(FileName);
        end;
      finally
        ReleaseStgMedium(StgMedium);
      end;
      Result := NOERROR;
    end;
  except
    on e: Exception do
      MessageBox(0, PChar(Format('Error: %s (%s)', [e.Message, e.ClassName])), 'dzContextMenu', MB_ICONERROR or MB_OK);
  end;
end;

function TContextMenu.QueryContextMenu(_Menu: HMENU; _indexMenu, _idCmdFirst, _idCmdLast, _UFlags: UINT): HResult;
var
  Id: integer;
  mii: TMenuItemInfo;
  Submenu: HMENU;
  MenuTitle: string;
begin
  Result := 0;
  try
    if ((_UFlags and $0000000F) = CMF_NORMAL) or ((_UFlags and CMF_EXPLORE) <> 0) then begin
      MenuTitle := FDm.IniFile.ReadString('global', 'caption', 'Submenu');
      Id := Integer(_idCmdFirst);
      Submenu := CreatePopupMenu;
      FDm.UpdateSubmenu(Submenu, Id);
      FillChar(mii, sizeof(mii), 0);
      mii.cbSize := sizeof(mii);
      mii.fMask := MIIM_SUBMENU or MIIM_ID or MIIM_TYPE or MIIM_STATE;
      mii.wID := Id;
      mii.hSubMenu := Submenu;
      mii.fType := MFT_STRING;
      mii.fState := MFS_ENABLED;
      mii.dwTypeData := PChar(MenuTitle);
      InsertMenuItem(_Menu, _indexMenu, LongBool(True), mii);
      Inc(Id);

    // Return value is number of items added to context menu
      FCmdCount := Id - integer(_idCmdFirst);
      Result := MakeResult(SEVERITY_SUCCESS, 0, FCmdCount);
    end;
  except
    on e: Exception do
      MessageBox(0, PChar(Format('Error: %s (%s)', [e.Message, e.ClassName])), 'dzContextMenu', MB_ICONERROR or MB_OK);
  end;
end;

function TContextMenu.GetCommandString(_idCmd, _uType: UINT; _pwReserved: PUINT; _PszName: LPSTR;
  _cchMax: UINT): HRESULT;
var
  szName: PWideChar absolute _PszName;
begin
  Result := E_INVALIDARG;
  try
    if _idCmd < FCmdCount then begin
      case _uType of
        GCS_HELPTEXTW: begin
            // Return the menu item's help
            StrLCopy(szName, PChar('execute some command'), _cchMax);
            Result := S_OK;
          end;
      end;
    end;
  except
    on e: Exception do
      MessageBox(0, PChar(Format('Error: %s (%s)', [e.Message, e.ClassName])), 'dzContextMenu', MB_ICONERROR or MB_OK);
  end;
end;

type
  TCMInvokeCommandInfoHack = record
    cbSize: DWORD;
    fMask: DWORD;
    hwnd: HWND;
    VerbLo: Word;
    VerbHi: Word;
    lpParameters: LPCSTR;
    lpDirectory: LPCSTR;
    nShow: Integer;
    dwHotKey: DWORD;
    hIcon: THandle;
  end;

function TContextMenu.InvokeCommand(var _ici: TCMInvokeCommandInfo): HResult;
var
  ici: TCMInvokeCommandInfoHack absolute _ici;
begin
  Result := E_FAIL;
  try
    if ici.VerbHi = 0 then begin
      FDm.DoCommand(ici.VerbLo);
      Result := S_OK;
    end;
  except
    on e: Exception do
      MessageBox(0, PChar(Format('Error: %s (%s)', [e.Message, e.ClassName])), 'dzContextMenu', MB_ICONERROR or MB_OK);
  end;
end;

type
  TContextMenuFactory = class(TComObjectFactory)
  public
    procedure UpdateRegistry(_Register: Boolean); override;
  end;

procedure TContextMenuFactory.UpdateRegistry(_Register: Boolean);
var
  ClassID: string;
  Ini: TMemIniFile;
  Sections: TStringList;
  i: Integer;
  FileType: string;
  reg: TRegistry;
  sl: TStringList;
  Key: string;
begin
  try
    ClassID := GUIDToString(TContextMenu.GUID);
    if _Register then begin
      inherited UpdateRegistry(_Register);

      if (Win32Platform = VER_PLATFORM_WIN32_NT) then begin
        reg := TRegistry.Create;
        try
          reg.RootKey := HKEY_LOCAL_MACHINE;
          reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions', True);
          reg.OpenKey('Approved', True);
          reg.WriteString(ClassID, 'dzContextMenu');
        finally
          FreeAndNil(reg);
        end;
      end;

      Sections := nil;
      Ini := Tdm_ContextMenu.IniFile;
      Sections := TStringList.Create;
      try
        Ini.ReadSections(Sections);
        for i := 0 to Sections.Count - 1 do begin
          FileType := Sections[i];
          if SameText(FileType, 'Global') then begin
          // maybe do something later on
          end else begin
            CreateRegKey(FileType + '\shellex', '', '');
            CreateRegKey(FileType + '\shellex\ContextMenuHandlers', '', '');
            CreateRegKey(FileType + '\shellex\ContextMenuHandlers\dzContextMenu', '', ClassID);
          end;
        end;
      finally
        FreeAndNil(Sections);
      end;
    end else begin
      if (Win32Platform = VER_PLATFORM_WIN32_NT) then begin
        reg := TRegistry.Create;
        try
          reg.RootKey := HKEY_LOCAL_MACHINE;
          reg.OpenKey('SOFTWARE\Microsoft\Windows\CurrentVersion\Shell Extensions', True);
          reg.OpenKey('Approved', True);
          reg.DeleteValue(ClassID);
        finally
          FreeAndNil(reg);
        end;
      end;

      sl := nil;
      reg := TRegistry.Create;
      try
        sl := TStringList.Create;
        reg.RootKey := HKEY_CLASSES_ROOT;
        reg.OpenKeyReadOnly('\');
        reg.GetKeyNames(sl);
        reg.CloseKey;
        for i := 0 to sl.Count - 1 do begin
          Key := '\' + sl[i] + '\shellex\ContextMenuHandlers\dzContextMenu';
          if reg.OpenKeyReadOnly(Key) then begin
            reg.CloseKey;
            reg.DeleteKey(Key);
          end;
        end;
      finally
        FreeAndNil(reg);
        FreeAndNil(sl);
      end;
      inherited UpdateRegistry(_Register);
    end;
  except
    on e: Exception do
      MessageBox(0, PChar(Format('Error: %s (%s)', [e.Message, e.ClassName])), 'dzContextMenu', MB_ICONERROR or MB_OK);
  end;
end;

initialization
  TContextMenuFactory.Create(ComServer, TContextMenu, TContextMenu.GUID,
    '', 'dzlib Context Menu Shell Extension', ciMultiInstance, tmApartment);

end.

