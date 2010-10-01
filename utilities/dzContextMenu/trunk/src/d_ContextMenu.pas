unit d_ContextMenu;

{$IFDEF debug}
{.$DEFINE show_attach_dialog}
{$ENDIF debug}

interface

uses
  Windows,
  SysUtils,
  Classes,
  IniFiles,
  Menus;

type
  Tdm_ContextMenu = class(TDataModule)
    ThePopupMenu: TPopupMenu;
  private
    FFiles: TStringList;
    FMaxItemCount: Integer;
    FSection: string;
    FItems: TStringList;
    procedure UpdatePopup;
  public
    class function IniFile: TMemIniFile;
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateSubmenu(_SubMenu: HMenu; var _Id: integer);
    procedure DoCommand(_Idx: integer);
    property Files: TStringList read FFiles;
    property MaxItemCount: Integer read FMaxItemCount;
  end;

implementation

{$R *.dfm}

uses
  u_dzShellApiUtils,
  u_dzFileUtils,
  u_dzOsUtils,
  u_dzExecutor;

const
  INI_FILE = 'menu.ini';

var
  gblIni: TMemIniFile = nil;

type
  TMenuItemEx = class(TMenuItem)
  private
    FExecutable: string;
  public
    procedure Execute(_Files: TStrings);
    property Executable: string read FExecutable write FExecutable;
  end;

constructor Tdm_ContextMenu.Create(_Owner: TComponent);
var
  Sections: TStringList;
  i: Integer;
  Ini: TMemIniFile;
begin
  inherited;
  FFiles := TStringList.Create;
  FItems := TStringList.Create;

  Ini := IniFile;
  FMaxItemCount := 0;
  Sections := nil;
  Sections := TStringList.Create;
  try
    Ini.ReadSections(Sections);
    for i := 0 to Sections.Count - 1 do begin
      FItems.Clear;
      Ini.ReadSection(Sections[i], FItems);
      // First entry is file extension
      FItems.Delete(0);
      if FItems.Count > FMaxItemCount then
        FMaxItemCount := FItems.Count;
    end;
  finally
    FreeAndNil(Sections);
  end;
end;

destructor Tdm_ContextMenu.Destroy;
begin
  FreeAndNil(FItems);
  FreeAndNil(FFiles);
  inherited;
end;

procedure Tdm_ContextMenu.DoCommand(_Idx: integer);
var
  mi: TMenuItemEx;
begin
  if _Idx < ThePopupMenu.Items.Count then begin
    mi := ThePopupMenu.Items[_Idx] as TMenuItemEx;
    mi.Execute(FFiles);
  end;
end;

procedure Tdm_ContextMenu.UpdatePopup;

  function HandleSection(const _Section: string): boolean;
  var
    i: integer;
    Ext: string;
    SectExt: string;
    ItemIdx: Integer;
    mi: TMenuItemEx;
  begin
    Result := false;
    for i := 0 to FFiles.Count - 1 do begin
      Ext := ExtractFileExt(FFiles[i]);
      SectExt := IniFile.ReadString(_Section, 'extension', '');
      if SameText(Ext, SectExt) then begin
        FItems.Clear;
        FSection := _Section;
        IniFile.ReadSection(FSection, FItems);
        FItems.Delete(0);
        for ItemIdx := 0 to FItems.Count - 1 do begin
          mi := TMenuItemEx.Create(Self);
          mi.Caption := FItems[ItemIdx];
          mi.Executable := IniFile.ReadString(_Section, FItems[ItemIdx], '');
          ThePopupMenu.Items.Add(mi);
        end;
        exit(true);
      end;
    end;
  end;

var
  SecIdx: Integer;
  Sections: TStringList;
begin
  ThePopupMenu.Items.Clear;

  Sections := TStringList.Create;
  try
    IniFile.ReadSections(Sections);
    for SecIdx := 0 to Sections.Count - 1 do begin
      if HandleSection(Sections[SecIdx]) then
        exit;
    end;
  finally
    FreeAndNil(Sections);
  end;
end;

procedure Tdm_ContextMenu.UpdateSubmenu(_SubMenu: HMenu; var _Id: integer);
var
  i: integer;
  mi: TMenuItem;
  mii: TMenuItemInfo;
begin
  UpdatePopup;

  for i := 0 to ThePopupMenu.Items.Count - 1 do begin
    mi := ThePopupMenu.Items[i];

    FillChar(mii, sizeof(mii), 0);
    mii.cbSize := sizeof(mii);
    mii.fMask := MIIM_CHECKMARKS or MIIM_ID or MIIM_TYPE or MIIM_STATE or MIIM_DATA;
    mii.wID := _Id;
    mii.hSubMenu := _SubMenu;
    if mi.IsLine then
      mii.fType := MFT_SEPARATOR
    else
      mii.fType := MFT_STRING;
    mii.fState := MFS_ENABLED;
    mii.dwItemData := integer(mi);
    mii.dwTypeData := PChar(mi.Caption);
    InsertMenuItem(_SubMenu, i, LongBool(True), mii);
    Inc(_Id);
  end;

end;

class function Tdm_ContextMenu.IniFile: TMemIniFile;
begin
  Result := gblIni;
end;

procedure Initialize;
var
  AppDataDir: string;
  ModuleName: string;
  IniName: string;
begin
  ModuleName := GetModuleFilename;
  AppDataDir := TWindowsShell.GetAppDataDir;
  IniName := itpd(AppDataDir) + ChangeFileExt(ExtractFileName(ModuleName), '') + '\' + INI_FILE;
  gblIni := TMemIniFile.Create(IniName);
end;

procedure Finalize;
begin
  FreeAndNil(gblIni);
end;

{ TMenuItemEx }

procedure TMenuItemEx.Execute(_Files: TStrings);
var
  Exec: TExecutor;
  s: string;
begin
  Exec := TExecutor.Create;
  try
    if not Exec.FindExecutable(Executable) then
      raise Exception.CreateFmt('Could not find executable %s', [Executable]);
    _Files.Delimiter := ' ';
    _Files.QuoteChar := '"';
    s := _Files.DelimitedText;
    Exec.Commandline := s;
    Exec.Visible := true;
    Exec.WorkingDir := Extractfiledir(_Files[0]);
    Exec.Execute;
  finally
    FreeAndNil(Exec)
  end;
end;

initialization
{$IFDEF show_attach_dialog}
  MessageBox(0, 'Attach debugger now!', 'Initialization', MB_ICONINFORMATION or MB_OK);
{$ENDIF show_attach_dialog}
  Initialize;
finalization
  Finalize;
end.

