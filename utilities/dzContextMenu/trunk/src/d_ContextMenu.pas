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
  Executable: string;
  Exec: TExecutor;
  s: string;
begin
  if _Idx < FItems.Count then begin
    Executable := IniFile.ReadString(FSection, FItems[_Idx], '');
    Exec := TExecutor.Create;
    try
      if not Exec.FindExecutable(Executable) then
        raise Exception.CreateFmt('Could not find executable %s', [Executable]);
      FFiles.Delimiter := ' ';
      FFiles.QuoteChar := '"';
      s := FFiles.DelimitedText;
      Exec.Commandline := s;
      Exec.Visible := true;
      Exec.WorkingDir := Extractfiledir(FFiles[0]);
      Exec.Execute;
    finally
      FreeAndNil(Exec)
    end;
  end;
end;

procedure Tdm_ContextMenu.UpdatePopup;
var
  SecIdx: Integer;
  SectExt: string;
  Ext: string;
  Sections: TStringList;
  i: Integer;
  ItemIdx: Integer;
  mi: TMenuItem;
begin
  ThePopupMenu.Items.Clear;

  Sections := TStringList.Create;
  try
    IniFile.ReadSections(Sections);
    for SecIdx := 0 to Sections.Count - 1 do begin
      for i := 0 to Files.Count - 1 do begin
        Ext := ExtractFileExt(Files[i]);
        SectExt := IniFile.ReadString(Sections[SecIdx], 'extension', '');
        if SameText(Ext, SectExt) then begin
          FItems.Clear;
          FSection := Sections[SecIdx];
          IniFile.ReadSection(FSection, FItems);
          FItems.Delete(0);
          for ItemIdx := 0 to FItems.Count - 1 do begin
            mi := TMenuItem.Create(Self);
            mi.Caption := FItems[ItemIdx];
            ThePopupMenu.Items.Add(mi);
          end;
          exit;
        end;
      end;
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

initialization
{$IFDEF show_attach_dialog}
  MessageBox(0, 'Attach debugger now!', 'Initialization', MB_ICONINFORMATION or MB_OK);
{$ENDIF show_attach_dialog}
  Initialize;
finalization
  Finalize;
end.

