unit d_ContextMenu;

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
  Dialogs;

const
  INI_FILE = 'c:\dzcontextmenu.ini';

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
begin
  if _Idx < FItems.Count then
    MessageBox(0, PChar(FItems[_Idx] + #13#10 + FFiles.Text), 'Execute', MB_ICONINFORMATION or MB_OK);
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
          IniFile.ReadSection(Sections[SecIdx], FItems);
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

initialization
//  MessageBox(0, 'Attach debugger now!', 'Initialization', MB_ICONINFORMATION or MB_OK);
  gblIni := TMemIniFile.Create(INI_FILE);
finalization
  FreeAndNil(gblIni);
end.

