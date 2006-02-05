unit u_DofVersionInfo;

interface

uses
  SysUtils,
  IniFiles,
  i_VersionInfo;

type
  {: Tries to read a <projectname>.dof file, succeeds, if it exists and IncludeVerInfo is <> 0
     @param Project is the project name (*.dpr file without extension)
     @param VersionInfo is a TVersionInfoRec record which will be filled with the version info }
  TDofVersionInfo = class(TInterfacedObject, IVersionInfo)
  protected
    FDofFile: TMemIniFile;
  protected // implementation of IVersionInfo
    function GetBuild: integer;
    procedure SetBuild(_Build: integer);
    function GetMajorVer: integer;
    procedure SetMajorVer(_MajorVer: integer);
    function GetMinorVer: integer;
    procedure SetMinorVer(_MinorVer: integer);
    function GetRelease: integer;
    procedure SetRelease(_Release: integer);
    function GetCompanyName: string;
    procedure SetCompanyName(_CompanyName: string);
    function GetFileDescription: string;
    procedure SetFileDescription(_FileDescription: string);
    function GetFileVersion: string;
    procedure SetFileVersion(_FileVersion: string);
    function GetInternalName: string;
    procedure SetInternalName(_InternalName: string);
    function GetLegalCopyright: string;
    procedure SetLegalCopyright(_LegalCopyright: string);
    function GetLegalTrademarks: string;
    procedure SetLegalTrademarks(_LegalTrademarks: string);
    function GetOriginalFilename: string;
    procedure SetOriginalFilename(_OriginalFilename: string);
    function GetProductName: string;
    procedure SetProductName(_ProductName: string);
    function GetProductVersion: string;
    procedure SetProductVersion(_ProductVersion: string);
    function GetAutoIncBuild: boolean;
    procedure SetAutoIncBuild(_AutoIncBuild: boolean);
    function GetComments: string;
    procedure SetComments(const _Comments: string);
  //
    procedure Assign(const _VersionInfo: IVersionInfo);
    procedure UpdateFile;
  //
    property AutoIncBuild: boolean read GetAutoIncBuild write SetAutoIncBuild;
    property Build: integer read GetBuild write SetBuild;
    property Comments: string read GetComments write SetComments;
    property CompanyName: string read GetCompanyName write SetCompanyName;
    property FileDescription: string read GetFileDescription write SetFileDescription;
    property FileVersion: string read GetFileVersion write SetFileVersion;
    property InternalName: string read GetInternalName write SetInternalName;
    property LegalCopyright: string read GetLegalCopyright write SetLegalCopyright;
    property LegalTrademarks: string read GetLegalTrademarks write SetLegalTrademarks;
    property MajorVer: integer read GetMajorVer write SetMajorVer;
    property MinorVer: integer read GetMinorVer write SetMinorVer;
    property OriginalFilename: string read GetOriginalFilename write SetOriginalFilename;
    property ProductName: string read GetProductName write SetProductName;
    property ProductVersion: string read GetProductVersion write SetProductVersion;
    property Release: integer read GetRelease write SetRelease;
  public
    constructor Create(const _Project: string);
    destructor Destroy; override;
  end;

implementation

const
  VERSION_INFO_SECTION = 'Version Info';
  VERSION_INFO_KEYS_SECTION = 'Version Info Keys';

{ TDofVersionInfo }

procedure TDofVersionInfo.Assign(const _VersionInfo: IVersionInfo);
begin
  AutoIncBuild := _VersionInfo.AutoIncBuild;
  Build := _VersionInfo.Build;
  Comments := _VersionInfo.Comments;
  CompanyName := _VersionInfo.CompanyName;
  FileDescription := _VersionInfo.FileDescription;
  FileVersion := _VersionInfo.FileVersion;
  InternalName := _VersionInfo.InternalName;
  LegalCopyright := _VersionInfo.LegalCopyright;
  LegalTrademarks := _VersionInfo.LegalTrademarks;
  MajorVer := _VersionInfo.MajorVer;
  MinorVer := _VersionInfo.MinorVer;
  OriginalFilename := _VersionInfo.OriginalFilename;
  ProductName := _VersionInfo.ProductName;
  ProductVersion := _VersionInfo.ProductVersion;
  Release := _VersionInfo.Release;
end;

constructor TDofVersionInfo.Create(const _Project: string);
begin
  inherited Create;
  FDofFile := TMemIniFile.Create(_Project + '.dof');
  if FDofFile.ReadInteger(VERSION_INFO_SECTION, 'IncludeVerInfo', 0) <> 1 then
    raise ENoVersionInfo.Create('.dof file does not contain version info');
end;

destructor TDofVersionInfo.Destroy;
begin
  FDofFile.Free;
  inherited;
end;

function TDofVersionInfo.GetAutoIncBuild: boolean;
begin
  Result := FDofFile.ReadInteger(VERSION_INFO_SECTION, 'AutoIncBuild', 0) <> 0;
end;

function TDofVersionInfo.GetBuild: integer;
begin
  Result := FDofFile.ReadInteger(VERSION_INFO_SECTION, 'Build', 0);
end;

function TDofVersionInfo.GetComments: string;
begin
  Result := FDofFile.ReadString(VERSION_INFO_KEYS_SECTION, 'Comments', '');
end;

function TDofVersionInfo.GetCompanyName: string;
begin
  Result := FDofFile.ReadString(VERSION_INFO_KEYS_SECTION, 'CompanyName', '');
end;

function TDofVersionInfo.GetFileDescription: string;
begin
  Result := FDofFile.ReadString(VERSION_INFO_KEYS_SECTION, 'FileDescription', '');
end;

function TDofVersionInfo.GetFileVersion: string;
begin
  Result := FDofFile.ReadString(VERSION_INFO_KEYS_SECTION, 'FileVersion', '');
end;

function TDofVersionInfo.GetInternalName: string;
begin
  Result := FDofFile.ReadString(VERSION_INFO_KEYS_SECTION, 'InternalName', '');
end;

function TDofVersionInfo.GetLegalCopyright: string;
begin
  Result := FDofFile.ReadString(VERSION_INFO_KEYS_SECTION, 'LegalCopyright', '');
end;

function TDofVersionInfo.GetLegalTrademarks: string;
begin
  Result := FDofFile.ReadString(VERSION_INFO_KEYS_SECTION, 'LegalTrademarks', '');
end;

function TDofVersionInfo.GetMajorVer: integer;
begin
  Result := FDofFile.ReadInteger(VERSION_INFO_SECTION, 'MajorVer', 0);
end;

function TDofVersionInfo.GetMinorVer: integer;
begin
  Result := FDofFile.ReadInteger(VERSION_INFO_SECTION, 'MinorVer', 0);
end;

function TDofVersionInfo.GetOriginalFilename: string;
begin
  Result := FDofFile.ReadString(VERSION_INFO_KEYS_SECTION, 'OriginalFilename', '');
end;

function TDofVersionInfo.GetProductName: string;
begin
  Result := FDofFile.ReadString(VERSION_INFO_KEYS_SECTION, 'ProductName', '');
end;

function TDofVersionInfo.GetProductVersion: string;
begin
  Result := FDofFile.ReadString(VERSION_INFO_KEYS_SECTION, 'ProductVersion', '');
end;

function TDofVersionInfo.GetRelease: integer;
begin
  Result := FDofFile.ReadInteger(VERSION_INFO_SECTION, 'Release', 0);
end;

procedure TDofVersionInfo.SetAutoIncBuild(_AutoIncBuild: boolean);
begin
  FDofFile.WriteBool(VERSION_INFO_SECTION, 'AutoIncBuild', _AutoIncBuild);
end;

procedure TDofVersionInfo.SetBuild(_Build: integer);
begin
  FDofFile.WriteInteger(VERSION_INFO_SECTION, 'Build', _Build);
end;

procedure TDofVersionInfo.SetComments(const _Comments: string);
begin
  FDofFile.WriteString(VERSION_INFO_KEYS_SECTION, 'Comments', _Comments);
end;

procedure TDofVersionInfo.SetCompanyName(_CompanyName: string);
begin
  FDofFile.WriteString(VERSION_INFO_KEYS_SECTION, 'CompanyName', _CompanyName);
end;

procedure TDofVersionInfo.SetFileDescription(_FileDescription: string);
begin
  FDofFile.WriteString(VERSION_INFO_KEYS_SECTION, 'FileDescription', _FileDescription);
end;

procedure TDofVersionInfo.SetFileVersion(_FileVersion: string);
begin
  FDofFile.WriteString(VERSION_INFO_KEYS_SECTION, 'FileVersion', _FileVersion);
end;

procedure TDofVersionInfo.SetInternalName(_InternalName: string);
begin
  FDofFile.WriteString(VERSION_INFO_KEYS_SECTION, 'InternalName', _InternalName);
end;

procedure TDofVersionInfo.SetLegalCopyright(_LegalCopyright: string);
begin
  FDofFile.WriteString(VERSION_INFO_KEYS_SECTION, 'LegalCopyright', _LegalCopyright);
end;

procedure TDofVersionInfo.SetLegalTrademarks(_LegalTrademarks: string);
begin
  FDofFile.WriteString(VERSION_INFO_KEYS_SECTION, 'LegalTrademarks', _LegalTrademarks);
end;

procedure TDofVersionInfo.SetMajorVer(_MajorVer: integer);
begin
  FDofFile.WriteInteger(VERSION_INFO_SECTION, 'MajorVer', _MajorVer);
end;

procedure TDofVersionInfo.SetMinorVer(_MinorVer: integer);
begin
  FDofFile.WriteInteger(VERSION_INFO_SECTION, 'MinorVer', _MinorVer);
end;

procedure TDofVersionInfo.SetOriginalFilename(_OriginalFilename: string);
begin
  FDofFile.WriteString(VERSION_INFO_KEYS_SECTION, 'OriginalFilename', _OriginalFilename);
end;

procedure TDofVersionInfo.SetProductName(_ProductName: string);
begin
  FDofFile.WriteString(VERSION_INFO_KEYS_SECTION, 'ProductName', _ProductName);
end;

procedure TDofVersionInfo.SetProductVersion(_ProductVersion: string);
begin
  FDofFile.WriteString(VERSION_INFO_KEYS_SECTION, 'ProductVersion', _ProductVersion);
end;

procedure TDofVersionInfo.SetRelease(_Release: integer);
begin
  FDofFile.WriteInteger(VERSION_INFO_SECTION, 'Release', _Release);
end;

procedure TDofVersionInfo.UpdateFile;
begin
  FDofFile.UpdateFile;
end;

end.

