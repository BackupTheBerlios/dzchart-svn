unit u_IniVersionInfo;

interface

uses
  SysUtils,
  IniFiles,
  i_VersionInfo;

type
  {: Tries to read a <projectname>.ini file, succeeds, if it exists
     @param Project is the project name (*.dpr file without extension)
     @param VersionInfo is a TVersionInfoRec record which will be filled with the version info }
  TIniVersionInfo = class(TInterfacedObject, IVersionInfo)
  protected
    FIniFile: TMemIniFile;
    FInfoSection: string;
    FInfoKeysSection: string;
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
    constructor Create(const _FullFilename: string; const _InfoSection: string;
      const _InfoKeysSection: string);
    destructor Destroy; override;
  end;

implementation

{ TIniVersionInfo }

constructor TIniVersionInfo.Create(const _FullFilename: string; const _InfoSection: string;
  const _InfoKeysSection: string);
begin
  inherited Create;
  FInfoSection := _InfoSection;
  FInfoKeysSection := _InfoKeysSection;
  FIniFile := TMemIniFile.Create(_FullFilename);
end;

destructor TIniVersionInfo.Destroy;
begin
  FIniFile.Free;
  inherited;
end;

procedure TIniVersionInfo.Assign(const _VersionInfo: IVersionInfo);
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

function TIniVersionInfo.GetAutoIncBuild: boolean;
begin
  Result := FIniFile.ReadInteger(FInfoSection, 'AutoIncBuild', 0) <> 0;
end;

function TIniVersionInfo.GetBuild: integer;
begin
  Result := FIniFile.ReadInteger(FInfoSection, 'Build', 0);
end;

function TIniVersionInfo.GetComments: string;
begin
  Result := FIniFile.ReadString(FInfoKeysSection, 'Comments', '');
end;

function TIniVersionInfo.GetCompanyName: string;
begin
  Result := FIniFile.ReadString(FInfoKeysSection, 'CompanyName', '');
end;

function TIniVersionInfo.GetFileDescription: string;
begin
  Result := FIniFile.ReadString(FInfoKeysSection, 'FileDescription', '');
end;

function TIniVersionInfo.GetFileVersion: string;
begin
  Result := FIniFile.ReadString(FInfoKeysSection, 'FileVersion', '');
end;

function TIniVersionInfo.GetInternalName: string;
begin
  Result := FIniFile.ReadString(FInfoKeysSection, 'InternalName', '');
end;

function TIniVersionInfo.GetLegalCopyright: string;
begin
  Result := FIniFile.ReadString(FInfoKeysSection, 'LegalCopyright', '');
end;

function TIniVersionInfo.GetLegalTrademarks: string;
begin
  Result := FIniFile.ReadString(FInfoKeysSection, 'LegalTrademarks', '');
end;

function TIniVersionInfo.GetMajorVer: integer;
begin
  Result := FIniFile.ReadInteger(FInfoSection, 'MajorVer', 0);
end;

function TIniVersionInfo.GetMinorVer: integer;
begin
  Result := FIniFile.ReadInteger(FInfoSection, 'MinorVer', 0);
end;

function TIniVersionInfo.GetOriginalFilename: string;
begin
  Result := FIniFile.ReadString(FInfoKeysSection, 'OriginalFilename', '');
end;

function TIniVersionInfo.GetProductName: string;
begin
  Result := FIniFile.ReadString(FInfoKeysSection, 'ProductName', '');
end;

function TIniVersionInfo.GetProductVersion: string;
begin
  Result := FIniFile.ReadString(FInfoKeysSection, 'ProductVersion', '');
end;

function TIniVersionInfo.GetRelease: integer;
begin
  Result := FIniFile.ReadInteger(FInfoSection, 'Release', 0);
end;

procedure TIniVersionInfo.SetAutoIncBuild(_AutoIncBuild: boolean);
begin
  FIniFile.WriteBool(FInfoSection, 'AutoIncBuild', _AutoIncBuild);
end;

procedure TIniVersionInfo.SetBuild(_Build: integer);
begin
  FIniFile.WriteInteger(FInfoSection, 'Build', _Build);
end;

procedure TIniVersionInfo.SetComments(const _Comments: string);
begin
  FIniFile.WriteString(FInfoKeysSection, 'Comments', _Comments);
end;

procedure TIniVersionInfo.SetCompanyName(_CompanyName: string);
begin
  FIniFile.WriteString(FInfoKeysSection, 'CompanyName', _CompanyName);
end;

procedure TIniVersionInfo.SetFileDescription(_FileDescription: string);
begin
  FIniFile.WriteString(FInfoKeysSection, 'FileDescription', _FileDescription);
end;

procedure TIniVersionInfo.SetFileVersion(_FileVersion: string);
begin
  FIniFile.WriteString(FInfoKeysSection, 'FileVersion', _FileVersion);
end;

procedure TIniVersionInfo.SetInternalName(_InternalName: string);
begin
  FIniFile.WriteString(FInfoKeysSection, 'InternalName', _InternalName);
end;

procedure TIniVersionInfo.SetLegalCopyright(_LegalCopyright: string);
begin
  FIniFile.WriteString(FInfoKeysSection, 'LegalCopyright', _LegalCopyright);
end;

procedure TIniVersionInfo.SetLegalTrademarks(_LegalTrademarks: string);
begin
  FIniFile.WriteString(FInfoKeysSection, 'LegalTrademarks', _LegalTrademarks);
end;

procedure TIniVersionInfo.SetMajorVer(_MajorVer: integer);
begin
  FIniFile.WriteInteger(FInfoSection, 'MajorVer', _MajorVer);
end;

procedure TIniVersionInfo.SetMinorVer(_MinorVer: integer);
begin
  FIniFile.WriteInteger(FInfoSection, 'MinorVer', _MinorVer);
end;

procedure TIniVersionInfo.SetOriginalFilename(_OriginalFilename: string);
begin
  FIniFile.WriteString(FInfoKeysSection, 'OriginalFilename', _OriginalFilename);
end;

procedure TIniVersionInfo.SetProductName(_ProductName: string);
begin
  FIniFile.WriteString(FInfoKeysSection, 'ProductName', _ProductName);
end;

procedure TIniVersionInfo.SetProductVersion(_ProductVersion: string);
begin
  FIniFile.WriteString(FInfoKeysSection, 'ProductVersion', _ProductVersion);
end;

procedure TIniVersionInfo.SetRelease(_Release: integer);
begin
  FIniFile.WriteInteger(FInfoSection, 'Release', _Release);
end;

procedure TIniVersionInfo.UpdateFile;
begin
  FIniFile.UpdateFile;
end;

end.

