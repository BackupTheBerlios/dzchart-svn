unit u_AbstractVersionInfo;

interface

uses
  i_VersionInfo;

type
  TAbstractVersionInfo = class(TInterfacedObject)
  private
    FFileVersion: string;
    FProductVersion: string;
    FProductName: string;
    FMinorVer: integer;
    FLegalTrademarks: string;
    FRelease: integer;
    FLegalCopyright: string;
    FMajorVer: integer;
    FCompanyName: string;
    FAutoIncBuild: boolean;
    FFileDescription: string;
    FBuild: integer;
    FInternalName: string;
    FOriginalFilename: string;
    FComments: string;
  protected
    procedure Assign(const _VersionInfo: IVersionInfo); virtual;
    function GetAutoIncBuild: Boolean; virtual;
    function GetBuild: Integer; virtual;
    function GetComments: string; virtual;
    function GetCompanyName: string; virtual;
    function GetFileDescription: string; virtual;
    function GetFileVersion: string; virtual;
    function GetInternalName: string; virtual;
    function GetLegalCopyright: string; virtual;
    function GetLegalTrademarks: string; virtual;
    function GetMajorVer: Integer; virtual;
    function GetMinorVer: Integer; virtual;
    function GetOriginalFilename: string;
    function GetProductName: string; virtual;
    function GetProductVersion: string; virtual;
    function GetRelease: Integer; virtual;
    procedure SetAutoIncBuild(_AutoIncBuild: Boolean); virtual;
    procedure SetBuild(_Build: Integer); virtual;
    procedure SetComments(const _Comments: string); virtual;
    procedure SetCompanyName(_CompanyName: string); virtual;
    procedure SetFileDescription(_FileDescription: string); virtual;
    procedure SetFileVersion(_FileVersion: string); virtual;
    procedure SetInternalName(_InternalName: string); virtual;
    procedure SetLegalCopyright(_LegalCopyright: string); virtual;
    procedure SetLegalTrademarks(_LegalTrademarks: string); virtual;
    procedure SetMajorVer(_MajorVer: Integer); virtual;
    procedure SetMinorVer(_MinorVer: Integer); virtual;
    procedure SetOriginalFilename(_OriginalFilename: string); virtual;
    procedure SetProductName(_ProductName: string); virtual;
    procedure SetProductVersion(_ProductVersion: string); virtual;
    procedure SetRelease(_Release: Integer); virtual;
    procedure UpdateFile; virtual; abstract;
    function VerInfoFilename: string; virtual; abstract;
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
  end;

implementation

procedure TAbstractVersionInfo.Assign(const _VersionInfo: IVersionInfo);
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

function TAbstractVersionInfo.GetAutoIncBuild: boolean;
begin
  Result := FAutoIncBuild;
end;

function TAbstractVersionInfo.GetBuild: integer;
begin
  Result := FBuild;
end;

function TAbstractVersionInfo.GetComments: string;
begin
  Result := FComments;
end;

function TAbstractVersionInfo.GetCompanyName: string;
begin
  Result := FCompanyName;
end;

function TAbstractVersionInfo.GetFileDescription: string;
begin
  Result := FFileDescription;
end;

function TAbstractVersionInfo.GetFileVersion: string;
begin
  Result := FFileVersion;
end;

function TAbstractVersionInfo.GetInternalName: string;
begin
  Result := FInternalName;
end;

function TAbstractVersionInfo.GetLegalCopyright: string;
begin
  Result := FLegalCopyright;
end;

function TAbstractVersionInfo.GetLegalTrademarks: string;
begin
  Result := FLegalTrademarks;
end;

function TAbstractVersionInfo.GetMajorVer: integer;
begin
  Result := FMajorVer;
end;

function TAbstractVersionInfo.GetMinorVer: integer;
begin
  Result := FMinorVer;
end;

function TAbstractVersionInfo.GetOriginalFilename: string;
begin
  Result := FOriginalFilename;
end;

function TAbstractVersionInfo.GetProductName: string;
begin
  Result := FProductName;
end;

function TAbstractVersionInfo.GetProductVersion: string;
begin
  Result := FProductVersion;
end;

function TAbstractVersionInfo.GetRelease: integer;
begin
  Result := FRelease;
end;

procedure TAbstractVersionInfo.SetAutoIncBuild(_AutoIncBuild: boolean);
begin
  FAutoIncBuild := _AutoIncBuild;
end;

procedure TAbstractVersionInfo.SetBuild(_Build: integer);
begin
  FBuild := _Build;
end;

procedure TAbstractVersionInfo.SetComments(const _Comments: string);
begin
  FComments := _Comments;
end;

procedure TAbstractVersionInfo.SetCompanyName(_CompanyName: string);
begin
  FCompanyName := _CompanyName;
end;

procedure TAbstractVersionInfo.SetFileDescription(_FileDescription: string);
begin
  FFileDescription := _FileDescription;
end;

procedure TAbstractVersionInfo.SetFileVersion(_FileVersion: string);
begin
  FFileVersion := _FileVersion;
end;

procedure TAbstractVersionInfo.SetInternalName(_InternalName: string);
begin
  FInternalName := _InternalName;
end;

procedure TAbstractVersionInfo.SetLegalCopyright(_LegalCopyright: string);
begin
  FLegalCopyright := _LegalCopyright;
end;

procedure TAbstractVersionInfo.SetLegalTrademarks(_LegalTrademarks: string);
begin
  FLegalTrademarks := _LegalTrademarks;
end;

procedure TAbstractVersionInfo.SetMajorVer(_MajorVer: integer);
begin
  FMajorVer := _MajorVer;
end;

procedure TAbstractVersionInfo.SetMinorVer(_MinorVer: integer);
begin
  FMinorVer := _MinorVer;
end;

procedure TAbstractVersionInfo.SetOriginalFilename(_OriginalFilename: string);
begin
  FOriginalFilename := _OriginalFilename;
end;

procedure TAbstractVersionInfo.SetProductName(_ProductName: string);
begin
  FProductName := _ProductName;
end;

procedure TAbstractVersionInfo.SetProductVersion(_ProductVersion: string);
begin
  FProductVersion := _ProductVersion;
end;

procedure TAbstractVersionInfo.SetRelease(_Release: integer);
begin
  FRelease := _Release;
end;

end.

