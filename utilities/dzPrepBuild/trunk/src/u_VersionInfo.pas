unit u_VersionInfo;

interface

uses
  SysUtils;

type
  TVersionInfo = class
  private
    FMajorVer: integer;
    FMinorVer: integer;
    FRelease: integer;
    FBuild: integer;

    FFileVersion: string;
    FProductVersion: string;
    FProductName: string;
    FLegalTrademarks: string;
    FLegalCopyright: string;
    FCompanyName: string;
    FAutoIncBuild: boolean;
    FFileDescription: string;
    FInternalName: string;
    FOriginalFilename: string;
    FComments: string;
  protected
    function GetAutoIncBuild: boolean;
    procedure SetAutoIncBuild(_AutoIncBuild: Boolean); virtual;
    //
    procedure Assign(_VersionInfo: TVersionInfo); virtual;
    //
    function GetMajorVer: Integer; virtual;
    function GetMinorVer: Integer; virtual;
    function GetRelease: Integer; virtual;
    function GetBuild: Integer; virtual;
    procedure SetMajorVer(_MajorVer: Integer); virtual;
    procedure SetMinorVer(_MinorVer: Integer); virtual;
    procedure SetRelease(_Release: Integer); virtual;
    procedure SetBuild(_Build: Integer); virtual;
    //
    function GetComments: string; virtual;
    function GetCompanyName: string; virtual;
    function GetFileDescription: string; virtual;
    function GetFileVersion: string; virtual;
    function GetInternalName: string; virtual;
    function GetLegalCopyright: string; virtual;
    function GetLegalTrademarks: string; virtual;
    function GetOriginalFilename: string;
    function GetProductName: string; virtual;
    function GetProductVersion: string; virtual;
    procedure SetComments(const _Comments: string); virtual;
    procedure SetCompanyName(_CompanyName: string); virtual;
    procedure SetFileDescription(_FileDescription: string); virtual;
    procedure SetFileVersion(_FileVersion: string); virtual;
    procedure SetInternalName(_InternalName: string); virtual;
    procedure SetLegalCopyright(_LegalCopyright: string); virtual;
    procedure SetLegalTrademarks(_LegalTrademarks: string); virtual;
    procedure SetOriginalFilename(_OriginalFilename: string); virtual;
    procedure SetProductName(_ProductName: string); virtual;
    procedure SetProductVersion(_ProductVersion: string); virtual;
  public
    procedure UpdateFileVersion;
    //
    property AutoIncBuild: boolean read GetAutoIncBuild write SetAutoIncBuild;
    //
    property MajorVer: integer read GetMajorVer write SetMajorVer;
    property MinorVer: integer read GetMinorVer write SetMinorVer;
    property Release: integer read GetRelease write SetRelease;
    property Build: integer read GetBuild write SetBuild;
    //
    property Comments: string read GetComments write SetComments;
    property CompanyName: string read GetCompanyName write SetCompanyName;
    property FileDescription: string read GetFileDescription write SetFileDescription;
    property FileVersion: string read GetFileVersion write SetFileVersion;
    property InternalName: string read GetInternalName write SetInternalName;
    property LegalCopyright: string read GetLegalCopyright write SetLegalCopyright;
    property LegalTrademarks: string read GetLegalTrademarks write SetLegalTrademarks;
    property OriginalFilename: string read GetOriginalFilename write SetOriginalFilename;
    property ProductName: string read GetProductName write SetProductName;
    property ProductVersion: string read GetProductVersion write SetProductVersion;
  end;

implementation

{ TVersionInfo }

procedure TVersionInfo.Assign(_VersionInfo: TVersionInfo);
begin
  AutoIncBuild := _VersionInfo.AutoIncBuild;

  MajorVer := _VersionInfo.MajorVer;
  MinorVer := _VersionInfo.MinorVer;
  Release := _VersionInfo.Release;
  Build := _VersionInfo.Build;

  Comments := _VersionInfo.Comments;
  CompanyName := _VersionInfo.CompanyName;
  FileDescription := _VersionInfo.FileDescription;
  FileVersion := _VersionInfo.FileVersion;
  InternalName := _VersionInfo.InternalName;
  LegalCopyright := _VersionInfo.LegalCopyright;
  LegalTrademarks := _VersionInfo.LegalTrademarks;
  OriginalFilename := _VersionInfo.OriginalFilename;
  ProductName := _VersionInfo.ProductName;
  ProductVersion := _VersionInfo.ProductVersion;
end;

function TVersionInfo.GetAutoIncBuild: boolean;
begin
  Result := FAutoIncBuild;
end;

function TVersionInfo.GetBuild: integer;
begin
  Result := FBuild;
end;

function TVersionInfo.GetComments: string;
begin
  Result := FComments;
end;

function TVersionInfo.GetCompanyName: string;
begin
  Result := FCompanyName;
end;

function TVersionInfo.GetFileDescription: string;
begin
  Result := FFileDescription;
end;

function TVersionInfo.GetFileVersion: string;
begin
  Result := FFileVersion;
end;

function TVersionInfo.GetInternalName: string;
begin
  Result := FInternalName;
end;

function TVersionInfo.GetLegalCopyright: string;
begin
  Result := FLegalCopyright;
end;

function TVersionInfo.GetLegalTrademarks: string;
begin
  Result := FLegalTrademarks;
end;

function TVersionInfo.GetMajorVer: integer;
begin
  Result := FMajorVer;
end;

function TVersionInfo.GetMinorVer: integer;
begin
  Result := FMinorVer;
end;

function TVersionInfo.GetOriginalFilename: string;
begin
  Result := FOriginalFilename;
end;

function TVersionInfo.GetProductName: string;
begin
  Result := FProductName;
end;

function TVersionInfo.GetProductVersion: string;
begin
  Result := FProductVersion;
end;

function TVersionInfo.GetRelease: integer;
begin
  Result := FRelease;
end;

procedure TVersionInfo.SetAutoIncBuild(_AutoIncBuild: boolean);
begin
  FAutoIncBuild := _AutoIncBuild;
end;

procedure TVersionInfo.SetBuild(_Build: integer);
begin
  FBuild := _Build;
end;

procedure TVersionInfo.SetComments(const _Comments: string);
begin
  FComments := _Comments;
end;

procedure TVersionInfo.SetCompanyName(_CompanyName: string);
begin
  FCompanyName := _CompanyName;
end;

procedure TVersionInfo.SetFileDescription(_FileDescription: string);
begin
  FFileDescription := _FileDescription;
end;

procedure TVersionInfo.SetFileVersion(_FileVersion: string);
begin
  FFileVersion := _FileVersion;
end;

procedure TVersionInfo.SetInternalName(_InternalName: string);
begin
  FInternalName := _InternalName;
end;

procedure TVersionInfo.SetLegalCopyright(_LegalCopyright: string);
begin
  FLegalCopyright := _LegalCopyright;
end;

procedure TVersionInfo.SetLegalTrademarks(_LegalTrademarks: string);
begin
  FLegalTrademarks := _LegalTrademarks;
end;

procedure TVersionInfo.SetMajorVer(_MajorVer: integer);
begin
  FMajorVer := _MajorVer;
end;

procedure TVersionInfo.SetMinorVer(_MinorVer: integer);
begin
  FMinorVer := _MinorVer;
end;

procedure TVersionInfo.SetOriginalFilename(_OriginalFilename: string);
begin
  FOriginalFilename := _OriginalFilename;
end;

procedure TVersionInfo.SetProductName(_ProductName: string);
begin
  FProductName := _ProductName;
end;

procedure TVersionInfo.SetProductVersion(_ProductVersion: string);
begin
  FProductVersion := _ProductVersion;
end;

procedure TVersionInfo.SetRelease(_Release: integer);
begin
  FRelease := _Release;
end;

procedure TVersionInfo.UpdateFileVersion;
begin
  FileVersion := Format('%d.%d.%d.%d', [MajorVer, MinorVer, Release, Build]);
end;

end.

