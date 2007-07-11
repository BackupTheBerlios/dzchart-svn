unit i_VersionInfo;

interface

uses
  SysUtils;

type
  ENoVersionInfo = class(Exception);

type
  IVersionInfo = interface ['{C228DED6-C3B2-43C0-B061-28EA5D2EB93C}']
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
    function VerInfoFilename: string;
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

end.

