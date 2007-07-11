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
    function ReadInteger(const _Section, _Ident: string; _Default: integer): integer; virtual;
    procedure WriteInteger(const _Section, _Ident: string; _Value: integer); virtual;
    function ReadString(const _Section, _Ident: string; _Default: string): string; virtual;
    procedure WriteString(const _Section, _Ident: string; _Value: string); virtual;
    function ReadBool(const _Section, _Ident: string; _Default: boolean): boolean; virtual;
    procedure WriteBool(const _Section, _Ident: string; _Value: boolean); virtual;
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
    {: Creates a TIniVersionInfo instance.
       @param FullFilename is the full filename including path and extension of
                           file to use
       @param InfoSection is the name of the section that contains the general
                          version info like Major/Minor version, Release etc.
                          In a Delphi .dof file this section is called [Version Info]
       @param InfoKeySection is the name of the section that contains the additional
                             strings of the version information
                             In a Delphi .dof file this section is called [Version Info Keys] }
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

function TIniVersionInfo.ReadBool(const _Section, _Ident: string; _Default: boolean): boolean;
begin
  Result := 0 <> ReadInteger(_Section, _Ident, Ord(_Default));
end;

function TIniVersionInfo.ReadInteger(const _Section, _Ident: string; _Default: integer): integer;
var
  s: string;
begin
  s := ReadString(_Section, _Ident, IntToStr(_Default));
  if not TryStrToInt(s, Result) then
    Result := _Default;
end;

function TIniVersionInfo.ReadString(const _Section, _Ident: string; _Default: string): string;
begin
  Result := FIniFile.ReadString(_Section, _Ident, _Default);
end;

procedure TIniVersionInfo.WriteBool(const _Section, _Ident: string; _Value: boolean);
begin
  WriteInteger(_Section, _Ident, Ord(_Value));
end;

procedure TIniVersionInfo.WriteInteger(const _Section, _Ident: string; _Value: integer);
begin
  WriteString(_Section, _Ident, IntToStr(_Value));
end;

procedure TIniVersionInfo.WriteString(const _Section, _Ident: string; _Value: string);
begin
  FIniFile.WriteString(_Section, _Ident, _Value);
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
  Result := ReadInteger(FInfoSection, 'AutoIncBuild', 0) <> 0;
end;

function TIniVersionInfo.GetBuild: integer;
begin
  Result := ReadInteger(FInfoSection, 'Build', 0);
end;

function TIniVersionInfo.GetComments: string;
begin
  Result := ReadString(FInfoKeysSection, 'Comments', '');
end;

function TIniVersionInfo.GetCompanyName: string;
begin
  Result := ReadString(FInfoKeysSection, 'CompanyName', '');
end;

function TIniVersionInfo.GetFileDescription: string;
begin
  Result := ReadString(FInfoKeysSection, 'FileDescription', '');
end;

function TIniVersionInfo.GetFileVersion: string;
begin
  Result := ReadString(FInfoKeysSection, 'FileVersion', '');
end;

function TIniVersionInfo.GetInternalName: string;
begin
  Result := ReadString(FInfoKeysSection, 'InternalName', '');
end;

function TIniVersionInfo.GetLegalCopyright: string;
begin
  Result := ReadString(FInfoKeysSection, 'LegalCopyright', '');
end;

function TIniVersionInfo.GetLegalTrademarks: string;
begin
  Result := ReadString(FInfoKeysSection, 'LegalTrademarks', '');
end;

function TIniVersionInfo.GetMajorVer: integer;
begin
  Result := ReadInteger(FInfoSection, 'MajorVer', 0);
end;

function TIniVersionInfo.GetMinorVer: integer;
begin
  Result := ReadInteger(FInfoSection, 'MinorVer', 0);
end;

function TIniVersionInfo.GetOriginalFilename: string;
begin
  Result := ReadString(FInfoKeysSection, 'OriginalFilename', '');
end;

function TIniVersionInfo.GetProductName: string;
begin
  Result := ReadString(FInfoKeysSection, 'ProductName', '');
end;

function TIniVersionInfo.GetProductVersion: string;
begin
  Result := ReadString(FInfoKeysSection, 'ProductVersion', '');
end;

function TIniVersionInfo.GetRelease: integer;
begin
  Result := ReadInteger(FInfoSection, 'Release', 0);
end;

procedure TIniVersionInfo.SetAutoIncBuild(_AutoIncBuild: boolean);
begin
  WriteBool(FInfoSection, 'AutoIncBuild', _AutoIncBuild);
end;

procedure TIniVersionInfo.SetBuild(_Build: integer);
begin
  WriteInteger(FInfoSection, 'Build', _Build);
end;

procedure TIniVersionInfo.SetComments(const _Comments: string);
begin
  WriteString(FInfoKeysSection, 'Comments', _Comments);
end;

procedure TIniVersionInfo.SetCompanyName(_CompanyName: string);
begin
  WriteString(FInfoKeysSection, 'CompanyName', _CompanyName);
end;

procedure TIniVersionInfo.SetFileDescription(_FileDescription: string);
begin
  WriteString(FInfoKeysSection, 'FileDescription', _FileDescription);
end;

procedure TIniVersionInfo.SetFileVersion(_FileVersion: string);
begin
  WriteString(FInfoKeysSection, 'FileVersion', _FileVersion);
end;

procedure TIniVersionInfo.SetInternalName(_InternalName: string);
begin
  WriteString(FInfoKeysSection, 'InternalName', _InternalName);
end;

procedure TIniVersionInfo.SetLegalCopyright(_LegalCopyright: string);
begin
  WriteString(FInfoKeysSection, 'LegalCopyright', _LegalCopyright);
end;

procedure TIniVersionInfo.SetLegalTrademarks(_LegalTrademarks: string);
begin
  WriteString(FInfoKeysSection, 'LegalTrademarks', _LegalTrademarks);
end;

procedure TIniVersionInfo.SetMajorVer(_MajorVer: integer);
begin
  WriteInteger(FInfoSection, 'MajorVer', _MajorVer);
end;

procedure TIniVersionInfo.SetMinorVer(_MinorVer: integer);
begin
  WriteInteger(FInfoSection, 'MinorVer', _MinorVer);
end;

procedure TIniVersionInfo.SetOriginalFilename(_OriginalFilename: string);
begin
  WriteString(FInfoKeysSection, 'OriginalFilename', _OriginalFilename);
end;

procedure TIniVersionInfo.SetProductName(_ProductName: string);
begin
  WriteString(FInfoKeysSection, 'ProductName', _ProductName);
end;

procedure TIniVersionInfo.SetProductVersion(_ProductVersion: string);
begin
  WriteString(FInfoKeysSection, 'ProductVersion', _ProductVersion);
end;

procedure TIniVersionInfo.SetRelease(_Release: integer);
begin
  WriteInteger(FInfoSection, 'Release', _Release);
end;

procedure TIniVersionInfo.UpdateFile;
begin
  FIniFile.UpdateFile;
end;

end.

