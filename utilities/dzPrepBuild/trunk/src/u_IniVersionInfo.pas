unit u_IniVersionInfo;

interface

uses
  SysUtils,
  IniFiles,
  i_VersionInfo,
  u_AbstractVersionInfo;

type
  {: Tries to read a <projectname>.ini file, succeeds, if it exists
     @param Project is the project name (*.dpr file without extension)
     @param VersionInfo is a TVersionInfoRec record which will be filled with the version info }
  TIniVersionInfo = class(TAbstractVersionInfo)
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
    procedure ReadValues;
    procedure WriteValues;
  private
  protected // implementation of IVersionInfo
    procedure UpdateFile; override;
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

uses
  u_dzTranslator;

{ TIniVersionInfo }

constructor TIniVersionInfo.Create(const _FullFilename: string; const _InfoSection: string;
  const _InfoKeysSection: string);
begin
  inherited Create;
  if not FileExists(_FullFilename) then
    raise ENoVersionInfo.CreateFmt(_('File %s does not exist.'), [_FullFilename]);
  FInfoSection := _InfoSection;
  FInfoKeysSection := _InfoKeysSection;
  FIniFile := TMemIniFile.Create(_FullFilename);
  ReadValues;
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

procedure TIniVersionInfo.ReadValues;
begin
  AutoIncBuild := ReadBool(FInfoSection, 'AutoIncBuild', False);
  Build := ReadInteger(FInfoSection, 'Build', 0);
  MajorVer := ReadInteger(FInfoSection, 'MajorVer', 0);
  MinorVer := ReadInteger(FInfoSection, 'MinorVer', 0);
  Release := ReadInteger(FInfoSection, 'Release', 0);

  Comments := ReadString(FInfoKeysSection, 'Comments', '');
  CompanyName := ReadString(FInfoKeysSection, 'CompanyName', '');
  FileDescription := ReadString(FInfoKeysSection, 'FileDescription', '');
  FileVersion := ReadString(FInfoKeysSection, 'FileVersion', '');
  InternalName := ReadString(FInfoKeysSection, 'InternalName', '');
  LegalCopyright := ReadString(FInfoKeysSection, 'LegalCopyright', '');
  LegalTrademarks := ReadString(FInfoKeysSection, 'LegalTrademarks', '');
  OriginalFilename := ReadString(FInfoKeysSection, 'OriginalFilename', '');
  ProductName := ReadString(FInfoKeysSection, 'ProductName', '');
  ProductVersion := ReadString(FInfoKeysSection, 'ProductVersion', '');
end;

procedure TIniVersionInfo.WriteValues;
begin
  WriteBool(FInfoSection, 'AutoIncBuild', AutoIncBuild);
  WriteInteger(FInfoSection, 'Build', Build);
  WriteString(FInfoKeysSection, 'Comments', Comments);
  WriteString(FInfoKeysSection, 'CompanyName', CompanyName);
  WriteString(FInfoKeysSection, 'FileDescription', FileDescription);
  WriteString(FInfoKeysSection, 'FileVersion', FileVersion);
  WriteString(FInfoKeysSection, 'InternalName', InternalName);
  WriteString(FInfoKeysSection, 'LegalCopyright', LegalCopyright);
  WriteString(FInfoKeysSection, 'LegalTrademarks', LegalTrademarks);
  WriteInteger(FInfoSection, 'MajorVer', MajorVer);
  WriteInteger(FInfoSection, 'MinorVer', MinorVer);
  WriteString(FInfoKeysSection, 'OriginalFilename', OriginalFilename);
  WriteString(FInfoKeysSection, 'ProductName', ProductName);
  WriteString(FInfoKeysSection, 'ProductVersion', ProductVersion);
  WriteInteger(FInfoSection, 'Release', Release);
end;

procedure TIniVersionInfo.UpdateFile;
begin
  WriteValues;
  FIniFile.UpdateFile;
end;

end.

