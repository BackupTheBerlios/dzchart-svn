{GXFormatter.config=twm}
unit u_dzVersionInfo;

interface

uses
  SysUtils;

type
  EApplicationInfo = class(Exception);
  EAIChecksumError = class(EApplicationInfo);
  EAIUnknownProperty = class(EApplicationInfo);
  EAIInvalidVersionInfo = class(EApplicationInfo);

type
  TFileProperty = (FpProductName, FpProductVersion, FpFileDescription, FpFileVersion);
  TFilePropertySet = set of TFileProperty;

type
  TFileVersionRec = record
    Major: integer;
    Minor: integer;
    Revision: integer;
    Build: integer;
    IsValid: boolean;
    procedure CheckValid;
    procedure Init(_Major, _Minor, _Revision, _Build: integer);
    class operator GreaterThan(_a, _b: TFileVersionRec): boolean;
    class operator GreaterThanOrEqual(_a, _b: TFileVersionRec): boolean;
    class operator Equal(_a, _b: TFileVersionRec): boolean;
    class operator NotEqual(_a, _b: TFileVersionRec): boolean;
    class operator LessThan(_a, _b: TFileVersionRec): boolean;
    class operator LessThanOrEqual(_a, _b: TFileVersionRec): boolean;
  end;

type
  IFileInfo = interface ['{BF3A3600-1E39-4618-BD7A-FBBD6C148C2E}']
    procedure SetAllowExceptions(_Value: boolean);
    ///<summary> If set to false, any exceptions will be ignored and an empty string will
    ///          be returned. </summary>
    property AllowExceptions: boolean write SetAllowExceptions;
    ///<summary> The file name.</summary>
    function FileName: string;
    ///<summary> The file directory whithout the filename with a terminating backslash </summary>
    function FileDir: string;
    ///<summary> The file description from the version resource </summary>
    function FileDescription: string;
    ///<summary> The file version from the file properties.</summary>
    function FileVersion: string;
    function FileVersionRec: TFileVersionRec;
    ///<summary> The file's product name from the resources </summary>
    function ProductName: string;
    ///<summary> The the product version from the file properties.</summary>
    function ProductVersion: string;

  end;

type
  ///<summary> abstract ancestor, do not instantiate this class, instantiate one of
  ///          the derived classes below </summary>
  TCustomFileInfo = class(TInterfacedObject)
  private
    FAllowExceptions: boolean;
    FFileName: string;

    FFilePropertiesRead: TFilePropertySet;
    FFileProperties: array[TFileProperty] of string;
    function GetFileProperty(_Property: TFileProperty): string;
  protected // implements IFileInfo
    procedure SetAllowExceptions(_Value: boolean);

    function FileName: string;
    function FileDir: string;
    function FileDescription: string;
    function FileVersion: string;
    function FileVersionRec: TFileVersionRec;

    function ProductName: string;
    function ProductVersion: string;
  public
    constructor Create;
    destructor Destroy; override;
    property AllowExceptions: boolean read FAllowExceptions write FAllowExceptions;
  end;

type
  ///<summary> Get informations about the given file.</summary>
  TFileInfo = class(TCustomFileInfo, IFileInfo)
  public
    constructor Create(const _Filename: string);
  end;

type
  ///<summary> Get informations about the current executable
  ///          If called from a dll it will return the info about the
  ///          calling executable, if called from an executable, it will return
  ///          info about itself. </summary>
  TApplicationInfo = class(TCustomFileInfo, IFileInfo)
  public
    constructor Create;
  end;

type
  ///<summary> Get informations about the current DLL.
  ///          It will always return info about itself regardless of whether it is
  ///          called from a dll or an executable </summary>
  TDllInfo = class(TCustomFileInfo, IFileInfo)
  public
    constructor Create;
  end;

implementation

uses
  Windows,
  Forms,
  IniFiles,
  JclFileUtils,
  u_dzTranslator,
  u_dzOsUtils;

{ TCustomFileInfo }

constructor TCustomFileInfo.Create;
begin
  inherited;

  FAllowExceptions := true;
  FFilePropertiesRead := [];
end;

function TCustomFileInfo.FileName: string;
begin
  result := FFileName;
end;

destructor TCustomFileInfo.Destroy;
begin
  inherited;
end;

function TCustomFileInfo.FileDescription: string;
begin
  Result := GetFileProperty(FpFileDescription);
end;

function TCustomFileInfo.FileDir: string;
begin
  Result := ExtractFileDir(FileName);
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

procedure TCustomFileInfo.SetAllowExceptions(_Value: boolean);
begin
  FAllowExceptions := _Value;
end;

function TCustomFileInfo.GetFileProperty(_Property: TFileProperty): string;
var
  fi: TJclFileVersionInfo;
begin
  result := '';

  if not (_Property in FFilePropertiesRead) then
    begin
      try
        case _Property of
          FpProductName,
            FpProductVersion,
            FpFileDescription,
            FpFileVersion:
            begin
              fi := TJclFileVersionInfo.Create(FileName);
              try
                FFileProperties[FpFileVersion] := fi.FileVersion;
                FFileProperties[FpFileDescription] := fi.FileDescription;
                FFileProperties[FpProductName] := fi.ProductName;
                FFileProperties[FpProductVersion] := fi.ProductVersion;

                Include(FFilePropertiesRead, FpFileVersion);
                Include(FFilePropertiesRead, FpFileDescription);
                Include(FFilePropertiesRead, FpProductVersion);
                Include(FFilePropertiesRead, FpProductName);
              finally
                fi.Free;
              end;
            end;
        end;
      except
        if FAllowExceptions then
          raise;
        exit;
      end;
    end;

  Result := FFileProperties[_Property];
end;

function TCustomFileInfo.FileVersion: string;
begin
  Result := GetFileProperty(FpFileVersion);
end;

function TCustomFileInfo.FileVersionRec: TFileVersionRec;
begin
  Result.IsValid := GetFileBuildInfo(FFileName, Result.Major, Result.Minor, Result.Revision, Result.Build);
end;

function TCustomFileInfo.ProductName: string;
begin
  Result := GetFileProperty(FpProductName);
end;

function TCustomFileInfo.ProductVersion: string;
begin
  Result := GetFileProperty(FpProductVersion);
end;

{ TFileInfo }

constructor TFileInfo.Create(const _Filename: string);
begin
  inherited Create;
  FFileName := ExpandFileName(_Filename);
end;

{ TApplicationInfo }

constructor TApplicationInfo.Create;
begin
  inherited Create;
  FFileName := GetModuleFilename(0);
end;

{ TDllInfo }

constructor TDllInfo.Create;
begin
  inherited Create;
  FFileName := GetModuleFilename;
end;

{ TFileVersionRec }

procedure TFileVersionRec.CheckValid;
begin
  if not IsValid then
    raise EAIInvalidVersionInfo.Create('Invalid version info');
end;

class operator TFileVersionRec.Equal(_a, _b: TFileVersionRec): boolean;
begin
  _a.CheckValid;
  _b.CheckValid;

  Result := (_a.Major = _b.Major) and (_a.Minor = _b.Minor) and (_a.Revision = _b.Revision) and (_a.Build = _b.Build);
end;

class operator TFileVersionRec.GreaterThan(_a, _b: TFileVersionRec): boolean;
begin
  _a.CheckValid;
  _b.CheckValid;

  Result := _a.Major > _b.Major;
  if not Result and (_a.Major = _b.Major) then
    begin
      Result := _a.Minor > _b.Minor;
      if not Result and (_a.Minor = _b.Minor) then
        begin
          Result := _a.Revision > _b.Revision;
          if not Result and (_a.Revision = _b.Revision) then
            Result := _a.Build > _b.Build;
        end;
    end;
end;

class operator TFileVersionRec.GreaterThanOrEqual(_a, _b: TFileVersionRec): boolean;
begin
  _a.CheckValid;
  _b.CheckValid;

  Result := not (_a < _b);
end;

procedure TFileVersionRec.Init(_Major, _Minor, _Revision, _Build: integer);
begin
  Major := _Major;
  Minor := _Minor;
  Revision := _Revision;
  Build := _Build;
end;

class operator TFileVersionRec.LessThan(_a, _b: TFileVersionRec): boolean;
begin
  _a.CheckValid;
  _b.CheckValid;

  Result := _a.Major < _b.Major;
  if not Result and (_a.Major = _b.Major) then
    begin
      Result := _a.Minor < _b.Minor;
      if not Result and (_a.Minor = _b.Minor) then
        begin
          Result := _a.Revision < _b.Revision;
          if not Result and (_a.Revision = _b.Revision) then
            Result := _a.Build < _b.Build;
        end;
    end;
end;

class operator TFileVersionRec.LessThanOrEqual(_a, _b: TFileVersionRec): boolean;
begin
  _a.CheckValid;
  _b.CheckValid;

  Result := not (_a > _b);
end;

class operator TFileVersionRec.NotEqual(_a, _b: TFileVersionRec): boolean;
begin
  _a.CheckValid;
  _b.CheckValid;

  Result := not (_a = _b);
end;

end.


