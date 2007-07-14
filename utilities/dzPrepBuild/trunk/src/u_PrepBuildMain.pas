unit u_PrepBuildMain;

interface

uses
  Windows,
  SysUtils,
  u_dzDefaultMain,
  u_dzGetOpt,
  i_VersionInfo;

type
  TPrepBuildMain = class(TDefaultMain)
  private
    function HandleExecOption(const _Command: string; _VersionInfo: IVersionInfo): integer;
  protected
    procedure InitCmdLineParser; override;
    function doExecute: integer; override;
  public

  end;

implementation

uses
  u_dzExecutor,
  u_dzShellApiUtils,
  u_DofVersionInfo,
  d_BdsProjVersionInfo,
  u_CentralIniVersionInfo,
  u_DummyVersionInfo;

{ TPrepBuildMain }

function DateTimeToString(const _Format: string; _dt: TDateTime): string;
begin
  SysUtils.DateTimeToString(Result, _Format, _dt);
end;

function TPrepBuildMain.HandleExecOption(const _Command: string; _VersionInfo: IVersionInfo): integer;
const
  DZ_MY_DOCUMENTS = 'dzMyDocuments';
  DZ_DATE = 'dzDate';
  DZ_TIME = 'dzTime';
  DZ_DATE_TIME = 'dzDateTime';
  DZ_VERSION = 'dzVersion.';
var
  MyDoc: string;
  Executor: TExecutor;
  dt: TDateTime;
begin
  MyDoc := TWindowsShell.GetMyDocumentsDir();
  Executor := TExecutor.Create;
  try
    Executor.ExeName := GetEnvironmentVariable('ComSpec');
    Executor.Commandline := '/c ' + _Command;
    Executor.Environment.Values[DZ_MY_DOCUMENTS] := MyDoc;

    dt := Now;
    Executor.Environment.Values[DZ_DATE] := DateTimeToString('yyyy-mm-dd', dt);
    Executor.Environment.Values[DZ_TIME] := DateTimeToString('hh-nn-ss', dt);
    Executor.Environment.Values[DZ_DATE_TIME] := DateTimeToString('yyyy-mm-dd_hh-nn-ss', dt);

    if Assigned(_VersionInfo) then begin
      Executor.Environment.Values[DZ_VERSION + 'MajorVer'] := IntToStr(_VersionInfo.MajorVer);
      Executor.Environment.Values[DZ_VERSION + 'MinorVer'] := IntToStr(_VersionInfo.MinorVer);
      Executor.Environment.Values[DZ_VERSION + 'Release'] := IntToStr(_VersionInfo.Release);
      Executor.Environment.Values[DZ_VERSION + 'Build'] := IntToStr(_VersionInfo.Build);
      Executor.Environment.Values[DZ_VERSION + 'FileDesc'] := _VersionInfo.FileDescription;
      Executor.Environment.Values[DZ_VERSION + 'InternalName'] := _VersionInfo.InternalName;
      Executor.Environment.Values[DZ_VERSION + 'OriginalName'] := _VersionInfo.OriginalFilename;
      Executor.Environment.Values[DZ_VERSION + 'Product'] := _VersionInfo.ProductName;
      Executor.Environment.Values[DZ_VERSION + 'ProductVersion'] := _VersionInfo.ProductVersion;
      Executor.Environment.Values[DZ_VERSION + 'Company'] := _VersionInfo.CompanyName;
      Executor.Environment.Values[DZ_VERSION + 'Copyright'] := _VersionInfo.LegalCopyright;
      Executor.Environment.Values[DZ_VERSION + 'Trademark'] := _VersionInfo.LegalTrademarks;
      Executor.Environment.Values[DZ_VERSION + 'Comments'] := _VersionInfo.Comments;
    end;

    Executor.Execute;
    Executor.Wait(INFINITE);
    Result := Executor.ExitCode;
  finally
    Executor.Free;
  end;
end;

function TPrepBuildMain.doExecute: integer;
var
  Param: string;
  ParamVersionInfo: IVersionInfo;
  VersionInfo: IVersionInfo;
  IntValue: integer;
  IconFile: string;
begin
  if FGetOpt.OptionPassed('ReadDof', Param) then
    ParamVersionInfo := TDofVersionInfo.Create(Param);

  if FGetOpt.OptionPassed('ReadBdsProj', Param) then begin
    if Assigned(ParamVersionInfo) then
      raise Exception.Create('You can only pass one of --ReadDof, --ReadBdsproj or --ReadIni');
    ParamVersionInfo := Tdm_BdsProjVersionInfo.Create(Param);
  end;

  if FGetOpt.OptionPassed('ReadIni', Param) then begin
    if Assigned(ParamVersionInfo) then
      raise Exception.Create('You can only pass one of --ReadDof, --ReadBdsproj or --ReadIni');
    ParamVersionInfo := TCentralVersionInfo.Create(Param);
  end;

  VersionInfo := TDummyVersionInfo.Create;
  if Assigned(ParamVersionInfo) then begin
    VersionInfo.Assign(ParamVersionInfo);
    ParamVersionInfo := nil;
  end;

  if FGetOpt.OptionPassed('MajorVer', Param) then begin
    if not TryStrToInt(Param, IntValue) then
      raise Exception.Create('Parameter to MajorVer must be a number');
    VersionInfo.MajorVer := IntValue;
  end;

  if FGetOpt.OptionPassed('MinorVer', Param) then begin
    if not TryStrToInt(Param, IntValue) then
      raise Exception.Create('Parameter to MinorVer must be a number');
    VersionInfo.MinorVer := IntValue;
  end;

  if FGetOpt.OptionPassed('Release', Param) then begin
    if not TryStrToInt(Param, IntValue) then
      raise Exception.Create('Parameter to Release must be a number');
    VersionInfo.Release := IntValue;
  end;

  if FGetOpt.OptionPassed('Build', Param) then begin
    if not TryStrToInt(Param, IntValue) then
      raise Exception.Create('Parameter to MinorVer must be a number');
    VersionInfo.MinorVer := IntValue;
  end;

  if FGetOpt.OptionPassed('FileDesc', Param) then
    VersionInfo.FileDescription := Param;

  if FGetOpt.OptionPassed('InternalName', Param) then
    VersionInfo.InternalName := Param;

  if FGetOpt.OptionPassed('OriginalName', Param) then
    VersionInfo.OriginalFilename := Param;

  if FGetOpt.OptionPassed('Product', Param) then
    VersionInfo.ProductName := Param;

  if FGetOpt.OptionPassed('ProductVersion', Param) then
    VersionInfo.ProductVersion := Param;

  if FGetOpt.OptionPassed('Company', Param) then
    VersionInfo.CompanyName := Param;

  if FGetOpt.OptionPassed('Copyright', Param) then
    VersionInfo.LegalCopyright := Param;

  if FGetOpt.OptionPassed('Trademark', Param) then
    VersionInfo.LegalTrademarks := Param;

  if FGetOpt.OptionPassed('Comments', Param) then
    VersionInfo.Comments := Param;

  if FGetOpt.OptionPassed('IncBuild') then
    VersionInfo.Build := VersionInfo.Build + 1;

  if FGetOpt.OptionPassed('UpdateDof', Param) then begin
    ParamVersionInfo := TDofVersionInfo.Create(Param);
    ParamVersionInfo.Assign(VersionInfo);
    ParamVersionInfo.UpdateFile;
  end;

  if FGetOpt.OptionPassed('UpdateBdsproj', Param) then begin
    ParamVersionInfo := Tdm_BdsProjVersionInfo.Create(Param);
    ParamVersionInfo.Assign(VersionInfo);
    ParamVersionInfo.UpdateFile;
  end;

  if FGetOpt.OptionPassed('UpdateIni', Param) then begin
    ParamVersionInfo := TCentralVersionInfo.Create(Param);
    ParamVersionInfo.Assign(VersionInfo);
    ParamVersionInfo.UpdateFile;
  end;

  if not FGetOpt.OptionPassed('Icon', IconFile) then
    IconFile := '';

  if FGetOpt.OptionPassed('WriteRc', Param) then begin
    //
  end;

  if FGetOpt.OptionPassed('Exec', Param) then
    HandleExecOption(Param, VersionInfo);

  Result := 0;
end;

procedure TPrepBuildMain.InitCmdLineParser;
begin
  inherited;
  FGetOpt.RegisterOption('ReadDof', 'read a .dof file to get the version information', true);
  FGetOpt.RegisterOption('ReadBdsproj', 'read a .bdsproj file to get the version information', true);
  FGetOpt.RegisterOption('ReadIni', 'read a .ini file to get the version information', true);
  FGetOpt.RegisterOption('Exec', 'execute the given program or script with extended environment', true);
  FGetOpt.RegisterOption('UpdateDof', 'update a .dof file with the version information', true);
  FGetOpt.RegisterOption('UpdateBdsproj', 'update a .bdsproj file with the version information', true);
  FGetOpt.RegisterOption('UpdateIni', 'update a .ini file with the version information', true);
  FGetOpt.RegisterOption('WriteRc', 'write version info to a .rc file', true);
  FGetOpt.RegisterOption('Icon', 'Assign an icon file to add to the .rc file', true);
  FGetOpt.RegisterOption('IncBuild', 'increment the build number', false);
  FGetOpt.RegisterOption('MajorVer', 'set the major version number', true);
  FGetOpt.RegisterOption('MinorVer', 'set the minor version number', true);
  FGetOpt.RegisterOption('Release', 'set the release number', true);
  FGetOpt.RegisterOption('Build', 'set the build number', true);
  FGetOpt.RegisterOption('FileDesc', 'set the file description', true);
  FGetOpt.RegisterOption('InternalName', 'set the internal name', true);
  FGetOpt.RegisterOption('OriginalName', 'set the original file name', true);
  FGetOpt.RegisterOption('Product', 'set the product name', true);
  FGetOpt.RegisterOption('ProductVersion', 'set the product version', true);
  FGetOpt.RegisterOption('Company', 'set the company name', true);
  FGetOpt.RegisterOption('Copyright', 'set the legal copyright', true);
  FGetOpt.RegisterOption('Trademark', 'set the legal trademark', true);
  FGetOpt.RegisterOption('Comments', 'set the comments', true);
end;

initialization
  MainClass := TPrepBuildMain;
end.

