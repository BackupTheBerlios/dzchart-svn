unit u_IncBuildNoMain;

interface

procedure Main;

implementation

uses
  SysUtils,
  IniFiles,
  oxmldom,
  XMLIntf,
  XmlDoc,
  Classes,
  ActiveX,
  i_VersionInfo,
  u_DofVersionInfo,
  d_BdsProjVersionInfo;

type
  EHalt = class(EAbort)
  public
    ResultCode: integer;
    constructor Create(_ResultCode: integer);
  end;

{ EHalt }

constructor EHalt.Create(_ResultCode: integer);
begin
  inherited CreateFmt('Halt with result code %d', [_ResultCode]);
  ResultCode := _ResultCode;
end;

procedure WriteRcFile(const _Project: string; _VersionInfo: IVersionInfo);
var
  t: TextFile;
begin
  Assignfile(t, _Project + '.rc');
  Rewrite(t);

  WriteLn(t, {     } 'LANGUAGE LANG_ENGLISH,SUBLANG_ENGLISH_US');
  WriteLn(t);
  WriteLn(t, {     } '1 VERSIONINFO LOADONCALL MOVEABLE DISCARDABLE IMPURE');
  WriteLn(t, Format('FILEVERSION %d, %d, %d, %d', [_VersionInfo.MajorVer, _VersionInfo.MinorVer, _VersionInfo.Release, _VersionInfo.Build]));
  WriteLn(t, Format('PRODUCTVERSION %d, %d, %d, %d',
    [_VersionInfo.MajorVer, _VersionInfo.MinorVer, _VersionInfo.Release, _VersionInfo.Build]));
  WriteLn(t, {     } 'FILEFLAGSMASK VS_FFI_FILEFLAGSMASK');
  WriteLn(t, {     } 'FILEOS VOS__WINDOWS32');
  WriteLn(t, {     } 'FILETYPE VFT_APP');
  WriteLn(t, {     } '{');
  WriteLn(t, {     } ' BLOCK "StringFileInfo"');
  WriteLn(t, {     } ' {');
  WriteLn(t, {     } '  BLOCK "040904E4"');
  WriteLn(t, {     } '  {');
  WriteLn(t, Format('   VALUE "CompanyName", "%s\000"', [_VersionInfo.CompanyName]));
  WriteLn(t, Format('   VALUE "FileDescription", "%s\000"', [_VersionInfo.FileDescription]));
  WriteLn(t, Format('   VALUE "FileVersion", "%s\000"', [_VersionInfo.FileVersion]));
  WriteLn(t, Format('   VALUE "InternalName", "%s\000"', [_VersionInfo.InternalName]));
  WriteLn(t, Format('   VALUE "LegalCopyright", "%s\000"', [_VersionInfo.LegalCopyright]));
  WriteLn(t, Format('   VALUE "LegalTrademarks", "%s\000"', [_VersionInfo.LegalTrademarks]));
  WriteLn(t, Format('   VALUE "OriginalFilename", "%s\000"', [_VersionInfo.OriginalFilename]));
  WriteLn(t, Format('   VALUE "ProductName", "%s\000"', [_VersionInfo.ProductName]));
  WriteLn(t, Format('   VALUE "ProductVersion", "%s\000"', [_VersionInfo.ProductVersion]));
  WriteLn(t, Format('   VALUE "Comments", "%s\000"', [_VersionInfo.Comments]));
  WriteLn(t, {     } '  }');
  WriteLn(t, {     } ' }');
  WriteLn(t, {     } ' BLOCK "VarFileInfo"');
  WriteLn(t, {     } ' {');
  WriteLn(t, {     } '  VALUE "Translation", 1033, 1252');
  WriteLn(t, {     } ' }');
  WriteLn(t, {     } '}');
  if FileExists(_Project + '.ico') then begin
    WriteLn(t);
    WriteLn(t, {     } 'MAINICON ICON LOADONCALL MOVEABLE DISCARDABLE IMPURE %s.ico');
  end;
  Close(t);
end;

procedure Usage;
begin
  WriteLn;
  WriteLn('Usage: IncBuildNo <projectname>');
  WriteLn;
  WriteLn('This Program tries to read version information from <projectname>.bdsproj');
  WriteLn('and if that fails from <projectname>.dof');
  WriteLn('If AutoIncBuild = true, it will increment the build number and write it back.');
  WriteLn('The information is then written to <projectname>.rc to be compiled with');
  WriteLn('the brcc32 tool.');
  WriteLn('If <projectname>.ico exists, this file will be included as an icon resource.');
  WriteLn;
  WriteLn('Example:');
  WriteLn('c:\myproject> IncBuildNo MyProject');
  WriteLn('c:\myproject> brcc32 MyProject.rc');
  raise EHalt.Create(1);
end;

procedure Main;
var
  Projectname: string;
  ResultCode: integer;
  BdsVerInfo: IVersionInfo;
  DelphiVerInfo: IVersionInfo;
  OtherVerinfo: IVersionInfo;
  VersionInfo: IVersionInfo;
begin
  Resultcode := 0;
  try
    try
      WriteLn('IncBuildNo (c) 2005 by Thomas Mueller (www.dummzeuch.de)');
      if ParamCount <> 1 then
        Usage;
      CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
      Projectname := ParamStr(1);

      try
        Write('Reading ', Projectname, '.bdsproj ');
        BdsVerInfo := Tdm_BdsProjVersionInfo.Create(Projectname);
        WriteLn('OK');
      except
        WriteLn('FAIL');
        BdsVerInfo := nil;
      end;

      try
        Write('Reading ', Projectname, '.dof ');
        DelphiVerInfo := TDofVersionInfo.Create(Projectname);
        WriteLn('OK');
      except
        WriteLn('FAIL');
        DelphiVerInfo := nil;
      end;

      if not Assigned(BdsVerInfo) then begin
        VersionInfo := DelphiVerInfo;
      end else if not Assigned(DelphiVerInfo) then
        VersionInfo := BdsVerInfo
      else begin
        if DelphiVerInfo.Build > BdsVerInfo.Build then begin
          VersionInfo := DelphiVerInfo;
          OtherVerinfo := BdsVerInfo;
        end else begin
          VersionInfo := BdsVerInfo;
          OtherVerinfo := DelphiVerInfo;
        end;
      end;

      if not Assigned(VersionInfo) then begin
        WriteLn(Format('Error: Could not read either %:0s.bdsproj or %:0s.dof', [Projectname]));
        raise EHalt.Create(1);
      end;

      if VersionInfo.AutoIncBuild then begin
        Write('Incrementing build number ');
        VersionInfo.Build := VersionInfo.Build + 1;
        VersionInfo.UpdateFile;
        WriteLn('OK');
      end;

      Write('Writing ', Projectname, '.rc file ');
      WriteRcFile(Projectname, VersionInfo);
      WriteLn('OK');

      if Assigned(OtherVerinfo) then begin
        Write('Updating other version info file ');
        OtherVerinfo.Assign(VersionInfo);
        OtherVerinfo.UpdateFile;
        WriteLn('OK');
      end;

    except
      on e: EHalt do
        ResultCode := e.ResultCode;
      on e: Exception do begin
        ResultCode := 1;
        WriteLn(e.ClassName, ': ', e.Message);
      end;
    end;
  finally
{$IFOPT D+}
    WriteLn;
    WriteLn('press <enter>');
    readln;
{$ENDIF}
  end;
  if ResultCode <> 0 then
    Halt(ResultCode);
end;

end.

