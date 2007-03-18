{GXFormatter.config=twm}
{: implements utility functions for file accesss }
unit u_dzFileUtils;

{$I jedi.inc}

interface

uses
  Windows,
  SysUtils,
  Classes;

type
  EFileUtils = class(Exception);
  ECreateUniqueDir = class(EFileUtils);
  {: raised by DelTree if the DirName parameter is not a valid directory name }
  EDirNotFound = class(EFileUtils);
  EPathTooLong = class(EFileUtils);

type
  TFileAttributes = (
    faReadonly,
    faHidden, // Hidden files
    faSysFile, //	System files
    faVolumeID, //	Volume ID files
    faDirectory, //	Directory files
    faArchive // Archive files
    );

  TFileAttributeSet = set of TFileAttributes;

type
  {: a simple wrapper around FindFirst/FindNext which allows to search for
     specified attributes only (e.g. only directories), it automatically
     ignores the special '.' and '..' directories. }
  TSimpleDirEnumerator = class
  protected
    {: stores the search mask ('c:\windows\*.exe') }
    fMask: string;
    {: set of attributes a file must match }
    fMustHaveAttr: TFileAttributeSet;
    {: set of attributes a file may have }
    fMayHaveAttr: TFileAttributeSet;
    {: internally used TSearchRec structure }
    fSr: TSearchRec;
    {: true if FindFirst was called and returned no error code }
    fActive: boolean;
    {: number of matching files found }
    fMatchCount: integer;
  public
    {: creates a TSimpleDirEnumerator, sets the Mask, MustHaveAttr and MayHaveAttr
       properties.
       MustHaveAttr is set to [] and MayHaveAttr is set to include all possible
       attributes, so calling FindNext will find any files or subdirectories,
       but the special '.' and '..' directories
       @param(Mask is the file search mask) }
    constructor Create(const _Mask: string);
    {: Destructor, will call FindClose if necessary }
    destructor Destroy; override;
    {: Calls SysUtils.FindFirst on first call and SysUtls.FindNext in later
       calls.
       @param Filename is the name of the file found, if result is true, if you need
              more information about it, use the SR property
       @Returns true, if a matching file was found, false otherwise }
    function FindNext(out _Filename: string): boolean; overload;
    {: Calls SysUtils.FindFirst on first call and SysUtls.FindNext in later
       calls. If it returns true, use the SR property to get information about
       the file. See the overloaded @link(FindNext) version if you need only
       the filename.
       @Returns(true, if a matching file was found, false otherwise) }
    function FindNext: boolean; overload;
    {: Calls FindNext until it returns false, stores all filenames in List and
       returns the number of files found.
       @param List is a TStrings object which will be filled with the filenames
              of matching files, may be nil.
       @returns the number of mathing files }
    function FindAll(_List: TStrings = nil): integer;
    {: Calls FindClose so FindNext will start again. Reset does not change any
       properties (e.g. Mask, MustHaveAttr, MayHaveAttr) }
    procedure Reset;
    {: Returns the number of matches so far, that is the number of successful
       calls to FindNext }
    property MatchCount: integer read fMatchCount;
    {: Returns the search mask }
    property Mask: string read fMask; // write fMask;
    {: the set of attributes a file must have to be found by FindNext }
    property MustHaveAttr: TFileAttributeSet read fMustHaveAttr write fMustHaveAttr;
    {: the set of allowed attributes for a file to be found by FindNext }
    property MayHaveAttr: TFileAttributeSet read fMayHaveAttr write fMayHaveAttr;
    {: the search rec containing additional information about the file }
    property Sr: TSearchRec read fSr;
  end;

type
  {: This class owns all utility functions as class methods so they don't pollute the name space }
  TFileSystem = class
  public
  {(*}
  type
    TCopyFileFlags = (cfFailIfExists, cfForceOverwrite, cfRaiseException);
    TCopyFileFlagSet = set of TCopyFileFlags;
    TMatchingFileResult = (mfNotFound, mfDirectory, mfFile, mfSpecial);
  const
  {: set of char constant containing all characters that are invalid in a filename }
    INVALID_FILENAME_CHARS: set of Char = ['\', '/', ':', '*', '?', '"', '<', '>', '|'];
  {*)}
    {: Returns a temporary filename.
       @param Directory is a string with the directory to create the file in, defaults
                        to the TEMP directory.
       @param Prefix is a string with a prefix for the filename, defaults to 'dz'.)
       @param Unique is an word that the function converts to a hexadecimal string
       for use in creating the temporary filename.)
       <ul>
         <li>If Unique is nonzero, the function appends the hexadecimal string to
             <b>Prefix</b>
             to form the temporary filename. In this case, the function does not create
             the specified file, and does not test whether the filename is unique.</li>
         <li>If Unique is zero, the function uses a hexadecimal string derived
             from the current system time. In this case, the function uses different
             values until it finds a unique filename, and then it creates the file
             in the <b>Directory</b>.</li>
       </ul>
       @returns(a filename to use for a temporary file.)}
    class function GetTempFileName(_Directory: string = ''; const _Prefix: string = 'dz';
      _Unique: word = 0): string;

    {: Calls the corresponding Windows function and returns the short path name
       for an *existing* file or directory. }
    class function GetShortPathname(const _LongName: string): string;

    {: Creates a unique subdirectory under BaseDir with the given Prefix
       if Basedir is an empty string the system's %TEMP% directory is used.
       @returns the name of the created directory }
    class function CreateUniqueDirectory(_BaseDir: string = ''; const _Prefix: string = 'dz'): string;

    {: Calls the Win32Api function GetTempPath but returns a string rather than
       a PChar.
       @returns(a string with the TEMP directory) }
    class function GetTempPath: string;
    {: Moves the file Source to Dest using the Windows MoveFile function.
       @param Source is a string containing the name of the existing file
       @param Dest is a string containing the destination file name
       @param RaiseException is a boolean which controls whether the function
              retrieves the Windows error and raises an exception
              if it fails. If false, it will not raise an exception
              but just return false if moving the file fails.
       @returns true, if the file could be moved, false otherwise. }
    class function MoveFile(const _Source, _Dest: string; _RaiseException: boolean = true): boolean;
    {: Copies the file Source to Dest using the Windows CopyFile function.
       @param Source is a string containing the name of the existing file
       @param Dest is a string containing the destination file name
       @param FailIfExists is a boolean specifying whether the copy operation
              should fail if the destination file already exists.
       @param RaiseException is a boolean which controls whether the function
              retrieves the Windows error and raises an exception
              if it fails. If false, it will not raise an exception
              but just return false if copying the file fails.
       @param ForceOverwrite is a boolean which controls whether the function removes
              a read-only flag from the destination file if necessary.
       @returns true, if the file could be copied, false otherwise. }
    class function CopyFile(const _Source, _Dest: string; _FailIfExists: boolean = true;
      _RaiseException: boolean = true; _ForceOverwrite: boolean = false): boolean; overload;
    class function CreateDir(const _DirecoryName: string; _RaiseException: boolean = true): boolean;
    {: Sets a file's readonly flag
       @param Filename is the file to change
       @param Set determines whether to set or clear the flag }
    class function SetReadonly(const _Filename: string; _Set: boolean; _RaiseException: boolean = true): boolean;
    {: Copies the file Source to Dest using the Windows CopyFile function.
       @param(Source is a string containing the name of the existing file)
       @param(Dest is a string containing the destination file name)
       @param(Flags is a set of TCopyFileFlags specifying whether the copy operation
              cfFailIfExists: fail if the destination file already exists.
              cfForceOverwrite: remove a read-only flag from the destination file if necessary.
              cfRaiseException: retrieve the Windows error and raise an exception if it fails.
                If not set, it will not raise an exception but just return false if
                copying the file fails.)
       @returns(true, if the file could be copied, false otherwise.) }
    class function CopyFile(const _Source, _Dest: string; _Flags: TCopyFileFlagSet): boolean; overload;
    {: Deletes the file using the SysUtils.DeleteFile function.
       @param(Filename is a string containing the name of the file)
       @param(RaiseException is a boolean which controls whether the function
              retrieves the Windows error and raises an exception
              if it fails. If false, it will not raise an exception
              but just return false if moving the file fails. )
       @param(Force is a boolean which controls whether this function will try to delete
              readonly files, If true, it will use SetFileAttr to reset the
              readonly attribut and try to delete the file again.)
       @returns(true, if the file could be deleted, false otherwise.) }
    class function DeleteFile(const _Filename: string; _RaiseException: boolean = true; _Force: boolean = false): boolean;
    {: Deletes all files in a directory matching a given filemask (non-recursive)
       @param Dir is a string containting the directory in which the files are to be
                  deleted, must NOT be empty
       @param Mask is a string containting the file search mask, all files matching
                   this mask will be deleted
       @param RaiseException is a boolean which controls whether the function
                             retrieves the Windows error and raises an exception
                             if it fails. If false, it will not raise an exception
                             but just return false if moving the file fails.
       @param Force is a boolean which controls whether this function will try to delete
                    readonly files, If true, it will use SetFileAttr to reset the
                    readonly attribut and try to delete the file again.
       @returns the number of files that could not be deleted. }
    class function DeleteMatchingFiles(const _Dir, _Mask: string;
      _RaiseException: boolean = true; _Force: boolean = false): integer;
    {: tries to find a matching file
       @param Mask is the filename mask to match
       @param Filename is the name of the file which has been found, only valid if result <> mfNotFound
       @returns mfNotFound, if no file was found, or mfDirectory, mfFile or mfSpecial
                describing the type of the file which has been found }
    class function FindMatchingFile(const _Mask: string; out _Filename: string): TMatchingFileResult;
    class function RemoveDir(const _Dirname: string; _RaiseException: boolean = true;
      _Force: boolean = false): boolean;
    {: Deletes a directory with all files and subdirectories.
       @param(Dirname is the name of the directory to delete)
       @param(Force specifies whether it should also delete readonly files) }
    class function DelTree(const _Dirname: string; _Force: boolean = false; _RaiseException: boolean = true): boolean;
    {: reads a text file and returns its content as a string
       @param Filename is the name of the file to read
       @returns the file's content as a string }
    class function ReadTextFile(const _Filename: string): string;

    {: checks whether the given string is a valid filename (without path), that is
       does not contain one of the characters defined in INVALID_FILENAME_CHARS
       @param s is the string to check
       @returns true, if the string is a valid filename, false otherwise }
    class function IsValidFilename(const _s: string): boolean; overload;
    {: checks whether the given string is a valid filename (without path), that is
       does not contain one of the characters defined in INVALID_FILENAME_CHARS and
       returns the first error position.
       @param s is the string to check
       @param ErrPos is the first error position, only valid it result = false
       @returns true, if the string is a valid filename, false otherwise }
    class function IsValidFilename(const _s: string; out _ErrPos: integer): boolean; overload;
  end;

implementation

uses
  FileCtrl,
  u_dzMiscUtils;

resourcestring
  STR_GETTEMPPATH_ERROR_DS = 'u_dzFileUtils.GetTempPath: %1:s (code: %0:d) calling Windows.GetTempPath';
  STR_GETTEMPPATH_ERROR2_DS = 'u_dzFileUtils.GetTempPath: %1:s (code: %0:d) calling Windows.GetTempPath (2nd)';
  STR_CREATEUNIQUEDIR_ERROR_S = 'could not find a unique directory name based on "%s"';
  STR_GETTEMPFILENAME_ERROR_DS = 'u_dzFileUtils.GetTempFilename: %1:s (Code: %0:d) calling Windows.GetTempFileName';
  STR_GETSHORTPATHNAME_ERROR_DS = 'u_dzFileUtils.GetShortPathname: %1:s (Code: %0:d) calling Windows.GetShortPathname';
  STR_GETSHORTPATHNAME_TOO_LONG_D = 'Short pathname is longer than MAX_PATH (%d) characters';
  // doppelte % zum Durchreichen in Prozedur
  STR_MOVEFILE_ERROR_SS = 'Error %%1:s (%%0:d) while trying to move "%s" to "%s".';
  // doppelte % zum Durchreichen in Prozedur
  STR_SETREADONLY_ERROR_S = 'Error %%1:s (%%0:d) while changing the readonly flag of "%s"';
  // doppelte % zum Durchreichen in Prozedur
  STR_COPYFILE_ERROR_SS = 'Error %%1:s (%%0:d) while trying to copy "%s" to "%s".';
  // doppelte % zum Durchreichen in Prozedur
  STR_DELETEFILE_ERROR_S = 'Error %%1:s (%%0:d) deleting file "%s"';
  // doppelte % zum Durchreichen in Prozedur
  STR_REMOVEDIR_ERROR_S = 'Error %%1:s (%%0:d) deleting directory "%s"';
  STR_DELTREE_ERROR_S = '"%s" does not exist or is not a directory';
  // doppelte % zum Durchreichen in Prozedur
  STR_CREATEDIR_ERROR_S = 'Error %%1:s (%%0:d) creating directory "%s"';
  { TSimpleDirEnumerator }

constructor TSimpleDirEnumerator.Create(const _Mask: string);
begin
  fMask := _Mask;
  fMustHaveAttr := [];
  fMayHaveAttr := [faHidden, faSysFile, faVolumeID, faDirectory, faArchive];
end;

destructor TSimpleDirEnumerator.Destroy;
begin
  Reset;
  inherited;
end;

function TSimpleDirEnumerator.FindAll(_List: TStrings): integer;
var
  s: string;
begin
  Result := 0;
  while FindNext(s) do begin
    Inc(Result);
    if Assigned(_List) then
      _List.Add(s);
  end;
end;

function TSimpleDirEnumerator.FindNext(out _Filename: string): boolean;
var
  Res: integer;
  Attr: integer;

  function AttrOk(_EnumAttr: TFileAttributes; _SysAttr: integer): boolean;
  begin
    Result := true;
    if _EnumAttr in fMustHaveAttr then
      if (Attr and _SysAttr) = 0 then
        Result := false;
  end;

  procedure CondAddAttr(_EnumAttr: TFileAttributes; _SysAttr: integer);
  begin
    if _EnumAttr in fMayHaveAttr then
      Attr := Attr + _SysAttr;
  end;

begin
  repeat
    if not fActive then begin
      fMatchCount := 0;
      Attr := 0;
      CondAddAttr(faReadOnly, SysUtils.faReadOnly);
      CondAddAttr(faHidden, SysUtils.faHidden);
      CondAddAttr(faSysFile, SysUtils.faSysFile);
      CondAddAttr(faVolumeID, SysUtils.faVolumeID);
      CondAddAttr(faDirectory, SysUtils.faDirectory);
      CondAddAttr(faArchive, SysUtils.faArchive);
      Res := FindFirst(fMask, Attr, fSr);
      Result := (Res = 0);
      if Result then
        fActive := true;
    end else begin
      Res := SysUtils.FindNext(fSr);
      Result := (Res = 0);
    end;
    if not Result then
      exit;
    if (sr.Name = '.') or (sr.Name = '..') then
      Continue;
    if fMustHaveAttr <> [] then begin
      Attr := fSr.Attr;
      if not AttrOk(faReadonly, SysUtils.faReadOnly) then
        Continue;
      if not AttrOk(faHidden, SysUtils.faHidden) then
        Continue;
      if not AttrOk(faSysFile, SysUtils.faSysFile) then
        Continue;
      if not AttrOk(faVolumeID, SysUtils.faVolumeID) then
        Continue;
      if not AttrOk(faDirectory, SysUtils.faDirectory) then
        Continue;
      if not AttrOk(faArchive, SysUtils.faArchive) then
        Continue;
    end;
    Inc(fMatchCount);
    _Filename := sr.Name;
    exit;
  until false;
end;

function TSimpleDirEnumerator.FindNext: boolean;
var
  s: string;
begin
  Result := FindNext(s);
end;

procedure TSimpleDirEnumerator.Reset;
begin
  if fActive then
    FindClose(fSr);
  fActive := false;
end;

{ TFileSystem }

class function TFileSystem.GetTempPath: string;
var
  Res: integer;
  LastError: integer;
begin
  SetLength(Result, 1024);
  Res := Windows.GetTempPath(1024, PChar(Result));
  if Res < 0 then begin
    LastError := GetLastError;
    RaiseLastOSErrorEx(LastError, STR_GETTEMPPATH_ERROR_DS);
  end;
  if Res > 1024 then begin
    SetLength(Result, Res + 1);
    Res := Windows.GetTempPath(Res + 1, PChar(Result));
    if Res < 0 then begin
      LastError := GetLastError;
      RaiseLastOsErrorEx(LastError, STR_GETTEMPPATH_ERROR2_DS);
    end;
  end;
  SetLength(Result, Res);
end;

class function TFileSystem.CreateDir(const _DirecoryName: string;
  _RaiseException: boolean = true): boolean;
var
  LastError: Cardinal;
begin
  Result := SysUtils.CreateDir(_DirecoryName);
  if not Result then begin
    if _RaiseException then     begin
      LastError := GetLastError;
      RaiseLastOsErrorEx(LastError, Format(STR_CREATEDIR_ERROR_S, [_DirecoryName]));
    end;
  end;
end;

class function TFileSystem.CreateUniqueDirectory(_BaseDir: string = ''; const _Prefix: string = 'dz'): string;
var
  Pid: DWord;
  Counter: integer;
  Ok: boolean;
  s: string;
begin
  if _BaseDir = '' then
    _BaseDir := GetTempPath;
  Pid := GetCurrentProcessId;
  s := IncludeTrailingPathDelimiter(_BaseDir) + _Prefix + '_' + IntToStr(Pid) + '_';
  Counter := 0;
  Ok := false;
  while not OK do begin
    Result := s + IntToStr(Counter);
    OK := CreateDir(Result, false);
    if not OK then begin
      Inc(Counter);
      if Counter > 1000 then
        raise ECreateUniqueDir.CreateFmt(STR_CREATEUNIQUEDIR_ERROR_S, [Result]);
    end;
  end;
end;

class function TFileSystem.GetTempFileName(_Directory: string = ''; const _Prefix: string = 'dz';
  _Unique: word = 0): string;
var
  Res: integer;
  LastError: Cardinal;
begin
  if _Directory = '' then
    _Directory := GetTempPath;
  SetLength(Result, MAX_PATH);
  Res := Windows.GetTempFileName(PChar(_Directory), PChar(_Prefix), _Unique, PChar(Result));
  if Res = 0 then begin
    LastError := GetLastError;
    RaiseLastOsErrorEx(LastError, STR_GETTEMPFILENAME_ERROR_DS);
  end;
  Result := PChar(Result); // remove trailing characters
end;

class function TFileSystem.GetShortPathname(const _LongName: string): string;
var
  Res: integer;
  LastError: Cardinal;
begin
  SetLength(Result, MAX_PATH);
  Res := Windows.GetShortPathname(PChar(_LongName), PChar(Result), Length(Result));
  if Res = 0 then begin
    LastError := GetLastError;
    RaiseLastOsErrorEx(LastError, STR_GETSHORTPATHNAME_ERROR_DS);
  end else if Res > MAX_PATH then
    raise EPathTooLong.CreateFmt(STR_GETSHORTPATHNAME_TOO_LONG_D, [MAX_PATH]);
  Result := PChar(Result); // truncate at first #0
end;

class function TFileSystem.MoveFile(const _Source, _Dest: string; _RaiseException: boolean = true): boolean;
var
  LastError: Cardinal;
begin
  Result := Windows.MoveFile(PChar(_Source), PChar(_Dest));
  if not Result and _RaiseException then begin
    LastError := GetLastError;
    RaiseLastOsErrorEx(LastError, Format(STR_MOVEFILE_ERROR_SS, [_Source, _Dest]));
  end;
end;

class function TFileSystem.SetReadonly(const _Filename: string; _Set: boolean; _RaiseException: boolean = true): boolean;
var
  Attr: integer;
  LastError: Cardinal;
begin
  Attr := FileGetAttr(_Filename);
  if _Set then
    Attr := Attr or SysUtils.faReadOnly
  else
    Attr := Attr and not SysUtils.faReadOnly;
  if FileSetAttr(_Filename, Attr) <> 0 then begin
    if _RaiseException then begin
      LastError := GetLastError;
      RaiseLastOsErrorEx(LastError, Format(STR_SETREADONLY_ERROR_S, [_Filename]));
    end;
    Result := false
  end else
    Result := true;
end;

class function TFileSystem.CopyFile(const _Source, _Dest: string; _FailIfExists: boolean = true;
  _RaiseException: boolean = true; _ForceOverwrite: boolean = false): boolean;
var
  LastError: Cardinal;
begin
  Result := Windows.CopyFile(PChar(_Source), PChar(_Dest), _FailIfExists);
  if not Result and not _FailIfExists and _ForceOverwrite then begin
    SetReadonly(_Dest, False, false);
    Result := Windows.CopyFile(PChar(_Source), PChar(_Dest), _FailIfExists);
  end;
  if not Result and _RaiseException then begin
    LastError := GetLastError;
    RaiseLastOsErrorEx(LastError, Format(STR_COPYFILE_ERROR_SS, [_Source, _Dest]));
  end;
end;

class function TFileSystem.CopyFile(const _Source, _Dest: string; _Flags: TCopyFileFlagSet): boolean;
begin
  Result := CopyFile(_Source, _Dest,
    cfFailIfExists in _Flags,
    cfRaiseException in _Flags,
    cfForceOverwrite in _Flags);
end;

class function TFileSystem.DeleteFile(const _Filename: string; _RaiseException: boolean = true;
  _Force: boolean = false): boolean;
var
  Attr: integer;
  LastError: Cardinal;
begin
  Result := SysUtils.DeleteFile(_Filename);
  if not Result and _Force then begin
    Attr := FileGetAttr(_Filename);
    Attr := Attr and not SysUtils.faReadOnly;
    FileSetAttr(_Filename, Attr);
    Result := SysUtils.DeleteFile(_Filename);
  end;
  if not Result and _RaiseException then begin
    LastError := GetLastError;
    RaiseLastOsErrorEx(LastError, Format(STR_DELETEFILE_ERROR_S, [_Filename]));
  end;
end;

class function TFileSystem.DeleteMatchingFiles(const _Dir, _Mask: string;
  _RaiseException: boolean = true; _Force: boolean = false): integer;
var
  sr: TSearchRec;
  Dir: string;
begin
  Assert(_Dir <> '', 'Dir parameter must not be an empty string');
  Assert(_Mask <> '', 'Dir parameter must not be an empty string');

  Result := 0;
  Dir := IncludeTrailingPathDelimiter(_Dir);
  if 0 = FindFirst(Dir + _Mask, faAnyFile, sr) then
    try
      repeat
        if (sr.Name <> '.') and (sr.Name <> '..') and ((sr.Attr and (SysUtils.faVolumeID or SysUtils.faDirectory)) = 0) then
          if not DeleteFile(Dir + sr.Name, _RaiseException, _Force) then
            Inc(Result);
      until 0 <> FindNext(sr);
    finally
      FindClose(sr);
    end;
end;

class function TFileSystem.FindMatchingFile(const _Mask: string; out _Filename: string): TMatchingFileResult;
var
  sr: TSearchRec;
begin
  Result := mfNotFound;
  if 0 = FindFirst(_Mask, faAnyFile, sr) then
    try
      repeat
        if (sr.Name <> '.') and (sr.Name <> '..') then begin
          _Filename := sr.Name;
          if (sr.Attr and SysUtils.faVolumeID) <> 0 then
            Result := mfSpecial
          else if (sr.Attr and SysUtils.faDirectory) <> 0 then
            Result := mfDirectory
          else
            Result := mfFile;
          exit;
        end;
      until 0 <> FindNext(sr);
    finally
      FindClose(sr);
    end;
end;

class function TFileSystem.RemoveDir(const _Dirname: string; _RaiseException: boolean = true; _Force: boolean = false): boolean;
var
  Attr: integer;
  LastError: Cardinal;
begin
  Result := SysUtils.RemoveDir(_Dirname);
  if not Result and _Force then begin
    Attr := FileGetAttr(_Dirname);
    Attr := Attr and not SysUtils.faReadOnly;
    FileSetAttr(_Dirname, Attr);
    Result := SysUtils.RemoveDir(_Dirname);
  end;
  if not Result and _RaiseException then begin
    LastError := GetLastError;
    RaiseLastOsErrorEx(LastError, Format(STR_REMOVEDIR_ERROR_S, [_Dirname]));
  end;
end;

class function TFileSystem.DelTree(const _Dirname: string; _Force: boolean = false; _RaiseException: boolean = true): boolean;
var
  sr: TSearchRec;
  Filename: string;
begin
  Result := DirectoryExists(ExcludeTrailingPathDelimiter(_Dirname));
  if not Result then begin
    if _RaiseException then
      raise EDirNotFound.CreateFmt(STR_DELTREE_ERROR_S, [_DirName]);
    exit;
  end;
  if 0 = FindFirst(IncludeTrailingPathDelimiter(_Dirname) + '*.*', faAnyFile, sr) then
    try
      repeat
        if (sr.Name = '.') or (sr.Name = '..') then begin
            // ignore
        end else begin
          Filename := IncludeTrailingPathDelimiter(_Dirname) + sr.Name;
          if (sr.Attr and SysUtils.faDirectory) <> 0 then begin
            Result := DelTree(Filename, _Force, _RaiseException);
            if not Result then
              exit;
          end else begin
            Result := DeleteFile(Filename, _RaiseException, _Force);
            if not Result then
              exit;
          end;
        end;
      until 0 <> FindNext(sr);
    finally
      SysUtils.FindClose(sr);
    end;
  Result := RemoveDir(_Dirname, _RaiseException, _Force);
end;

class function TFileSystem.ReadTextFile(const _Filename: string): string;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(_Filename);
    Result := sl.Text;
  finally
    sl.Free;
  end;
end;

class function TFileSystem.IsValidFilename(const _s: string; out _ErrPos: integer): boolean;
var
  i: Integer;
begin
  Result := False;

  if _s = '' then begin
    _ErrPos := 0;
    exit;
  end;

  if Length(_s) > MAX_PATH then begin
    _ErrPos := MAX_PATH;
    exit;
  end;

  for i := 1 to Length(_s) do begin
    if _s[i] in INVALID_FILENAME_CHARS then begin
      _ErrPos := i;
      Exit;
    end;
  end;
  Result := True;
end;

class function TFileSystem.IsValidFilename(const _s: string): boolean;
var
  ErrPos: integer;
begin
  Result := IsValidFilename(_s, ErrPos);
end;

end.

