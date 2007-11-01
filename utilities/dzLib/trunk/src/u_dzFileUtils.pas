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
  EInvalidPropertyCombination = class(EFileUtils);
  EFileNotFound = class(EFileUtils);

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

  TFileInfoRec = record
  public
    Filename: string;
    Size: Int64;
    Timestamp: TDateTime;
  end;

type
  {: a simple wrapper around FindFirst/FindNext which allows to search for
     specified attributes only (e.g. only directories), it automatically
     ignores the special '.' and '..' directories. }
  TSimpleDirEnumerator = class
  protected
    {: stores the search mask ('c:\windows\*.exe') }
    FMask: string;
    {: set of attributes a file must match }
    FMustHaveAttr: TFileAttributeSet;
    {: set of attributes a file may have }
    FMayHaveAttr: TFileAttributeSet;
    {: internally used TSearchRec structure }
    FSr: TSearchRec;
    {: true if FindFirst was called and returned no error code }
    FActive: boolean;
    {: number of matching files found }
    FMatchCount: integer;
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
    {: creates a TSimpleDirEnumerator, calls its FindAll method and frees it }
    class function Execute(const _Mask: string; _List: TStrings): integer;
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
    property MatchCount: integer read FMatchCount;
    {: Returns the search mask }
    property Mask: string read FMask; // write fMask;
    {: the set of attributes a file must have to be found by FindNext }
    property MustHaveAttr: TFileAttributeSet read FMustHaveAttr write FMustHaveAttr;
    {: the set of allowed attributes for a file to be found by FindNext }
    property MayHaveAttr: TFileAttributeSet read FMayHaveAttr write FMayHaveAttr;
    {: the search rec containing additional information about the file }
    property Sr: TSearchRec read FSr;
  end;

type
  {: represents the status of a CopyFile/MoveFileWithProgress operation, passed
     as parameter to the callback function. }
  TCopyProgressStatus = class
  public
  {(*}
  type
    {: possible return values for the callback function }
    TProgressResult = (
      prContinue, {:< continue with the copy/move operation }
      prCancel,   {:< cancel the operation, cannot be resumed }
      prStop,     {:< stop the operation, can be resumed, if cfwRestartable was passed }
      prQuiet);   {:< continue the operation, do not call the callback }
    {: reason for calling the callback function }
    TProgressReason = (
      prChunkFinished, {:< a chunk of the file has been copied }
      prStreamSwitch); {:< started to copy a new stream (set in the first callback) }
  {*)}
  protected
    FTotalFileSize: LARGE_INTEGER;
    FTotalBytesTransferred: LARGE_INTEGER;
    FStreamSize: LARGE_INTEGER;
    FStreamBytesTransferred: LARGE_INTEGER;
    FStreamNumber: LongWord;
    FCallbackReason: TProgressReason;
    FSourceFile: THandle;
    FDestinationFile: THandle;
  public
    {: total size of the file }
    property TotalFileSize: LARGE_INTEGER read FTotalFileSize;
    {: total bytes that have been transferred so far }
    property TotalBytesTransferred: LARGE_INTEGER read FTotalBytesTransferred;
    {: size of the stream that is currently being transferred }
    property StreamSize: LARGE_INTEGER read FStreamSize;
    {: bytes of the current stream taht have been transferred so far }
    property StreamBytesTransferred: LARGE_INTEGER read FStreamBytesTransferred;
    {: Number of the current stream, starts with 1 (usually always 1) }
    property StreamNumber: LongWord read FStreamNumber;
    {: reason for callback }
    property CallbackReason: TProgressReason read FCallbackReason;
    {: Handle of source file }
    property SourceFile: THandle read FSourceFile;
    {: Handle of destination file }
    property DestinationFile: THandle read FDestinationFile;
  end;
  TCopyFileProgressEvt = procedure(_Status: TCopyProgressStatus;
    var _Continue: TCopyProgressStatus.TProgressResult) of object;

  {: defines the action to take if a file already exists but has a different content }
  TFileExistsAction = (feaIgnore, feaOverwrite);
  TOnSyncing = procedure(_Sender: TObject; const _SrcDir, _DstDir: string) of object;
  {: called if a destination file already exists
     @param Action is the action to take, default is feaIgnore }
  TOnFileExists = procedure(_Sender: TObject; const _SrcFile, _DstFile: TFileInfoRec; var _Action: TFileExistsAction) of object;
  {: Synchronizes two directories }
  TDirectorySync = class
  private
    FOnSyncingDir: TOnSyncing;
    FOnSyncingFile: TOnSyncing;
    FOnFileExists: TOnFileExists;
//    FOnDifferentFileExists: TOnDifferentFileExists;
//    FCheckContent: boolean;
//    procedure doOnDifferentFileExists(const _Filename: string; var _Action: TFileExistsAction);
    procedure doOnSyncingDir(const _SrcDir, _DstDir: string);
    procedure doOnSyncingFile(const _SrcFile, _DstFile: string);
    function doOnFileExists(const _SrcDir, _DstDir, _Filename: string): TFileExistsAction;
  public
    {: Checks if there are files in the source directory that are already in
       the destination directory, for each file that exists, the OnFileExists
       event is called. }
    procedure CheckOneWay(const _SrcDir, _DstDir: string);
    {: copies all files from DirA to DirB if they don't already exists
       (not implemented: if CheckContent=true, the content existing files will be checked and if
                         it doesn't match, OnDifferentFileExists is called ) }
    procedure SyncOneWay(const _SrcDir, _DstDir: string);
    {: calls SyncOneWay(DirA, DirB) and SyncOneWay(DirB, DirA)
       (not implemented: if CheckContent=true, the content existing files will be checked and if
                         it doesn't match, OnDifferentFileExists is called ) }
    procedure SyncBothWays(const _DirA, _DirB: string);
//    {: Not implemented: Called, if the content of an existing file is different }
//    property OnDifferentFileExists: TOnDifferentFileExists read FOnDifferentFileExists write FOnDifferentFileExists;
//    {: Not implemented: if true, OnDifferentFileExists will be called }
//    property CheckContent: boolean read FCheckContent write FCheckContent default false;
    {: called when a new directory is entered, to abort synchronization,
       raise an exception (e.g. SysUtils.Abort), and catch it in the calling method }
    property OnSyncingDir: TOnSyncing read FOnSyncingDir write FOnSyncingDir;
    {: called when a file is being copied, to abort synchronization,
      raise an exception (e.g. SysUtils.Abort), and catch it in the calling method }
    property OnSyncingFile: TOnSyncing read FOnSyncingFile write FOnSyncingFile;
    {: called from CheckOneWay if a destination file already exists }
    property OnFileExists: TOnFileExists read FOnFileExists write FOnFileExists;
  end;

  {: This class owns all utility functions as class methods so they don't pollute the name space }
  TFileSystem = class
  public
  {(*}
  type
    TCopyFileFlags = (cfFailIfExists, cfForceOverwrite, cfRaiseException);
    TCopyFileFlagSet = set of TCopyFileFlags;
    TMatchingFileResult = (mfNotFound, mfDirectory, mfFile, mfSpecial);
    TCopyFileWithProgressFlags = (cfwFailIfExists, cfwRestartable, cfwRaiseException);
    TCopyFileWithProgressFlagSet = set of TCopyFileWithProgressFlags;
    TCopyFileWithProgressResult = (cfwOK, cfwAborted, cfwError);
    TMoveFileWithProgressFlags = (
      mfwFailIfExists, {:< fail if the destination file already exists }
      mfwAllowCopy,    {:< allow using copy and delete if necessary }
      mfwDelayUntilReboot, {:< wait until next reboot for moving the file }
      mfwWriteThrough, {:< Setting this value guarantees that a move performed as a copy and delete operation is flushed to disk before the function returns. }
      mfwFailIfNotTrackable, {:< The function fails if the source file is a link source, but the file cannot be tracked after the move. }
      mfwRaiseException); {:< raise an exception if there is an error }
    TMoveFileWithProgressFlagSet = set of TMoveFileWithProgressFlags;
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
    class function CopyFile(const _Source, _Dest: string;
      _Flags: TCopyFileFlagSet = [cfRaiseException]): boolean; overload;
    {: Copies the file Source to Dest using the Windows CopyFileEx function which
       allows for a progress callback
       @param(Source is a string containing the name of the existing file)
       @param(Dest is a string containing the destination file name)
       @param(Flags is a set of TCopyFileWithProgressFlags specifying whether the copy operation
              cfwFailIfExists: fail if the destination file already exists.
              cfwRestartable: stores information in the destination file that allows
                to restart a stopped copy operation
              cfwRaiseException: retrieve the Windows error and raise an exception if it fails.
                If not set, it will not raise an exception but just return cfwAborted
                or cfwError if copying the file fails. (set by default))
       @returns cfeOK, if the copying succeeds, cfeAborted if the copying was aborted or
                stopped in the callback function and cfeError on any other error.
       @raises  EOSError if an error occurs and cfwRaiseException was passed }
    class function CopyFileWithProgress(const _Source, _Dest: string; _Progress: TCopyFileProgressEvt;
      _Flags: TCopyFileWithProgressFlagSet = [cfwRaiseException]): TCopyFileWithProgressResult;
    {: Copies the file Source to Dest using the Windows MoveFileWithProgress function which
       allows for a progress callback
       NOTE: If the file can be moved rather than copied, no call to the callback
             function will occur!
       @param(Source is a string containing the name of the existing file)
       @param(Dest is a string containing the destination file name)
       @param(Flags is a set of TCopyFileWithProgressFlags specifying whether the copy operation
              cfwFailIfExists: fail if the destination file already exists.
              cfwRestartable: stores information in the destination file that allows
                to restart a stopped copy operation
              cfwRaiseException: retrieve the Windows error and raise an exception if it fails.
                If not set, it will not raise an exception but just return cfwAborted
                or cfwError if copying the file fails. (set by default))
       @returns cfeOK, if the copying succeeds, cfeAborted if the copying was aborted or
                stopped in the callback function and cfeError on any other error.
       @raises  EOSError if an error occurs and cfwRaiseException was passed }
    class function MoveFileWithProgress(const _Source, _Dest: string; _Progress: TCopyFileProgressEvt;
      _Flags: TMoveFileWithProgressFlagSet = [mfwRaiseException]): TCopyFileWithProgressResult;
    {: Creates a directory (parent directories must already exist)
       @param DirectoryName is the name for the new directory
       @param RaiseException determines whether an exception is raised on error, default = true
       @returns true, if the directory was created
       @raises EOSError if there was an error and RaiseException was true }
    class function CreateDir(const _DirectoryName: string; _RaiseException: boolean = true): boolean;
    {: Creates a new directory, including the creation of parent directories as needed.
       @param DirectoryPath is the name for the new directory
       @param RaiseException determines whether an exception is raised on error, default = true
       @returns true, if the directory was created
       @raises EOSError if there was an error and RaiseException was true }
    class function ForceDir(const _DirectoryPath: string; _RaiseException: boolean = true): boolean;
    {: Sets a file's readonly flag
       @param Filename is the file to change
       @param Set determines whether to set or clear the flag }
    class function SetReadonly(const _Filename: string; _Set: boolean; _RaiseException: boolean = true): boolean;
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
    {: creates a backup of the file appending the current date and time to the base
       file name.
       @param Filename is the name of the file to back up
       @param BackupDir is a directory in which to create the backup file, if empty
                        the same directory as the original file is used }
    class procedure BackupFile(const _Filename: string; _BackupDir: string = '');
    class function GetFileInfo(const _Filename: string): TFileInfoRec;
    {: Returns the free space (in bytes) on the disk with the given drive letter }
    class function DiskFree(_DriveLetter: char): Int64;
  end;

type
  {: callback event for generating a filename for the given generation }
  TOnGenerateFilename = procedure(_Sender: TObject; _Generation: integer; var _Filename: string) of object;
type
  TFileGenerationHandler = class
  private
    FBaseName: string;
    FSuffix: string;
    FOnGenerateFilename: TOnGenerateFilename;
    FMaxGenerations: integer;
    FResultContainsNumber: boolean;
    FOldestIsHighest: boolean;
    FPrependZeros: integer;
    function GenerateFilename(_Generation: integer): string;
  public
    {: @param BaseName is the base filename to which by default _<n> followed by
                       the Suffix will be appended
       @param Suffix is the suffix for the filename, usually an extension which
                     must include the dot (.) }
    constructor Create(const _BaseName, _Suffix: string);
    {: generates the filename and returns it }
    function Execute(_KeepOriginal: boolean): string;
    {: the maximum of file generations that should be kept }
    property MaxGenerations: integer read FMaxGenerations write FMaxGenerations default 5;
    {: should the resulting filename contain a number? }
    property ResultContainsNumber: boolean read FResultContainsNumber write FResultContainsNumber default false;
    {: does the oldest file have the highest number? }
    property OldestIsHighest: boolean read FOldestIsHighest write FOldestIsHighest default true;
    property PrependZeros: integer read FPrependZeros write FPrependZeros default 0;
    {: allows read access to the file's base name as passed to the constructor }
    property BaseName: string read FBaseName;
    property Suffix: string read FSuffix;
    {: callback event for generating a filename for the given generation }
    property OnGenerateFilename: TOnGenerateFilename read FOnGenerateFilename write FOnGenerateFilename;
  end;

{: This is an abbreviation for IncludeTrailingPathDelimiter }
function itpd(const _Dirname: string): string; inline;

implementation

uses
  FileCtrl,
  u_dzMiscUtils,
  u_dzStringUtils,
  u_dzDateUtils;

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

function itpd(const _Dirname: string): string; inline;
begin
  Result := IncludeTrailingPathDelimiter(_Dirname);
end;

{ TSimpleDirEnumerator }

constructor TSimpleDirEnumerator.Create(const _Mask: string);
begin
  FMask := _Mask;
  FMustHaveAttr := [];
  FMayHaveAttr := [faHidden, faSysFile, faVolumeID, faDirectory, faArchive];
end;

destructor TSimpleDirEnumerator.Destroy;
begin
  Reset;
  inherited;
end;

class function TSimpleDirEnumerator.Execute(const _Mask: string; _List: TStrings): integer;
var
  enum: TSimpleDirEnumerator;
begin
  enum := TSimpleDirEnumerator.Create(_Mask);
  try
    Result := enum.FindAll(_List);
  finally
    enum.Free;
  end;
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
    if _EnumAttr in FMustHaveAttr then
      if (Attr and _SysAttr) = 0 then
        Result := false;
  end;

  procedure CondAddAttr(_EnumAttr: TFileAttributes; _SysAttr: integer);
  begin
    if _EnumAttr in FMayHaveAttr then
      Attr := Attr + _SysAttr;
  end;

begin
  repeat
    if not FActive then begin
      FMatchCount := 0;
      Attr := 0;
      CondAddAttr(faReadOnly, SysUtils.faReadOnly);
      CondAddAttr(faHidden, SysUtils.faHidden);
      CondAddAttr(faSysFile, SysUtils.faSysFile);
      CondAddAttr(faVolumeID, SysUtils.faVolumeID);
      CondAddAttr(faDirectory, SysUtils.faDirectory);
      CondAddAttr(faArchive, SysUtils.faArchive);
      Res := FindFirst(FMask, Attr, FSr);
      Result := (Res = 0);
      if Result then
        FActive := true;
    end else begin
      Res := SysUtils.FindNext(FSr);
      Result := (Res = 0);
    end;
    if not Result then
      exit;
    if (sr.Name = '.') or (sr.Name = '..') then
      Continue;
    if FMustHaveAttr <> [] then begin
      Attr := FSr.Attr;
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
    Inc(FMatchCount);
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
  if FActive then
    FindClose(FSr);
  FActive := false;
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

class function TFileSystem.CreateDir(const _DirectoryName: string;
  _RaiseException: boolean = true): boolean;
var
  LastError: Cardinal;
begin
  Result := SysUtils.CreateDir(_DirectoryName);
  if not Result then begin
    if _RaiseException then begin
      LastError := GetLastError;
      RaiseLastOsErrorEx(LastError, Format(STR_CREATEDIR_ERROR_S, [_DirectoryName]));
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
  s := itpd(_BaseDir) + _Prefix + '_' + IntToStr(Pid) + '_';
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

class function TFileSystem.GetFileInfo(const _Filename: string): TFileInfoRec;
var
  sr: TSearchRec;
  Res: integer;
begin
  Res := FindFirst(_Filename, faAnyFile, sr);
  if Res <> 0 then
    raise EFileNotFound.CreateFmt('File not found: "%s"', [_Filename]);
  try
    Result.Filename := _Filename;
    Result.Size := sr.Size;
    Result.Timestamp := FileDateToDateTime(sr.Time);
  finally
    FindClose(sr);
  end;
end;

class function TFileSystem.DiskFree(_DriveLetter: char): Int64;
var
  ErrorMode: Cardinal;
begin
  if _DriveLetter in ['a'..'z'] then
    _DriveLetter := chr(Ord(_DriveLetter) - Ord('a') + Ord('A'));

  if not (_DriveLetter in ['A'..'Z']) then
    Result := -1
  else begin
    ErrorMode := SetErrorMode(SEM_FAILCRITICALERRORS);
    try
      try
        Result := SysUtils.DiskFree(Ord(_DriveLetter) - Ord('A') + 1);
      except
        Result := -1;
      end;
    finally
      SetErrorMode(ErrorMode);
    end;
  end;
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

class procedure TFileSystem.BackupFile(const _Filename: string; _BackupDir: string = '');
var
  Ext: string;
  FilenameOnly: string;
  Base: string;
begin
  if _BackupDir = '' then
    _BackupDir := ExtractFilePath(_Filename);
  _BackupDir := itpd(_BackupDir);
  FilenameOnly := ExtractFileName(_Filename);
  Ext := ExtractFileExt(FilenameOnly);
  Base := ChangeFileExt(FilenameOnly, '');
  CopyFile(_Filename, _BackupDir + Base + '_' + ReplaceChars(DateTime2Iso(now, true), ': ', '-_') + Ext, true);
end;

class function TFileSystem.CopyFile(const _Source, _Dest: string; _Flags: TCopyFileFlagSet): boolean;
begin
  Result := CopyFile(_Source, _Dest,
    cfFailIfExists in _Flags,
    cfRaiseException in _Flags,
    cfForceOverwrite in _Flags);
end;

type
  TProgressRedir = class(TCopyProgressStatus)
  strict private
    FOnProgress: TCopyFileProgressEvt;
  private
    CancelFlag: BOOL;
    function doProgress(): TCopyProgressStatus.TProgressResult;
  public
    constructor Create(_OnProgress: TCopyFileProgressEvt);
  end;

//  PROGRESS_CONTINUE = 0;
//  PROGRESS_CANCEL = 1;
//  PROGRESS_STOP = 2;
//  PROGRESS_QUIET = 3;

//  CALLBACK_CHUNK_FINISHED = $00000000;
//  CALLBACK_STREAM_SWITCH = $00000001;

function ProgressCallback(
  _TotalFileSize, _TotalBytesTransferred, _StreamSize, _StreamBytesTransferred: LARGE_INTEGER;
  _StreamNumber, _CallbackReason: LongWord;
  _SourceFile, _DestinationFile: THandle; _Data: pointer): LongWord; far; stdcall;
var
  Status: TProgressRedir;
begin
  Status := TProgressRedir(_Data);
  Status.FTotalFileSize := _TotalFileSize;
  Status.FTotalBytesTransferred := _TotalBytesTransferred;
  Status.FStreamSize := _StreamSize;
  Status.FStreamBytesTransferred := _StreamBytesTransferred;
  Status.FStreamNumber := _StreamNumber;
  case _CallbackReason of
    CALLBACK_CHUNK_FINISHED: Status.FCallbackReason := prChunkFinished;
    CALLBACK_STREAM_SWITCH: Status.FCallbackReason := prStreamSwitch;
  else
    // Shouldn't happen, assume CALLBACK_CHUNK_FINISHED for now
    Status.FCallbackReason := prChunkFinished;
  end;
  Status.FSourceFile := _SourceFile;
  Status.FDestinationFile := _DestinationFile;
  case Status.doProgress() of
    prContinue: Result := PROGRESS_CONTINUE;
    prCancel: Result := PROGRESS_CANCEL;
    prStop: Result := PROGRESS_STOP;
    prQuiet: Result := PROGRESS_QUIET;
  else // should not happen, assume prContinue
    Result := PROGRESS_CONTINUE;
  end;
end;

//  COPY_FILE_FAIL_IF_EXISTS = $00000001;
//  COPY_FILE_RESTARTABLE = $00000002;

class function TFileSystem.CopyFileWithProgress(const _Source, _Dest: string;
  _Progress: TCopyFileProgressEvt;
  _Flags: TCopyFileWithProgressFlagSet = [cfwRaiseException]): TCopyFileWithProgressResult;
var
  Redir: TProgressRedir;
  Flags: DWORD;
  Res: BOOL;
  LastError: DWORD;
begin
  Redir := TProgressRedir.Create(_Progress);
  try
    Flags := 0;
    if cfwFailIfExists in _Flags then
      Flags := Flags or COPY_FILE_FAIL_IF_EXISTS;
    if cfwRestartable in _Flags then
      Flags := Flags or COPY_FILE_RESTARTABLE;
    Res := Windows.CopyFileEx(PChar(_Source), PChar(_Dest), @ProgressCallback, Redir,
      @Redir.CancelFlag, Flags);
    if not Res then begin
      LastError := GetLastError;
      if LastError = ERROR_REQUEST_ABORTED then
        Result := cfwAborted
      else begin
        if cfwRaiseException in _Flags then
          RaiseLastOsErrorEx(LastError, Format(STR_COPYFILE_ERROR_SS, [_Source, _Dest]));
        Result := cfwError;
      end;
    end else
      Result := cfwOK;
  finally
    Redir.Free;
  end;
end;

//  MOVEFILE_REPLACE_EXISTING       = $00000001;
//  MOVEFILE_COPY_ALLOWED           = $00000002;
//  MOVEFILE_DELAY_UNTIL_REBOOT     = $00000004;
//  MOVEFILE_WRITE_THROUGH          = $00000008;
//  MOVEFILE_CREATE_HARDLINK        = $00000010;
//  MOVEFILE_FAIL_IF_NOT_TRACKABLE  = $00000020;

class function TFileSystem.MoveFileWithProgress(const _Source, _Dest: string;
  _Progress: TCopyFileProgressEvt;
  _Flags: TMoveFileWithProgressFlagSet = [mfwRaiseException]): TCopyFileWithProgressResult;
var
  Redir: TProgressRedir;
  Flags: DWORD;
  Res: BOOL;
  LastError: DWORD;
begin
  Redir := TProgressRedir.Create(_Progress);
  try
    Flags := MOVEFILE_REPLACE_EXISTING;
    if mfwFailIfExists in _Flags then
      Flags := Flags - MOVEFILE_COPY_ALLOWED;
    if mfwAllowCopy in _Flags then
      Flags := Flags or MOVEFILE_COPY_ALLOWED;
    if mfwDelayUntilReboot in _Flags then
      Flags := Flags or MOVEFILE_DELAY_UNTIL_REBOOT;
    if mfwWriteThrough in _Flags then
      Flags := Flags or MOVEFILE_WRITE_THROUGH;
    if mfwFailIfNotTrackable in _Flags then
      Flags := Flags or MOVEFILE_FAIL_IF_NOT_TRACKABLE;
    Res := Windows.MoveFileWithProgress(PChar(_Source), PChar(_Dest),
      @ProgressCallback, Redir, Flags);
    if not Res then begin
      LastError := GetLastError;
      if mfwRaiseException in _Flags then
        RaiseLastOsErrorEx(LastError, Format(STR_COPYFILE_ERROR_SS, [_Source, _Dest]));

      if LastError = ERROR_REQUEST_ABORTED then
        Result := cfwAborted
      else
        Result := cfwError;
    end else
      Result := cfwOK;
  finally
    Redir.Free;
  end;
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

class function TFileSystem.ForceDir(const _DirectoryPath: string; _RaiseException: boolean = true): boolean;
var
  LastError: Cardinal;
begin
  Result := ForceDirectories(_DirectoryPath);
  if not Result and _RaiseException then begin
    LastError := GetLastError;
    RaiseLastOsErrorEx(LastError, Format(STR_CREATEDIR_ERROR_S, [_DirectoryPath]));
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

{ TProgressRedir }

constructor TProgressRedir.Create(_OnProgress: TCopyFileProgressEvt);
begin
  inherited Create;
  FOnProgress := _OnProgress;
end;

function TProgressRedir.doProgress(): TCopyProgressStatus.TProgressResult;
begin
  Result := prContinue;
  if Assigned(FOnProgress) then
    FOnProgress(Self, Result);
end;

{ TFileGenerationHandler }

constructor TFileGenerationHandler.Create(const _BaseName, _Suffix: string);
begin
  inherited Create;
  FMaxGenerations := 5;
  FOldestIsHighest := true;
  FResultContainsNumber := false;
  FPrependZeros := 0;
  FBaseName := _BaseName;
  FSuffix := _Suffix;
end;

function TFileGenerationHandler.Execute(_KeepOriginal: boolean): string;

  function doNoNumberOldIsHighest(): string;
  var
    i: Integer;
    dst: string;
    src: string;
    MaxGen: integer;
  begin
    MaxGen := FMaxGenerations - 1;
    for i := MaxGen - 1 downto 1 do begin
      dst := GenerateFilename(i + 1);
      if FileExists(dst) then
        TFileSystem.DeleteFile(dst);
      src := GenerateFilename(i);
      if FileExists(src) then
        TFileSystem.MoveFile(src, dst);
    end;
    dst := GenerateFilename(1);
    Result := GenerateFilename(0);
    if FileExists(dst) then
      TFileSystem.DeleteFile(dst);
    if FileExists(Result) then begin
      if _KeepOriginal then
        TFileSystem.CopyFile(Result, dst, true)
      else
        TFileSystem.MoveFile(Result, dst);
    end;
  end;

  function doNumberOldIsHighest(): string;
  var
    i: Integer;
    dst: string;
    src: string;
    MaxGen: integer;
  begin
    MaxGen := FMaxGenerations;
    for i := MaxGen - 1 downto 1 do begin
      dst := GenerateFilename(i + 1);
      if FileExists(dst) then
        TFileSystem.DeleteFile(dst);
      src := GenerateFilename(i);
      if FileExists(src) then
        TFileSystem.MoveFile(src, dst);
    end;
    Result := GenerateFilename(1);
  end;

  function doNoNumberOldIsLowest(): string;
  var
    i: Integer;
    MaxGen: integer;
    src: string;
    dst: string;
    SlotFound: Boolean;
  begin
    Result := GenerateFilename(0);
    if not FileExists(Result) then
      exit;

    SlotFound := false;
    MaxGen := FMaxGenerations - 1;
    for i := 1 to MaxGen do begin
      dst := GenerateFilename(i);
      if not FileExists(dst) then begin
        SlotFound := true;
        break;
      end;
    end;

    if not SlotFound then begin
      dst := GenerateFilename(1);
      if FileExists(dst) then
        TFileSystem.DeleteFile(dst);
      for i := 2 to MaxGen do begin
        src := GenerateFilename(i);
        if FileExists(src) then
          TFileSystem.MoveFile(src, dst);
        dst := src;
      end;
    end;

    if _KeepOriginal then
      TFileSystem.CopyFile(Result, dst, true)
    else
      TFileSystem.MoveFile(Result, dst);
  end;

  function doNumberOldIsLowest(): string;
  var
    i: Integer;
    MaxGen: integer;
  begin
    MaxGen := FMaxGenerations;
    for i := 1 to MaxGen do begin
      Result := GenerateFilename(i);
      if not FileExists(Result) then
        exit;
    end;

    TFileSystem.DeleteFile(GenerateFilename(1));
    for i := 2 to MaxGen do begin
      TFileSystem.MoveFile(GenerateFilename(i), GenerateFilename(i - 1));
    end;
    Result := GenerateFilename(MaxGen);
    if _KeepOriginal then
      TFileSystem.CopyFile(GenerateFilename(MaxGen - 1), Result, true);
  end;

begin
  if FResultContainsNumber then begin
    if _KeepOriginal then
      raise EInvalidPropertyCombination.Create('Combination of ResultContainsNumber and KeepOriginal is not allowed');
    if FOldestIsHighest then begin
      Result := doNumberOldIsHighest();
    end else begin
      Result := doNumberOldIsLowest();
    end;
  end else begin
    if FOldestIsHighest then begin
      Result := doNoNumberOldIsHighest();
    end else begin
      Result := doNoNumberOldIsLowest();
    end;
  end;
end;

function TFileGenerationHandler.GenerateFilename(_Generation: integer): string;
begin
  if _Generation = 0 then
    Result := FBaseName + FSuffix
  else begin
    if FPrependZeros = 0 then
      Result := FBaseName + '_' + IntToStr(_Generation) + FSuffix
    else
      Result := Format('%s_%.*u%s', [FBaseName, FPrependZeros, _Generation, FSuffix]);
  end;
  if Assigned(FOnGenerateFilename) then
    FOnGenerateFilename(Self, _Generation, Result);
end;

{ TDirectorySync }

//procedure TDirectorySync.doOnDifferentFileExists(const _Filename: string; var _Action: TFileExistsAction);
//begin
//  _Action := feaIgnore;
//  if Assigned(FOnDifferentFileExists) then
//    FOnDifferentFileExists(_Filename, _Action);
//end;

function TDirectorySync.doOnFileExists(const _SrcDir, _DstDir, _Filename: string): TFileExistsAction;
var
  Src: TFileInfoRec;
  Dst: TFileInfoRec;
begin
  Result := feaIgnore;
  if not Assigned(FOnFileExists) then
    exit;

  Src := TFileSystem.GetFileInfo(_SrcDir + _Filename);
  Dst := TFileSystem.GetFileInfo(_DstDir + _Filename);
  FOnFileExists(self, Src, Dst, Result);
end;

procedure TDirectorySync.doOnSyncingDir(const _SrcDir, _DstDir: string);
begin
  if Assigned(FOnSyncingDir) then
    FOnSyncingDir(Self, _SrcDir, _DstDir);
end;

procedure TDirectorySync.doOnSyncingFile(const _SrcFile, _DstFile: string);
begin
  if Assigned(FOnSyncingFile) then
    FOnSyncingFile(self, _SrcFile, _DstFile);
end;

procedure TDirectorySync.CheckOneWay(const _SrcDir, _DstDir: string);
var
  Filename: string;
  EnumA: TSimpleDirEnumerator;
  DstDirBS: string;
  SrcDirBS: string;
begin
  doOnSyncingDir(_SrcDir, _DstDir);
  SrcDirBS := itpd(_SrcDir);
  DstDirBS := itpd(_DstDir);
  EnumA := TSimpleDirEnumerator.Create(SrcDirBS + '*.*');
  try
    while EnumA.FindNext(Filename) do begin
      if (EnumA.Sr.Attr and SysUtils.faDirectory) <> 0 then begin
        if DirectoryExists(DstDirBS + Filename) then
          CheckOneWay(SrcDirBS + Filename, DstDirBS + Filename);
      end else if FileExists(DstDirBS + Filename) then begin
        doOnFileExists(SrcDirBS, DstDirBS, Filename);
      end else begin
        doOnSyncingFile(SrcDirBS + Filename, DstDirBS + Filename);
      end;
    end;
  finally
    EnumA.Free;
  end;
end;

procedure TDirectorySync.SyncOneWay(const _SrcDir, _DstDir: string);
var
  Filename: string;
  EnumA: TSimpleDirEnumerator;
  DstDirBS: string;
  SrcDirBS: string;
begin
  doOnSyncingDir(_SrcDir, _DstDir);
  SrcDirBS := itpd(_SrcDir);
  DstDirBS := itpd(_DstDir);
  EnumA := TSimpleDirEnumerator.Create(SrcDirBS + '*.*');
  try
    while EnumA.FindNext(Filename) do begin
      if (EnumA.Sr.Attr and SysUtils.faDirectory) <> 0 then begin
        if not DirectoryExists(DstDirBS + Filename) then
          TFileSystem.CreateDir(DstDirBS + Filename);
        SyncOneWay(SrcDirBS + Filename, DstDirBS + Filename);
      end else if FileExists(DstDirBS + Filename) then begin
        if doOnFileExists(SrcDirBS, DstDirBS, Filename) = feaOverwrite then begin
          doOnSyncingFile(SrcDirBS + Filename, DstDirBS + Filename);
          TFileSystem.CopyFile(SrcDirBS + Filename, DstDirBS + Filename, false, true);
        end;
      end else begin
        doOnSyncingFile(SrcDirBS + Filename, DstDirBS + Filename);
        TFileSystem.CopyFile(SrcDirBS + Filename, DstDirBS + Filename, true, true);
      end;
    end;
  finally
    EnumA.Free;
  end;
end;

procedure TDirectorySync.SyncBothWays(const _DirA, _DirB: string);
begin
  SyncOneWay(_DirA, _DirB);
  SyncOneWay(_DirB, _DirA);
end;

end.

