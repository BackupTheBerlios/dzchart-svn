{GXFormatter.config=twm}
{: This unit implements a TdzFile and a TdzTempFile class.
   TdzFile encapsulates the rather complex CreateFile WinApi call,
   TdzTempFile is a descendant of TdzFile, uses a temporary filename
   and sets the properties to sensible values for temporary files. }
unit u_dzFileStreams;

{$I jedi.inc}

interface

uses
  Classes,
  SysUtils,
  Windows;

type
  {: Parent of all exceptions raised in u_dzFiles }
  EdzFile = class(Exception);

type
  {: File access modes.
     <ul>
       <li>faRead = open file for reading</li>
       <li>faWrite = open file for writing</li>
     </ul> }
  TFileAccessModes = (faRead, faWrite);
  {: Set of file access modes. Used to pass combinations of faRead and faWrite }
  TFileAccessModeSet = set of TFileAccessModes;
  {: File share modes.
     <ul>
       <li>fsRead = share file for reading</li>
       <li>fsWrite = share file for writing</li>
     <ul> }
  TFileShareModes = (fsRead, fsWrite);
  {: Set of file share modes. Used to pass combinations of fsRead and fsWrite }
  TFileShareModeSet = set of TFileShareModes;
  {: Possible actions to take on opening a file.
     <ul>
       <li>fcCreateFailIfExists = Create a new file. If it already exists, fail.</li>
       <li>fcCreateTruncateIfExists = Create a new file. If it already exists, truncate it.</li>
       <li>fcOpenFailIfNotExists = Open an existing file. If it does not exists, fail.</li>
       <li>fcOpenCreateIfNotExists = Open an existing file. If it does not exist, create it.</li>
       <li>fcOpenTruncateFailIfNotExists = Open an existing file and truncate it. If it does not exist, fail.</li>
     </ul> }
  TFileCreateDisposition = (fcCreateFailIfExists, fcCreateTruncateIfExists,
    fcOpenFailIfNotExists, fcOpenCreateIfNotExists,
    fcOpenTruncateFailIfNotExists);

const
  {: Constant to pass the file access mode read/write }
  faReadWrite = [faRead, faWrite];
  {: Constant to pass the file share mode read/write }
  fsReadWrite = [fsRead, fsWrite];
  {: Constant to pass the file share mode no sharing }
  fsNoSharing = [];

type
  {: Enhanced TFileStream based on THandleStream.
     Allows to specify all the parameters that can be specified to the
     Windows API function CreateFile }
  TdzFile = class(THandleStream)
  protected
    {: Stores the Filename property }
    fFilename: string;
    {: Stores the AccessMode property }
    fAccessMode: TFileAccessModeSet;
    {: Stores the ShareMode property }
    fShareMode: TFileShareModeSet;
    {: Stores the CreateDisposition property }
    fCreateDisposition: TFileCreateDisposition;
    {: Stores the SecurityAttributes property }
    fSecurityAttributes: PSecurityAttributes;
    {: Stores the FileAttributes property }
    fFileAttributes: DWORD;
    {: Stores the FileFlags property }
    fFileFlags: DWORD;
    {: Stores the ResetReadOnly property }
    FResetReadOnly: boolean;
    {: Set methdo for FileAttributes property }
    procedure SetFileAttributes(_FileAttributes: DWORD);
    {: Set method for FileFlags property }
    procedure SetFileFlags(_FileFlags: DWORD);
    {: Set method for Filename property }
    procedure SetFilename(const _Filename: string);
  public
    {: Creates a TdzFile object.
       @param Filename is a string containing the name of the file to open }
    constructor Create(const _Filename: string);
    {: Standard destructor.
       Flushes and closes the file if it is still open, destroys the object }
    destructor Destroy; override;
    {: Opens the file as specified by the properties, raises an exception on error }
    procedure Open;
    {: Opens the file as specified by the properties, returns false on error }
    function OpenNoException: boolean;
    {: Opens the file in readonly mode, with
       CreateDisposition = fcOpenFailIfNotExists
       ShareMode = [fsRead]
       AccessMode = [faRead]
       raises an exception on error }
    procedure OpenReadonly;
    {: Opens the file and seeks to the end. Returns the new position (that is: The file length). }
    function Append: longint;
    {: Closes the file and sets the handle to INVALID_HANDLE_VALUE }
    procedure Close;
    {: returns true if Position = Size }
    function EOF: boolean;
    {: The file's name as passed to the Create constructor. }
    property Filename: string read fFilename write SetFilename;
    {: The file's access mode.
       Can be set to [faRead], [faWrite] or [faRead, faWrite]. }
    property AccessMode: TFileAccessModeSet read fAccessMode write fAccessMode;
    {: The file's share mode.
       Can be set to [], [fsRead], [fsWrite], [fsRead, fsWrite]. There are also
       two predefined constants fsNoSharing and fsReadWrite }
    property ShareMode: TFileShareModeSet read fShareMode write fShareMode;
    {: The file's security attributes.
       See the Windows API function CreateFile for details. }
    property SecurityAttributes: PSecurityAttributes
      read fSecurityAttributes write fSecurityAttributes;
    {: Specifies the action to take in various error conditions.
       <ul>
         <li>fcCreateFailIfExists = Create a new file. If it already exists, fail.</li>
         <li>fcCreateTruncateIfExists = Create a new file. If it already exists, truncate it.</li>
         <li>fcOpenFailIfNotExists = Open an existing file. If it does not exists, fail.</li>
         <li>fcOpenCreateIfNotExists = Open an existing file. If it does not exist, create it.</li>
         <li>fcOpenTruncateFailIfNotExists = Open an existing file and truncate it. If it does not exist, fail.</li>
       </ul> }
    property CreateDisposition: TFileCreateDisposition
      read fCreateDisposition write fCreateDisposition;
    {: The file's attributes.
       Can be set to a combination of:
       <ul>
         <li>FILE_ATTRIBUTE_ARCHIVE</li>
         <li>FILE_ATTRIBUTE_COMPRESSED</li>
         <li>FILE_ATTRIBUTE_HIDDEN</li>
         <li>FILE_ATTRIBUTE_NORMAL</li>
         <li>FILE_ATTRIBUTE_OFFLINE</li>
         <li>FILE_ATTRIBUTE_READONLY</li>
         <li>FILE_ATTRIBUTE_SYSTEM</li>
         <li>FILE_ATTRIBUTE_TEMPORARY</li>
       </ul>
       See the Windows API help for a description. Note that these attribute
       are only used when actually creating a new file. }
    property FileAttributes: DWORD read fFileAttributes write SetFileAttributes;
    {: The file's flags.
       Can be set to a combination of:
       <ul>
         <li>FILE_FLAG_WRITE_THROUGH</li>
         <li>FILE_FLAG_OVERLAPPED</li>
         <li>FILE_FLAG_NO_BUFFERING</li>
         <li>FILE_FLAG_RANDOM_ACCESS</li>
         <li>FILE_FLAG_SEQUENTIAL_SCAN</li>
         <li>FILE_FLAG_DELETE_ON_CLOSE</li>
         <li>FILE_FLAG_BACKUP_SEMANTICS</li>
         <li>FILE_FLAG_POSIX_SEMANTICS</li>
       </ul>
       See the Windows API help for a description. Note that these attribute
       are only used when actually creating a new file. }
    property FileFlags: DWORD read fFileFlags write SetFileFlags;
    {: This flag determines whether to try resetting the readonly flag of the file
       opening fails. Only used if AccessMode contains faWrite.
       Usually you don't want this, so the default is false }
    property ResetReadOnly: boolean read FResetReadOnly write FResetReadOnly;
  end;

type
  {: Represents a temporary file.
     This is just a TdzFile object which is created in the TEMP directory and
     the following properties:<br>
       AccessMode := faReadWrite;<br>
       ShareMode := fsReadWrite;<br>
       CreateDisposition := fcCreateTruncateIfExists;<br>
       FileAttributes := FILE_ATTRIBUTE_TEMPORARY;<br>
       FileFlags := FILE_FLAG_DELETE_ON_CLOSE;<br>
  }
  TdzTempFile = class(TdzFile)
  public
    constructor Create(_Directory: string = ''; const _Prefix: string = 'dz';
      _Unique: word = 0);
  end;

implementation

uses
  u_dzFileUtils,
  u_dzMiscUtils;

const
  cFileAccessModeValues: array[TFileAccessModes] of DWORD =
    (GENERIC_READ, GENERIC_WRITE);
  cFileShareModeValues: array[TFileShareModes] of DWORD =
    (FILE_SHARE_READ, FILE_SHARE_WRITE);
  cFileCreateDispositionValues: array[TFileCreateDisposition] of DWORD =
    (CREATE_NEW, CREATE_ALWAYS, OPEN_EXISTING, OPEN_ALWAYS, TRUNCATE_EXISTING);

type
  {: This is a hack to access the fHandle field which is private in the
     declaration of THandleStream. It will stop working when Borland adds more
     fields in front of FHandle. Check when upgrading! }
  tHandleStreamHack = class(TStream)
  private
    FHandle: DWord;
  end;

constructor TdzFile.Create(const _Filename: string);
const
  ROUTINE_ID = 'TdzFile.Create';
begin
  inherited Create(integer(INVALID_HANDLE_VALUE));
  fFilename := _Filename;
  fAccessMode := [faRead];
  fShareMode := [fsRead];
  fCreateDisposition := fcOpenFailIfNotExists;
  fSecurityAttributes := nil;
  fFileAttributes := FILE_ATTRIBUTE_NORMAL;
  fFileFlags := 0;
end;

destructor TdzFile.Destroy;
const
  ROUTINE_ID = 'TdzFile.Destroy';
begin
  Close;
  inherited;
end;

procedure TdzFile.Close;
const
  ROUTINE_ID = 'TdzFile.Close';
begin
  if tHandleStreamHack(self).fHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(tHandleStreamHack(self).fHandle);
  tHandleStreamHack(self).fHandle := INVALID_HANDLE_VALUE;
end;

procedure TdzFile.OpenReadonly;
const
  ROUTINE_ID = 'TdzFile.OpenReadonly';
begin
  CreateDisposition := fcOpenFailIfNotExists;
  ShareMode := [fsRead];
  AccessMode := [faRead];
  Open;
end;

procedure TdzFile.Open;
const
  ROUTINE_ID = 'TdzFile.Open';
begin
  if not OpenNoException then
    RaiseLastOSErrorEx('%1:s (%0:d)');
end;

function TdzFile.OpenNoException: boolean;
const
  ROUTINE_ID = 'TdzFile.OpenNoException';
var
  fam: TFileAccessModes;
  Access: DWORD;
  fsm: TFileShareModes;
  ShareMode: DWORD;
  Disposition: DWORD;
  TriedResetReadonly: boolean;
begin
  Access := 0;
  for fam := low(TFileAccessModes) to high(TFileAccessModes) do
    if fam in fAccessMode then
      Access := Access or cFileAccessModeValues[fam];
  ShareMode := 0;
  for fsm := low(TFileShareModes) to high(TFileShareModes) do
    if fsm in fShareMode then
      ShareMode := ShareMode or cFileShareModeValues[fsm];
  Disposition := cFileCreateDispositionValues[fCreateDisposition];

  TriedResetReadonly := false;
  repeat
    THandleStreamHack(self).fHandle := Windows.CreateFile(pChar(fFilename), Access,
      ShareMode, fSecurityAttributes, Disposition, fFileAttributes or fFileFlags, 0);
    Result := (DWORD(Handle) <> INVALID_HANDLE_VALUE);
    if not Result and ResetReadonly and not TriedResetReadonly then
      TFileSystem.SetReadonly(FFilename, false, false);
  until Result or TriedResetReadonly or not ResetReadonly or not (faWrite in AccessMode) and ResetReadonly;

end;

function TdzFile.Append: longint;
begin
  Open;
  Result := Seek(0, soFromEnd);
end;

procedure TdzFile.SetFileAttributes(_FileAttributes: DWORD);
const
  cValidAttributes = FILE_ATTRIBUTE_ARCHIVE or FILE_ATTRIBUTE_COMPRESSED or
    FILE_ATTRIBUTE_HIDDEN or FILE_ATTRIBUTE_NORMAL or
    FILE_ATTRIBUTE_OFFLINE or FILE_ATTRIBUTE_READONLY or
    FILE_ATTRIBUTE_SYSTEM or FILE_ATTRIBUTE_TEMPORARY;
begin
  if (_FileAttributes and not cValidAttributes) <> 0 then
    raise EdzFile.Create('Invalid file attributes');
  fFileAttributes := _FileAttributes;
end;

procedure TdzFile.SetFileFlags(_FileFlags: DWORD);
const
  cValidFlags = (FILE_FLAG_WRITE_THROUGH or FILE_FLAG_OVERLAPPED or
    FILE_FLAG_NO_BUFFERING or FILE_FLAG_RANDOM_ACCESS or
    FILE_FLAG_SEQUENTIAL_SCAN or FILE_FLAG_DELETE_ON_CLOSE or
    FILE_FLAG_BACKUP_SEMANTICS or FILE_FLAG_POSIX_SEMANTICS);
begin
  if (_FileFlags and not cValidFlags) <> 0 then
    raise EdzFile.Create('Invalid file flags');
  FFileFlags := _FileFlags;
end;

{ TdzTempFile }

constructor TdzTempFile.Create(_Directory: string; const _Prefix: string;
  _Unique: word);
begin
  inherited Create(TFileSystem.GetTempFileName(_Directory, _Prefix, _Unique));
  AccessMode := faReadWrite;
  ShareMode := fsReadWrite;
  CreateDisposition := fcCreateTruncateIfExists;
  FileAttributes := FILE_ATTRIBUTE_TEMPORARY;
  FileFlags := FILE_FLAG_DELETE_ON_CLOSE;
end;

procedure TdzFile.SetFilename(const _Filename: string);
begin
  if Self.Handle <> integer(INVALID_HANDLE_VALUE) then
    raise EdzFile.Create('Cannot change filename when file is open.');
  fFilename := _Filename;
end;

function TdzFile.EOF: boolean;
begin
  Result := Position = Size;
end;

end.

