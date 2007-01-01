unit dzAutoUtil;

{$I delphiversions.inc}

interface

uses
  Windows,
  ActiveX,
  SysUtils,
  Classes,
  Dialogs,
  ToolsAPI;

type
  TOnAutoSave = procedure(const _Filename: string) of object;
  TOnError = procedure(const _Error: string) of object;

var
  OnAutoSave: TOnAutoSave = nil;
  OnError: TOnError = nil;

type
{$IFNDEF delphi6up}
  // Base class for all notifiers, wizards, etc.
  // The base class does nothing for each method, but derived classes
  // can override any or all of these methods. Note that any given notifier
  // interface might not call some (or any) of these methods.
  TNotifierObject = class(TInterfacedObject, IOTANotifier)
  public
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  end;
{$ENDIF}

  TFileList = class;
  TIdeFile = class;
  TModuleList = class;

  // Each Delphi module has a corresponding TModule object in the wizard.
  // The TModule object is a module notifier and it keeps track of the
  // individual files that comprise a module.
  TModule = class(TNotifierObject, IOTANotifier, IOTAModuleNotifier)
  private
    fFileList: TFileList; // list of editor notifiers (TIdeFile objects)
    fModule: IOTAModule; // Tools API module interface
    fModuleList: TModuleList; // parent that contains this module
    fNotifierIndex: Integer;
  protected
    procedure AddNotifier;
    procedure RemoveNotifier;
    procedure GetFiles;
    property NotifierIndex: Integer read fNotifierIndex;

  public
    constructor Create(const Module: IOTAModule; List: TModuleList);
    destructor Destroy; override;
    procedure AutoSave;

    // When the user saves a file, Delphi calls the module's AfterSave,
    // but not AfterSave for the individual editors.
    procedure AfterSave;
    procedure Destroyed;

    procedure GetFileList(List: TStrings);
    function CheckOverwrite: Boolean;
    procedure ModuleRenamed(const NewName: string);

    property Module: IOTAModule read fModule;
    property FileList: TFileList read fFileList;
  end;

  // Keep a list of open modules.
  TModuleList = class
  private
    fList: TList;
    function GetCount: Integer;
    function GetModules(Index: Integer): TModule;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AutoSave;
    procedure Add(const Module: IOTAModule);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure GetFileList(List: TStrings);
    procedure Remove(Module: TModule);

    property Count: Integer read GetCount;
    property Modules[Index: Integer]: TModule read GetModules; default;
  end;

  // Each file that is open in the IDE is represented by a TIdeFile object.
  // Derived classes specialize for different kinds of files, which are
  // auto-saved differently, but the basic notifier behavior is common to
  // all the files. Note that each module can have multiple files.
  TIdeFile = class
  private
    fEditor: IOTAEditor;
    fFileName: string;
  public
    constructor Create(const Editor: IOTAEditor);
    procedure doAutoSave; virtual;
    procedure AutoSave;
    procedure Renamed; virtual;
    procedure Saved; virtual;

    property Editor: IOTAEditor read fEditor;
    property FileName: string read fFileName;
  end;

  TFormFile = class(TIdeFile)
  public
    procedure doAutoSave; override;
  end;

  TSourceFile = class(TIdeFile)
  public
    procedure doAutoSave; override;
  end;

  TResourceFile = class(TIdeFile)
  public
    procedure doAutoSave; override;
  end;

  // Keep a list of TIdeFile objects.
  TFileList = class
  private
    fList: TList;
    function GetCount: Integer;
    function GetFiles(Index: Integer): TIdeFile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AutoSave;
    procedure Add(const Editor: IOTAEditor);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure GetFileList(List: TStrings);
    procedure Remove(IdeFile: TIdeFile);
    procedure Renamed;
    procedure Saved;

    property Count: Integer read GetCount;
    property Files[Index: Integer]: TIdeFile read GetFiles; default;
  end;

  // A file stream that stores an auto-save file.
  TAutoSaveStream = class(THandleStream)
  public
    constructor Create(const Filename: string);
    destructor Destroy; override;
  end;

function MakeAutoSaveFilename(const FileName: string): string;

implementation

uses
{$ifdef delphi6up}
  RtlConsts;
{$else}
  Consts;
{$endif}

resourcestring
  S_Error = 'Error auto-saving %s'#13#10'%s';

{ Form the auto-save file name by prepending a tilde to the
  real file name. For example, c:\dir\foo.pas becomes
  c:\dir\~foo.pas. }

function MakeAutoSaveFilename(const FileName: string): string;
begin
  Result := ExtractFilePath(FileName);
  if (Length(Result) > 0) and (Result[Length(Result)] <> '\') then
    Result := Result + '\';
  Result := Result + '~' + ExtractFileName(FileName);
end;

{ TAutoSaveStream }

{ Create an auto-save file, and open a stream to write that file.
  TFileStream doesn't let you set the hidden file attribute when
  creating the file, so inherit from THandleStream and open the
  file by calling Windows' CreateFile API function. }

constructor TAutoSaveStream.Create(const Filename: string);
var
  SaveFile: string;
  Handle: THandle;
begin
  SaveFile := MakeAutoSaveFilename(Filename);
  Handle := Windows.CreateFile(PChar(SaveFile), Generic_Write, 0, nil,
    Create_Always, File_Attribute_Hidden, 0);

  if Handle = Invalid_Handle_Value then
    raise EFCreateError.CreateFmt(S_Error, [FileName]);

  inherited Create(Handle);
end;

destructor TAutoSaveStream.Destroy;
begin
  CloseHandle(Handle);
  inherited Destroy;
end;

{$IFNDEF delphi6up}
{ TNotifierObject }

procedure TNotifierObject.AfterSave;
begin
end;

procedure TNotifierObject.BeforeSave;
begin
end;

procedure TNotifierObject.Destroyed;
begin
end;

procedure TNotifierObject.Modified;
begin
end;
{$ENDIF}

{ TIdeFile }
// Remember the editor interface and register the notifier.

constructor TIdeFile.Create(const Editor: IOTAEditor);
begin
  fEditor := Editor;
  fFileName := Editor.FileName;
  inherited Create;
end;

// Delete the auto-saved backup file.

procedure TIdeFile.Saved;
begin
  DeleteFile(MakeAutoSaveFilename(Editor.FileName));
end;

// When the user renames a file, clean up the autosaved files, too.
// Instead of messing around renaming the autosave files, it's easier
// just to create new ones with the new name and delete the old
// ones with the old name.
//
// At first, it seems safest to create the new files first, then
// delete the old ones. That way, there is no window where a system
// crash could occur after deleting the old autosaved files but before
// the new ones were created. On the other hand, what if the file isn't
// really renamed, but Delphi thinks it is? The IDE isn't always smart
// about telling the difference between UNC names and drive letters,
// for example. If the autosave wizard were to create the new files,
// and then delete the old ones, that would be wrong if the old and
// new files are the same.
//
// The best solution is to open the old and new files and see whether
// they are the same. Win32 can return this information by comparing
// the volume identifier and the file index number, but I'm too lazy.
// The short cut is to delete the old file first, then create the new one.

procedure TIdeFile.Renamed;
begin
  // Delete the old autosaved backup files.
  DeleteFile(MakeAutoSaveFilename(FileName));
  // Remember the new file name.
  fFileName := Editor.FileName;
  // Save under the new name.
  AutoSave;
end;

{ TFormFile }
// Auto-save a form file by writing its resource.

procedure TFormFile.doAutoSave;
var
  Stream: TStream;
  Adapter: IStream;
begin
  // The form editor interface needs an IStream, so create
  // an adapter for the TAutoSaveStream.
  Stream := TAutoSaveStream.Create(Editor.FileName);
  try
    Adapter := TStreamAdapter.Create(Stream);
    try
      (Editor as IOTAFormEditor).GetFormResource(Adapter);
      inherited;
    except
      on Ex: Exception do
      { Propagate the exception, but with the file name. }
        raise Exception.CreateFmt(S_Error, [Editor.Filename, Ex.Message]);
    end;
  finally
    Stream.Free;
  end;
end;

{ TSourceFile }
// Auto-save a file in the text editor.

procedure TSourceFile.doAutoSave;
const
  BufSize = 8192;
var
  SourceEditor: IOTASourceEditor;
  Reader: IOTAEditReader;
  Buffer: PChar;
  Count: Integer;
  Stream: TStream;
begin
  Stream := nil;
  SourceEditor := Editor as IOTASourceEditor;
  Reader := SourceEditor.CreateReader;
  GetMem(Buffer, BufSize);
  try
    Stream := TAutoSaveStream.Create(Editor.FileName);
    try
      repeat
        Count := Reader.GetText(Stream.Position, Buffer, BufSize);
        Stream.WriteBuffer(Buffer^, Count);
      until Count <> BufSize;
      inherited;
    except
      on Ex: Exception do
      { Propagate the exception, but with the file name. }
        raise Exception.CreateFmt(S_Error, [Editor.Filename, Ex.Message]);
    end;
  finally
    Stream.Free;
    FreeMem(Buffer);
  end;
end;

{ TFileList }

constructor TFileList.Create;
begin
  inherited;
  fList := TList.Create;
end;

destructor TFileList.Destroy;
begin
  Clear;
  fList.Free;
  inherited;
end;

procedure TFileList.Add(const Editor: IOTAEditor);
var
  FormEditor: IOTAFormEditor;
  SourceEditor: IOTASourceEditor;
  ResourceFile: IOTAProjectResource;
begin
  if Succeeded(Editor.QueryInterface(IOTAFormEditor, FormEditor)) then
    fList.Add(TFormFile.Create(Editor))
  else
    if Succeeded(Editor.QueryInterface(IOTASourceEditor, SourceEditor)) then
      fList.Add(TSourceFile.Create(Editor))
    else
      if Succeeded(Editor.QueryInterface(IOTAProjectResource, ResourceFile)) then
        fList.Add(TResourceFile.Create(Editor))
      else
        ShowMessage('Unknown file type. Cannot autosave ' + Editor.FileName);
end;

// Autosave all the modified files.

procedure TFileList.AutoSave;
var
  I: Integer;
  IdeFile: TIdeFile;
begin
  for I := 0 to fList.Count - 1 do
    begin
      IdeFile := Files[I];
      if IdeFile.Editor.Modified then
        begin
          IdeFile.AutoSave;
        end;
    end;
end;

// Delete all the files in the list.

procedure TFileList.Clear;
var
  I: Integer;
begin
  for I := fList.Count - 1 downto 0 do
    Delete(I);
end;

// Delete and free a file in the list.

procedure TFileList.Delete(Index: Integer);
var
  IdeFile: TIdeFile;
begin
  IdeFile := Files[Index];
  fList.Delete(Index);
  IdeFile.Free;
end;

// Get the list size.

function TFileList.GetCount: Integer;
begin
  Result := fList.Count;
end;

// Get a file from the list.

function TFileList.GetFiles(Index: Integer): TIdeFile;
begin
  Result := fList[Index];
end;

// When a notifier is destroyed, the notifier calls back to the list
// and the list deletes the notifier.

procedure TFileList.Remove(IdeFile: TIdeFile);
begin
  Delete(fList.IndexOf(IdeFile));
end;

// The module was renamed, so rename all the editor files.

procedure TFileList.Renamed;
var
  I: Integer;
begin
  for I := 0 to fList.Count - 1 do
    Files[I].Renamed;
end;

// The module was saved, so delete all the autosave files.

procedure TFileList.Saved;
var
  I: Integer;
begin
  for I := 0 to fList.Count - 1 do
    Files[I].Saved;
end;

// Copy all the file names in the list to "List".
// This method is used for debugging.

procedure TFileList.GetFileList(List: TStrings);
var
  I: Integer;
begin
  for I := 0 to fList.Count - 1 do
    List.AddObject(Files[I].FileName, TObject(Files[I].Editor));
end;

{ TModule }

constructor TModule.Create(const Module: IOTAModule; List: TModuleList);
begin
  inherited Create;
  fModule := Module;
  fFileList := TFileList.Create;
  fModuleList := List;
  GetFiles;
  AddNotifier;
end;

destructor TModule.Destroy;
begin
  RemoveNotifier;
  fFileList.Free;
  fFileList := nil;
  inherited;
end;

// Autosave all the modified files for this module.

procedure TModule.AutoSave;
begin
  FileList.AutoSave;
end;

// Module notifier method. Always return True because the autosave wizard does't care.

function TModule.CheckOverwrite: Boolean;
begin
  Result := True;
end;

// When the module is destroyed, free the TModule object.
// Also, free the autosaved files as though the user saved the file.
// If the user deliberately abandons modified files that means the
// user wants to discard those changes. The IDE prompts the user for
// confirmation, so it's safe to abandon the autosave files, too.

procedure TModule.Destroyed;
begin
  AfterSave; // Pretend the user saved the module.
  RemoveNotifier;
  fModuleList.Remove(Self); // Get rid of this module notifier.
end;

procedure TModule.ModuleRenamed(const NewName: string);
begin
  FileList.Renamed;
end;

// Get a list of all the files associated with this module, for debugging.

procedure TModule.GetFileList(List: TStrings);
begin
  FileList.GetFileList(List);
end;

// Initialize the module with all of its editor files.

procedure TModule.GetFiles;
var
  I: Integer;
begin
  for I := 0 to Module.GetModuleFileCount - 1 do
    FileList.Add(Module.GetModuleFileEditor(I));
end;

// After the user saves a file, delete all the autosave files.

procedure TModule.AfterSave;
begin
  FileList.Saved;
end;

procedure TModule.AddNotifier;
begin
  fNotifierIndex := Module.AddNotifier(Self);
end;

procedure TModule.RemoveNotifier;
begin
  if NotifierIndex >= 0 then
    begin
      Module.RemoveNotifier(NotifierIndex);
      fNotifierIndex := -1;
    end;
end;

{ TModuleList }

constructor TModuleList.Create;
begin
  inherited;
  fList := TList.Create;
end;

destructor TModuleList.Destroy;
begin
  Clear;
  fList.Free;
  inherited;
end;

// Add a module to the list.

procedure TModuleList.Add(const Module: IOTAModule);
begin
  fList.Add(TModule.Create(Module, Self));
end;

// Autosave all the modules and all their files.

procedure TModuleList.AutoSave;
var
  I: Integer;
begin
  for I := 0 to fList.Count - 1 do
    Modules[I].AutoSave;
end;

// Clear all the modules from the list.

procedure TModuleList.Clear;
var
  I: Integer;
begin
  for I := fList.Count - 1 downto 0 do
    Delete(I);
end;

// Return the size of the list.

function TModuleList.GetCount: Integer;
begin
  Result := fList.Count;
end;

// Get a module, conveniently type cast to TModule.

function TModuleList.GetModules(Index: Integer): TModule;
begin
  Result := fList[Index];
end;

// Remove a module from the list.

procedure TModuleList.Remove(Module: TModule);
begin
  Delete(fList.IndexOf(Module));
end;

// Delete a module from an index in the list. Automatically free
// the module, which also unregisters the notifier.

procedure TModuleList.Delete(Index: Integer);
var
  Module: TModule;
begin
  Module := Modules[Index];
  fList.Delete(Index);
  Module.RemoveNotifier;
end;

// Get a list of all the files in all the modules, for debugging.

procedure TModuleList.GetFileList(List: TStrings);
var
  I: Integer;
begin
  for I := 0 to fList.Count - 1 do
    Modules[I].GetFileList(List);
end;

// Return the number of bytes occupied by a resource type or name.
// If the value is numeric, the size is 4 bytes; otherwise, it is
// the number of bytes occupied by the name, including the trailing
// zero word. The resource file uses Unicode, so the actual number
// of bytes is twice the size of the string (plus trailing #0).

function ResTypeSize(ResType: PChar): Integer;
begin
  if LongRec(ResType).Hi = 0 then
    Result := SizeOf(LongInt)
  else
    Result := SizeOf(WideChar) * (StrLen(ResType) + 2);
end;

// Add zero bytes to a stream until its size is an integral
// multiple of PadAmount.

procedure PadStream(Stream: TStream; PadAmount: Integer);
var
  Zero: Integer;
  PadSize: Integer;
begin
  Zero := 0;
  PadSize := PadAmount - Stream.Size mod PadAmount;
  if PadSize <> PadAmount then
    Stream.WriteBuffer(Zero, PadSize);
end;

// Write the resource type or name to the stream.

procedure WriteResType(Stream: TStream; ResType: PChar);
var
  I: Integer;
  C: WideChar;
  W: Word;
begin
  if LongRec(ResType).Hi = 0 then
    begin
    // The Delphi format uses a high word of 0 for a numeric resource
    // type or name, but the .RES file format uses $FFFF.
      W := $FFFF;
      Stream.WriteBuffer(W, SizeOf(W));
      W := LongRec(ResType).Lo;
      Stream.WriteBuffer(W, SizeOf(W));
    end
  else
    begin
    // Write the name, converting each ANSI character to Unicode.
    // Include the trailing #0 character, then pad to a LongWord boundary.
      I := 0;
      repeat
        C := WideChar(ResType[I]);
        Stream.WriteBuffer(C, SizeOf(C));
        Inc(I);
      until ResType[I - 1] = #0;
      PadStream(Stream, SizeOf(LongInt));
    end;
end;

procedure WriteResource(Stream: TStream; DataSize: LongInt; Data: PChar;
  ResType: PChar; ResName: PChar; DataVersion: LongInt; Flags, Language: Word;
  Version, Characteristics: LongInt);
var
  HeaderSize: LongInt;
  HeaderStream: TMemoryStream;
begin
  // Use an in-memory stream for the resource header, to buffer
  // the header and write it to the auto-save file in a single operation.
  HeaderSize := SizeOf(DataSize) + SizeOf(HeaderSize) +
    SizeOf(DataVersion) + SizeOf(Flags) + SizeOf(Language) +
    SizeOf(Version) + SizeOf(Characteristics) +
    ResTypeSize(ResType) + ResTypeSize(ResName);
  HeaderStream := TMemoryStream.Create;
  try
    HeaderStream.WriteBuffer(DataSize, SizeOf(DataSize));
    HeaderStream.WriteBuffer(HeaderSize, SizeOf(HeaderSize));
    WriteResType(HeaderStream, ResType);
    WriteResType(HeaderStream, ResName);
    HeaderStream.WriteBuffer(DataVersion, SizeOf(DataVersion));
    HeaderStream.WriteBuffer(Flags, SizeOf(Flags));
    HeaderStream.WriteBuffer(Language, SizeOf(Language));
    HeaderStream.WriteBuffer(Version, SizeOf(Version));
    HeaderStream.WriteBuffer(Characteristics, SizeOf(Characteristics));
    PadStream(HeaderStream, SizeOf(LongInt));
    Stream.CopyFrom(HeaderStream, 0);
  finally
    HeaderStream.Free;
  end;
  if DataSize > 0 then
    begin
      Stream.WriteBuffer(Data^, DataSize);
      PadStream(Stream, SizeOf(LongInt));
    end;
end;

{ TResourceFile }

procedure TResourceFile.doAutoSave;
var
  ResourceFile: IOTAProjectResource;
  Resource: IOTAResourceEntry;
  Stream: TAutoSaveStream;
  I: Integer;
  Characteristics, DataVersion, Flags, Language, Version: LongInt;
begin
  ResourceFile := Editor as IOTAProjectResource;
  Stream := TAutoSaveStream.Create(Editor.FileName);
  try
    // Every .RES file starts with an empty resource entry.
    WriteResource(Stream, 0, nil, nil, nil, 0, 0, 0, 0, 0);
    for I := 0 to ResourceFile.GetEntryCount - 1 do
      begin
        Resource := ResourceFile.GetEntry(I);
        Resource.GetHeaderValue(hvFlags, Flags);
        Resource.GetHeaderValue(hvLanguage, Language);
        Resource.GetHeaderValue(hvCharacteristics, Characteristics);
        Resource.GetHeaderValue(hvDataVersion, DataVersion);
        Resource.GetHeaderValue(hvVersion, Version);
        WriteResource(Stream, Resource.DataSize, Resource.GetData,
          Resource.GetResourceType, Resource.GetResourceName,
          DataVersion, Flags, Language, Version, Characteristics);
      end;
    inherited;
  finally
    Stream.Free;
  end;
end;

{ TIdeFile }

procedure TIdeFile.AutoSave;
begin
  try
    doAutoSave;
    if Assigned(OnAutoSave) then
      OnAutoSave(fFileName);
  except
    on e: exception do
      if Assigned(OnError) then
        OnError(e.Message);
  end;
end;

procedure TIdeFile.doAutoSave;
begin
// does nothing
end;

end.


