{.GXFormatter.config=twm}
///<summary> declares TdzDllLoader class to make handling of DLLs easier </summary>
unit u_dzDllLoader;

interface

uses
  Windows,
  SysUtils,
  u_dzVersionInfo;

type
  ///<summary> parent exception class for all exceptions raised in u_dzDllLoader </summary>
  EdzDllLoader = class(Exception);
  ///<summary> raised if LoadLibrary fails </summary>
  EDllLoadError = class(EdzDllLoader);
  ///<summary> raised if GetProcAddress fails </summary>
  ENoEntryPoint = class(EdzDllLoader);

type
  ///<summary> wrapper for the Load/FreeLibrary and GetProcAddress API calls </summary>
  TdzDllLoader = class(TInterfacedObject)
  protected
    ///<summary> module handle of dll as returned by LoadLibrary </summary>
    FDllHandle: THandle;
    ///<summary> Version info of dll </summary>
    FDllVersion: IFileInfo;

    ///<summary> name of wrapped dll </summary>
    FDllName: string;
    ///<summary> calls GetProcAddress and raises ENoEntryPoint if it returns nil
    ///          @param EntryPoint is the name of the entry point to get
    ///          @param DefaultFunc is a function pointer to assign if the entry point cannot be found
    ///                             if it is nil, an ENoEntryPoint exception will be raise in that case.
    ///                             Note: This function pointer must match the calling convention of
    ///                             the entry point and unless the calling convention is cdecl
    ///                             it must also match number of parameters of the entry point.
    ///                             See also the NotSupportedN functions in this unit.
    ///          @returns a pointer to the entry pointer
    ///          @raises ENoEntryPoint on failure </summary>
    function TryGetProcAddress(const _EntryPoint: string; _DefaultFunc: pointer = nil): pointer; overload;
    ///<summary> calls GetProcAddress for MSC mangled entry points and raises ENoEntryPoint if it returns nil
    ///          @param EntryPoint is the name of the entry point to get
    ///          @param DWordParams is the number of DWord parameters of the entry point, used to
    ///                             generate the actual name of the entry point
    ///          @param DefaultFunc is a function pointer to assign if the entry point cannot be found
    ///                             if it is nil, an ENoEntryPoint exception will be raise in that case.
    ///                             Note: This function pointer must match the calling convention of
    ///                             the entry point and unless the calling convention is cdecl
    ///                             it must also match number of parameters of the entry point.
    ///                             See also the NotSupportedN functions in u_dzDllLoader.
    ///          @returns a pointer to the entry pointer
    ///          @raises ENoEntryPoint on failure </summary>
    function TryGetProcAddress(const _EntryPoint: string; _DWordParams: integer; _DefaultFunc: pointer = nil): pointer; overload;
    procedure LoadDll; virtual;
    procedure UnloadDll; virtual;
    ///<summary> Called by the constructor after the dll has been loaded. Override
    ///          it to initialize any entry points </summary>
    procedure InitEntryPoints; virtual; abstract;
  public
    ///<summary> tries to load the given dll and raises EDllLoadError if it fails
    ///          @param DllName is hte name of the dll to load, can contain absolute path
    ///          @raises EDllLoadError on failure </summary>
    constructor Create(const _DllName: string); overload;
    ///<summary> assumes that the dll has already been loaded and uses the given DllHandle,
    ///          NOTE: The destructor will call FreeLibrary anyway, so make sure you don't
    ///                store the dll handle anywhere else! </summary>
    constructor Create(const _DllName: string; _DllHandle: THandle); overload;
    ///<summary> calls FreeLibrary </summary>
    destructor Destroy; override;
    ///<summary> Generates a TVersionInfo object on demand and returns it </summary>
    function DllVersion: IFileInfo;
    ///<summary> returns the full path of the dll that has been loaded </summary>
    function DllFilename: string;
    ///<summary> returns the dll name as passed to the constructor </summary>
    property DllName: string read FDllName;
  end;

///<summary> dummy implementation for unsupported functions with 1 long parameter </summary>
function NotSupported1(one: integer): integer; stdcall;

///<summary> dummy implementation for unsupported functions with 2 long parameters </summary>
function NotSupported2(one, two: integer): integer; stdcall;

///<summary> dummy implementation for unsupported functions with 3 long parameters </summary>
function NotSupported3(one, two, three: integer): integer; stdcall;

///<summary> dummy implementation for unsupported functions with 4 long parameters </summary>
function NotSupported4(one, two, three, four: integer): integer; stdcall;

///<summary> dummy implementation for unsupported functions with 5 long parameters </summary>
function NotSupported5(one, two, three, four, five: integer): integer; stdcall;

///<summary> dummy implementation for unsupported functions with 6 long parameters </summary>
function NotSupported6(one, two, three, four, five, six: integer): integer; stdcall;

///<summary> since for cdecl calling convention, the caller cleans up the stack, we only
///          need one dummy function </summary>
function CdeclNotSupported(): integer; cdecl;

///<summary> since for cdecl calling convention, the caller cleans up the stack, we only
///          need one dummy function </summary>
function CdeclReturn0: integer; cdecl;

implementation

uses
  u_dzMiscUtils,
  u_dzOsUtils,
  u_dzTranslator;

function _(const _s: string): string; inline;
begin
  Result := DGetText(_s, 'dzlib');
end;

{ TdzDllLoader }

constructor TdzDllLoader.Create(const _DllName: string);
begin
  inherited Create;
  FDllName := _DllName;
  LoadDll;
  InitEntryPoints;
end;

constructor TdzDllLoader.Create(const _DllName: string; _DllHandle: THandle);
begin
  inherited Create;
  FDllName := _DllName;
  FDllHandle := _DllHandle;
  InitEntryPoints;
end;

destructor TdzDllLoader.Destroy;
begin
  UnloadDll;
  inherited;
end;

function TdzDllLoader.DllFilename: string;
begin
  Result := GetModuleFilename(FDllHandle);
end;

function TdzDllLoader.DllVersion: IFileInfo;
begin
  if not Assigned(FDllVersion) then begin
    FDllVersion := TFileInfo.Create(FDllName);
    FDllVersion.AllowExceptions := false;
  end;
  Result := FDllVersion;
end;

procedure TdzDllLoader.LoadDll;
begin
  FDllHandle := LoadLibrary(PChar(FDllName));
  if FDllHandle = 0 then
    raise EDllLoadError.CreateFmt(_('Could not load %s.'), [FDllName]);
end;

function TdzDllLoader.TryGetProcAddress(const _EntryPoint: string; _DWordParams: integer; _DefaultFunc: pointer): pointer;
var
  EntryPoint: string;
begin
  EntryPoint := '_' + _EntryPoint + '@' + IntToStr(_DWordParams);
  Result := TryGetProcAddress(EntryPoint, _DefaultFunc);
end;

function TdzDllLoader.TryGetProcAddress(const _EntryPoint: string; _DefaultFunc: pointer = nil): pointer;
var
  ErrCode: integer;
begin
  Result := GetProcAddress(FDllHandle, PChar(_EntryPoint));
  if not Assigned(Result) then begin
    if Assigned(_DefaultFunc) then
      Result := _DefaultFunc
    else begin
      ErrCode := GetLastError;
      RaiseLastOsErrorEx(ErrCode, Format(_('Could not find entry point %s in %s'#13#10'ERROR= %%d, %%s'), [_EntryPoint, FDllName]));
    end;
  end;
end;

procedure TdzDllLoader.UnloadDll;
begin
  if FDllHandle <> 0 then
    FreeLibrary(FDllHandle);
  FDllHandle := 0;
end;

function NotSupported1(one: integer): integer; stdcall;
begin
  raise ENoEntryPoint.Create(_('DLL entry point not initialized'));
end;

function NotSupported2(one, two: integer): integer; stdcall;
begin
  raise ENoEntryPoint.Create(_('DLL entry point not initialized'));
end;

function NotSupported3(one, two, three: integer): integer; stdcall;
begin
  raise ENoEntryPoint.Create(_('DLL entry point not initialized'));
end;

function NotSupported4(one, two, three, four: integer): integer; stdcall;
begin
  raise ENoEntryPoint.Create(_('DLL entry point not initialized'));
end;

function NotSupported5(one, two, three, four, five: integer): integer; stdcall;
begin
  raise ENoEntryPoint.Create(_('DLL entry point not initialized'));
end;

function NotSupported6(one, two, three, four, five, six: integer): integer; stdcall;
begin
  raise ENoEntryPoint.Create(_('DLL entry point not initialized'));
end;

function CdeclNotSupported(): integer; cdecl;
begin
  raise ENoEntryPoint.Create(_('DLL entry point not initialized'));
end;

function CdeclReturn0: integer; cdecl;
begin
  Result := 0;
end;

end.

