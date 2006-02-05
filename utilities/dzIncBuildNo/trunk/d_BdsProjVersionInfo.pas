unit d_BdsProjVersionInfo;

interface

uses
  Windows,
  SysUtils,
  Classes,
  xmldom,
  XMLIntf,
  msxmldom,
  XMLDoc,
  i_VersionInfo;

type
  Tdm_BdsProjVersionInfo = class(TDataModule, IVersionInfo)
    ProjDoc: TXMLDocument;
  private
    function GetChildNodeContent(_Parent: IXMLNode; const _NodeName, _AttrName: string): string;
    function GetVersionInfo(const _Name: string): string;
    function GetVersionInfoKey(const _Name: string): string;
    procedure SetChildNodeContent(_Parent: IXMLNode; const _NodeName, _AttrName, _Value: string);
    procedure SetVersionInfo(const _Name, _Value: string);
    procedure SetVersionInfoKey(const _Name, _Value: string);
  protected // IInterface
    FRefCount: integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    function _AddRef: integer; stdcall;
    function _Release: integer; stdcall;
  protected
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
    procedure Assign(const _VersionInfo: IVersionInfo); reintroduce;
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
  protected
    FBdsProjFile: string;
    FVersionInfo: IXMLNode;
    FVersionInfoKeys: IXMLNode;
  public
    constructor Create(const _Project: string); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ Tdm_BdsProjVersionInfo }

procedure Tdm_BdsProjVersionInfo.Assign(const _VersionInfo: IVersionInfo);
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

function Tdm_BdsProjVersionInfo.GetChildNodeContent(_Parent: IXMLNode; const _NodeName, _AttrName: string): string;
var
  Node: IXMLNode;
begin
  // <VersionInfo Name="IncludeVerInfo">True</VersionInfo>
  Node := _Parent.ChildNodes.First;
  while Assigned(Node) do begin
    if Node.nodeName = _NodeName then begin
      if SameText(Node.Attributes['Name'], _AttrName) then begin
        Result := Node.Text;
        exit;
      end;
    end;
    Node := Node.nextSibling;
  end;
end;

function Tdm_BdsProjVersionInfo.GetVersionInfo(const _Name: string): string;
begin
  Result := GetChildNodeContent(FVersionInfo, 'VersionInfo', _Name);
end;

function Tdm_BdsProjVersionInfo.GetVersionInfoKey(const _Name: string): string;
begin
  Result := GetChildNodeContent(FVersionInfoKeys, 'VersionInfoKeys', _Name);
end;

procedure Tdm_BdsProjVersionInfo.SetChildNodeContent(_Parent: IXMLNode; const _NodeName, _AttrName, _Value: string);
var
  Node: IXMLNode;
begin
  // <*NodeName* Name="*AttrName*">*Value*</VersionInfo>
  Node := _Parent.ChildNodes.First;
  while Assigned(Node) do begin
    if Node.nodeName = _NodeName then begin
      if SameText(Node.Attributes['Name'], _AttrName) then begin
        Node.Text := _Value;
        exit;
      end;
    end;
    Node := Node.nextSibling;
  end;
end;

constructor Tdm_BdsProjVersionInfo.Create(const _Project: string);
var
  BorlandProject: IXMLNode;
  DelphiPersonality: IXMLNode;
begin
  inherited Create(nil);
  FBdsProjFile := _Project + '.bdsproj';
  ProjDoc.FileName := FBdsProjFile;
  ProjDoc.Active := True;
  BorlandProject := ProjDoc.DocumentElement;
  DelphiPersonality := BorlandProject.childNodes['Delphi.Personality'];
  FVersionInfo := DelphiPersonality.childNodes['VersionInfo'];
  FVersionInfoKeys := DelphiPersonality.childNodes['VersionInfoKeys'];

  if not SameText(GetChildNodeContent(FVersionInfo, 'VersionInfo', 'IncludeVerInfo'), 'True') then
    raise ENoVersionInfo.Create('.bdsproj file does not contain version information');
end;

destructor Tdm_BdsProjVersionInfo.Destroy;
begin
  FVersionInfo := nil;
  inherited;
end;

function Tdm_BdsProjVersionInfo.GetAutoIncBuild: boolean;
begin
  Result := SameText(GetVersionInfo('AutoIncBuild'), 'True');
end;

function Tdm_BdsProjVersionInfo.GetBuild: integer;
begin
  Result := StrToIntDef(GetVersionInfo('Build'), 0);
end;

function Tdm_BdsProjVersionInfo.GetComments: string;
begin
  Result := GetVersionInfoKey('Comments');
end;

function Tdm_BdsProjVersionInfo.GetCompanyName: string;
begin
  Result := GetVersionInfoKey('CompanyName');
end;

function Tdm_BdsProjVersionInfo.GetFileDescription: string;
begin
  Result := GetVersionInfoKey('FileDescription');
end;

function Tdm_BdsProjVersionInfo.GetFileVersion: string;
begin
  Result := GetVersionInfoKey('FileVersion');
end;

function Tdm_BdsProjVersionInfo.GetInternalName: string;
begin
  Result := GetVersionInfoKey('InternalName');
end;

function Tdm_BdsProjVersionInfo.GetLegalCopyright: string;
begin
  Result := GetVersionInfoKey('LegalCopyright');
end;

function Tdm_BdsProjVersionInfo.GetLegalTrademarks: string;
begin
  Result := GetVersionInfoKey('LegalTrademarks');
end;

function Tdm_BdsProjVersionInfo.GetMajorVer: integer;
begin
  Result := StrToIntDef(GetVersionInfo('MajorVer'), 0);
end;

function Tdm_BdsProjVersionInfo.GetMinorVer: integer;
begin
  Result := StrToIntDef(GetVersionInfo('MinorVer'), 0);
end;

function Tdm_BdsProjVersionInfo.GetOriginalFilename: string;
begin
  Result := GetVersionInfoKey('OriginalFilename');
end;

function Tdm_BdsProjVersionInfo.GetProductName: string;
begin
  Result := GetVersionInfoKey('ProductName');
end;

function Tdm_BdsProjVersionInfo.GetProductVersion: string;
begin
  Result := GetVersionInfoKey('ProductVersion');
end;

function Tdm_BdsProjVersionInfo.GetRelease: integer;
begin
  Result := StrToIntDef(GetVersionInfo('Release'), 0);
end;

procedure Tdm_BdsProjVersionInfo.SetAutoIncBuild(_AutoIncBuild: boolean);
begin
  if _AutoIncBuild then
    SetVersionInfo('AutoIncBuild', 'True')
  else
    SetVersionInfo('AutoIncBuild', 'Frue')
end;

procedure Tdm_BdsProjVersionInfo.SetBuild(_Build: integer);
begin
  SetVersionInfo('Build', IntToStr(_Build));
end;

procedure Tdm_BdsProjVersionInfo.SetComments(const _Comments: string);
begin
  SetVersionInfoKey('Comments', _Comments);
end;

procedure Tdm_BdsProjVersionInfo.SetCompanyName(_CompanyName: string);
begin
  SetVersionInfoKey('CompanyName', _CompanyName);
end;

procedure Tdm_BdsProjVersionInfo.SetFileDescription(_FileDescription: string);
begin
  SetVersionInfoKey('FileDescription', _FileDescription);
end;

procedure Tdm_BdsProjVersionInfo.SetFileVersion(_FileVersion: string);
begin
  SetVersionInfoKey('FileVersion', _FileVersion);
end;

procedure Tdm_BdsProjVersionInfo.SetInternalName(_InternalName: string);
begin
  SetVersionInfoKey('InternalName', _InternalName);
end;

procedure Tdm_BdsProjVersionInfo.SetLegalCopyright(_LegalCopyright: string);
begin
  SetVersionInfoKey('LegalCopyright', _LegalCopyright);
end;

procedure Tdm_BdsProjVersionInfo.SetLegalTrademarks(_LegalTrademarks: string);
begin
  SetVersionInfoKey('LegalTrademarks', _LegalTrademarks);
end;

procedure Tdm_BdsProjVersionInfo.SetMajorVer(_MajorVer: integer);
begin
  SetVersionInfo('MajorVer', IntToStr(_MajorVer));
end;

procedure Tdm_BdsProjVersionInfo.SetMinorVer(_MinorVer: integer);
begin
  SetVersionInfo('MinorVer', IntToStr(_MinorVer));
end;

procedure Tdm_BdsProjVersionInfo.SetOriginalFilename(_OriginalFilename: string);
begin
  SetVersionInfoKey('OriginalFilename', _OriginalFilename);
end;

procedure Tdm_BdsProjVersionInfo.SetProductName(_ProductName: string);
begin
  SetVersionInfoKey('ProductName', _ProductName);
end;

procedure Tdm_BdsProjVersionInfo.SetProductVersion(_ProductVersion: string);
begin
  SetVersionInfoKey('ProductVersion', _ProductVersion);
end;

procedure Tdm_BdsProjVersionInfo.SetRelease(_Release: integer);
begin
  SetVersionInfo('Release', IntToStr(_Release));
end;

procedure Tdm_BdsProjVersionInfo.SetVersionInfo(const _Name, _Value: string);
begin
  SetChildNodeContent(FVersionInfo, 'VersionInfo', _Name, _Value);
end;

procedure Tdm_BdsProjVersionInfo.SetVersionInfoKey(const _Name, _Value: string);
begin
  SetChildNodeContent(FVersionInfoKeys, 'VersionInfoKeys', _Name, _Value);
end;

procedure Tdm_BdsProjVersionInfo.UpdateFile;
begin
  ProjDoc.SaveToFile(FBdsProjFile);
end;

// standard TInterfacedObject implementation of IInterface

function Tdm_BdsProjVersionInfo.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE
end;

function Tdm_BdsProjVersionInfo._AddRef: integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function Tdm_BdsProjVersionInfo._Release: integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

end.

