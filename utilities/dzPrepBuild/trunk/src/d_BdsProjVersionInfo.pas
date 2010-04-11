unit d_BdsProjVersionInfo;

interface

uses
  Windows,
  SysUtils,
  Classes,
  d_XmlVersionInfo,
  xmldom,
  XMLIntf,
  msxmldom,
  XMLDoc;

type
  Tdm_BdsProjVersionInfo = class(Tdm_XmlVersionInfo)
  private
  protected
    procedure InitVersionNodes; override;
  public
    class function FilenameFor(const _Project: string): string; override;
  end;

implementation

{$R *.dfm}

{ Tdm_BdsProjVersionInfo }

class function Tdm_BdsProjVersionInfo.FilenameFor(const _Project: string): string;
begin
  Result := ChangeFileExt(_Project, '.bdsproj');
end;

procedure Tdm_BdsProjVersionInfo.InitVersionNodes;
var
  BorlandProject: IXMLNode;
  DelphiPersonality: IXMLNode;
begin
  BorlandProject := ProjDoc.DocumentElement;
  DelphiPersonality := BorlandProject.childNodes['Delphi.Personality'];
  FVersionInfo := DelphiPersonality.childNodes['VersionInfo'];
  FVersionInfoKeys := DelphiPersonality.childNodes['VersionInfoKeys'];
end;

end.

