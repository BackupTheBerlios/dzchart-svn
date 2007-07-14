unit u_DummyVersionInfo;

interface

uses
  i_VersionInfo,
  u_AbstractVersionInfo;

type
  TDummyVersionInfo = class(TAbstractVersionInfo, IVersionInfo)
  protected
    procedure UpdateFile; override;
    function VerInfoFilename: string; override;
  end;

implementation

{ TDummyVersionInfo }

procedure TDummyVersionInfo.UpdateFile;
begin
  // ignore
end;

function TDummyVersionInfo.VerInfoFilename: string;
begin
  Result := '';
end;

end.

