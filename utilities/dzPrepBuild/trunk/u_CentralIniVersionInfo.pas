unit u_CentralIniVersionInfo;

interface

uses
  i_VersionInfo,
  u_IniVersionInfo;

type
  {: This is a specialized descendant of TIniVersionInfo which
     allows any entry of the file to redirect to a different file.
     This can be done for single entries or for a whole section.
     For redirecting a single entry, the value must contain a string
     REDIRECT:<filename>,<section><entry>
     For redirecting a section it must contain only one entry
     redirect=<filename>,<section>
     These redirections will be used for reading and writing, that is it can be
     used to maintain/increment a central build number for several branches of
     a project where the files have different version numbers but the build
     number should be increased for a build of any of these versions. }
  TCentralVersionInfo = class(TIniVersionInfo, IVersionInfo)
  private
  protected
    function ReadString(const _Section, _Ident: string; _Default: string): string; override;
    procedure WriteString(const _Section, _Ident: string; _Value: string); override;
  public
    {: Creates a TCentralVersionInfo instance
       @param ProjectName is the name of the project (.dpr file without extension)
                          The constructor appends "_Version.ini" to this
                          name and tries to open the file }
    constructor Create(const _ProjectName: string);
  end;

implementation

uses
  IniFiles;

{ TCentralVersionInfo }

constructor TCentralVersionInfo.Create(const _ProjectName: string);
begin
  inherited Create(_ProjectName + '_Version.ini');
end;

function TCentralVersionInfo.ReadString(const _Section, _Ident: string; _Default: string): string;
var
  Redir: string;
  IniFile: TMemIniFile;
begin
  Redir := FIniFile.ReadString(_Section, 'redirect', '');
  if Redir = '' then
    IniFile := FIniFile
  else begin

  end;
end;

procedure TCentralVersionInfo.WriteString(const _Section, _Ident: string; _Value: string);
begin

end;

end.

