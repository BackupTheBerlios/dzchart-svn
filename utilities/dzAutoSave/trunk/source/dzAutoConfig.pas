unit dzAutoConfig;

{$I delphiversions.inc}

{ Properties for the auto-save expert. }

interface

uses
  Windows,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  ExtCtrls,
  StdCtrls,
  ComCtrls;

type
  TAutoSaveForm = class(TForm)
    b_Cancel: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label2: TLabel;
    ud_Interval: TUpDown;
    ed_Interval: TEdit;
    chk_SaveTimed: TCheckBox;
    b_Ok: TButton;
    Icon: TImage;
    Label3: TLabel;
    Bevel1: TBevel;
    Memo1: TMemo;
    TabSheet3: TTabSheet;
    FileList: TListBox;
    chk_SaveOnCompile: TCheckBox;
    ud_ToolPosition: TUpDown;
    ed_ToolPosition: TEdit;
    Label1: TLabel;
    chk_ShowMessage: TCheckBox;
    l_Version: TLabel;
    procedure FileListDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    function GetSaveInterval: integer;
    function GetSaveOnCompile: boolean;
    function GetSaveTimed: boolean;
    procedure SetSaveInterval(const _SaveInterval: integer);
    procedure SetSaveOncompile(const _SaveOnCompile: boolean);
    procedure SetSaveTimed(const _SaveTimed: boolean);
    function GetToolPosition: integer;
    procedure SetToolPosition(const _ToolPosition: integer);
    function GetShowMessageView: boolean;
    procedure SetShowMessageView(const _ShowMessageView: boolean);
  public
    property SaveTimed: boolean read GetSaveTimed write SetSaveTimed;
    property SaveOnCompile: boolean read GetSaveOnCompile write SetSaveOncompile;
    property SaveInterval: integer read GetSaveInterval write SetSaveInterval;
    property ToolPosition: integer read GetToolPosition write SetToolPosition;
    property ShowMessageView: boolean read GetShowMessageView write SetShowMessageView;
  end;

implementation

uses
  ToolsAPI;

{$R *.DFM}

function GetModuleFilename: string;
var
  Buffer: array[0..260] of Char;
begin
  SetString(Result, Buffer, Windows.GetModuleFileName(HInstance, Buffer, SizeOf(Buffer)))
end;

function GetFileBuildInfo: string;
var
  VerInfoSize: DWORD;
  VerInfo: Pointer;
  VerValueSize: DWORD;
  VerValue: PVSFixedFileInfo;
  Dummy: DWORD;
  Major, Minor, Revision, Build: integer;
  Filename: string;
begin
  Filename := GetModuleFileName;
  VerInfoSize := GetFileVersionInfoSize(PChar(Filename), Dummy);
  Result := '';
  if (VerInfoSize <> 0) then
    begin
      GetMem(VerInfo, VerInfoSize);
      try
        GetFileVersionInfo(PChar(Filename), 0, VerInfoSize, VerInfo);
        VerQueryValue(VerInfo, '\', Pointer(VerValue), VerValueSize);
        with VerValue^ do
          begin
            Major := dwFileVersionMS shr 16;
            Minor := dwFileVersionMS and $FFFF;
            Revision := dwFileVersionLS shr 16;
            Build := dwFileVersionLS and $FFFF;
            Result := Format('version %d.%.2d.%.3d build %d', [Major, Minor, Revision, Build]);
          end;
      finally
        FreeMem(VerInfo, VerInfoSize);
      end;
    end;
end;

procedure TAutoSaveForm.FileListDblClick(Sender: TObject);
begin
  IOTAEditor(Pointer(FileList.Items.Objects[FileList.ItemIndex])).Show;
end;

function TAutoSaveForm.GetSaveInterval: integer;
begin
  result := ud_Interval.Position;
end;

procedure TAutoSaveForm.SetSaveInterval(const _SaveInterval: integer);
begin
  ud_Interval.Position := _SaveInterval;
end;

function TAutoSaveForm.GetSaveOnCompile: boolean;
begin
  Result := chk_SaveOnCompile.Checked;
end;

procedure TAutoSaveForm.SetSaveOncompile(const _SaveOnCompile: boolean);
begin
  chk_SaveOnCompile.Checked := _SaveOnCompile;
end;

function TAutoSaveForm.GetSaveTimed: boolean;
begin
  Result := chk_SaveTimed.Checked;
end;

procedure TAutoSaveForm.SetSaveTimed(const _SaveTimed: boolean);
begin
  chk_SaveTimed.Checked := _SaveTimed;
end;

function TAutoSaveForm.GetToolPosition: integer;
begin
  Result := ud_ToolPosition.Position - 1;
end;

procedure TAutoSaveForm.SetToolPosition(const _ToolPosition: integer);
begin
  ud_ToolPosition.Position := _ToolPosition + 1;
end;

function TAutoSaveForm.GetShowMessageView: boolean;
begin
  Result := chk_ShowMessage.Checked;
end;

procedure TAutoSaveForm.SetShowMessageView(const _ShowMessageView: boolean);
begin
  chk_ShowMessage.Checked := _ShowMessageView;
end;

procedure TAutoSaveForm.FormCreate(Sender: TObject);
begin
  l_Version.Caption := GetFileBuildInfo;
{$ifdef delphi6up}
  chk_ShowMessage.Visible := true
{$endif}
end;

end.

