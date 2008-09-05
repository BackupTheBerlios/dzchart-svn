unit wf_dzWizardFrame;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs;

type
  TPrevNext = (pnPrevious, pnNext);

  Tfr_dzWizardFrame = class;

  TBeforePageChange = procedure(_Sender: TObject; _Direction: TPrevNext;
    _OldPageId: integer; var _NewPageId: integer;
    _OldPageData, _NewPageData: Pointer;
    var _CanChange: boolean) of object;

  TAfterPageChange = procedure(_Sender: TObject; _Direction: TPrevNext;
    _OldPageId, _NewPageId: integer;
    _OldPageData, _NewPageData: Pointer) of object;

  Tfr_dzWizardFrame = class(TFrame)
  private
    FPageId: integer;
    FNextPageId: integer;
    FDescription: string;
    FData: pointer;
    FBeforeExit: TBeforePageChange;
    FAfterEnter: TAfterPageChange;
  protected
    function GetNextPageId: integer; virtual;
    procedure SetNextPageId(const _Value: integer); virtual;
  public
    constructor Create(_Owner: TComponent); override;
    procedure PageActivate; virtual;
    procedure PageDeactivate; virtual;
    function Exiting(_Direction: TPrevNext; var _NewPageId: integer; _NewPage: Tfr_dzWizardFrame): boolean; virtual;
    procedure Entered(_Direction: TPrevNext; _OldPageId: integer; _OldPage: Tfr_dzWizardFrame); virtual;
    property PageId: integer read FPageId write FPageId;
    property NextPageId: integer read GetNextPageId write SetNextPageId;
    property Description: string read FDescription write FDescription;
    property Data: pointer read FData write FData;
    property BeforeExit: TBeforePageChange read FBeforeExit write FBeforeExit;
    property AfterEnter: TAfterPageChange read FAfterEnter write FAfterEnter;
  end;

implementation

{$R *.dfm}

{ Tfr_dzWizardFrame }

constructor Tfr_dzWizardFrame.Create(_Owner: TComponent);
begin
  inherited;
  FPageId := -1;
  FNextPageId := -1;
end;

procedure Tfr_dzWizardFrame.Entered(_Direction: TPrevNext; _OldPageId: integer; _OldPage: Tfr_dzWizardFrame);
begin
  if Assigned(FAfterEnter) then
    FAfterEnter(Self, _Direction, _OldPageId, PageId, _OldPage, Self);
end;

function Tfr_dzWizardFrame.Exiting(_Direction: TPrevNext;
  var _NewPageId: integer; _NewPage: Tfr_dzWizardFrame): boolean;
begin
  Result := true;
  if Assigned(FBeforeExit) then
    FBeforeExit(Self, _Direction, PageId, _NewPageId, Self, _NewPage, Result);
end;

function Tfr_dzWizardFrame.GetNextPageId: integer;
begin
  Result := FNextPageId;
end;

procedure Tfr_dzWizardFrame.PageActivate;
begin
  Align := alClient;
  Visible := true;
end;

procedure Tfr_dzWizardFrame.PageDeactivate;
begin
  Visible := false;
end;

procedure Tfr_dzWizardFrame.SetNextPageId(const _Value: integer);
begin
  FNextPageId := _Value;
end;

end.

