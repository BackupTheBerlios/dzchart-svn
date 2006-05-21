unit w_FeedEditForm;

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
  Dialogs,
  StdCtrls,
  u_FeedDesc;

type
  Tf_FeedEditForm = class(TForm)
    b_OK: TButton;
    b_Cancel: TButton;
    l_Name: TLabel;
    ed_FeedName: TEdit;
    l_FeedUrl: TLabel;
    ed_FeedUrl: TEdit;
    procedure b_OKClick(Sender: TObject);
  private
    FFeed: TFeedDesc;
    procedure SetFeed(const _Feed: TFeedDesc);
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
    property Feed: TFeedDesc read FFeed write SetFeed;
  end;

implementation

{$R *.dfm}

{ Tf_FeedEditForm }

procedure Tf_FeedEditForm.b_OKClick(Sender: TObject);
begin
  FFeed.FeedName := ed_FeedName.Text;
  FFeed.FeedUrl := ed_FeedUrl.Text;
end;

constructor Tf_FeedEditForm.Create(_Owner: TComponent);
begin
  inherited;
  FFeed := TFeedDesc.Create;
end;

destructor Tf_FeedEditForm.Destroy;
begin
  FFeed.Free;
  inherited;
end;

procedure Tf_FeedEditForm.SetFeed(const _Feed: TFeedDesc);
begin
  FFeed.Assign(_Feed);
  ed_FeedName.Text := FFeed.FeedName;
  ed_FeedUrl.Text := FFeed.FeedUrl;
end;

end.

