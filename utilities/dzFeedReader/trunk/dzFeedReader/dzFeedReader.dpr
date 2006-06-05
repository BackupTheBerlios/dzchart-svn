program dzFeedReader;

uses
  Forms,
  w_MainForm in 'w_MainForm.pas' {f_MainForm},
  wf_TileFrame in 'wf_TileFrame.pas' {fr_TileFrame: TFrame},
  u_FeedDesc in 'u_FeedDesc.pas',
  w_FeedEditForm in 'w_FeedEditForm.pas' {f_FeedEditForm},
  u_FeedFrameList in 'u_FeedFrameList.pas',
  u_EventThread in 'u_EventThread.pas',
  wf_RssFrame in 'wf_RssFrame.pas' {fr_RssFrame: TFrame},
  w_HintWindow in 'w_HintWindow.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_MainForm, f_MainForm);
  Application.Run;
end.
