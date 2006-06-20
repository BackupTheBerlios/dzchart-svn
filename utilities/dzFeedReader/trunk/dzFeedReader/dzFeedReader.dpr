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
  w_HintWindow in 'w_HintWindow.pas',
  gtcXdom31DomUnit in '..\..\gtcXDOM-3.1-DOM\gtcXdom31DomUnit.pas',
  Xdom_3_2 in '..\..\openxml\Xdom_3_2\Xdom_3_2.pas',
  XmlRulesUtils in '..\..\openxml\Utilities\XmlRulesUtils.pas',
  AbnfUtils in '..\..\openxml\Utilities\AbnfUtils.pas',
  AutoListUtils in '..\..\openxml\Utilities\AutoListUtils.pas',
  AutoStrCtrls in '..\..\openxml\Utilities\AutoStrCtrls.pas',
  cUnicodeCodecs in '..\..\openxml\Utilities\cUnicodeCodecs.pas',
  LangUtils in '..\..\openxml\Utilities\LangUtils.pas',
  MiscUtils in '..\..\openxml\Utilities\MiscUtils.pas',
  ParserUtils in '..\..\openxml\Utilities\ParserUtils.pas',
  TreeUtils in '..\..\openxml\Utilities\TreeUtils.pas',
  UnicodeUtils in '..\..\openxml\Utilities\UnicodeUtils.pas',
  UriUtils in '..\..\openxml\Utilities\UriUtils.pas',
  WideStringUtils in '..\..\openxml\Utilities\WideStringUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_MainForm, f_MainForm);
  Application.Run;
end.
