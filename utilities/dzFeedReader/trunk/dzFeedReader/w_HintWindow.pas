unit w_HintWindow;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  Forms;

type
  TMyHintWindow = class(THintWindow)
  protected
    l_Description: TLabel;
    p_Title: TPanel;
    l_Title: TLabel;
    p_Link: TPanel;
    l_Link: TLabel;
  public
    constructor Create(_Owner: TComponent); override;
    procedure ShowHint(_Pos: TPoint; const _Title, _Description, _Link: string);
    procedure HideHint;
  end;

implementation

uses
  Types,
  Graphics;

{ TMyHintWindow }

constructor TMyHintWindow.Create(_Owner: TComponent);
begin
  inherited;
  Width := 305;
  Height := 129;
  BevelOuter := bvNone;
  Color := clInfoBk;
  Ctl3D := True;
  ParentCtl3D := False;

  p_Title := TPanel.Create(Self);
  p_Title.Name := 'p_Title';
  p_Title.Parent := self;
  p_Title.Height := 17;
  p_Title.Align := alTop;
  p_Title.BevelOuter := bvNone;
  p_Title.Color := clActiveCaption;
  p_Title.TabOrder := 0;
  p_Title.Caption := '';

  l_Title := TLabel.Create(Self);
  l_Title.Name := 'l_Title';
  l_Title.Parent := p_Title;
  l_Title.Left := 0;
  l_Title.Top := 2;
  l_Title.Width := 100;
  l_Title.Caption := 'DescriptionGoesHere';
  l_Title.Font.Color := clCaptionText;

  l_Description := TLabel.Create(Self);
  l_Description.Top := p_Title.Height + 1;
  l_Description.Name := 'l_Description';
  l_Description.Parent := self;
  l_Description.Left := 0;
  l_Description.Top := 21;
  l_Description.Width := 85;
  l_Description.Height := 26;
  l_Description.Caption := 'the content goes here';
  l_Description.WordWrap := True;

  p_Link := TPanel.Create(Self);
  p_Link.Name := 'p_Link';
  p_Link.Parent := self;
  p_Link.Height := 17;
  p_Link.Align := alBottom;
  p_Link.BevelOuter := bvNone;
  p_Link.TabOrder := 1;
  p_Link.Caption := '';

  l_Link := TLabel.Create(Self);
  l_Link.Name := 'l_Link';
  l_Link.Parent := p_Link;
  l_Link.Left := 0;
  l_Link.Top := 2;
  l_Link.Width := 65;
  l_Link.Height := 13;
  l_Link.Caption := 'LinkGoesHere';
end;

procedure TMyHintWindow.HideHint;
begin
  ShowWindow(Handle, SW_HIDE);
end;

procedure TMyHintWindow.ShowHint(_Pos: TPoint; const _Title, _Description, _Link: string);
const
  MIN_WIDTH = 100;
var
  w: integer;
  h: integer;
  Size: integer;
begin
  l_Title.Caption := _Title;
  l_Description.Caption := _Description;
  l_Link.Caption := _Link;
  w := MIN_WIDTH;
  Size := l_Title.Canvas.TextWidth(_Title);
  if Size > w then
    w := Size;
  Size := l_Link.Canvas.TextWidth(_Link);
  if Size > w then
    w := Size;
  ClientWidth := w;
  l_Description.Width := w;
  h := l_Description.Top + l_Description.Height + p_Link.Height + (l_Description.Top - p_Title.Height);
  ClientHeight := h;
  Left := _Pos.X + 16;
  Top := _Pos.Y + 16;
  ParentWindow := Application.Handle;
  ShowWindow(Handle, SW_SHOWNOACTIVATE);
  BringToFront;
  Invalidate;
end;

end.

