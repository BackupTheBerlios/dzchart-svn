{GXFormatter.config=twm}
{: generic progress form
   This unit implements a generic progress form.
   Create it, set FormCaption, optionally set Max, show the form.
   Every now and then call Progress.
   Free after you are done.
   Note: You can use up to two %d and %% in FormCaption, examples:
     FormCaption := 'progress %d of %d';
     FormCaption := '%d%% done'; // display percentage
     FormCaption := 'Progress - Line %d';
   @author twm
}
unit w_dzProgress;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls;

type
  EdzProgress = class(Exception);

type
  Tf_dzProgress = class(TForm)
    l_Action: TLabel;
    pb_Progress: TProgressBar;
    b_Cancel: TButton;
    procedure b_CancelClick(Sender: TObject);
  private
    fCancelVisible: boolean;
    fCancelPressed: boolean;
    fProgressPos: integer;
    fMax: integer;
    fAction: string;
    fActionVisible: boolean;
    fPercent: integer;
    fFormCaption: string;
    fFormCaptionParams: integer;
    fFormCaptionPercent: boolean;
    procedure SetFormCaption(const _FormCaption: string);
    procedure InternalSetCaption;
    procedure SetProgressPos(_ProgressPos: integer);
    procedure SetMax(_Max: integer);
    procedure SetAction(const _Action: string);
    procedure SetCancelVisible(_Visible: boolean);
    procedure SetActionVisible(_Visible: boolean);
    function AnalyseCaption(const _Caption: string;
      var _ParamCount: integer; var _Percent: boolean;
      var _Error: string): boolean;
  public
    constructor Create(_Owner: TComponent); overload; override;
    property FormCaption: string read fFormCaption write SetFormCaption;
    property ProgressPos: integer read fProgressPos write SetProgressPos;
    property Max: integer read fMax write SetMax;
    property Action: string read fAction write SetAction;
    property CancelVisible: boolean read fCancelVisible write SetCancelVisible;
    property ActionVisible: boolean read fActionVisible write SetActionVisible;
    procedure Progress(_Position: integer; const _Action: string;
      var _Abort: boolean); overload;
    procedure Progress(_Position: integer; var _Abort: boolean); overload;
  end;

implementation

{$R *.DFM}

uses
  StrUtils,
  u_dzTranslator,
  w_dzDialog;

resourcestring
  // Wirklich abbrechen?
  QuestionAbort = 'Do you really want to abort?';
  // Fortschritt (%d%%)
  FormCaptionD = 'Progress (%d%%)';
  // Tf_HkProgress.SetFormCaption: Ungültige FormCaption, %s.
  InvalidFormCaptionS = 'Tf_HkProgress.SetFormCaption: Invalid FormCaption, %s';
  // darf kein "%" am Ende haben
  PercentAtEnd = 'must not contain a % at the end';
  // auf "%" muß "d" oder "%" folgen
  InvalidFormatChar = '"%" must be followed by "d" or "%".';
  // zuviele Parameter
  TooManyParameters = 'too many parameters';

constructor Tf_dzProgress.Create(_Owner: tComponent);
begin
  inherited;
  pb_Progress.Position := 0;
  fMax := 100;
  l_Action.Caption := '';
  FormCaption := FormCaptionD;

  TranslateComponent(Self);
end;

procedure Tf_dzProgress.b_CancelClick(Sender: TObject);
begin
  fCancelPressed := (mrYes = Tf_dzDialog.ShowMessage(mtConfirmation, QuestionAbort, [dbeYes, dbeNo], Self));
end;

procedure Tf_dzProgress.InternalSetCaption;
var
  OldPercent: integer;
begin
  if fFormCaptionPercent then
  begin
    OldPercent := fPercent;
    fPercent := (fProgressPos * 100) div fMax;
    if OldPercent = fPercent then
      exit;
  end;
  case fFormCaptionParams of
    0: Caption := fFormCaption;
    1:
      if fFormCaptionPercent then
        Caption := Format(fFormCaption, [fPercent])
      else
        Caption := Format(fFormCaption, [fProgressPos]);
    2: Caption := Format(fFormCaption, [fProgressPos, fMax]);
  end;
end;

procedure Tf_dzProgress.Progress(_Position: integer; var _Abort: boolean);
begin
  fProgressPos := _Position;
  pb_Progress.Position := _Position;
  InternalSetCaption;
  Application.ProcessMessages;
  _Abort := fCancelPressed;
end;

procedure Tf_dzProgress.Progress(_Position: integer; const _Action: string;
  var _Abort: boolean);
begin
  fAction := _Action;
  l_Action.Caption := _Action;
  Progress(_Position, _Abort);
end;

function Tf_dzProgress.AnalyseCaption(const _Caption: string; var _ParamCount: integer;
  var _Percent: boolean; var _Error: string): boolean;
var
  p: integer;
begin
  Result := false;
  _Percent := false;
  _ParamCount := 0;
  p := PosEx('%', _Caption, 1);
  while p <> 0 do
  begin
    if (p >= Length(_Caption)) then
    begin
      _Error := PercentAtEnd;
      exit;
    end;
    if _Caption[p + 1] = 'd' then
      Inc(_ParamCount)
    else if _Caption[p + 1] = '%' then
    begin
      _Percent := true;
      Inc(p)
    end
    else
    begin
      _Error := InvalidFormatChar;
      exit;
    end;
    p := PosEx('%', _Caption, p + 1);
  end;
  if _ParamCount > 2 then
    _Error := TooManyParameters
  else
    Result := true;
end;

procedure Tf_dzProgress.SetFormCaption(const _FormCaption: string);
var
  Error: string;
  Percent: boolean;
  Params: integer;
begin
  if not AnalyseCaption(_FormCaption, Params, Percent, Error) then
    raise EdzProgress.CreateFmt(InvalidFormCaptionS, [Error]);
  fFormCaption := _FormCaption;
  fFormCaptionParams := Params;
  fFormCaptionPercent := Percent;
  InternalSetCaption;
end;

procedure Tf_dzProgress.SetProgressPos(_ProgressPos: integer);
begin
  fProgressPos := _ProgressPos;
  pb_Progress.Position := _ProgressPos;
  InternalSetCaption;
end;

procedure Tf_dzProgress.SetMax(_Max: integer);
begin
  fMax := _Max;
  pb_Progress.Max := _Max;
  InternalSetCaption;
end;

procedure Tf_dzProgress.SetAction(const _Action: string);
begin
  fAction := _Action;
  l_Action.Caption := _Action;
  l_Action.Layout := tlCenter;
end;

procedure Tf_dzProgress.SetCancelVisible(_Visible: boolean);
begin
  fCancelVisible := _Visible;
  b_Cancel.Visible := fCancelVisible;
  if not fCancelVisible then
    pb_Progress.Width := ClientWidth - 2 * pb_Progress.Left
  else
    pb_Progress.Width := b_Cancel.Left - 2 * pb_Progress.Left;
end;

procedure Tf_dzProgress.SetActionVisible(_Visible: boolean);
begin
  fActionVisible := _Visible;
  l_Action.Visible := fActionVisible;
  if not fActionVisible then
  begin
    ClientHeight := 2 * 5 + b_Cancel.Height;
    b_Cancel.Top := (ClientHeight - b_Cancel.Height) div 2;
    pb_Progress.Top := (ClientHeight - pb_Progress.Height) div 2;
  end
  else
  begin
    // as designed
    b_Cancel.Top := 20;
    pb_Progress.Top := 24;
    Height := 76;
  end;
end;

end.

