unit w_StringParser;

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
  ExtCtrls,
  u_StringParser;

type
  Tf_StringParser = class(TForm)
    led_ParseString: TLabeledEdit;
    b_Parse: TButton;
    b_Close: TButton;
    procedure b_CloseClick(Sender: TObject);
    procedure b_ParseClick(Sender: TObject);
  private
    FParser: TStringParser;
  public
    constructor Create(_Owner: TComponent); override;
    destructor Destroy; override;
  end;

var
  f_StringParser: Tf_StringParser;

implementation

{$R *.dfm}

{ Tf_StringParser }

constructor Tf_StringParser.Create(_Owner: TComponent);
begin
  inherited;
  FParser := TStringParser.Create;
end;

destructor Tf_StringParser.Destroy;
begin
  FParser.Free;
  inherited;
end;

procedure Tf_StringParser.b_ParseClick(Sender: TObject);
begin
  FParser.Execute(led_ParseString.Text);
  MessageDlg('String literal parsed OK.', mtInformation, [mbOK], 0)
end;

procedure Tf_StringParser.b_CloseClick(Sender: TObject);
begin
  Close;
end;

end.

