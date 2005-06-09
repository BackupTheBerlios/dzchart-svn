unit u_HtmlRadiobutton;

interface

uses
  SysUtils,
  Classes,
  u_HtmlContainer,
  u_HtmlInputField;

type
  THtmlRadioButton = class(THtmlInputField)
  private
    FChecked: boolean;
  protected
    FOnClick: string;
    procedure GetParams(_Params: TStrings); override;
  public
    constructor Create(const _Name, _Value: string);
    property OnClick: string read FOnClick write FOnClick;
    property Checked: boolean read FChecked write FChecked;
  end;

type
  THtmlRadioButtonLabel = class(THtmlContainer)
  protected
    FRadioButton: THtmlRadioButton;
    FCaption: string;
    procedure WriteContent; override;
  public
    constructor Create(const _Name, _Caption, _Value: string; _Checked: boolean);
    procedure MakeAutoSubmit(const _Form: string);
  end;

implementation

{ THtmlRadioButton }

constructor THtmlRadioButton.Create(const _Name, _Value: string);
begin
  inherited Create(_Name, 'radio', _Value);
end;

procedure THtmlRadioButton.GetParams(_Params: TStrings);
begin
  inherited;
  if FOnClick <> '' then begin
    _Params.Add(Format('onclick="%s"', [FOnClick]));
  end;
  if FChecked then begin
    _Params.Add('checked="checked"');
  end;
end;

{ THtmlRadioButtonLabel }

constructor THtmlRadioButtonLabel.Create(const _Name, _Caption, _Value: string; _Checked: boolean);
begin
  inherited Create('label');
  FRadioButton := THtmlRadioButton.Create(_Name, _Value);
  FRadioButton.Checked := _Checked;
  Add(FRadioButton);
  FCaption := _Caption;
end;

procedure THtmlRadioButtonLabel.MakeAutoSubmit(const _Form: string);
begin
  FRadioButton.OnClick := Format('document.%s.submit();', [_Form]);
end;

procedure THtmlRadioButtonLabel.WriteContent;
begin
  inherited;
  Write(FCaption);
end;


end.
