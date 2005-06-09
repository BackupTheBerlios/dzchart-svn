unit u_HtmlInputField;

interface

uses
  SysUtils,
  Classes,
  u_HtmlFormField;

type
  THtmlInputField = class(THtmlFormField)
  private
    fType: string;
    fValue: string;
  protected
    procedure GetParams(_Params: TStrings); override;
  public
    constructor Create(const _Name: string; const _Type: string; const _Value: string = '');
  end;

type
  THtmlSubmitButton = class(THtmlInputField)
  public
    constructor Create(const _Name, _Value: string);
  end;

type
  THtmlTextField = class(THtmlInputField)
  private
    FSize: integer;
    FMaxLen: integer;
  protected
    procedure GetParams(_Params: TStrings); override;
  public
    constructor Create(const _Name, _Value: string);
    property Size: integer read FSize write FSize;
    property MaxLen: integer read FMaxLen write FMaxLen;
  end;

implementation

{ THtmlInputField }

constructor THtmlInputField.Create(const _Name, _Type: string; const _Value: string);
begin
  inherited Create('input', _Name);
  fType := _Type;
  fValue := _Value;
end;

procedure THtmlInputField.GetParams(_Params: TStrings);
begin
  inherited;
  _Params.Add(Format('type="%s"', [fType]));
  _Params.Add(Format('value="%s"', [fValue]));
end;

{ THtmlSubmitButton }

constructor THtmlSubmitButton.Create(const _Name, _Value: string);
begin
  inherited Create(_Name, 'submit', _Value);
end;

{ THtmlTextField }

constructor THtmlTextField.Create(const _Name, _Value: string);
begin
  inherited Create(_Name, 'text', _Value);
  fSize := -1;
  fMaxLen := -1;
end;

procedure THtmlTextField.GetParams(_Params: TStrings);
begin
  inherited;
  if FSize <> -1 then begin
    _Params.Add(Format('size="%d"', [FSize]));
  end;
  if FMaxLen <> -1 then begin
    _Params.Add(Format('maxlength="%d"', [FMaxLen]));
  end;
end;

end.
