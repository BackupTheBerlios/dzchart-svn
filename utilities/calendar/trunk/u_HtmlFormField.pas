unit u_HtmlFormField;

interface

uses
  SysUtils,
  Classes,
  u_HtmlComponent;

type
  THtmlFormField = class(THtmlComponent)
  protected
    fName: string;
    procedure GetParams(_Params: TStrings); override;
  public
    constructor Create(const _Tag: string; const _Name: string);
    property Name: string read fName;
  end;

implementation

{ THtmlFormField }

constructor THtmlFormField.Create(const _Tag: string; const _Name: string);
begin
  inherited Create(_Tag);
  fName := _Name;
end;

procedure THtmlFormField.GetParams(_Params: TStrings);
begin
  inherited;
  _Params.Add(Format('name="%s"', [fName]));
end;

end.
