unit u_HtmlForm;

interface

uses
  SysUtils,
  Classes,
  u_HtmlContainer;

type
  THtmlForm = class(THtmlContainer)
  protected
    FAction: string;
    FMethod: string;
    FName: string;
    procedure GetParams(_Params: TStrings); override;
  public
    constructor Create(const _Name: string; const _Method: string; const _Action: string);
  end;

implementation

{ THtmlForm }

constructor THtmlForm.Create(const _Name: string; const _Method: string; const _Action: string);
begin
  inherited Create('form');
  FName := _Name;
  FMethod := _Method;
  FAction := _Action;
end;

procedure THtmlForm.GetParams(_Params: TStrings);
begin
  inherited GetParams(_Params);
  if FName <> '' then
    _Params.Add(Format('name="%s"', [FName]));
  _Params.Add(Format('method="%s"', [FMethod]));
  _Params.Add(Format('action="%s"', [FAction]));
end;

end.
