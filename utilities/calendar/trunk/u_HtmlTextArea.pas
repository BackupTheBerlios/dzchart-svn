unit u_HtmlTextArea;

interface

uses
  SysUtils,
  Classes,
  u_HtmlComponent,
  u_HtmlFormField;

type
  THtmlTextArea = class(THtmlFormField)
  private
    FRows: integer;
    FCols: integer;
  protected
    procedure GetParams(_Params: TStrings); override;
  public
    constructor Create(const _Name: string; _Rows, _Cols: integer);
  end;

implementation

{ THtmlTextArea }

constructor THtmlTextArea.Create(const _Name: string; _Rows, _Cols: integer);
begin
  inherited Create('textarea', _Name);
  FRows := _Rows;
  FCols := _Cols;
end;

procedure THtmlTextArea.GetParams(_Params: TStrings);
begin
  inherited;
  _Params.Add(Format('rows="%d"', [FRows]));
  _Params.Add(Format('cols="%d"', [FCols]));
end;

end.
