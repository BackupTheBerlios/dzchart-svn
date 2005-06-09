unit u_HtmlDocument;

interface

uses
  SysUtils,
  Classes,
  u_HtmlComponent,
  u_HtmlContainer;

type
  THtmlDocument = class(THtmlContainer)
  public
    constructor Create;
  end;

implementation

{ THtmlDocument }

constructor THtmlDocument.Create;
begin
  inherited Create('body');
end;

end.
