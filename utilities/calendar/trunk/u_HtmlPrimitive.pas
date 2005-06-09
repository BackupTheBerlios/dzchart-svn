unit u_HtmlPrimitive;

interface

uses
  SysUtils,
  Classes,
  u_HtmlComponent;

type
  THtmlPrimitive = class(THtmlComponent)
  protected
    FContent: String;
    procedure WriteContent; override;
  public
    constructor Create(const _Tag: string; const _CssClass: string = ''; const _Content: string = '');
    property Content: string read FContent write FContent;
  end;

implementation

{ THtmlPrimitive }

constructor THtmlPrimitive.Create(const _Tag: string; const _CssClass: string = ''; const _Content: string = '');
begin
  inherited Create(_Tag, _CssClass);
  FContent := _Content;
end;

procedure THtmlPrimitive.WriteContent;
begin
  Write(FContent);
end;

end.
