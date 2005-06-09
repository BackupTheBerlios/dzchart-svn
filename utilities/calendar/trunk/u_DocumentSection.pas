unit u_DocumentSection;

interface

uses
  SysUtils,
  Classes,
  u_CalendarGlobals,
  u_HtmlContainer,
  u_CalendarParams;

type
  TDocumentSection = class(THtmlContainer)
  protected
    FId: string;
    FParams: TCalendarParams;
    procedure GetParams(_Params: TStrings); override;
    function SectionTitle: string; virtual; abstract;
    procedure WriteContent; override;
    procedure WriteSectionContent(_Container: THtmlContainer); virtual; abstract;
  public
    constructor Create(const _Id: string; _Params: TCalendarParams);
  end;

implementation

uses
  u_HtmlCombobox,
  u_HtmlPrimitive,
  u_HtmlForm;

{ TDocumentSection }

constructor TDocumentSection.Create(const _Id: string; _Params: TCalendarParams);
begin
  inherited Create('div');
  FId := _Id;
  FParams := _Params;
  CssClass := 'section';
end;

procedure TDocumentSection.GetParams(_Params: TStrings);
begin
  inherited;
  _Params.Add(Format('id="%s"', [FId]));
end;

procedure TDocumentSection.WriteContent;
var
  cnt: THtmlContainer;
begin
  Add(THtmlPrimitive.Create('h1', 'sectiontitle', SectionTitle));

  cnt := THtmlContainer.Create('div');
  cnt.CssClass := 'sectioncontent';
  Add(cnt);

  WriteSectionContent(cnt);
  inherited;
end;

end.
