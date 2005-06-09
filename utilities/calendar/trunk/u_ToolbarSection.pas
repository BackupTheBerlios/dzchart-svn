unit u_ToolbarSection;

interface

uses
  SysUtils,
  Classes,
  u_CalendarGlobals,
  u_HtmlContainer,
  u_CalendarParams,
  u_DocumentSection;

type
  TToolbarSection = class(TDocumentSection)
  protected
    function SectionTitle: string; override;
    procedure WriteSectionContent(_Container: THtmlContainer); override;
  public
  end;

implementation

uses
  u_HtmlCombobox,
  u_HtmlPrimitive,
  u_HtmlForm;

{ TToolbarSection }

function TToolbarSection.SectionTitle: string;
begin
  Result := 'Month';
end;

procedure TToolbarSection.WriteSectionContent(_Container: THtmlContainer);
const
  FORM_NAME = 'toolbar';
  SELECTED = 'selected="selected"';
var
  i: integer;
  cmb: THtmlCombobox;
  cnt: THtmlContainer;
  frm: THtmlForm;
begin
  frm := THtmlForm.Create(FORM_NAME, 'get', CALENDAR_URL);
  _Container.Add(frm);

  cnt := THtmlContainer.Create('div');
  cnt.CssClass := 'dateselect';
  frm.Add(cnt);

  cmb := THtmlCombobox.Create('month');
  cmb.CssClass := 'month';
  cmb.MakeAutoSubmit(FORM_NAME);
  for i := 1 to 12 do begin
    cmb.AddOption(IntToStr(i), longmonths[i], i = fParams.CurrentMonth);
  end;
  cnt.Add(cmb);

  cmb := THtmlCombobox.Create('year');
  cmb.CssClass := 'year';
  cmb.MakeAutoSubmit(FORM_NAME);
  for i := fParams.CurrentYear - 10 to fParams.CurrentYear + 10 do begin
    cmb.AddOption(IntToStr(i), IntToStr(i), i = fParams.CurrentYear);
  end;
  cnt.Add(cmb);

  FParams.AddHiddenFields(cnt, [cpCurrentDate]);
end;

end.
