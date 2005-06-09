unit u_OptionsSection;

interface

uses
  SysUtils,
  Classes,
  u_CalendarGlobals,
  u_HtmlContainer,
  u_CalendarParams,
  u_DocumentSection;

type
  TOptionsSection = class(TDocumentSection)
  private
    procedure WriteAddMonths(_Container: THtmlContainer; const _Caption,
      _Direction: string; _CurrVal: integer);
  protected
    function SectionTitle: string; override;
    procedure WriteSectionContent(_Container: THtmlContainer); override;
  public
  end;

implementation

uses
  u_HtmlCombobox,
  u_HtmlPrimitive,
  u_HtmlForm,
  u_HtmlRadiobutton,
  u_HtmlInputField;

{ TOptionsSection }

function TOptionsSection.SectionTitle: string;
begin
  Result := 'Options';
end;

 // The options section consists of the following parts:
 // section(
 //   sectiontitle()
 //   sectioncontent(
 //     <setting1>(
 //       title()
 //       content()
 //     )<setting>
 //     .. more settings here ...
 //   )sectioncontent
 // )section

procedure TOptionsSection.WriteSectionContent(_Container: THtmlContainer);
const
  FORM_NAME = 'options';
var
  frm: THtmlForm;
  cnt: THtmlContainer;
  OuterDiv: THtmlContainer;
  TitleDiv: THtmlPrimitive;
  ContentDiv: THtmlContainer;
  rb: THtmlRadioButtonLabel;
begin
  frm := THtmlForm.Create(FORM_NAME, 'get', CALENDAR_URL);
  _Container.Add(frm);
  cnt := THtmlContainer.Create('div');
  frm.Add(cnt);

  OuterDiv := THtmlContainer.Create('div');
  OuterDiv.CssClass := 'starton';
  cnt.Add(OuterDiv);

  TitleDiv := THtmlPrimitive.Create('div');
  TitleDiv.CssClass := 'title';
  TitleDiv.Content := 'Week starts on';
  OuterDiv.Add(TitleDiv);

  ContentDiv := THtmlContainer.Create('div');
  ContentDiv.CssClass := 'content';
  OuterDiv.Add(ContentDiv);

  rb := THtmlRadioButtonLabel.Create('starton', 'Sunday', '0', FParams.StartDayOfWeek = 0);
  rb.MakeAutoSubmit(FORM_NAME);
  ContentDiv.Add(rb);

  rb := THtmlRadioButtonLabel.Create('starton', 'Monday', '1', FParams.StartDayOfWeek = 1);
  rb.MakeAutoSubmit(FORM_NAME);
  ContentDiv.Add(rb);

  //  Write(lang_options);

  fParams.AddHiddenFields(cnt, [cpStartDayOfWeek, cpLanguage, cpCurrentDate]);

  WriteAddMonths(_Container, 'past months', 'past', fParams.MonthsPast);
  WriteAddMonths(_Container, 'future months', 'future', fParams.MonthsFuture);
end;

procedure TOptionsSection.WriteAddMonths(_Container: THtmlContainer; const _Caption, _Direction: string; _CurrVal: integer);
var
  excl: TParametersSet;
  cnt: THtmlContainer;
  frm: THtmlForm;
  btn: THtmlSubmitButton;
  fld: THtmlTextField;
begin
  if _Direction = 'past' then
    excl := [cpMonthsPast]
  else if _Direction = 'future' then
    excl := [cpMonthsFuture]
  else
    raise ECalendarException.CreateFmt('invalid direction %s', [_Direction]);

  cnt := THtmlContainer.Create('div');
  cnt.CssClass := 'months';
  _Container.Add(cnt);

  frm := THtmlForm.Create('', 'get', CALENDAR_URL);
  cnt.Add(frm);
  cnt := THtmlContainer.Create('div');
  frm.Add(cnt);

  cnt.Add(THtmlPrimitive.Create('div', 'title', _Caption));

  btn := THtmlSubmitButton.Create(Format('btn_%s', [_Direction]), '+');
  btn.CssClass := 'button';
  cnt.Add(btn);

  fld := THtmlTextField.Create(Format('cal_%s', [_Direction]), IntToStr(_CurrVal));
  fld.Size := 2;
  fld.MaxLen := 2;
  cnt.Add(fld);

  btn := THtmlSubmitButton.Create(Format('btn_%s', [_Direction]), 'Set');
  btn.CssClass := 'button';
  cnt.Add(btn);

  if _CurrVal > 0 then begin
    btn := THtmlSubmitButton.Create(Format('btn_%s', [_Direction]), '-');
    btn.CssClass := 'button';
    cnt.Add(btn);
  end;

  fParams.AddHiddenFields(cnt, excl);
end;

end.
