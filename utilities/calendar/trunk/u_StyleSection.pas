unit u_StyleSection;

interface

uses
  SysUtils,
  Classes,
  u_CalendarGlobals,
  u_HtmlContainer,
  u_CalendarParams,
  u_DocumentSection;

type
  TStyleSection = class(TDocumentSection)
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

{ TStyleSection }

function TStyleSection.SectionTitle: string;
begin
  Result := 'Style';
end;

procedure TStyleSection.WriteSectionContent(_Container: THtmlContainer);
const
  FORM_NAME = 'styleselect';
var
  sr: TSearchRec;
  res: integer;
  s: string;
  cmb: THtmlCombobox;
  frm: THtmlForm;
  cnt: THtmlContainer;
begin
  frm := THtmlForm.Create(FORM_NAME, 'get', CALENDAR_URL);
  _Container.Add(frm);
  cnt := THtmlContainer.Create('div');
  frm.Add(cnt);

  cmb := THtmlCombobox.Create('style');
  cmb.Sorted := true;
  cmb.MakeAutoSubmit(FORM_NAME);
  res := FindFirst(DOCUMENT_ROOT + CSS_SUBDIR + '/*.css', SysUtils.faReadOnly, sr);
  if res = 0 then
    try
      while res = 0 do begin
        if (sr.Name <> '..') and (sr.Name <> '.') then begin
          s := ChangeFileExt(sr.Name, '');
          cmb.AddOption(s, s, s = fParams.Style);
        end;
        res := FindNext(sr);
      end;
    finally
      FindClose(sr);
    end;
  cnt.Add(cmb);

  fParams.AddHiddenFields(cnt, [cpStyle]);
end;

end.
