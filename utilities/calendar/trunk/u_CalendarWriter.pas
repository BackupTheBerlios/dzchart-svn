unit u_CalendarWriter;

{$optimization off}

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  u_CalendarGlobals,
  u_Appointments,
  u_CalendarParams,
  u_ContentWriter,
  u_Popup,
  u_ToolbarSection,
  u_CalendarSection,
  u_StyleSection,
  u_PopupSection,
  u_OptionsSection;

type
  TCalendarWriter = class(TContentWriter)
  public
    procedure WriteContent; override;
    function Title: string; override;
  end;

implementation

uses
  StrUtils,
  Math,
  DateUtils,
  u_HtmlCombobox,
  u_HtmlContainer,
  u_DetailsWriter;

const
  OPTIONS_SECTION = 'options';

function TCalendarWriter.Title: string;
begin
  Result := Format('Calendar - %s %d', [longmonths[fParams.CurrentMonth], fParams.CurrentYear]);
end;

procedure TCalendarWriter.WriteContent;
var
  Calendar: TCalendarSection;
  i: integer;
  Popup: TPopup;
  cnt: THtmlContainer;
begin
  Add(TToolbarSection.Create('toolbar', fParams));
  Calendar := TCalendarSection.Create(fParams);
  Add(Calendar);
  Add(TStyleSection.Create('styleselect', fParams));

  cnt := THtmlContainer.Create('div');
  cnt.CssClass := 'popup_section';
  cnt.Id := 'popup';
  Add(Cnt);
  for i := 0 to Calendar.Popups.Count - 1 do begin
    Popup := Calendar.Popups[i] as TPopup;
    Cnt.Add(TPopupSection.Create(Popup, fParams));
  end;

  Add(TOptionsSection.Create('options', fParams));

  inherited;
//  WriteSection(OPTIONS_SECTION, WriteOptions);
end;

 //  if fClockPostion <> cpOff then
 //     Write('<script type="text/javascript" src="js/clock.js"></script>');
//  WriteFmt('<p><img src="%0:s/img/valid-xhtml10.png" alt="Valid XHTML 1.0!" height="31" width="88" /></p>', [CALENDAR_URL]);

end.


