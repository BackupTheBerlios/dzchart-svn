program Calendar;

{%File 'data/template.html'}
{%File 'data/css/none.css'}
{%File 'data/css/default_options.css'}
{%File 'data/css/default_all.css'}
{%File 'data/css/minimalist_options.css'}
{%File 'data/css/minimalist_all.css'}
{%File 'data/css/minimalist_calendar.css'}
{%File 'data/css/default_calendar.css'}
{%File 't_dzTypedSortedCollection.tpl'}
{%File 'data/css/notitle_all.css'}
{%File 'data/css/notitle_calendar.css'}

(*
{$ifdef linux}
  execshieldfix,
  QForms,
{$else}
  Forms,
{$endif}
*)

uses
{$ifdef linux}
  execshieldfix,
  QForms,
{$else}
  Forms,
{$endif}
  SysUtils,
  u_Calendar in 'u_Calendar.pas',
  u_Appointments in 'u_Appointments.pas',
  u_DetailsWriter in 'u_DetailsWriter.pas',
  u_ContentWriter in 'u_ContentWriter.pas',
  u_CalendarParams in 'u_CalendarParams.pas',
  u_CalendarWriter in 'u_CalendarWriter.pas',
  u_CalendarGlobals in 'u_CalendarGlobals.pas',
  u_HtmlCombobox in 'u_HtmlCombobox.pas',
  u_Popup in 'u_Popup.pas',
  d_Calendar in 'd_Calendar.pas' {dm_Calendar: TDataModule},
  u_AptDelete in 'u_AptDelete.pas',
  u_AptAdd in 'u_AptAdd.pas',
  u_HtmlComponent in 'u_HtmlComponent.pas',
  u_HtmlTable in 'u_HtmlTable.pas',
  u_HtmlTextArea in 'u_HtmlTextArea.pas',
  u_HtmlFormField in 'u_HtmlFormField.pas',
  u_HtmlInputField in 'u_HtmlInputField.pas',
  u_HtmlDocument in 'u_HtmlDocument.pas',
  u_HtmlContainer in 'u_HtmlContainer.pas',
  u_ToolbarSection in 'u_ToolbarSection.pas',
  u_CalendarSection in 'u_CalendarSection.pas',
  u_OptionsSection in 'u_OptionsSection.pas',
  u_PopupSection in 'u_PopupSection.pas',
  u_HtmlPrimitive in 'u_HtmlPrimitive.pas',
  u_HtmlForm in 'u_HtmlForm.pas',
  u_DocumentSection in 'u_DocumentSection.pas',
  u_StyleSection in 'u_StyleSection.pas',
  u_HtmlRadiobutton in 'u_HtmlRadiobutton.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tdm_Calendar, dm_Calendar);
  while true do begin
    Application.ProcessMessages;
    Sleep(0);
  end;
end.

