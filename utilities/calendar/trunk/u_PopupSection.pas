unit u_PopupSection;

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  u_CalendarGlobals,
  u_HtmlContainer,
  u_CalendarParams,
  u_DocumentSection,
  u_Popup;

type
  TPopupSection = class(TDocumentSection)
  protected
    fPopup: TPopup;
    function SectionTitle: string; override;
    procedure WriteSectionContent(_Container: THtmlContainer); override;
  public
    constructor Create(_Popup: TPopup; _Params: TCalendarParams);
  end;

implementation

uses
  u_HtmlPrimitive,
  u_HtmlTable, u_HtmlComponent;


{ TPopupSection }

constructor TPopupSection.Create(_Popup: TPopup; _Params: TCalendarParams);
begin
  inherited Create(_Popup.Id, _Params);
  fPopup := _Popup;
end;

function TPopupSection.SectionTitle: string;
begin
  Result := Format('%d %s', [fPopup.Day, longmonths[fPopup.Month]]);
end;

procedure TPopupSection.WriteSectionContent(_Container: THtmlContainer);
var
  AptIdx: integer;
  tbl: THtmlTable;
begin
  tbl := THtmlTable.Create;
  _Container.Add(tbl);
  tbl.CssClass := 'sectioncontent';
  if Assigned(fPopup.Holiday) then begin
    with tbl.AddRow do begin
      with AddCell do begin
        CssClass := 'holiday';
        ColSpan := 2;
        Content := fPopup.Holiday.Desc;
      end;
    end;
  end;
  for AptIdx := 0 to fPopup.Appointments.Count - 1 do begin
    with tbl.AddRow do begin
      with AddCell do begin
        Content := fPopup.Appointments[AptIdx].TimeStr;
      end;
      with AddCell do begin
        CssClass := 'appointment';
        Content := fPopup.Appointments[AptIdx].Desc;
      end;
    end;
  end;
  inherited;
end;

end.
