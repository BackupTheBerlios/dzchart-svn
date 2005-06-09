unit u_AptDelete;

{$optimization off}

interface

uses
  SysUtils,
  u_CalendarGlobals,
  u_ContentWriter,
  u_CalendarParams,
  u_DetailsWriter;

type
  TAptDelete = class(TDetailsWriter)
  protected
  public
    constructor Create(_Params: TCalendarParams);
    destructor Destroy; override;
    procedure WriteContent; override;
    function Title: string; override;
  end;

implementation

{ TAptDelete }

constructor TAptDelete.Create(_Params: TCalendarParams);
begin
  inherited Create(_Params);
end;

destructor TAptDelete.Destroy;
begin

  inherited;
end;

procedure TAptDelete.WriteContent;
var
  Idx: integer;
begin
  Write('<div class="section">'#10);

  Write('<h1 class="sectiontitle">Result of Delete action</h1>'#10);

  Write('<div class="sectioncontent">'#10);

  if fParams.AptsToDelete.Count > 0 then begin
    Write('<table>'#10);
    fAppointments.Search(fParams.DetailDate, Idx);
    while (Idx < fAppointments.Count) and (fAppointments[Idx].DateStr = fParams.DetailDate) do begin
      if fParams.AptsToDelete.IndexOf(fAppointments[Idx].TimeDesc) <> -1 then begin
        WriteFmt('<tr><td>%s</td></tr>'#10, [fAppointments[Idx].TimeDesc]);
        fAppointments.AtDelete(Idx);
      end else begin
        Inc(Idx);
      end;
    end;
    fAppointments.WriteToFile(APPOINTMENTS_DATA);
    Write('</table>'#10);
    Write('have been deleted.');
  end else begin
    Write('nothing to do');
  end;
  Write('</div>'); // sectioncontent
  Write('</div>'); // section
end;

function TAptDelete.Title: string;
begin
  Result := Format('Calendar - %s', [fParams.DetailDate]);
end;

end.

