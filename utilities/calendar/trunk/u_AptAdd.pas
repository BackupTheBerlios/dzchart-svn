unit u_AptAdd;


{$optimization off}

interface

uses
  SysUtils,
  Classes,
  u_CalendarGlobals,
  u_ContentWriter,
  u_CalendarParams,
  u_DetailsWriter;

type
  TAptAdd = class(TDetailsWriter)
  protected
  public
    constructor Create(_Params: TCalendarParams);
    destructor Destroy; override;
    procedure WriteContent; override;
    function Title: string; override;
  end;

implementation

{ TAptAdd }

constructor TAptAdd.Create(_Params: TCalendarParams);
begin
  inherited Create(_Params);
end;

destructor TAptAdd.Destroy;
begin

  inherited;
end;

procedure TAptAdd.WriteContent;
var
  Idx: integer;
  AptDate: TDateTime;
  AptTime: TDateTime;
  AptDesc: string;
  Year, Month, Day: integer;
  Hour, Minute: integer;
begin
  Write('<div class="section">'#10);

  Write('<h1 class="sectiontitle">Result of Add action</h1>'#10);

  Write('<div class="sectioncontent">'#10);

  try
    if fParams.AptAddDesc = '' then
      begin
        Write('Please specify a description for the appointment!');
        exit;
      end;

    AptDesc := fParams.AptAddDesc;
    if TryStrToInt(fParams.AptAddYear, Year) and TryStrToInt(fParams.AptAddMonth, Month) and TryStrToInt(fParams.AptAddDay, Day) then
      begin
        AptDate := EncodeDate(StrToInt(fParams.AptAddYear), StrToInt(fParams.AptAddMonth),StrToInt(fParams.AptAddDay));
        if TryStrToInt(fParams.AptAddHour, Hour) and TryStrToInt(fParams.AptAddMinute, Minute) then
          begin
            AptTime := EncodeTime(Hour, Minute, 0, 0);
            fAppointments.AddAppointment(AptDate, AptTime, AptDesc);
          end
        else
          fAppointments.AddAppointment(AptDate, AptDesc);
      fAppointments.WriteToFile(APPOINTMENTS_DATA);
      Write('The appointment has been added.');
    end else begin
      Write('Please specify a date for the appointment!');
    end;
  finally
    Write('</div>'); // sectioncontent
    Write('</div>'); // section
  end;
end;

function TAptAdd.Title: string;
begin
  Result := Format('Calendar - %s', [fParams.DetailDate]);
end;

end.

