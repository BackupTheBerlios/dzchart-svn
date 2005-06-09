unit u_Popup;

interface

uses
  SysUtils,
  Classes,
  u_Appointments;

type
  TPopup = class
  protected
    FId: string;
    FDate: TDateTime;
    FHoliday: TAppointment;
    FAppointments: TAptColl;
  public
    constructor Create(_Date: TDateTime; const _Holiday: TAppointment; _Appointments: TAptColl);
    function Month: integer;
    function Day: integer;
    property Id: string read FId;
    property Date: TDateTime read FDate;
    property Holiday: TAppointment read FHoliday;
    property Appointments: TAptColl read FAppointments;
    class function BuildId(_Date: TDateTime): string;
  end;

implementation

{ TPopup }

uses
  DateUtils;

class function TPopup.BuildId(_Date: TDateTime): string;
begin
  Result := 'id' + FormatDateTime('yyyymmdd', _Date);
end;

constructor TPopup.Create(_Date: TDateTime; const _Holiday: TAppointment; _Appointments: TAptColl);
begin
  inherited Create;
  FDate := _Date;
  FHoliday := _Holiday;
  FAppointments := _Appointments.Clone;
  FId := BuildId(_Date);
end;

function TPopup.Day: integer;
begin
  Result := DayOfTheMonth(FDate);
end;

function TPopup.Month: integer;
begin
  Result := MonthOf(FDate);
end;

end.


