unit u_CalendarGlobals;

interface

uses
  SysUtils,
  DateUtils;

type
  ECalendarException = class(exception);

type
  TMonths = 1..12;

const
  DOCUMENT_ROOT = 'data';
  CALENDAR_URL = '/calendar';
  CSS_SUBDIR = '/css';
  APPOINTMENTS_DATA = 'data/appointments';

const
  // abbreviated month names, used internally
  months: array[TMonths] of string = (
    'Jan',
    'Feb',
    'Mar',
    'Apr',
    'May',
    'Jun',
    'Jul',
    'Aug',
    'Sep',
    'Oct',
    'Nov',
    'Dec'
    );

  longmonths: array[TMonths] of string = (
    'January',
    'February',
    'March',
    'April',
    'May',
    'June',
    'July',
    'August',
    'September',
    'October',
    'November',
    'December'
    );

  dow: array[0 .. DaySunday] of string = (
    'Su', 'Mo', 'Tu', 'We', 'Th', 'Fr', 'Sa', 'Su'
    );

function StartsWith(const _Start, _s: string): boolean;
function TailStr(const _s: string; _Start: integer): string;

implementation

uses
  StrUtils;

function StartsWith(const _Start, _s: string): boolean;
begin
  Result := _Start = LeftStr(_s, Length(_Start));
end;

function TailStr(const _s: string; _Start: integer): string;
begin
  if _Start > Length(_s) then
    Result := ''
  else
    Result := Copy(_s, _Start, Length(_s) - _Start + 1);
end;

function StrToBoolDef(const _s: string; _Def: Boolean): boolean;
begin
  if (_s = '0') or (_s = '') then
    Result := false
  else if _s = '1' then
    Result := true
  else
    Result := _Def;
end;

end.


