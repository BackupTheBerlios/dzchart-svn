unit u_CalendarSection;

interface

uses
  SysUtils,
  Classes,
  Contnrs,
  u_CalendarGlobals,
  u_HtmlContainer,
  u_CalendarParams,
  u_DocumentSection;

type
  TCalendarSection = class(TDocumentSection)
  private
    FPopups: TObjectList;
    procedure WriteMonth(_Container: THtmlContainer; const _Month: TDateTime);
  protected
    function SectionTitle: string; override;
    procedure WriteSectionContent(_Container: THtmlContainer); override;
  public
    constructor Create(_Params: TCalendarParams);
    destructor Destroy; override;
    property Popups: TObjectList read FPopups;
  end;

implementation

uses
  DateUtils,
  u_HtmlCombobox,
  u_HtmlPrimitive,
  u_HtmlForm,
  u_HtmlTable,
  u_Appointments,
  u_Popup, u_HtmlComponent;

{ TCalendarSection }

constructor TCalendarSection.Create(_Params: TCalendarParams);

  function AssertHolidays(_Month: TDateTime): TDateTime;
  begin
    Result := _Month;
    FParams.InitHolidays(YearOf(_Month));
  end;

  procedure CreatePopups(_Month: TDateTime);
  var
    Holiday: TAppointment;
    Appointments: TAptColl;
    Days: integer;
    i: integer;
    Day: TDateTime;
  begin
    Day := _Month;
    Days := DaysInMonth(_Month);
    for i := 1 to Days do begin
      Appointments := TAptColl.Create;
      try
        if fParams.HasDescription(Day, Holiday, Appointments) then begin
          FPopups.Add(TPopup.Create(Day, Holiday, Appointments));
        end;
      finally
        Appointments.Free;
      end;
      Day := IncDay(Day);
    end;
  end;

var
  Month: TDateTime;
  i: integer;
begin
  inherited Create('calendar', _Params);
  FPopups := TObjectList.Create;

  Month := fParams.CurrentDate;
  for i := fParams.MonthsPast downto 1 do begin
    CreatePopups(AssertHolidays(IncMonth(Month, -i)));
  end;

  CreatePopups(AssertHolidays(Month));

  for i := 1 to fParams.MonthsFuture do begin
    CreatePopups(AssertHolidays(IncMonth(Month, i)));
  end;
end;

destructor TCalendarSection.Destroy;
begin
  FPopups.Free;
  inherited;
end;

function TCalendarSection.SectionTitle: string;
begin
  Result := 'Calendar';
end;

procedure TCalendarSection.WriteSectionContent(_Container: THtmlContainer);
var
  i: integer;
  Month: TDateTime;
  cnt: THtmlContainer;
begin
  Month := fParams.CurrentDate;
  for i := fParams.MonthsPast downto 1 do begin
    cnt := THtmlContainer.Create('div');
    cnt.CssClass := 'prev';
    _Container.Add(cnt);
    WriteMonth(cnt, IncMonth(Month, -i));
  end;

  cnt := THtmlContainer.Create('div');
  cnt.CssClass := 'current';
  _Container.Add(cnt);
  WriteMonth(cnt, Month);

  for i := 1 to fParams.MonthsFuture do begin
    cnt := THtmlContainer.Create('div');
    cnt.CssClass := 'next';
    _Container.Add(cnt);
    WriteMonth(cnt, IncMonth(Month, i));
  end;
  inherited;
end;

procedure TCalendarSection.WriteMonth(_Container: THtmlContainer; const _Month: TDateTime);
var
  tbl: THtmlTable;
  row: THtmlTableRow;
  Today: integer;
  CalDate: TDateTime;
  Days: integer;
  Column: integer;
  j: integer;
  wkDay: integer;
  date_class: string;
  desc: string;
  enddesc: string;
  cmdate: integer;
  PrevMonth: TDateTime;
  NextMonth: TDateTime;
  dstr: string;
  DateStr: string;
  Holiday: TAppointment;
  Appointments: TAptColl;
  onMouseover: string;
  onMouseout: string;
begin
  if (MonthOf(fParams.Today) = MonthOf(_Month)) and
    (YearOf(fParams.Today) = YearOf(_Month)) then
    Today := fParams.TodaysDay
  else
    Today := 0;

  CalDate := _Month;
  Days := DaysInMonth(CalDate);
  wkDay := DayOfTheWeek(CalDate);
  date_class := '';
  desc := '';
  enddesc := '';

  PrevMonth := IncMonth(fParams.CurrentDate, -1);
  NextMonth := IncMonth(fParams.CurrentDate, 1);

  tbl := THtmlTable.Create;
  tbl.CssClass := 'headtable';
  tbl.Border := 0;
  tbl.Cellpadding := 1;
  tbl.Cellspacing := 0;
  tbl.Width := '100%';
  _Container.Add(tbl);
  with tbl.AddRow do begin
    CssClass := 'headrow';
    with AddCell do begin
      CssClass := 'back';
      Width := '1%';
      Align := 'left';
      Content := Format('<a href="%s&amp;year=%d&amp;month=%d">&lt;&lt;</a>',
        [fParams.BuildFatUrl('', [cpCurrentDate]), YearOf(PrevMonth), MonthOf(PrevMonth)]);
    end;
    with AddCell do begin
      CssClass := 'caption';
      Align := 'center';
      Content := Format('<b><span class="month">%s</span>&nbsp;&nbsp;<span class="year">%d</span></b>',
        [longmonths[MonthOf(_Month)], YearOf(_Month)]);
    end;
    with AddCell do begin
      CssClass := 'forward';
      Width := '1%';
      Align := 'right';
      Content := Format('<a href="%s&amp;year=%d&amp;month=%d">&gt;&gt;</a>',
        [fParams.BuildFatUrl('', [cpCurrentDate]), YearOf(NextMonth), MonthOf(NextMonth)]);
    end;
  end;

  tbl := THtmlTable.Create;
  tbl.CssClass := 'contenttable';
  tbl.Border := 0;
  tbl.Cellpadding := 1;
  tbl.Cellspacing := 0;
  tbl.Width := '100%';
  _Container.Add(tbl);
  with tbl.AddRow do begin
    CssClass := 'dow';

    with AddCell do begin
      CssClass := 'weekofyear';
      Align := 'center';
      Content := '<span class="weekhead">Wk</span>';
    end;

    for Column := 0 to 6 do begin
      with AddCell do begin
        Align := 'center';
        Content := dow[((Column + Ord(fParams.StartDayOfWeek)) mod 7)];
      end;
    end;
  end;

  row := tbl.AddRow;
  row.Align := 'center';

  with row.AddCell do begin
    CssClass := 'weekofyear';
    Align := 'center';
    Content := Format('<span class="week">%d</span>', [WeekOfTheYear(CalDate)]);
  end;

  // start the first line with blank spaces until we get to the first day of the month
  for Column := 1 to (7 - Ord(fParams.StartDayOfWeek) + wkDay) mod 7 do
    with row.AddCell do begin
      Content := '&nbsp;';
    end;

  // since javascript doesn't do modulus on negative numbers,
  // add 7 to anything that might be negative
  Column := (7 - fParams.StartDayOfWeek + wkDay) mod 7;
  cmdate := Column - ((7 - fParams.StartDayOfWeek + wkDay) mod 7);

  // write the weekdays
  while cmDate < Days do begin
    // what is the date ?
    Inc(cmdate);
    // if we have reached the end of a week, start another one
    if (0 = (Column mod 7)) and (Column <> 0) then begin
      row := tbl.AddRow;
      row.Align := 'center';

      with row.AddCell do begin
        CssClass := 'weekofyear';
        Align := 'center';
        Content := Format('<span class="week">%d</span>', [WeekOfTheYear(CalDate + Column)]);
      end;
    end;

    desc := '';
    enddesc := '';
    if fParams.isHoliday(calDate) or fParams.isWeekend(calDate) then
      date_class := 'holiday'
    else
      date_class := 'day';

    Appointments := TAptColl.Create;
    try
      if fParams.HasDescription(calDate, Holiday, Appointments) then begin
        fParams.DetailDate := IsoDate(calDate);
        DateStr := IntToStr(cmdate) + ' ' + months[MonthOf(calDate)];
        dstr := '';
        if Assigned(Holiday) then
          Appointments.AtInsert(0, Holiday.Clone);
        if Appointments.Count <> 0 then begin
          for j := 0 to Appointments.Count - 2 do
            dstr := dstr + Appointments[j].Desc + ' / ';
          dstr := dstr + Appointments[Appointments.Count - 1].Desc;
        end;
        onMouseover := Format('document.getElementById(''%s'').style.display=''block'';', [TPopup.BuildId(CalDate)]);
        onMouseout := Format('document.getElementById(''%s'').style.display=''none'';', [TPopup.BuildId(CalDate)]);
        dstr := DateStr + ': ' + dstr;
        desc := Format(
          '<a class="desc" target="_content" href="%s&amp;date=%s" title="%s" onMouseover="%s" onMouseout="%s">',
          [fParams.BuildFatUrl('show', [cpCurrentDate]), IsoDate(calDate), dStr, onMouseover, onMouseout]);
      end else begin
        desc := Format(
          '<a target="_content" href="%s&amp;date=%s" title="%s">',
          [fParams.BuildFatUrl('show', [cpCurrentDate]), IsoDate(calDate), IsoDate(calDate)]);
      end;
      enddesc := '</a>';
    finally
      Appointments.Free;
    end;

    if cmdate = Today then begin
      date_class := 'today';
    end;

    with row.AddCell do begin
      CssClass := date_class;
      Content := desc + IntToStr(cmdate) + enddesc;
    end;

    calDate := IncDay(calDate);
    Inc(Column);

  end;

  while 0 <> (Column mod 7) do begin
    with row.AddCell do begin
      Content := '&nbsp;';
    end;
    Inc(Column);
  end;
end;

end.
