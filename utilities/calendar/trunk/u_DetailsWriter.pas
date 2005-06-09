unit u_DetailsWriter;

{$optimization off}

interface

uses
  SysUtils,
  Classes,
  u_CalendarGlobals,
  u_ContentWriter;

type
  TDetailsWriter = class(TContentWriter)
  private
  public
    function Title: string; override;
    procedure WriteContent; override;
  end;

implementation

uses
  StrUtils,
  DateUtils,
  Math,
  u_Appointments,
  u_HtmlTable,
  u_HtmlCombobox,
  u_HtmlTextArea,
  u_HtmlInputField;

procedure TDetailsWriter.WriteContent;
var
  DateStr: string;
  Holiday: TAppointment;
  Appointments: TAptColl;
  i: integer;
  CurrDate: TDateTime;
  cmb: THtmlComboBox;
  tbl: THtmlTable;
  txt: THtmlTextArea;
  btn: THtmlSubmitButton;
  edt: THtmlInputField;
begin
  DateStr := fParams.DetailDate;
  Write('<div class="section">'#10);

  WriteFmt('<h1 class="sectiontitle">%s</h1>'#10, [DateStr]);

  Write('<div class="sectioncontent">'#10);

  InitHolidays(StrToInt(LeftStr(DateStr, 4)));
  Appointments := TAptColl.Create;
  try
    if HasDescription(DateStr, Holiday, Appointments) then begin
      if Appointments.Count > 0 then begin
        WriteFmt('<form action="%s/delete"><div>'#10, [CALENDAR_URL]);
      end;
      tbl := THtmlTable.Create;
      try
        if Assigned(Holiday) then begin
          with tbl.AddRow do begin
            AddCell.Content := '&nbsp;';
            AddCell.Content := '&nbsp;';
            with AddCell do begin
              CssClass := 'holiday';
              Content := Holiday.Desc;
            end;
          end;
        end;

        for i := 0 to Appointments.Count - 1 do begin
          with tbl.AddRow do begin
            AddCell.Content := Format('<input type="checkbox" name="appointment" value="%s" />',
              [Appointments[i].TimeDesc]);
            AddCell.Content := Appointments[i].TimeStr;
            with AddCell do begin
              CssClass := 'appointmentdesc';
              Content := Appointments[i].Desc;
            end;
          end;
        end;

        if Appointments.Count > 0 then begin
          with tbl.AddRow do begin
            with AddCell do begin
              ColSpan := 3;
              Content := '<input type="submit" value="Delete" />';
            end;
          end;
        end;

        Write(tbl.Html);
      finally
        FreeAndNil(tbl);
      end;

      if Appointments.Count > 0 then begin
        fParams.AddHiddenFields(self, []);
        WriteFmt('<input type="hidden" name="date" value="%s" />'#10, [DateStr]);
        Write('</div></form>'#10);
      end;
    end else
      Write('<p>no appointments</p>');
  finally
    Appointments.Free;
  end;
  Write('</div>'); // sectioncontent
  Write('</div>'); // section

  Write('<div class="section">'#10);
  Write('<h1 class="sectiontitle">Add Appointment</h1>'#10);
  Write('<div class="sectioncontent">'#10);

  WriteFmt('<form action="%s/add"><div>'#10, [CALENDAR_URL]);

  if not TryIsoDateToDate(DateStr, CurrDate) then begin
    CurrDate := Date;
  end;

  tbl := THtmlTable.Create;
  try
    with tbl.AddRow do begin
      AddCell.Content := 'Year';
      AddCell.Content := 'Month';
      AddCell.Content := 'Day';
    end;

    with tbl.AddRow do begin
      cmb := THtmlCombobox.Create('addyear');
      try
        for i:=-5 to 10 do begin
          cmb.AddOption(IntToStr(YearOf(IncMonth(CurrDate, i * 12))), '', i = 0);
        end;
        AddCell.Content := cmb.Html;
      finally
        FreeAndNil(cmb);
      end;

      cmb := THtmlCombobox.Create('addmonth');
      try
        for i:=1 to 12 do begin
          cmb.AddOption(IntToStr(i), '', i = MonthOf(currDate));
        end;
        AddCell.Content := cmb.Html;
      finally
        FreeAndNil(cmb);
      end;

      cmb := THtmlCombobox.Create('addday');
      try
        for i:=1 to 31 do begin
          cmb.AddOption(IntToStr(i), '', i = DayOf(currDate));
        end;
        AddCell.Content := cmb.Html;
      finally
        FreeAndNil(cmb);
      end;
    end;

    with tbl.AddRow do begin
      AddCell.Content := 'Hour';
      with AddCell do begin
        ColSpan := 2;
        Content := 'Minute';
      end;
    end;

    with tbl.AddRow do begin
      with AddCell do begin
        cmb := THtmlCombobox.Create('addhour');
        try
          cmb.AddOption('none', '', true);
          for i:=0 to 23 do begin
            cmb.AddOption(Format('%.2d', [i]));
          end;
          Content := cmb.Html;
        finally
          FreeAndNil(cmb);
        end;
      end;

      with AddCell do begin
        ColSpan := 2;
        cmb := THtmlCombobox.Create('addminute');
        try
          cmb.AddOption('none', '', true);
          for i:=0 to 59 do begin
            cmb.AddOption(Format('%.2d', [i]));
          end;
          Content := cmb.Html;
        finally
          FreeAndNil(cmb);
        end;
      end;
    end;

    with tbl.AddRow do begin
      with AddCell do begin
        ColSpan := 3;
        Content := 'Description';
      end;
    end;

    with tbl.AddRow do begin
      with AddCell do begin
        ColSpan := 3;
        txt := THtmlTextArea.Create('adddesc', 4, 20);
        try
          Content := txt.Html;
        finally
          FreeAndNil(txt);
        end;
      end;
    end;

    with tbl.AddRow do begin
      with AddCell do begin
        ColSpan := 3;
        btn := THtmlSubmitButton.Create('', 'Add');
        try
          Content := btn.Html;
        finally
          FreeAndNil(btn);
        end;
      end;
    end;

    Write(tbl.Html);
  finally
    FreeAndNil(tbl);
  end;

  fParams.AddHiddenFields(self, []);

  edt := THtmlInputField.Create('date', 'hidden', DateStr);
  try
    Write(edt.Html);
  finally
    FreeAndNil(edt);
  end;

  Write('</div></form>'#10);


  Write('</div>'); // sectioncontent
  Write('</div>'); // section
end;

function TDetailsWriter.Title: string;
begin
  Result := Format('Calendar - %s', [fParams.DetailDate]);
end;

end.


