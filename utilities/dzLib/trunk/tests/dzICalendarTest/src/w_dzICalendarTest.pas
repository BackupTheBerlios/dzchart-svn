unit w_dzICalendarTest;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ComCtrls;

type
  TForm1 = class(TForm)
    b_Start: TButton;
    lv_Events: TListView;
    procedure b_StartClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  u_dzFileUtils,
  u_dzVclUtils,
  u_dzICalendar;

procedure TForm1.b_StartClick(Sender: TObject);
const
// IcsFile = 'feiertage_deutschland-2010-2013.ics';
  IcsFile = 'example.ics';
var
  ICal: TdzICalendar;
  i: Integer;
  li: TListItem;
  Event: TdzICalendarEvent;
begin
  ICal := TdzICalendar.Create;
  try
    ICal.LoadFromFile(itpd(TApplication_GetExePath) + IcsFile);
    lv_Events.Items.BeginUpdate;
    try
      for i := 0 to ICal.EventCount - 1 do begin
        Event := ICal.Events[i];
        li := lv_Events.Items.Add;
        if Event.DTStart.IsValid then
          li.Caption := string(Event.DTStart)
        else
          li.Caption := '';
        if Event.DTEnd.IsValid then
          li.SubItems.Add(string(Event.DTEnd))
        else
          li.SubItems.Add('');
        if Event.Duration.IsValid then
          li.SubItems.Add(string(Event.Duration))
        else
          li.SubItems.Add('');
        li.SubItems.Add(Event.Summary);
        li.SubItems.Add(Event.Description);
      end;
    finally
      lv_Events.Items.EndUpdate;
    end;
  finally
    FreeAndNil(ICal);
  end;
end;

end.

