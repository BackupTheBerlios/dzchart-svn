unit u_AppointmentsTest;

{$optimization off}

interface

uses
  SysUtils,
  u_Appointments;

procedure Main;

implementation

procedure Main;
var
  Apts: TAptColl;
  Idx: integer;
  Apt: TAppointment;
begin
  Apts := TAptColl.Create;
  Apts.Duplicates := true;
  Apts.AddAppointment(EncodeDate(2005,05,05), -1, 'twm''s 39. Geburtstag');
  Apts.AddAppointment(EncodeDate(2004,05,05), -1, 'twm''s 38. Geburtstag');
  Apts.AddAppointment(EncodeDate(2003,05,05), -1, 'twm''s 37. Geburtstag');
  Apts.AddAppointment(EncodeDate(2003,05,05), EncodeTime(12,00,00,0), 'twm''s 37. Geburtstag, 12 Uhr Mittags');
  Assert(Apts.Count = 4);
  Assert(Apts[0].DateTimeStr = '2003-05-05');
  Assert(Apts[1].DateTimeStr = '2003-05-05 12:00');
  Assert(Apts[2].DateTimeStr = '2004-05-05');
  Assert(Apts[3].DateTimeStr = '2005-05-05');

  Assert(Apts.Search('2003-05-05', Idx));
  Assert(Idx = 0);
  Assert(Apts.Search('2003-05-05 12:00', Idx));
  Assert(Idx = 1);
  Assert(Apts.Search('2004-05-05', Idx));
  Assert(Idx = 2);
  Assert(Apts.Search('2005-05-05', Idx));
  Assert(Idx = 3);

  Assert(not Apts.Search('2003-05-05 11:00', Idx));
  Assert(Idx = 1);

  WriteLn('Tests passed');
end;

end.
