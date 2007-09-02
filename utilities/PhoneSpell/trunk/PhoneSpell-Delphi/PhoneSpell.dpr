program PhoneSpell;

uses
  Forms,
  w_PhoneSpell in 'w_PhoneSpell.pas' {f_PhoneSpell};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_PhoneSpell, f_PhoneSpell);
  Application.Run;
end.
