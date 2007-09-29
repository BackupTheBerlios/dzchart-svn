program FamilyData;

uses
  Forms,
  w_Family in '..\w_Family.pas' {f_Family};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_Family, f_Family);
  Application.Run;
end.
