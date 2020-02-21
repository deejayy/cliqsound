program clickety;

uses
  Forms,
  Mainu in 'mainu.pas' {KeyForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TKeyForm, KeyForm);
  Application.Run;
end.
