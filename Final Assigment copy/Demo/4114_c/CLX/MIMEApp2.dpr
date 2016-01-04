program MIMEApp2;

uses
  QForms,
  MIMEApp2U in 'MIMEApp2U.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
