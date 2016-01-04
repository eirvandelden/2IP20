program MIMEApp1;

uses
  QForms,
  MIMEApp1U in 'MIMEApp1U.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
