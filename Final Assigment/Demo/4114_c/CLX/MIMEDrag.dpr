program MIMEDrag;

uses
  QForms,
  MIMEDrag1U in 'MIMEDrag1U.pas' {Form1},
  MIMEDrag2U in 'MIMEDrag2U.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
