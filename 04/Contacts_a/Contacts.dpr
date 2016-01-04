program Contacts;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  PersonBase in 'PersonBase.pas',
  PersonArray in 'PersonArray.pas',
  PersonStringList in 'PersonStringList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
