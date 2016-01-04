program Unbounded;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  Stack in '..\Units\Stack.pas',
  Queue in '..\Units\Queue.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
