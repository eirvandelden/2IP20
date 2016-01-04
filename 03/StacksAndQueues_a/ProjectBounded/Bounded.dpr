program Bounded;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  BoundedQueue in '..\Units\BoundedQueue.pas',
  BoundedStack in '..\Units\BoundedStack.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
