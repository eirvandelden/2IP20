program Linked;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  Stack_Array in '..\Units\Stack_Array.pas',
  Queue_Array in '..\Units\Queue_Array.pas',
  AbsStack in '..\Units\AbsStack.pas',
  AbsQueue in '..\Units\AbsQueue.pas',
  Stack_Linked in '..\Units\Stack_Linked.pas',
  Queue_Linked in '..\Units\Queue_Linked.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
