program DisplayTreeProject;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  DisplayTree in 'DisplayTree.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
