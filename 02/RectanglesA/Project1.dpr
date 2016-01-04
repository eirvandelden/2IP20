program Project1;

uses
  Forms,
  Main in 'Main.pas' {Form1},
  Fig0 in 'Fig0.pas',
  RandomGen in 'RandomGen.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
