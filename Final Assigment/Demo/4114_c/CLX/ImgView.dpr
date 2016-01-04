program ImgView;

uses
  QForms,
  ImgViewU in 'ImgViewU.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Image Viewer';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
