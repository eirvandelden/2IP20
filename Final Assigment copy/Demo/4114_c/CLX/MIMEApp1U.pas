unit MIMEApp1U;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QTypes, QExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Image1: TImage;
    procedure SharedStartDrag(Sender: TObject; var DragObject: TDragObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.SharedStartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    Memo1.Lines.SaveToStream(MS);
    MS.Position := 0;
    if not AddDragFormat('text/plain', MS) then
      raise Exception.Create('Failed to add text drag format');
    //Reset memory stream so it can be re-used
    MS.SetSize(0);
    Image1.Picture.Bitmap.SaveToStream(MS);
    MS.Position := 0;
    if not AddDragFormat('image/bmp', MS) then
      raise Exception.Create('Failed to add bitmap drag format');
  finally
    MS.Free;
  end;
end;

end.
