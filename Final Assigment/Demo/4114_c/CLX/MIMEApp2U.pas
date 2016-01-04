unit MIMEApp2U;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    ListBox1: TListBox;
    Label1: TLabel;
    Image1: TImage;
    procedure SharedDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure SharedDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.xfm}

procedure TForm2.SharedDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
{var
  Formats: TStrings;}
begin
  Accept := True;
  {Formats := TStringList.Create;
  try
    SupportedDragFormats(Formats);
    Accept :=
      (Formats.IndexOf('text/plain') <> -1) or
      (Formats.IndexOf(SDelphiBitmap) <> -1)
  finally
    Formats.Free
  end;}
end;

procedure TForm2.SharedDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  MS: TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    //Display supported MIME formats in listbox
    ListBox1.Clear;
    SupportedDragFormats(ListBox1.Items);
    if SaveDragDataToStream(MS, 'text/plain') then
    begin
      MS.Position := 0;
      Memo1.Lines.LoadFromStream(MS);
    end;
    //Reset memory stream
    MS.SetSize(0);
    if SaveDragDataToStream(MS, 'image/bmp') then
    begin
      MS.Position := 0;
      Image1.Picture.Bitmap.LoadFromStream(MS);
    end;
  finally
    MS.Free;
  end;
end;

end.
