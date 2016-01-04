unit ImgViewU;

interface

uses
  SysUtils, Types, Classes, Variants, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls;

type
  TForm1 = class(TForm)
    edtPath: TEdit;
    btnGetPath: TButton;
    lstImages: TListBox;
    imgLoadedImg: TImage;
    Label1: TLabel;
    Label2: TLabel;
    procedure btnGetPathClick(Sender: TObject);
    procedure edtPathChange(Sender: TObject);
    procedure imgLoadedImgDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure imgLoadedImgDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lstImagesDblClick(Sender: TObject);
    procedure lstImagesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

function FixPath(const Path: String): String;
begin
  Result := Path;
  if not IsPathDelimiter(Result, Length(Result)) then
    Result := Result + PathDelim;
end;

procedure TForm1.btnGetPathClick(Sender: TObject);
var
  Path: String;
const
{$ifdef MSWINDOWS}
  Root = '';
{$endif}
{$ifdef LINUX}
  Root = '/';
{$endif}
begin
  if SelectDirectory('Locate image directory', Root, Path) then
    edtPath.Text := FixPath(Path)
end;

procedure TForm1.edtPathChange(Sender: TObject);
var
  Path: String;
  RetVal: Integer;
  SR: TSearchRec;
begin
  Path := FixPath((Sender as TEdit).Text) + '*.bmp';
  RetVal := FindFirst(Path, faArchive, SR);
  if RetVal = 0 then
    try
      lstImages.Clear;
      while RetVal = 0 do
      begin
        lstImages.Items.Add(SR.Name);
        RetVal := FindNext(SR)
      end
    finally
      FindClose(SR)
    end;
end;

procedure TForm1.lstImagesDblClick(Sender: TObject);
begin
  imgLoadedImg.Picture.LoadFromFile(
    FixPath(edtPath.Text) + lstImages.Items[lstImages.ItemIndex])
end;

procedure TForm1.lstImagesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  //Check this is a listbox left mouse button event
  if (Sender is TCustomListBox) and (Button = mbLeft) then
    with TCustomListBox(Sender) do
      //Verify mouse is over a listbox item
      if ItemAtPos(Point(X, Y), True) <> -1 then
        //Start a non-immediate drag operation
        BeginDrag(False)
end;

procedure TForm1.imgLoadedImgDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source = lstImages
end;

procedure TForm1.imgLoadedImgDragDrop(Sender, Source: TObject; X,
  Y: Integer);
begin
  lstImagesDblClick(lstImages);
end;

end.
 