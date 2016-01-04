unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Buttons;

type
  TForm1 = class(TForm)
    PathEdit: TEdit;
    BrowsePathButton: TButton;
    Image1: TImage;
    ListBox1: TListBox;
    procedure BrowsePathButtonClick(Sender: TObject);
    procedure PathEditChange(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Image1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Image1DragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  FileCtrl;

{$R *.dfm}

function fixPath(const Path: String): String;
begin
  Result := Path;
  if Result[Length(Result)] <> '\'
  then Result := Result + '\';
end;



procedure TForm1.BrowsePathButtonClick(Sender: TObject);
var
  VPath: string;
begin
  if SelectDirectory('Locate image directory', '', VPath)
  then PathEdit.Text := VPath;
end;

procedure TForm1.PathEditChange(Sender: TObject);
var
  VPath: String;
  VRetVal: Integer;
  VRec: TSearchRec;
begin
  VPath := FixPath((Sender as TEdit).Text) + '*.bmp';
  VRetVal := FindFirst(VPath, faAnyFile, VRec);
  if VRetVal = 0 then
    try
      ListBox1.Clear;
      while VRetVal = 0 do
      begin
        ListBox1.Items.Add(VRec.Name);
        VRetVal := FindNext(VRec);
      end{while}
    finally
      FindClose(VRec)
    end{try}
end;

procedure TForm1.ListBox1DblClick(Sender: TObject);
var
  VFileName: string;
begin
  with ListBox1
  do VFileName := Items[ItemIndex];

  Image1.Picture.LoadFromFile( FixPath(PathEdit.Text) + VFileName );
end;

procedure TForm1.ListBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  { Check this is a listbox left mouse button event }
  if (Sender is TCustomListBox) and (Button = mbLeft) then
    with TCustomListBox(Sender) do
      { Verify mouse is over a listbox item }
      if ItemAtPos(Point(X, Y), True) <> -1 then
        { Start a non-immediate drag operation }
        BeginDrag(False)
end;

procedure TForm1.Image1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source = ListBox1;
end;

procedure TForm1.Image1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  ListBox1DblClick(ListBox1);
end;

end.
