unit DragImageU;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls;

type
  //Delphi 6/Kylix 1 forces you to go inherit from TDragObject,
  //rather than TDragControlObject, if you want the drag object
  //passed as the Source parameter to OnDragOver and OnDragDrop.
  //However, drag images are catered for by TDragControlObject,
  //so we are forced to use it
  TTextDragObject = class(TDragControlObject)
  private
    FImgIdx: Integer;
    FData: String;
  protected
    function GetDragImageIndex: Integer; override;
  public
    constructor Create(Control: TControl; const Data: String); reintroduce;
    property Data: String read FData;
  end;

  TForm1 = class(TForm)
    Label1: TLabel;
    ListBox1: TListBox;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure Label1StartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure ListBox1StartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure SharedEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    FDragObject: TTextDragObject;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

{ TTextDragObject }

constructor TTextDragObject.Create(Control: TControl; const Data: String);
var
  Bmp: TBitmap;
  Txt: String;
begin
  inherited Create(Control);
  FData := Data;
  Bmp := TBitmap.Create;
  try
    //Make up some string to write on bitmap
    Txt := Format('The control called %s says "%s" at %s',
      [Control.Name, Data, FormatDateTime('h:nn am/pm', Time)]);
    Bmp.Canvas.Font := Form1.Font;
    Bmp.Canvas.Font.Name := 'Arial';
    Bmp.Canvas.Font.Size := 10;
    Bmp.Canvas.Font.Style := Bmp.Canvas.Font.Style + [fsItalic];
    //Give bitmap a non-szero size so we can call TextHeight/TextWidth
    //Qt does not permit this on a zero-sized canvas
    Bmp.Height := 1;
    Bmp.Width := 1;
    Bmp.Height := Bmp.Canvas.TextHeight(Txt);
    Bmp.Width := Bmp.Canvas.TextWidth(Txt);
    //Fill background with white, which will be the transparent colour
    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.FillRect(Rect(0, 0, Bmp.Width, Bmp.Height));
    //Write a string on bitmap
    Bmp.Canvas.TextOut(0, 0, Txt);
    //Add bitmap to image list, making the olive pixels transparent
    DragImageList.Width := Bmp.Width;
    DragImageList.Height := Bmp.Height;
    FImgIdx := DragImageList.AddMasked(Bmp, clWhite);
  finally
    Bmp.Free
  end
end;

function TTextDragObject.GetDragImageIndex: Integer;
begin
  Result := FImgIdx
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListBox1.ItemIndex := 0;
end;

procedure TForm1.Label1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FDragObject := TTextDragObject.Create(Label1, Label1.Caption);
  DragObject := FDragObject;
end;

procedure TForm1.ListBox1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FDragObject := TTextDragObject.Create(ListBox1, ListBox1.Items[ListBox1.ItemIndex]);
  DragObject := FDragObject;
end;

procedure TForm1.SharedEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  //All draggable controls share this event handler
  FDragObject.Free;
  FDragObject := nil
end;

procedure TForm1.Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := True//Source is TTextDragObject
end;

procedure TForm1.Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  (Sender as TPanel).Caption := FDragObject.Data//TTextDragObject(Source).Data
end;

end.
