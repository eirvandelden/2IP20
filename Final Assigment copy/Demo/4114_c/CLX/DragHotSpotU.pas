unit DragHotSpotU;

interface

uses
  QButtons,
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QStdCtrls, QExtCtrls, QTypes;

type
  TControlDragObject = class(TDragControlObject)
  private
    FX, FY: Integer;
    FImgIdx: Integer;
  protected
    function GetDragImageHotSpot: TPoint; override;
    function GetDragImageIndex: Integer; override;
  public
    constructor CreateWithHotSpot(Control: TWinControl; X, Y: Integer);
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure FormDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Button1Click(Sender: TObject);
    procedure Button1StartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure Button1EndDrag(Sender, Target: TObject; X, Y: Integer);
  private
    FDragObject: TControlDragObject;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

{ TControlDragObject }

type
  TControlAccess = class(TControl);

constructor TControlDragObject.CreateWithHotSpot(Control: TWinControl; X,
  Y: Integer);
var
  Bmp: TBitmap;
  TextSize: TSize;
  Text: String;
begin
  inherited Create(Control);
  FX := X;
  FY := Y;
  //Make image and add it to drag image list
  //Make bitmap that is same size as control
  Bmp := TBitmap.Create;
  try
    Bmp.Canvas.Font := TControlAccess(Control).Font;
    //Qt Canvas must not have non-zero size for TextHeight/TextWidth to work
    Bmp.Height := Control.Height;
    Bmp.Width := Control.Width;
    //Draw button face with white background for transparency
    DrawButtonFace(Bmp.Canvas, Rect(0, 0, Bmp.Width, Bmp.Height), 2, False, True, False, clWhite);
    //Write a string on bitmap
    Text := (Control as TButton).Caption;
    TextSize := Bmp.Canvas.TextExtent(Text);
    Bmp.Canvas.TextOut(
      (Bmp.Width - TextSize.cx) div 2,
      (Bmp.Height - TextSize.cy) div 2, Text);
    //Add bitmap to image list, making the grey pixels transparent
    DragImageList.Width := Bmp.Width;
    DragImageList.Height := Bmp.Height;
    FImgIdx := DragImageList.AddMasked(Bmp, clWhite);
  finally
    Bmp.Free
  end
end;

function TControlDragObject.GetDragImageHotSpot: TPoint;
begin
  Result := Point(FX, FY)
end;

function TControlDragObject.GetDragImageIndex: Integer;
begin
  Result := FImgIdx
end;

{ TForm1 }

procedure TForm1.Button1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Shift = [ssAlt] then
    Button1.BeginDrag(True)
end;

procedure TForm1.FormDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source = Button1
end;

procedure TForm1.FormDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  with Button1 do
    SetBounds(X - FDragObject.FX, Y - FDragObject.FY, Width, Height)
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Hello world')
end;

procedure TForm1.Button1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
var
  Pt: TPoint;
begin
  //Get cursor pos
  GetCursorPos(Pt);
  //Make cursor pos relative to button
  Pt := Button1.ScreenToClient(Pt);
  //Pass info to drag object
  FDragObject := TControlDragObject.CreateWithHotSpot(Button1, Pt.X, Pt.Y);
  DragObject := FDragObject;
end;

procedure TForm1.Button1EndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  FreeAndNil(FDragObject)
end;

end.
