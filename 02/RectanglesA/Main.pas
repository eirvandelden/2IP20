unit Main;

// Programma 3: Rectangels
// Etienne van Delden
// 0618959
// 11-04-2007


// INTEFACE --------------
interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls,
  Fig0;


type
  TForm1 = class(TForm)
    Panel1: TPanel;
    NewRectangleButton: TButton;
    NewListButton: TButton;
    XLEdit: TEdit;
    StaticText1: TStaticText;
    YLEdit: TEdit;
    XHEdit: TEdit;
    YHEdit: TEdit;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    CountEdit: TEdit;
    StaticText5: TStaticText;
    ClearButton: TButton;
    Image1: TImage;
    edtSelected: TEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure NewListButtonClick(Sender: TObject);
    procedure NewRectangleButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    FList: TRectangleList;
    FSelected: Integer;
    function RandomRectangle: TRectangle;
    function RandomRectangleList: TRectangleList;
    function FindLastWindow(AList: TRectangleList; AX, AY: Integer): Integer;
    procedure ClearImage;
    procedure DrawRectangle(ARectangle: TRectangle; APenColor: TColor;
                ABrushColor: TColor);
    procedure DrawRectangleList;
    procedure DrawSelectedRectangle;
    procedure UpdateViews;
    procedure BoundedBorder;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  BBXL: Integer;
  BBXH: Integer;
  BBYL: Integer;
  BBYH: Integer;

// IMPLEMENTATION --------------  
implementation

uses
  Math;

{$R *.dfm}

{ TForm1 }

// FUNCTION -- Give a random (valid) retangle
function TForm1.RandomRectangle: TRectangle;
var
  VMaxX, VMaxY: Integer;
  VX1, VX2, VY1, VY2: Integer;
begin
  VMaxX := Image1.Width;
  VMaxY := Image1.Height;
  VX1 := Random(VMaxX);
  VX2 := Random(VMaxX);
  VY1 := Random(VMaxY);
  VY2 := Random(VMaxY);
  Result := TRectangle.Create(Min(VX1, VX2), Min(VY1, VY2), Max(VX1, VX2),
              Max(VY1, VY2));
end;

// FUNCTION -- Give multiple random (valid) retangles
function TForm1.RandomRectangleList: TRectangleList;
var
  I, N: Integer;
begin
  Result := TRectangleList.Create;
  N := Random(20);
  for I := 0 to N - 1 do
  begin
    Result.Add(RandomRectangle);
  end{for};
end;

// FUNCTION --
function TForm1.FindLastWindow(AList: TRectangleList; AX, AY: Integer):
           Integer;
var
  I: Integer;
begin
  Result := -1;
  with FList do
  begin
    for I := 0 to Count - 1 do
    begin
      if GetItem(I).Contains(AX, AY)
      then Result := I;
    end{for};
  end{with}
end;

// PROCEDURE --    ( iets met wit tekenen)
procedure TForm1.ClearImage;
begin
  with Image1.Canvas do
  begin
    Brush.Color := clWhite;
    Rectangle(ClipRect);
  end;
end;

// PROCEDURE -- Teken de geven TRectangle
procedure TForm1.DrawRectangle(ARectangle: TRectangle; APenColor, ABrushColor:
            TColor);
begin
  with Image1.Canvas do
  begin
    // draw rectangle
    Pen.Color := APenColor;
    Brush.Color := ABrushColor;
    Rectangle(ARectangle.XL, ARectangle.YL, ARectangle.XH, ARectangle.YH);
  end;
end;

// PROCEDURE -- Teken een gegeven aantal Rectangles
procedure TForm1.DrawRectangleList;
var
  I: Integer;
  VR: TRectangle;
  L : String;
  A : TRect;

begin

  for I := 0 to FList.Count - 1 do
  begin
    VR := FList.GetItem(I);
    DrawRectangle(VR, clBlack, clYellow);
    L:= IntToStr(I);

    A.Left := VR.XL+1;
    A.Right := VR.XH-1;
    A.Top := VR.YH-1;
    A.Bottom := VR.YL+1;

    with Image1.Canvas do begin
       TextRect(A, VR.XL+1, VR.YL+1,  L );
    end;

  end; // for loop


end;

// PROCEDURE -- Teken de geselecteerde Rectangle
procedure TForm1.DrawSelectedRectangle;
var
  VS: TRectangle;
  A: TRect;
begin
  if FSelected <> -1 then
  begin
    VS := FList.GetItem(FSelected);
    DrawRectangle(VS, clBlack, clAqua);
    A.Left := VS.XL+1;
    A.Right := VS.XH-1;
    A.Top := VS.YH-1;
    A.Bottom := VS.YL+1;

    with Image1.Canvas do begin
       TextRect(A, VS.XL+1, VS.YL+1, IntToStr(FSelected) );
    end;
  end;
end;

procedure TForm1.UpdateViews;
var
  VS: TRectangle;
begin
  // Update CountEdit
  CountEdit.Text := IntToStr(FList.Count);

  // Update coordinate edits
  if FSelected = -1
  then
  begin
    XLEdit.Text := '';
    YLEdit.Text := '';
    XHEdit.Text := '';
    YHEDit.Text := '';
  end
  else
  begin
    VS := FList.GetItem(FSelected);
    XLEdit.Text := IntToStr(VS.XL);
    YLEdit.Text := IntToStr(VS.YL);
    XHEdit.Text := IntToStr(VS.XH);
    YHEdit.Text := IntToStr(VS.YH);
  end;

   edtSelected.Text := IntToStr(FSelected);

  // Update image

  ClearImage;
  BoundedBorder;
  DrawRectangleList;
  DrawSelectedRectangle;


end;

// PROCEDURE -- Maak een random aantal Rectangles
procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  FList := RandomRectangleList;
  FSelected := -1;
  UpdateViews;
end;

// PROCEDURE -- Erase de oude Rectangles en maak een random aantal nieuwe aan
procedure TForm1.NewListButtonClick(Sender: TObject);
begin
  FList.Free;
  FList := RandomRectangleList;
  FSelected := -1;
  UpdateViews;
end;

// PROCEDURE -- Voeg een nieuwe Rectangle aan de lijst toe
//                De-selecteer de huidige geselecteerde Rectangle
//                Update de view
procedure TForm1.NewRectangleButtonClick(Sender: TObject);
begin
  FList.Add(RandomRectangle);
  FSelected := FList.Count - 1;
  UpdateViews;
end;

// PROCEDURE -- Maak de lijst met Rectangles leeg
//                De-selecteer de huidige geselecteerde Rectangle
//                Update de view
procedure TForm1.ClearButtonClick(Sender: TObject);
begin
  FList.Clear;
  FSelected := -1;
  UpdateViews;
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FSelected := FindLastWindow(FList, X, Y);
  edtSelected.Text := IntToStr(FSelected);
  UpdateViews;
end;

procedure TForm1.BoundedBorder;

var
  I: Integer;                         // hulp var
  VR: TRectangle;                     // hulp var met rectangles

begin
  if FList.Count = 0 then begin
    exit
  end

  else if Flist.Count = 1 then begin
    BBXL := 999;
    BBXH := 0;
    BBYL := 999;
    BBYH := 0;

    VR := FList.GetItem(0);
    BBXL := Min(BBXL,VR.XL);
    BBXH := Max(BBXH,VR.XH);
    BBYL := Min(BBYL,VR.YL);
    BBYH := Max(BBYH,VR.YH);

    with Image1.Canvas do begin
      // draw rectangle
      Pen.Color := clRed;
      Rectangle(BBXL, BBYL, BBXH, BBYH);
    end;  // end with Canvas
  end // end if

  else if FList.Count > 1 then begin
    BBXL := 999;
    BBXH := 0;
    BBYL := 999;
    BBYH := 0;

    for I := 0 to FList.Count-1 do begin
      VR := FList.GetItem(I);
      BBXL := Min(BBXL,VR.XL);
      BBXH := Max(BBXH,VR.XH);
      BBYL := Min(BBYL,VR.YL);
      BBYH := Max(BBYH,VR.YH);
    end; // end for loop

    with Image1.Canvas do begin
      // draw rectangle
      Pen.Color := clRed;
      Rectangle(BBXL, BBYL, BBXH, BBYH);
    end; // image
  end; // end if
end; //end procedure

end. 
