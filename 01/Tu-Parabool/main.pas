{ Naam: Etienne van Delden
  Studentnummer: 0618959
  Datum: 01-04-2007}

unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Calculate;

const
     Digit = [ '-', '0' .. '9' ];

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    EditA: TEdit;
    EditB: TEdit;
    EditC: TEdit;
    EditD: TEdit;
    EditW1: TEdit;
    EditW2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Panel1: TPanel;
    UpDown1: TUpDown;
    UpDown2: TUpDown;
    UpDown3: TUpDown;
    procedure EditAChange(Sender: TObject);
    procedure EditBChange(Sender: TObject);
    procedure EditCChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
    procedure calc;                               // calculate graph
    procedure GDrawAxes;                          // draw axes
    procedure GDrawGraphs(A, B, C:Real; Erase:Boolean); // draw graph
    function Scale(S: Real): Real;
    function UnScale(S: Real): Real;
  end; 

var
  Form1: TForm1;
  prev_A: Real;                                   // global var for previous A
  prev_B: Real;                                   // global var for previous B
  prev_C: Real;                                   // global var for previous C

implementation

{ TForm1 }

 // Before we start working, we need to check the input. If we can work with the input
 //   we go on to the calc procudere. We'll exit if not.
 
 
procedure TForm1.EditAChange(Sender: TObject);
  var I: Extended;                                // Helper variable  

begin
  I := 2;                                         // Initialise I
  if TryStrToFloat(EditA.Text, I) then            // Can we make a float out of this Str?
    calc                                          // We can! let's go calculate!
  else
    exit;                                         // Doh! We need valid input!
end;

procedure TForm1.EditBChange(Sender: TObject);
  var I: Extended;
begin
  I := 2;                                         // Initialise I
  if TryStrToFloat(EditB.Text, I) then            // Can we make a float out of this Str?
    calc                                          // We can! let's go calculate!
  else
  exit;                                           // Doh! We need valid input!
end;

procedure TForm1.EditCChange(Sender: TObject);
  var I: Extended;
begin
  I := 2;                                         // Initialise I
  if TryStrToFloat(EditC.Text, I) then            // Can we make a float out of this Str?
    calc                                          // We can! let's go calculate!
  else
    exit;                                         // Doh! We need valid input!
end;


  // This is the Calculation procedure, a.k.a. the Heart of the Program 
procedure TForm1.calc;

var
  b_A: Boolean;                                 // bool: is A <> 0?
  b_D: Boolean;                                 // bool: is 0 <= D ?
  D: Real;                                      // the discriminant
  str_D: String;                                // D as a string
  str_W1: String;                               // W1 as a string
  str_W2: String;                               // W2 as a string
  W1: Real;
  W2: Real;

begin
  GDrawGraphs(prev_A, prev_B, prev_C, True);    // first erase the OLD graph
  GDrawAxes;                                    // redraw the axes just in case

//////////// INITILIASATION /////////////
  b_A := False;                                 // let's assume we A = 0
  b_D := False;                                 // let's assume 0 > D
  D := 0;                                       // reset D
  str_D := ' ';                                 // reset strings
  str_W1 := ' ';
  str_W2 := ' ';
  W1 := 0;                                      // reset W1
  W2 := 0;                                      // reset W2

///////////// Program /////////////
    // We need to calculate the discriminant
  D := Discr( StrToFloat(EditA.Text), StrToFloat(EditB.Text), StrToFloat(EditC.Text));
  str( D, str_D);                                 // save D to a string
  EditD.Text :=  str_D;                           // print the string
    
    // Let's check wether or not we can calculate furhter, utilising the D we just found
  if 0 <= D then
    b_D := true                                   // we can continue
  else
    b_D := false;                                 // woops, D isn't suitable
    
    // Let's check wether or not we can calculate furhter, utilising A   
  if EditA.Text <> '0' then
    b_A := true                                   // we can continue
  else
    b_A:= false;                                 // woops, A isn't suitable

    // Now we can calculate W1 and W2 if and only if both D and A are suitable
    //  in all other cases, we'll just reset W1 and W2
  if (b_D and b_A) then begin
    VKV(StrToFloat(EditA.Text), StrToFloat(EditB.Text), StrToFloat(EditC.Text), W1, W2);
  end
  else begin
    W1 := 0;
    W2 := 0;
  end;

     
  EditW1.Text:= FloatToSTr(W1);                           // print the string of W1
  EditW2.Text:= FloatToStr(W2);                           // print the string of W2

    // All editboxes have been filled, let's redraw the graph
  GDrawGraphs(StrToFloat(EditA.Text), StrToFloat(EditB.Text), StrToFloat(EditC.Text), False);
     
  end;


  // Procedure to draw the axes
procedure TForm1.GDrawAxes;
begin
  with Image1.Canvas do
    begin                                         // draw x-axis
      MoveTo(0,300);
      LineTo(600,300);
                                                  // draw y-axis
      MoveTo(300,0);
      LineTo(300,600);
    end;
end;


  // when we open the form, we need to make something to draw on. 
procedure TForm1.FormCreate(Sender: TObject);
var
   Bitmap: TBitmap;                               // For storage of drawn data
begin
  Bitmap := TBitmap.Create;
  Bitmap.Width := 600;
  Bitmap.Height := 600;
  Image1.Picture.Graphic := Bitmap;

  // Apparently, Lazarus needs a reactanle to draw on. Really weird, but without this
  //  I just don't get a white board to draw on without this.      
  Image1.canvas.rectangle(0,0,600,600);
  GDrawAxes                                         // Let's draw our axes, we haven't done so before
end;

  // The "Draw a given Graph" Procedure
  //  Because the original code neglected to save the old values of A,B and C
  //    I adjusted it so it now saves the values, when we're drawing.
procedure TForm1.GDrawGraphs( A, B, C: Real; Erase: Boolean);
var
   X, Y: Integer;
   OldMode: TPenMode;
begin
     {Save current pen mode; set new penmode}
 with Image1.Canvas.Pen do begin
   OldMode := Mode;
   
   if Erase
   then Mode := pmNotXor
        { Erasure }
   else begin
    Mode := pmCopy;  { Normal drawing}
    prev_A := A;                                  
    prev_B := B;
    prev_C := C;
  end;
end;

 {Find starting point for drawing}
 X := 0;
 Y := Round(Scale(-F(A,B,C,UnScale(X))));
 
 with Image1.Canvas do MoveTo(X, Y);
 {draw}
 while (X < 600) do begin
   X := X+1;
   Y := Round(Scale(-F(A,B,C,UnScale(X))));
   with Image1.Canvas do (LineTo(X,Y));
 end;
 
 {Restore Pen mode}
 Image1.Canvas.Pen.Mode := OldMode;
 
end;

function TForm1.Scale(S: Real): Real;
begin
  Scale := S * 30 + 300;
end;

function TForm1.Unscale(S: Real): Real;
begin
  UnScale := (S-300) /30;
end;

 initialization
  {$I main.lrs}

end.

