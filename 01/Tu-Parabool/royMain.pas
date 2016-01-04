unit Main;
  { Author: Roy A. J. Berkeveld (0608170)
    Date: 2007-03-30
  }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Calculate;

type
  TMainForm = class(TForm)
    Panel: TPanel;
    EditA: TEdit;
    EditB: TEdit;
    EditC: TEdit;
    UpDownA: TUpDown;
    UpDownB: TUpDown;
    UpDownC: TUpDown;
    EditD: TEdit;
    EditW1: TEdit;
    EditW2: TEdit;
    LabelA: TLabel;
    LabelB: TLabel;
    LabelC: TLabel;
    LabelD: TLabel;
    LabelW1: TLabel;
    LabelW2: TLabel;
    Graph: TImage;
    procedure FormCreate(Sender: TObject);
    procedure EditAChange(Sender: TObject);
    procedure EditBChange(Sender: TObject);
    procedure EditCChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure GDrawAxes; // To draw x- an y-axis on the center of the graph
    function Scale(S: Real): Real; // To convert coord to graph
    function UnScale(S: Real): Real; // To revert conversion to graph
    procedure GDrawGraph(A, B, C: Real; Erase: Boolean); // To draw the graph
    procedure UpdateDW1W2; // To update fields D, W1 and W2, and redraw graph
  end;

var
  MainForm: TMainForm;
  A, B, C: Real; // Current term values of the drawn graph

implementation

{$R *.dfm}

procedure TMainForm.FormCreate(Sender: TObject);
  { Execute on the start of the application }
var
  Bitmap: TBitmap; // To store all drawn data
begin
  UpdateDW1W2; // Update fields D, W1 and W2 (according to A, B and C)
  // Initialize the bitmap and associate to Graph
  Bitmap := TBitmap.Create;
  Bitmap.Width := 600;
  Bitmap.Height := 600;
  Graph.Picture.Graphic := Bitmap;
  GDrawAxes; // Draw our axes
end;

procedure TMainForm.GDrawAxes;
  // Draws the x- and y-axis on the graph
begin
  with Graph.Canvas do begin
    // draw x-axis
    MoveTo(0,300);
    LineTo(600,300);
    // draw y-axis
    MoveTo(300,0);
    LineTo(300,600);
  end;
end;

procedure TMainForm.GDrawGraph(A, B, C: Real; Erase: Boolean);
  // Draws the line corresponding to A, B and C in F
var
  X, Y: Integer; // Used to iterate points and store result
  OldMode: TPenMode;
begin
  {Save current pen mode; set new pen mode}
  with Graph.Canvas.Pen do
  begin
    OldMode := Mode;
    if Erase
    then Mode := pmNotXor
         {erasure (see Delphi Help on pmNotXor and TPenMode)}
    else Mode := pmCopy;
         {normal drawing in color specified by Color property}
  end;

  {Find starting point for drawing}
  X := 0;
  Y := Round(Scale(-F(A,B,C,UnScale(X))));
  with Graph.Canvas do MoveTo(X,Y);

  {Draw}
  while (X <> 600) do
  begin
    X := X+1;
    Y := Round(Scale(-F(A,B,C,UnScale(X))));
    with Graph.Canvas do LineTo(X, Y);
  end;

  {Restore pen mode}
  Graph.Canvas.Pen.Mode := OldMode;
end;

function TMainForm.Scale(S: Real): Real;
  { To convert coord to graph }
begin
  Scale := S * 30 + 300;
end;

function TMainForm.UnScale(S: Real): Real;
  { To revert conversion to graph }
begin
  UnScale := (S-300)/30;
end;

procedure TMainForm.UpdateDW1W2;
  { Update the D, W1 and W2 fields according to A, B and C, ánd redraw the
    corresponding graph }
  { Note: I had to hack a bit with the onchange events of the A, B and C
    editboxes. When invalid data is entered, it may not error as the invalid
    data could be part of uncompleted, valid data. This is so that it will
    silently ignore invalid data entry and abort it's update. The tutorial does
    not provide anything on this so I felt forced to implement it as below. }
var
  D, W1, W2: Real; // To store calculated new values before commiting
  ExtA, ExtB, ExtC: Extended; // To temporarily store returned values
begin
  GDrawGraph(A, B, C, True); // Erase previous line (using pre-stored globals)
  GDrawAxes; // Redraw the axes (in case we accidentally erased one)
  // Check if the values from the editboxes are valid Floats (and get them)
  if not (TryStrToFloat(EditA.Text, ExtA) // This is the described hack
    and TryStrToFloat(EditB.Text, ExtB)
    and TryStrToFloat(EditC.Text, ExtC)) then begin
    // Reset A, B and C values (to prevent oddities with erase)
    A := 0;
    B := 0;
    C := 0;
    // Abort this update, as we cannot convert the currently entered data
    exit;
  end;
  // Extract the Real parts from retuned extended A, B and C values
  A := ExtA;
  B := ExtB;
  C := ExtC;
  D := Discr(A, B, C); // Calculate determinant...
  EditD.Text := FloatToStr(D); // ...and display it
  // If W1 and W2 exist we can show them
  if (A <> 0) and (D >= 0) then begin
    VKV(A, B, C, W1, W2);
    EditW1.Text := FloatToStr(W1);
    EditW2.Text := FloatToStr(W2);
  // If not, we should clear these boxes
  end else begin
    EditW1.Text := '';
    EditW2.Text := '';
  end;
  // Draw the function in the graph
  GDrawGraph(A, B, C, False);
end;


procedure TMainForm.EditAChange(Sender: TObject);
  { Update all fields when EditA is changed }
begin
  UpdateDW1W2;
end;

procedure TMainForm.EditBChange(Sender: TObject);
  { Update all fields when EditB is changed }
begin
  UpdateDW1W2;
end;

procedure TMainForm.EditCChange(Sender: TObject);
  { Update all fields when EditC is changed }
begin
  UpdateDW1W2;
end;

end.
