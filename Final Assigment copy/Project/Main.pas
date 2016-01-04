unit Main;
//# BEGIN TODO author name, id.nr., date for unit header
{
Etienne van Delden
0618959
01-06-2007
}
//# END TODO

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, Grids,
  Base, PuzzleIO, ComCtrls;

type
  TForm1 = class(TForm)
    PiecesGrid: TStringGrid;
    BoxGrid: TDrawGrid;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    PrintSetup1: TMenuItem;
    Print1: TMenuItem;
    N2: TMenuItem;
    SaveAs1: TMenuItem;
    Save1: TMenuItem;
    Open1: TMenuItem;
    New1: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject);
    procedure Open1Click(Sender: TObject);

    procedure PiecesGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure BoxGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);

    procedure PiecesGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PiecesGridStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure PiecesGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure PiecesGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PiecesGridEndDrag(Sender, Target: TObject; X, Y: Integer);

    procedure BoxGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure BoxGridStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure BoxGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure BoxGridDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure BoxGridEndDrag(Sender, Target: TObject; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    FPuzzle: TPuzzle; // the puzzle being manipulated

    // Globals to support dragging of a piece
    FDragOldPosition: TPosition; // position of piece at start of drag
    FDragPlacement: TPlacement; // piece and orientation being dragged
    FDragDelta: TPosition; // rel. pos. of unit square of piece under cursor
    FDragBitmap: TBitmap; // image of piece/orientation being dragged
    FDragHotSpot: TPoint;

    procedure DoReadPuzzle(AFileName: String);
    procedure PlacementInPiecesGrid(APlacement: TPlacement; Remove: Boolean);
    procedure ClearPiecesGrid;
    procedure FillPiecesGrid;
    procedure NextOrientationInPiecesGrid(APlacement: TPlacement);
    procedure PlacePieceInBox(const APieceName: String;
      AOrientationIndex: Integer; ARowIndex, AColIndex: Integer);
    procedure UpdatePiecesGrid;
    procedure UpdateBoxGrid;
    procedure UpdateViews;
  end;

  TMyDragObject =
  class(TDragControlObjectEx)
  private
    FDragImageList: TDragImageList;
  public
    function GetDragImages: TDragImageList; override;
  end;


const
  fnDefaultPuzzleFolder = '..\Puzzles';
  fnDefaultPuzzleFileName = 'SimplePuzzle.txt';

var
  Form1: TForm1;

implementation //==============================================================

uses
  Math;

{$R *.dfm}

// The following function is used to create a bitmap from parameter APlacement.
// The bitmap can be attached to the cursr when dragging a piece.
//The drawing attributes are taken from parameter AGrid.
function PlacementToBitmap(APlacement: TPlacement; AGrid: TDrawgrid): TBitmap;
var
  VPositionList: TPositionList;
  VRow, VCol, VMaxRow, VMaxCol: Integer;
  VRect: TRect;
  J: Integer;
  VBitmap: TBitmap;
  VPieceColor, VBackgroundColor: TColor;
  VColWidth, VRowheight, VGridLineWidth: Integer;
begin
  // get the position list from APlacement
  VPositionList := APlacement.FOrientation.PositionList;

  // determine the maximum row and column index (0-based) of the bounding grid
  VMaxRow := -1;
  VMaxCol := -1;
  for J := 0 to length(VPositionList) - 1 do
    with VPositionList[J] do
    begin
      VMaxRow := Max(VMaxRow, Row);
      VMaxCol := Max(VMaxCol, Col)
    end;

  // determine values of necessary drawing attributes
  VPieceColor := APlacement.FPiece.Color;
  VBackgroundColor := Agrid.Color;
  VColWidth := AGrid.DefaultColWidth;
  VRowHeight := AGrid.DefaultRowHeight;
  VGridLineWidth := AGrid.GridLineWidth;

  // create bitmap and set sizes and drawing attributes
  VBitmap := TBitmap.Create;
  with VBitMap, Canvas do
  begin
    Width := (VMaxCol + 1) * (VColWidth + VGridLineWidth);
    Height := (VMaxRow + 1) * (VRowHeight + VGridLineWidth);
    Pen.Width := VGridLineWidth;

    // draw background cells
    Brush.Color := VBackgroundColor;
    for VCol := 0 to VMaxCol do
      for VRow := 0 to VMaxRow do
      begin
         with VRect do
         begin
           Left   := VCol * (VColWidth + VGridLineWidth);
           Right  := Left +  VColWidth + VGridLineWidth;
           Top    := VRow * (VRowHeight + VGridLineWidth);
           Bottom := Top  +  VRowHeight + VGridLineWidth;
         end;
         FillRect(VRect);
      end{for};

    // draw piece cells
    Brush.Color := VPieceColor;
    for J := 0 to Length(VPositionList) - 1 do
      with VPositionList[J], VRect do
      begin
           Left   := Col  * (VColWidth + VGridLineWidth);
           Right  := Left +  VColWidth + VGridLineWidth;
           Top    := Row  * (VRowHeight + VGridLineWidth);
           Bottom := Top  +  VRowHeight + VGridLineWidth;

        FillRect(VRect);
      end;{with}

  end{with VBitmap, Canvas};

  SetLength(VPositionList, 0); // free VPositionList

  Result := VBitmap;
end;


{ TForm1 }

procedure TForm1.PlacementInPiecesGrid(APlacement: TPlacement; Remove: Boolean);
  // Removes APlacement if Remove, else adds APlacement from/to PiecesGrid
var
  VPositionList: TPositionList; // to capture result of CoverP
  J: Integer; // to traverse APositionList
begin
  VPositionList := CoverP(APlacement);

  for J := 0 to Length(VPositionList) - 1 do
    with VPositionList[J] do
    begin
      if Remove
      then PiecesGrid.Objects[Col, Row] := nil
      else PiecesGrid.Objects[Col, Row] := APlacement;
    end; { with }

  Finalize(VPositionList);
end;

procedure TForm1.ClearPiecesGrid;
  // Free all placements on PiecesGrid
var
  ColIndex, RowIndex: Integer; // to traverse PiecesGrid.Objects
  VPlacement: TPlacement; // in PiecesGrid at Col, Row
begin
  with PiecesGrid do
  begin

    for RowIndex := 0 to RowCount - 1 do
      for ColIndex := 0 to ColCount - 1 do
      begin
        VPlacement := Objects[ColIndex, RowIndex] as TPlacement;
        if VPlacement <> nil
        then PlacementInPiecesGrid(VPlacement, True);
        VPlacement.Free;
        Cells[Col, Row] := '';
      end; { for RowIndex }
    end; { with }
end;

procedure TForm1.FillPiecesGrid;
var
  VPos: TPosition; // current position in PiecesGrid
  VRowCount, VColCount: Integer; // to determine size of PiecesGrid
  I: Integer; // to traverse the pieces
  VPiece: TPiece; // piece being placed
  VOrientation: TOrientation; // first orientation of VPiece
  VPieceSize: Integer; // max width of VPiece orientation
  VPlacement: TPlacement; // of VPiece
begin
  VColCount := 1;
  VRowCount := 1;
  VPos.Col := 1;
  VPos.Row := 2; // constant

  // calculate placements for pieces in PiecesGrid, depending on their max size
  with FPuzzle.Stock do
  begin
    for I := 0 to Count - 1 do
    begin
      VPiece := GetPiece(I);
      VOrientation := VPiece.GetOrientation(0); // initial orientation
      VPlacement := TPlacement.Create(VPiece, VOrientation, VPos);

      // determine size of square bounding box for all orientations
      VPieceSize := Max(VOrientation.MaxCol + 1, VOrientation.MaxRow + 1);
      VPos.Col := VPos.Col + VPieceSize + 1;
      // update dimensions of PiecesGrid
      VRowCount := Max(VRowCount, 2 + VPieceSize + 1);
      VColCount := VPos.Col;

      // set dimensions for Piecesgrid
      PiecesGrid.ColCount := VColCount;
      PiecesGrid.RowCount := VRowCount;

      // assign placement to corresponding cells in PiecesGrid,
      PlacementInPiecesGrid(VPlacement, False);

      // write name on top
      PiecesGrid.Cells[VPos.Col - 1 - VPieceSize, 0] := VPlacement.FPiece.Name;
    end; { for I }

  end; { with }
end;

procedure TForm1.NextOrientationInPiecesGrid(APlacement: TPlacement);
var
  I: Integer; // to traverse orientations of APlacement.FPiece
  VIndex: Integer; // to determine index of next orientation
begin
  // remove references to old placement from PiecesGrid
  PlacementInPiecesGrid(APlacement, True);

  // determine new orientation as successor of old orientation
  // in orientation list of piece
  with APlacement, FPiece do
  begin
    VIndex := -1;

    for I := 0 to OrientationCount - 1 do
      if GetOrientation(I) = FOrientation
      then VIndex := (I + 1) mod OrientationCount;

    FOrientation := GetOrientation(VIndex);
  end; {with APlacement, FPiece}

  // put references to new placement in PiecesGrid
  PlacementInPiecesGrid(APlacement, False);
end;


procedure TForm1.DoReadPuzzle(AFileName: String);
begin
  ClearPiecesGrid;
  FPuzzle.Free;
  FPuzzle := ReadPuzzle(AFileName);
end;


procedure TForm1.UpdateBoxGrid;
begin
  BoxGrid.RowCount := FPuzzle.Box.RowCount;
  BoxGrid.ColCount := FPuzzle.Box.ColCount;
  BoxGrid.Refresh;
end;

procedure TForm1.UpdatePiecesGrid;
begin
  PiecesGrid.Refresh;
end;

procedure TForm1.UpdateViews;
begin
  UpdatePiecesGrid;
  UpdateBoxGrid;
end;

procedure TForm1.PlacePieceInBox(const APieceName: String;
  AOrientationIndex: Integer; ARowIndex, AColIndex: Integer);
var
  VPiece: TPiece; // piece to be placed in box, name is APieceName
  VOrientation: TOrientation; // orientation of VPiece for AOrientationIndex
  VPosition: TPosition; // position in box at ARowIndex, AColIndex
  VPlacement: TPlacement; // (VPiece, VOrientation, VPosition)
begin
  VPiece := FPuzzle.Stock.FindPiece(APieceName);
  VOrientation := VPiece.GetOrientation(AOrientationIndex);
  VPosition.Row := ARowIndex;
  VPosition.Col := AColIndex;
  VPlacement := TPlacement.Create(VPiece, VOrientation, VPosition);
  FPuzzle.DoPlacement(VPlacement);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer; // to traverse controls of this form
begin
  ChDir(fnDefaultPuzzleFolder);
  DoReadPuzzle(fnDefaultPuzzleFileName);
  FillPiecesGrid;

// Temporarily, for testing purposes ------------------------------------------
// Assume: Simple Puzzle is loaded
    PlacePieceInBox('A', 0, 0, 1);
    PlacePieceInBox('B', 1, 0, 0);
//-----------------------------------------------------------------------------

  // Make sure that a drag image is shown when the mouse moves over Form1,
  // or one of its embedded control components
  ControlStyle := ControlStyle + [csDisplayDragImage];
  for I := 0 to ControlCount - 1 do
    with Controls[I] do
      ControlStyle := ControlStyle + [csDisplayDragImage];

  UpdateViews;
end;

procedure TForm1.FormClose(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    DoReadPuzzle(OpenDialog1.FileName);
    FillPiecesGrid;
    UpdateViews;
  end; { if }
end;

procedure TForm1.PiecesGridDrawCell(Sender: TObject; ACol, ARow: Integer;
            Rect: TRect; State: TGridDrawState);
var
  VPiece: TPiece; // piece at ARow, ACol
  VColor: TColor; // color of VPiece
  VMultiplicity: Integer; // multiplicity as shown in PiecesGrid
begin
  with PiecesGrid do
  begin
    if Objects[ACol, ARow] <> nil then // cell covered by a piece
    begin
      VPiece := (Objects[ACol, ARow] as TPLacement).FPiece;
      // determine multiplicity as shown in PiecesGrid
      VMultiplicity := VPiece.Multiplicity;
      if (FDragPlacement <> nil) and (FDragPlacement.FPiece = VPiece)
      then Dec(VMultiplicity);
      // determine color
      if VMultiplicity > 0
      then VColor := VPiece.Color
      else VColor := clLtGray;
    end
    else begin
      VColor := clWhite;
    end;

    if Cells[ACol, ARow] = '' then // cell without string
    begin
      // draw cell
      Canvas.Brush.Color := VColor;
      Canvas.FillRect(Rect);
    end;
  end; { with }
end;

procedure TForm1.BoxGridDrawCell(Sender: TObject; ACol, ARow: Integer;
            Rect: TRect; State: TGridDrawState);
var
  VColor: TColor; // color for grid cell
  VState: TCellState; // cell state at ARow, ACol
begin
  with FPuzzle.Box do
  begin
    if ValidCoordinates(ARow, ACol) then
    begin
      VState := FPuzzle.Box.CellState(ARow, ACol);
      case VState of
        csFree:
          begin
            VColor := clWhite;
          end;
        csOccupied:
          begin
            VColor := FPuzzle.Box.CellOccupant(ARow, ACol).FPiece.Color;
          end;
        csBlocked:
          begin
            VColor := clBlack;
          end;
      end;{case}
     end;{if}
    end;{with}

  BoxGrid.Canvas.Brush.Color := VColor;
  BoxGrid.Canvas.FillRect(Rect);

  // draw edges, depending on whether neighbors belong to same placement
//  BoxGrid.Canvas.Brush.Color := clBlack;
//  BoxGrid.Canvas.FrameRect(Rect); // test
end;

procedure TForm1.PiecesGridMouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
var
  VCol, VRow: Integer;    // to determine cell coordinates of mouse down
  VPlacement: TPlacement; // placement of the selected cell
  VLeftTopCell: TRect;    // lefttop cell of bounding grid of piece
  VDummyPos: TPosition;
begin
  with Sender as TStringGrid do
  begin
    // determine which cell was clicked
    MouseToCell(X, Y, VCol, VRow);
    // check if click inside grid
    if not ( (0 <= VCol) and (VCol < ColCount) and (0 <= VRow) and (VRow < RowCount) )
    then Exit;
    VPlacement := Objects[VCol, VRow] as TPlacement;
    if VPlacement = nil then Exit;

    // clicked on a piece
    case Button of
      mbLeft:
        begin
          if VPlacement.FPiece.Multiplicity > 0 then
          begin
            FDragOldPosition := VPlacement.FPosition;
            VDummyPos.Row := -1;
            VDummyPos.Col := -1;
            FDragPlacement :=
              TPlacement.Create(
                VPlacement.FPiece,
                VPlacement.FOrientation,
                VDummyPos);                // set by BoxGridDragOver

            FDragDelta.Row := VRow - FDragOldPosition.Row;
            FDragDelta.Col := VCol - FDragOldPosition.Col;

            // determine the hotspot coordinates
            VLeftTopCell := CellRect(FDragOldPosition.Col, FDragOldPosition.Row);
            FDragHotSpot := Point(X - VLeftTopCell.Left, Y - VLeftTopCell.Top);

            FDragBitmap := PlacementToBitmap(FDragPlacement, Sender as TDrawGrid);

            UpdatePiecesGrid;
            BeginDrag(False);
          end;
        end; { mbLeft }
      mbRight:
        begin
          // determine next orientation of piece and update PiecesGrid
          NextOrientationInPiecesGrid(Objects[VCol, Vrow] as TPlacement);
          UpdatePiecesGrid;
        end; { mbRight }
    else // other modifiers
      // ignore
    end; { case }
  end; { with Sender }
end;

procedure TForm1.PiecesGridStartDrag(Sender: TObject;
            var DragObject: TDragObject);
begin
  DragObject := TMyDragObject.Create(Sender as TStringGrid);
end;

procedure TForm1.PiecesGridDragOver(Sender, Source: TObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
begin
//# BEGIN TODO statements to determine acceptability of drop on PiecesGrid
 // Replace this line by your text.
//# END TODO
end;

procedure TForm1.PiecesGridDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
//# BEGIN TODO statements to execute drop on PiecesGrid
//  Replace this line by your text.
//# END TODO
end;

procedure TForm1.PiecesGridEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Target = nil then // drag was cancelled
  begin
    FreeAndNil(FDragPlacement);
  end;
  UpdatePiecesGrid;
end;


procedure TForm1.BoxGridMouseDown(Sender: TObject; Button: TMouseButton;
            Shift: TShiftState; X, Y: Integer);
var
  VCol, VRow: Integer;    // to determine cell coordinates of mouse down
  VPlacement: TPlacement; // placement of the selected cell
  VDummyPos: TPosition;
  VLeftTopCell: TRect;    // lefttop cell of bounding grid of piece
begin
  with Sender as TDrawGrid do
  begin
    // determine which cell was clicked
    MouseToCell(X, Y, VCol, VRow);

    if not FPuzzle.Box.ValidCoordinates(VRow, VCol) then Exit;
    VPlacement := FPuzzle.Box.CellOccupant(VRow, VCol);
    if VPlacement = nil then Exit;

    // clicked on a piece
    case Button of
      mbLeft:
        begin
          // cf. PiecesGridMouseDown
//# BEGIN TODO statements to initiate drag from BoxGrid
 // Replace this line by your text.
//# END TODO
        end
    else // other modifiers
      // ignore
    end; { case }
  end; { with Sender }
end;

procedure TForm1.BoxGridStartDrag(Sender: TObject;
            var DragObject: TDragObject);
begin
  DragObject := TMyDragObject.Create(Sender as TDrawGrid);
end;

procedure TForm1.BoxGridDragOver(Sender, Source: TObject; X, Y: Integer;
            State: TDragState; var Accept: Boolean);
var
  VCol, VRow: Integer;
  VPosition: TPosition;
begin
  if FDragPlacement <> nil then
  begin // a piece is being dragged
    with Sender as TDrawGrid do
    begin
      // determine cell under mouse
      MouseToCell(X, Y, VCol, VRow);

      // correct with relative position contained in FDragDelta
      VPosition.Row := VRow - FDragDelta.Row;
      VPosition.Col := VCol - FDragDelta.Col;

      // acceptance test
      FDragPlacement.FPosition := VPosition;
      Accept := FPuzzle.CanDoPlacement(FDragPlacement);
    end{with}
  end
  else
    Accept := False;
end;

procedure TForm1.BoxGridDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  // FDragPlacement.FPosition was filled in by BoxGridMouseOver
  FPuzzle.DoPlacement(FDragPlacement);
  FDragPlacement := nil;
  UpdateBoxGrid;
end;

procedure TForm1.BoxGridEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
//# BEGIN TODO statements to end dragging from BoxGrid
//  Replace this line by your text.
//# END TODO
end;


{ TMyDragObject }

function TMyDragObject.GetDragImages: TDragImageList;
begin
  with Form1 do
  begin
    if not Assigned(FDragImageList)
    then FDragImageList := TDragImageList.Create(nil);
    FDragImageList.Clear;
    FDragImageList.Height := FDragBitmap.Height;
    FDragImageList.Width := FDragBitmap.Width;
    FDragImageList.AddMasked(FDragBitmap, clWhite);
    FDragImageList.SetDragImage(0, FDragHotSpot.X, FDragHotSpot.Y);
    Result := FDragImageList;
  end;
end;

end.
