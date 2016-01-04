unit Solvers;

//------------------------------------------------------------------------------
// This unit defines several Solver classes. A Solver class embodies a
// particular solution strategy.
// All Solver classes descend from the abstract base class TSolver.
// - TDummySolver produces a fixed solution. It is intended for
//   testing and demonstration purposes only.
// - TBacktrackSolver uses a standard backtracking algorithm.
// - TTom is reserved for the smartest solution ;-)

//# BEGIN TODO author name, id.nr., date for unit header
 // Etienne van Delden
 // 0618959
 // 11-06-07
//# END TODO

interface

uses
  Classes, Dialogs, SysUtils,
  Base;

type
  // TSolver -------------------------------------------------------------------
  // TSolver is the abstract base class for all Solver classes.
  // It refers to a TPuzzle object, the puzzle to be solved.
  // - FindAll finds all solutions
  // There are several hooks for evoking actions, realized by callbacks (the
  // mechanism is the same as the event handling mechanism, and uses the
  // event type TNotifyEvent):
  // - Every time a solution is found, the OnFound procedure is called, if
  //   assigned.
  // - Every time a piece is placed, the OnPiecePlaced procedure is called, if
  //   assigned.
  // - Every time a piece is found, the OnPieceremoved procedure is called, if
  //   assigned.
  //----------------------------------------------------------------------------

  TSolver =
  class(TObject)
  protected
    FPuzzle: TPuzzle; // the (partially solved) puzzle to be solved fully
    FSolutionCount: Integer; // number of solutions found
    FOnFound: TNotifyEvent; // to signal that a solution was found
    FOnPiecePlaced: TNotifyEvent; // to signal that a piece was placed
    FOnPieceRemoved: TNotifyEvent; // to signal that a piece was removed
    procedure SolutionFound;
  public
    // construction/destruction ---------------------------
    constructor Create(APuzzle: TPuzzle);
      { create a solver for APuzzle, calling OnFound for each solution }

    // commands -------------------------------------------
    procedure FindAll; virtual;
    // pre:  FPuzzle <> nil
    // post: all solutions have been found, and
    //       SolutionFound has been called for each of them.
    //       When assigned, OnPiecePlaced and OnPieceRemoved are called
    //       for each piece placed/removed respectively.

    property Puzzle: TPuzzle read FPuzzle write FPuzzle;
    property SolutionCount: Integer read FSolutionCount;
    property OnFound: TNotifyEvent read FOnFound write FOnFound;
    property OnPiecePlaced: TNotifyEvent read FOnPiecePlaced write FOnPiecePlaced;
    property OnPieceRemoved: TNotifyEvent read FOnPieceRemoved write FOnPieceRemoved;
  end;

  TDummySolver =
  class(TSolver)
  public
    procedure FindAll; override;
  end;

  TBacktrackSolver =
  class(TSolver)
  protected
    procedure EmptyCell(var Solved: Boolean; var APosition: TPosition);
      { pre: true
        post: Solved = all cells are nonempty,
              if not Solved, then cell APosition is empty }
    procedure Search;
      { pre: state = S
        post: all solutions that are an extension of the current state
              have been processed (SolutionFound called),
              piece placement/removal has been properly notified,
              state = S  }
  public
    // commands -------------------------------------------
    procedure FindAll; override;
  end;

implementation //===============================================================

{ TSolver }

constructor TSolver.Create(APuzzle: TPuzzle);
begin
  inherited Create;
  FPuzzle := APuzzle;
  FSolutionCount:= 0;
end;

procedure TSolver.SolutionFound;
begin
  FSolutionCount := FSolutionCount + 1;
  if Assigned(FOnFound)
  then FOnFound(Self);
end;

procedure TSolver.FindAll;
begin
  Assert(Assigned(FPuzzle),
    'TSolver.pre failed: Puzzle property has not been assigned to');
  FSolutionCount := 0;
end;


{ TDummySolver }

procedure TDummySolver.FindAll;
var
  VPiece: TPiece; // piece for VPlacement
  VPosition: TPosition; // position for VPlacement
  VPlacement: TPlacement; // to put into box
begin
  inherited FindAll;
  with FPuzzle do
  begin
    VPiece := FPuzzle.Stock.FindPiece('A');
    VPosition.Col := 2;
    VPosition.Row := 0;
    VPlacement := TPlacement.Create(VPiece, VPiece.GetOrientation(0), VPosition);
    FPuzzle.DoPlacement(VPlacement);

    VPiece := FPuzzle.Stock.FindPiece('B');
    VPosition.Col := 0;
    VPosition.Row := 0;
    VPlacement := TPlacement.Create(VPiece, VPiece.GetOrientation(1), VPosition);
    FPuzzle.DoPlacement(VPlacement);

    VPiece := FPuzzle.Stock.FindPiece('C');
    VPosition.Col := 1;
    VPosition.Row := 0;
    VPlacement := TPlacement.Create(VPiece, VPiece.GetOrientation(0), VPosition);
    FPuzzle.DoPlacement(VPlacement);
  end;

  SolutionFound;
end;


{ TBacktrackSolver }



procedure TBacktrackSolver.EmptyCell(var Solved: Boolean; var APosition: TPosition);

//# BEGIN TODO implementation block for TBackTrackSolver.EmptyCell

var
	Found: boolean;                                 // zolang we geen leeg vakje hebben gevonden
	i_Row: Integer;                                 // hulp var om rijen bij te houden
	i_Col: Integer;                                 // hulp var om rijen bij te houden

begin
 // Empty cell krijgt een bool solved en aan positie mee. Als de solved true is, is de puzzel opgelost en is er dus geen lege positie
 // is de bool false, dan is de puzzel niet opgelost en geeft hij de eerst volgende lege  blokje terug


////// INITIALISATION //////////////////////////////
 APosition.Col := 0;                              // we gaan er van uit dat we niks vinden
 APosition.Row := 0;
 Solved := True;

 i_Col := 0;
 i_Row := 0;
 Found := False;


while (not Found) and (i_Row < FPuzzle.Box.RowCount)  do //zolang we nog niets gevonden hebben en niet te ver zijn gegaan
begin

  while (not Found) and (i_Col < FPuzzle.Box.ColCount) do //zolang we nog niets gevonden hebben en niet te ver zijn gegaan
  begin

    // als we een leeg stukje vinden, dan wordt Found true (en breekt de while), geef de juist waarden terug
    if (FPuzzle.Box.CellState(i_Row, i_Col) = csFree) then begin
      Solved := False;
		  Found := True;
		  APosition.Col := i_Col;
		  APosition.Row := i_Row;		
    end; //if
    
    // niets gevonden (dat i_Col als nog groeit is niet erg, die gebruiken we na vinden niet meer
		inc(i_Col);

  end; //while Col
  
  // we zijn alle kolommen van een rij langs gegaan, op naar begin volgende rij
  i_Col := 0;  
  inc(i_Row);

 end; //while Row

 //# END TODO
end;



procedure TBacktrackSolver.Search;

// recursion bound function: number of empty cells in FPuzzle
//# BEGIN TODO implementation block for TBackTrackSolver.Search

var
 aPiece: TPiece;
 aOrientation: TOrientation;
 aPlacement: TPlacement;
 solved: boolean;
 aEmptyCell: TPosition;
 i: integer;
 j: integer;



begin

EmptyCell(Solved, aEmptyCell);                   // eerst gaan we kijken wat volgende lege cell is als de puzzel is opgelost

if (not Solved) and (Fpuzzle.Stock.Count > 0) then begin // dan controleren we of ie opgelost is, zo niet doen we iets


for i := 0 to FPuzzle.Stock.Count - 1 do begin   // we gaan alle puzzel stukjes af
  aPiece := FPuzzle.Stock.GetPiece(i);           // namelijk, dit stukje

	for j := 0 to aPiece.OrientationCount - 1 do begin  // en gaan alle orientaties van dit stukje af
    aOrientation := aPiece.GetOrientation(j);    // dan pakken  we een orientatie van ons stukje
    EmptyCell(Solved, aEmptyCell);               // eerst gaan we kijken wat volgende lege cell is als de puzzel is opgelost

    aEmptyCell.Row := aEmptyCell.Row - aOrientation.Position(0).Row; // kleine correctie ivm stukjes zonder linker bovenhoek
    aEmptyCell.Col := aEmptyCell.Col - aOrientation.Position(0).Col;

    aPlacement := TPlacement.Create(aPiece, aOrientation, aEmptyCell );   // we maken een plaatsing aan

    if FPuzzle.CanDoPlacement(aPlacement) then begin  // en we kunnen ons stukje plaatsen

      FPuzzle.DoPlacement(aPlacement);            // doe dat dan ook

	   // contole op de showmoves vink
      if Assigned(FOnPiecePlaced) then begin
        FOnPiecePlaced(nil);                      // puzzel stukje echt zichtbaar plaatsen (met showmoves)
      end; //if assigned

	    EmptyCell(Solved, aEmptyCell);              // hebben we nu de puzzel opgelost?

      if Solved then begin
         SolutionFound;                           // we hebben een oplossing!
      end
      else begin
        Search;                                   // nee, we moeten verder zoeken
      end; //if solved

      FPuzzle.UndoPlacement(APlacement);          // we halen het stukje weg

                                                  // contole op de showmoves vink
      if Assigned(FOnPieceRemoved) then begin
        FOnPieceRemoved(nil);                     // puzzel stukje echt zichtbaar verwijdere (met showmoves)
      end; //if assigned

	  end
    else begin
    end; //if CanDoPlacement


	end; //for orientation

end; //for piece


end;//if not Solved

//# END TODO
end;

procedure TBacktrackSolver.FindAll;
begin
  inherited FindAll;
  Search;
end;

end.
