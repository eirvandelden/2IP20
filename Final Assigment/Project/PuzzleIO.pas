unit PuzzleIO;
  { To read and write puzzles as text files }
//# BEGIN TODO author name, id.nr., date for unit header
  {
  Etienne van Delden
  0618959
  01-06-2007
  }
//# END TODO

interface

uses Base;

function ReadPuzzle(const AFileName: String): TPuzzle;
  // pre: file contains valid puzzle definition
  // ret: puzzle defined in file
function ReadBox(const AFileName: String): TBox;
  // pre: file contains valid box definition
  // ret: box defined in file
function ReadBagOfPieces(const AFileName: String): TBagOfPieces;
  // pre: file contains valid bag-of-pieces definition
  // ret: bag-of-pieces defined in file

// Write routines not yet implemented <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
procedure WritePuzzle(const AFileName: String; APuzzle: TPuzzle);
procedure WriteBox(const AFileName: String; ABox: TBox);
procedure WriteBagOfPieces(const AFileName: String; ABagOfPieces: TBagOfPieces);


implementation //=========================================================

uses
  SysUtils, IniFiles, Graphics;


//-------------------------------------------------------------------------
function ReadPiece(AName: String; AIniFile: TMemIniFile): TPiece; forward;
function ReadOrientation(AName: String; AIniFile: TMemIniFile): TOrientation; forward;

function ReadPuzzle(const AFileName: String): TPuzzle;
var
  VIniFile: TMemIniFile; // to access data as INI file
  VName: String; // to read puzzle name
  VPiecesFileName: String; // to read file name for bag of pieces
  VBagOfPieces: TBagOfPieces; // to construct the bag of pieces
  VBoxFileName: String; // to read file name for box
  VBox: TBox; // to construct the box
begin
  Assert(FileExists(AFileName),
    Format('ReadPuzzle.pre failed: File %s does not exist',
      [AFileName]));
  VIniFile := TMemIniFile.Create(AFileName);
  with VIniFile do
  begin
    // check filekind
    Assert( ReadString('Main', 'kind', '????') = 'puzzle',
      Format('File %s is not a puzzle file', [FileName]));

    // read puzzle name
    VName := ReadString('Puzzle', 'name', '????');

    // read pieces file
    VPiecesFileName := ReadString('Puzzle', 'pieces', '????');
    VBagOfPieces := ReadBagOfPieces(VPiecesFileName);

    // read box
    VBoxFileName := ReadString('Puzzle', 'box', '????');
    VBox := ReadBox(VBoxFileName);

    // build result
    Result := TPuzzle.Create(VName, VBox, VBagOfPieces);
  end;{with}
  VIniFile.Free;
end;

function ReadBox(const AFileName: String): TBox;
var
  VIniFile: TMemIniFile; // to access data as INI file
  VName: String; // to read name of box
  VBoxWidth: Integer; // to read box width
  VBoxHeight: Integer; // to read box height
  VRowString: String; // to read a box row
  Row, Col: Integer; // to traverse cells of box data read from file
begin
  Assert(FileExists(AFileName),
    Format('ReadBox.pre failed: File %s does not exist',
      [AFileName]));
  VIniFile := TMemIniFile.Create(AFileName);
  with VIniFile do
  begin
    // check filekind
    Assert(ReadString('Main', 'kind', '????') = 'box',
      Format('File %s is not a box file', [FileName]));

    // read its name
    VName := ReadString('Box', 'name', '????');

    // read box dimensions
    VBoxWidth := ReadInteger('Box', 'boxwidth', -1);
    Assert(VBoxWidth > 0,
      Format('ReadBox: BoxWidth = %d <= 0', [VBoxWidth]));
    VBoxHeight := ReadInteger('Box', 'boxheight', -1);
    Assert(VBoxHeight > 0,
      Format('ReadBox: BoxHeight = %d <= 0', [VBoxHeight]));

    // create free box
    Result := TBox.Create(VName, VBoxHeight, VBoxWidth);

    // read and block initial contents, row after row
    for Row := 0 to VBoxHeight - 1 do
    begin
      // read rowstring
      VRowString := ReadString('Box', 'row' + IntToStr(Row), '');
      Assert(Length(VRowString) = VBoxWidth,
        Format('In file %s, row%d of the box has wrong length %d',
          [FileName, Row, Length(VRowString)] ));

      // adjust state of cells in row Row
      for Col := 0 to VBoxWidth - 1 do
      begin
        if VRowString[Col + 1] <> '.' then // N.B. Strings are indexed from 1
          Result.BlockCell(Row, Col);
      end; { for Col }

    end; { for Row }

  end; { with }
  VIniFile.Free;
end;

function ReadBagOfPieces(const AFileName: String): TBagOfPieces;
var
  VIniFile: TMemIniFile; // to access data as INI file
  VName: String; // to read name of bag of pieces
  VPieceNames: String; // to read list of piece names
  VPieceName: String; // to read piece name
  VPiece: TPiece; // to capture result of ReadPiece
  I: Integer; // to traverse pieces
begin
  Assert(FileExists(AFileName),
    Format('ReadBagOfPieces.pre failed: File %s does not exist',
      [AFileName]));
  VIniFile := TMemIniFile.Create(AFileName);
  with VIniFile do
  begin
    // check filekind
    Assert(ReadString('Main', 'kind', '????') = 'pieces',
      Format('File %s is not a pieces file', [FileName]));

    // read its name
    VName := ReadString('Pieces', 'name', '????');

    // create empty stock
    Result := TBagOfPieces.Create(VName);

    // read list of piece names
    VPieceNames := ReadString('Pieces', 'piecenames', '');
    Assert(Length(VPieceNames) > 0, 'ReadBagOfPieces: # Pieces = 0');

    // read pieces and add to stock, one by one
    for I := 1 to Length(VPieceNames) do
    begin
      VPieceName := VPieceNames[I];
      VPiece := ReadPiece(VPieceName, VIniFile);

      Result.AddPiece(VPiece);
    end;{for}

  end;{with}
  VIniFile.Free;
end;

function ReadPiece(AName: String; AIniFile: TMemIniFile): TPiece;
var
  VColorName: String; // to read the color
  VColor: TColor; // to construct the color
  VMultiplicity: Integer; // to read multiplicity
  NOrientations: Integer; // number of orientation to read
  i: Integer; // to traverse the orientations
  VOrientation: TOrientation; // to read an orientation
begin
  with AIniFile do
  begin
    // read color and multiplicity
    VColorName := ReadString(AName, 'color', 'clBlack');
    VColor := StringToColor(VColorName);
    VMultiplicity := ReadInteger(AName, 'multiplicity', 1);
    Assert(VMultiplicity > 0,
      Format('ReadPiece: Multiplicity = %d <= 0', [VMultiplicity]));

    // create piece
    Result := TPiece.Create(AName, VColor, VMultiplicity);

    // read and add its orientations
    NOrientations := ReadInteger(AName, 'orientations', 1);

    for i := 0 to NOrientations -1 do
    begin
      VOrientation := ReadOrientation(AName+IntToStr(i), AIniFile);
      Result.AddOrientation(VOrientation);
    end; { for i }

  end; { with }
end;

function ReadOrientation(AName: String; AIniFile: TMemIniFile): TOrientation;
var
  VRows, VCols: Integer; // number of rows and columns in bounding box
  VRowString: String; // to read one row from INI file
  VRow, VCol: Integer; // to traverse the bounding box
  VPositionList: TPositionList; // VPositionList[0..VCount-1] occuppied
  VCount: Integer; // number of occupied positions
begin
  with AIniFile do
  begin
    // read orientation dimensions
    VRows := ReadInteger(AName, 'rows', -1);
    Assert(VRows > 0,
      Format('ReadOrientation: # Rows = %d <= 0', [VRows]));
    VCols := ReadInteger(AName, 'cols', -1);
    Assert(VCols > 0,
      Format('ReadOrientation: # Cols = %d <= 0', [VCols]));

    // set initial length of positionlist
    // (too big, will be shrunk when actual size is known)
    SetLength(VPositionList, VRows * VCols);
    VCount := 0;

    // read position strings, row after row
    for VRow := 0 to VRows - 1 do
    begin
      // read rowstring
      VRowString := ReadString(AName, 'row' + IntToStr(VRow), '');
      Assert(Length(VRowString) = VCols,
        Format('In file %s, for piece orientation %s, row%d has wrong length %d',
          [FileName, AName, VRow, Length(VRowString)] ));

      // add occupied positions to VPositionList
      // N.B. Strings are indexed from 1
//# BEGIN TODO statements to add occupied positions to VPositionList
for VCol := 1 to VCols do begin
   if VRowString[VCol] = AName[1] then begin
     VPositionList[VCount].Row := VRow;
     VPositionList[VCount].Col := VCol-1;
     inc(VCount);
   end;

end;

//# END TODO

    end; { for VRow }

  end; { with }

  SetLength(VPositionList, VCount);
  Result := TOrientation.Create(VPositionList);
end;


procedure WritePiece(AIniFile: TMemIniFile; APiece: TPiece); forward;
procedure WriteOrientation(AIniFile: TMemIniFile; AOrientation: TOrientation); forward;

procedure WritePuzzle(const AFileName: String; APuzzle: TPuzzle);
begin

end;

procedure WriteBox(const AFileName: String; ABox: TBox);
begin

end;

procedure WriteBagOfPieces(const AFileName: String; ABagOfPieces: TBagOfPieces);
begin

end;

procedure WritePiece(AIniFile: TMemIniFile; APiece: TPiece);
begin

end;

procedure WriteOrientation(AIniFile: TMemIniFile; AOrientation: TOrientation);
begin

end;


end.
