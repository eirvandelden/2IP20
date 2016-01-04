De versie Puzzel2007_A5 bevat de volgende wijzigingen.

In Main.pas, Main.dfm:

  1.  Op het BoxGrid worden zwarte lijntjes om stukjes getekend.
      Met dank aan Joost Hausmans.

  2.  FDragBitmap wordt na gebruik opgeruimd.  TMyDragObject heeft een
      eigen Destroy.  Met dank aan Benji Vos.

  3.  Captions van PiecesGrid, BoxGrid en Form1 aangepast.

  4.  FindAll button, ShowMoves/ShowSolutions checkboxes, Log memo
      en Save-Solution-As menu item, met afhandeling.

  5.  Gebruikt unit Solvers.  Attribuut FSolver toegevoegd aan TForm1.
      Handlers voor OnFound, OnPiecePlaced, OnPieceRemoved events.

  6.  Extra commentaar bij FDragPlacement.

Unit Solvers.pas is toegevoegd.

De versie Puzzel2007_A4 bevat de volgende wijzigingen:

  1.  TForm1.BoxGridClick is nu geheel verwijderd.

  2.  In PiecesGrid worden alle cellen zonder string wit gekleurd.
      Dat onderdrukt hinderlijke highlight van een aldaar geselecteerde cell.

  3.  Drag & drop is toegevoegd.  PiecesGrid --> BoxGrid werkt.
      BoxGrid --> PiecesGrid en BoxGrid --> BoxGrid moet ingevuld worden.

De versie Puzzle2007_A3 was alleen voor intern gebruik.

De versie Puzzel2007_A2 bevat de volgende wijzigingen:

  1.  De demo maakt bij opstarten '..\Puzzles' de standaardmap.
      Zie fnDefaultPuzzleFolder (nieuw), TForm1.FormCreate in Main.pas.

      N.B. De open-dialoog verandert de standaardmap niet.
      Dus alleen puzzles in '..\Puzzles' zijn ermee te openen
      (want de doos- en stukjes-beschrijvingen worden daar gezocht,
      ongeacht waar de puzzel-beschrijving staat).

  2.  Via een aparte Assert wordt gecontroleerd of de bestanden
      bestaan alvorens ze te openen.
      Zie ReadPuzzle, ReadBox, ReadBogOfPieces in PuzzleIO.pas.

      De demo breekt nog steeds af als de standaardpuzzel
      (SimplePuzzle.txt) niet bestaat.

  3.  De controle op de inhoud van puzzelbeschrijvingen is iets
      aangescherpt.  Zie Read... in PuzzleIO.pas.

  4.  SimplePieces.txt is gecorrigeerd: [C] heeft nu orientations=1.

  5.  De afmetingen van het PiecesGrid zijn nu beter.
      Zie FillPiecesGrid (voorheen DoPlacePieces) in Main.pas.

  6.  Bij klikken in de Box wordt het hokje niet meer rood gekleurd.
      Zie TForm1.BoxGridClick in Main.pas.

  7.  Bij inlezen van een nieuwe puzzel wordt de oude puzzel eerst
      opgeruimd.  Zie DoReadPuzzle, ClearPiecesGrid (nieuw) in Main.pas 
      en TBox.Clear in Base.pas.

  8.  Het File>Exit menu item werkt, en de niet-geimplementeerde menu
      items zijn uitgezet.  Zie MainMenu1, Exit1, etc. in Main.dfm (Form1).

  9.  Rechts-klikken op een stukje in het PiecesGrid toont de
      volgende orientatie (cyclisch).  Zie TForm1.PiecesGridMouseDown,
      (nieuw), TForm1.NextOrientationInPiecesGrid (nieuw) in Main.pas
      en de OnMouseDown event van PiecesGrid in Main.dfm (Form1).

 10.  De //# ... TODO markeringen zijn voorzien van een aanvullend
      commentaar.  De markering in de unit header is iets aangepast,
      zodat de unit niet compileert bij niet invullen van auteur, etc.

 11.  De code die initieel een paar stukjes plaatst is vereenvoudigd.
      Die kan nu makkelijker aangepast worden voor eigen tests.
      Zie TForm1.FormCreate, PlacePieceInBox (nieuw) in Main.pas.

(Einde)
