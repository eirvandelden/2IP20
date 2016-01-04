unit PersonLinkedList;

interface

uses
  PersonBase;

type
  PCell = ^TCell;
  TCell =
  record
    FName: String;
    FData: TPersonData;
    FNext: PCell;
  end;

type
  TPersonList_LinkedList =
  class(TPersonList)
  protected
    {OWNCODE ADDED}
    FHead: PCell;
    FTail: PCell;
    FCount: Integer;
  public
    // construction and destruction ---
    constructor Create;
    // pre: true
    // post: Abstr = []

    destructor Destroy; override;

    // primitive queries --------------
    function Count: Integer; override;
    // pre: true
    // ret: |(Abstr)|

    function Indexof(AFamilyName: String): Integer; override;
    // pre: true
    // ret: I s.t. Abstr[I].s = AFamilyName, otherwise -1

    function Get(Index: Integer): TPersonData; override;
    // pre: 0 <= I < |Abstr|
    // ret: Abstr[I].d

    // derived queries ----------------

    function Has(AFamilyName: String): Boolean; override;
    // pre: true
    // ret: (exists I: 0 <= I < |Abstr|: Abstr[I].s = AFamilyName)

    function GetDataOf(AFamilyName: string): TPersonData; override;
    // pre: Has(AFamilyName)
    // ret: Abstr[I].d, where I = IndexOf(AFamilyName)

    // commands -----------------------
    procedure Clear; override;
    // pre: true
    // post: Abstr = []

    procedure Add(AData: TPersonData); override;
    // pre: not Has(AData.FamilyName)
    // effect: Abstr := old Abstr ++ [(AData.FamilyName, Adata)]

    procedure Change(AFamilyName: String; ANewData: TPersonData); override;
    // pre: Abstr :: S1 ++ [(AFamilyName, d)] ++ S2
    //      AFamilyName = ANewData.FamilyName
    // post: Abstr = S1 ++ [(AFamilyName, ANewData)] ++ S2

    procedure Delete(AFamilyName: String); override;
    // pre: Abstr :: S1 ++ [(AFamilyName, d)] ++ S2
    // post: Abstr = S1 ++ S2


    // model variables ----------------
    // Abstr: finite list of pairs (s,d), where s in String and d in TPersonData

    // invariants ---------------------
    // IsMap: (forall I, J: 0 <= I < J < |Abstr|: Abstr[I}.s <> Abstr[J].s)
    // Consistent: (forall I: 0 <= I < |Abstr|: Abstr[I].s = Abstr[I].d.FamilyName)
    //
  end;


implementation //===============================================================

uses
  SysUtils; // for Format function

//------------------------------------------------------------------------------
// Note:
// A simple-minded linked list implementation with just a head pointer to a
// chain of cells ending in nil may lead to a lot of case analysis (e.g.: list
// is empty or not, element does occur or not, element to be deleted is first
// or last or any other, etc).
// Some simple provisions may significantly reduce case analysis
// - Rather than representing an empty list by a nil pointer, use a special
//   "sentinel" cell. The FName or FData field of this cell can then be used to
//   perform a simple linear search with sentinel.
// - Rather than using a pointer FHead to the first cell of the list, use a
//   special "prehead" cell such that FPreHead^.FNext points to the first cell.
//   This way, also the first cell has a predecessor.
//------------------------------------------------------------------------------



{ TPersonList_LinkedList }

procedure TPersonList_LinkedList.Add(AData: TPersonData);

var
  C: PCell;

begin
{OWNCODEADDED}

  Assert((Indexof(AData.FamilyName) = -1), 'TPersonList_LinkedList.Add pre failed.');  // controleer de pre

  New(C);                                         // maak een nieuwe pointer
  C^.FData := AData;                              // laat hem wijzen naar de nieuwe waardes
  C^.FNext := nil;                                // er is geen volgende

  if FHead = nil then begin                       // als er geen kop was, maak dan een nieuw kop
    FHead := C;
  end //if
  else begin
    FTail^.FNext := C;                            // er was een kop, de staart moet wijzen naar de nieuwe C
  end; // else

  FTail := C;                                     // De cel C wordt de nieuwe staart
  FCount := FCount + 1;                           // we hebben een nieuwe cel toegevoegd
end;

procedure TPersonList_LinkedList.Change(AFamilyName: String;
            ANewData: TPersonData);

var
  Index, I: Integer;
  C: PCell;

begin
{OWNCODEADDED}

  Index := Indexof(AFamilyName);                  // haal de index can de naam die we veranderen op

  Assert((Index <> -1), 'TPersonList_LinkedList.Change pre failed');

  C := FHead;                                     // Zoekd e juiste cel

  for I := 0 to Index - 1 do begin                // C wordt de FNext van de index die we zoeken
    C := C^.FNext;
  end; //for

  C.FData := ANewData;                            // Geef C de nieuwe waardes

end;

procedure TPersonList_LinkedList.Clear;
var
  C, H: PCell;
begin
{OWNCODEADDED}

  C := FHead;                                     // Zoek het begin

  while C <> nil do begin                         // zolang de Cell niet leeg is
    H := C;                                       // (onthoud oude cel waardes)
    C := C^.FNext;                                // C wijst naar de volgende cel in de keten
    Dispose(H);                                   // gooi de oude waardes weg
  end; //while

  FHead := nil;                                   // we hebben nu geen kop meer
  FTail := nil;                                   // we hebben nu geen staart meer
  FCount := 0;                                    // we hebben nu geen elementen meer

end;

function TPersonList_LinkedList.Count: Integer;
begin
{OWNCODEADDED}

  Result := FCount;                               // dit blijft onzinnige een hele procedure voor die FCount aanmaken

end;

constructor TPersonList_LinkedList.Create;
begin
{OWNCODEADDED}
  inherited Create;                               // maak alle object eigenschappen van alle parents aan
  FHead := nil;                                   // ons net aangemaakte object heeft geen kop
  FTail := nil;                                   // ... en geen staart
  FCount := 0;                                    // met zonder elementen!
end;

procedure TPersonList_LinkedList.Delete(AFamilyName: String);
var
  C, H: PCell;
  Index, I: Integer;
begin
{OWNCODEADDED}

  Index := Indexof(AFamilyName);                  // haal de index van de naam die we verwijderen

  Assert((Index <> -1), 'TPersonList_LinkedList.Delete pre failed');


  if Index = 0 then begin                             //  stel we hebben de eerste persoon
    C := FHead;                                   //  we pakken de eerste cel
    FHead := FHead^.FNext;                        //  en we laten de kop nu wijzen naar die van de volgende
    Dispose(C);                                   // en we gooien de oude eerste cel weg
  end //if
  else begin
    C := FHead;                                   //  we pakken de eerste cel

    for I := 1 to Index-1 do begin                // we gaan naar de op 1 na laatste cel die we willen hebben (dus voor de cel die we verwijderen
      C := C^.FNext;
    end; //for

    H := C^.FNext;                                // onze hulp cel wordt de cel die weg moet
    C^.FNext := H^.FNext;                         // we laten de cel voor die die we verwijderen wijzen naar die die erna zit
    Dispose(H);                                   // en knikkeren de huidige cel weg
  end; // else

  FCount := FCount - 1;                           // oi, 1 cel verwijderd, dus 1 cel minder!
end;

destructor TPersonList_LinkedList.Destroy;
var
  C, H: PCell;
begin
{OWNCODEADDED}

  C := FHead;                                     // we pakken de eerste cel

  while C <> nil do begin                        // zolang we geen lege cel hebben
    H := C;                                      // onthoudt de huidige cel
    C := C^.FNext;                               // de huidige cel wordt de volgende
    Dispose(H);                                  // verwijder de oude cel
  end; // while

  inherited Destroy;                             // nu we dat allemaal hebben gedaan, kunnen alle andere eigenschappen ook weg
end;

function TPersonList_LinkedList.Get(Index: Integer): TPersonData;

var
  C: PCell;
  I: Integer;
begin
{OWNCODEADDED}

 Assert((0 <= Index) and (Index < FCount) ,
    Format('TPersonList_Array.Get.pre failed; Index = %d',[Index]));
  I := 0;                                         // init I
  C := FHead;                                     // init Cell

  while Index <> I do begin                       // loop alle cellen langs todat we de laatste hebben
    C := C^.FNext;
    I := I + 1;
  end; //while

  Result := C^.FData;                             //  retourneer de cel die we willen hebben

end;

function TPersonList_LinkedList.GetDataOf(AFamilyName: string): TPersonData;
var
  Index: Integer;
begin
{OWNCODEADDED}

  Index := Indexof(AFamilyName);                  // haal de index van de naam die we verwijderen

  Assert((Index <> -1), 'TPersonList_LinkedList.GetDataOf pre failed');

  Result := Get(Index);                           // retourneer uitkomst
end;

function TPersonList_LinkedList.Has(AFamilyName: String): Boolean;
begin
{OWNCODEADDED}
  Result := Indexof(AFamilyName) <> -1;           // zit de persoon wel of niet in de lijst?
end;

function TPersonList_LinkedList.Indexof(AFamilyName: String): Integer;
var
  C: PCell;
  I: Integer;
begin
{OWNCODEADDED}
  C := FHead;                                     // init cell
  I := 0;                                         // init hulp var
  Result := -1;                                   // init result (we gaan ervan uit dat we niemand vinden)

  while C <> nil do begin                         // zolang het einde van de lijst niet bereikt is

    if C.FData.FamilyName = AFamilyName then begin  // als we de juiste persoon gevonden hebben
       Result := I;                               // retourneer de index
       Break;                                     // sluit de while
    end // if
    else begin
      C := C^.FNext;                              // ga naar de volgende cell
      I := I + 1;                                 // verhoog I
    end; // else

  end; //while
  
end;

end.
