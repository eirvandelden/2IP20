unit PersonLinkedList;

//Sander Leemans, 0608896

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

    function Get(I: Integer): TPersonData; override;
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
  H: PCell;
begin
  Assert(not Has(AData.FamilyName), 'naam bestaat al');
  New(H);
  H^.FData := AData;
  H^.FNext := nil;

  if FHead = nil then begin
    FHead := H
  end else begin
    FTail^.FNext := H;
  end;
  FTail := H;
  FCount := FCount + 1;
end;

procedure TPersonList_LinkedList.Change(AFamilyName: String;
            ANewData: TPersonData);
var
  I, J: Integer;
  P: PCell;
begin
  I := Indexof(AFamilyName);
  Assert(I<>-1, 'naam bestaat niet');
  P := FHead;
  for J := 1 to I do begin
    P := P^.FNext;
  end;
  P.FData := ANewData;
end;

procedure TPersonList_LinkedList.Clear;
var
  P, H: PCell;
begin
  P := FHead;
  while P <> nil do
  begin
    H := P;
    P := P^.FNext;
    Dispose(H);
  end;
  FHead := nil;
  FTail := nil;
  FCount := 0;
end;

function TPersonList_LinkedList.Count: Integer;
begin
  Result := FCount;
end;

constructor TPersonList_LinkedList.Create;
begin
  inherited Create;
  FHead := nil;
  FTail := nil;
  FCount := 0;
end;

procedure TPersonList_LinkedList.Delete(AFamilyName: String);
var
  P: PCell;
  Q: PCell;
  I: Integer;
  J: Integer;
begin
  I := Indexof(AFamilyName);
  Assert(I<>-1, 'naam bestaat niet');
  if I = 0 then begin
    P := FHead;
    FHead := FHead^.FNext;
    Dispose(P);
  end else begin
    P := FHead;
    for J := 1 to I-1 do begin
      P := P^.FNext;
    end;
    Q := P^.FNext;
    P^.FNext := Q^.FNext;
    Dispose(Q);
  end;
  FCount := FCount - 1;
end;

destructor TPersonList_LinkedList.Destroy;
var
  P, H: PCell;
begin                         
  P := FHead;
  while P <> nil do
  begin
    H := P;
    P := P^.FNext;
    Dispose(H);
  end;
  inherited Destroy;
end;

function TPersonList_LinkedList.Get(I: Integer): TPersonData;
var
  P: PCell;
  J: Integer;
begin
  Assert((0 <= I) and (I < FCount) ,
    Format('TPersonList_Array.Get.pre failed; I = %d',[I]));
  J := 0;
  P := FHead;
  while I <> J do begin
    P := P^.FNext;
    J := J + 1;
  end;
  Result := P^.FData;
end;

function TPersonList_LinkedList.GetDataOf(AFamilyName: string): TPersonData;
var
  I: Integer;
begin
  I := Indexof(AFamilyName);
  Assert(I<>-1, 'naam bestaat niet');
  Result := Get(I);
end;

function TPersonList_LinkedList.Has(AFamilyName: String): Boolean;
begin
  Result := Indexof(AFamilyName) <> -1;
end;

function TPersonList_LinkedList.Indexof(AFamilyName: String): Integer;
var
  H: Pcell;
  I: Integer;
begin
  H := FHead;
  Result := -1;
  I := 0;
  while H <> nil do begin
    if H.FData.FamilyName = AFamilyName then begin
      Result := I;
    end;
    H := H^.FNext;
    I := I + 1;
  end
end;

end.
