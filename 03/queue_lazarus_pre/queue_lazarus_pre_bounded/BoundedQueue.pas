unit BoundedQueue;

// Etienne van Delden
// 0618959
// 14-04-2007


// Deze unit bevat een volledige class header en een volledig contract voor
// de class TBoundedQueueOfInt. De bodies van de methods zijn echter nog niet
// ingevuld; in plaats daarvan staat slechts het commentaar {TODO}. Al deze
// dummies dienen vervangen te worden door echte code die voldoet aan het
// contract van de class.
//
// Ook van de twee hulpfuncties QueueInternally en QueueExternally is (vrijwel)
// geen code gegeven. Deze functies dienen een string op te leveren die de
// interne resp. externe inhoud van het queue object voorstelt. Van deze
// functies is geen specificatie gegeven, maar zie unit BoundedStack voor
// vergelijkbare hulpfuncties voor stacks.

interface

  // ---------------------------------------------------------------------------
  // Class TBoundedQueueOfInt implements a *bounded* queue of integers.
  // It provides the following public operations:
  // - Create(ASize) create queue, initially empty, with length bounded by ASize
  // - Count         number of elements in the queue
  // - Size          upper bound for Count
  // - First         return first element of queue
  // - IsEmpty       return whether queue is empty
  // - IsFull        return whether queue is full
  // - Put(AValue)   append AValue to the end of the queue
  // - RemFirst      remove first element of the queue
  //
  // The implementation is based on a dynamic array FA of which the length
  // is set by Create; the length remains constant.
  // The element count is maintained in FCount.
  // The array is used cyclically. The head and tail positions are maintained
  // in fields FHead and FTail, respectively.
  // If the queue is not empty FHead indicates the first used position.
  // If the queue is not full, FTail indicates the first free position.
  // N.B. the situation FHead = FTail occurs both when IsFull or IsEmpty.
  // Invariant I2 (which follows from I0 and the definitions of IsFull and
  // IsEmpty) ensures that not(IsFull and IsEmpty).
  //----------------------------------------------------------------------------

type
  TBoundedQueueOfInt =
  class(TObject)
  private
    // fields -------------------------
    FA: array of Integer; // N.B.: dynamic array
    FCount: Integer;
    FHead: Integer;
    FTail: Integer;

    // private invariants -------------
    // Pri0: 0 < Length(FA)
    // Pri1: 0 <= FCount <= Length(FA)
    // Pri2: 0 <= FHead < Length(FA)
    // Pri3: 0 <= FTail < Length(FA)

    // representation -----------------
    // if IsEmpty or (FHead < FTail) -> Abstr = FA[FHead..FTail)
    // [] IsFull  or (FTail < FHead  -> Abstr = FA[FTail..L) ++ FA[0..FHead),
    //                                    where L = Length(FA)
    // fi
    // Count = FCount
    // Size = Length(FA) (N.B. Length is Pascal operator for dynamic arrays)

  public
    // construction -------------------
    constructor Create(ASize: Integer);
    // pre: 0 < ASize
    // post: Abstr = [], Size = ASize

    // queries ------------------------
    function Count: Integer;
    // pre: true
    // ret: |Abstr|

    function Size: Integer;

    function First: Integer;
    // pre: not IsEmpty, Abstr :: [X] ++ S
    // ret: X

    function IsEmpty: Boolean;
    // pre: true
    // ret: Count = 0

    function IsFull: Boolean;
    // pre: true
    // ret: Count = Size

    // commands -----------------------
    procedure Put(AValue: Integer);
    // pre: not IsFull, Abstr = S
    // post: Abstr = S ++ [AValue]

    procedure RemFirst;
    // pre: not IsEmpty, Abstr :: [X] ++ S
    // post: Abstr = S

    // model variables ----------------
    // Abstr: sequence of Integer

    // public invariants --------------
    // I0: 0 < Size
    // I1: 0 <= Count <= Size
    // I2: not (IsFull and IsEmpty)   // follows from I0
    


  end;

function QueueInternally(AQueue: TBoundedQueueOfInt): String;
function QueueExternally(AQueue: TBoundedQueueOfInt): String;
function IsValid(AIndex: Integer; AQueue: TBoundedQueueOfInt): Boolean;  // Is het I'de getal een getal in de array?


implementation // ==============================================================


uses
  SysUtils, Math; // for Format function

{ TBoundedQueueOfInt }

var
  limit: Integer;                       // global var om de grootste lengte van de array bij te houden


function TBoundedQueueOfInt.Count: Integer;
begin
    Result := FCount;
end;

constructor TBoundedQueueOfInt.Create(ASize: Integer);
begin
  {TODO}
    Assert(0 < ASize,
    Format('TBoundedStackOfInt.Create.pre failed; ASize = %d', [ASize] ));
  inherited Create;
  SetLength(FA, ASize);
  FCount := 0;                          // Initieel zijn er geen elementen
  FHead := 0;                           // Initieel staat de header op nul (vooraan)
  FTail := 0;                           // Initieel staat de tail op nul
  Limit := ASize;                       // het limiet wordt de ASize
end;

function TBoundedQueueOfInt.IsEmpty: Boolean;
begin
  {TODO}
    Result := Count = 0;                // Een array is leeg als ie geen elementen bevat
end;

function TBoundedQueueOfInt.IsFull: Boolean;
begin
  {TODO}
    Result := Count = Size;               // Een array is vol als ie geen elementen bevat
end;

procedure TBoundedQueueOfInt.Put(AValue: Integer);
begin
  {TODO}
     Assert( not IsFull, 'TBoundedStackOfInt.Push.pre failed'); // array is vol

    FA[FTail] := AValue;                // De tail wijst normaal de laatste element aan,
                                        // de tail is net opgehoogt en er mag dus een nieuw element worden toegekend 
    FCount := FCount + 1;               // Er is een element bijgekomen
    FTail := (FTail + 1) mod Length(FA);
end;

procedure TBoundedQueueOfInt.RemFirst;

var
   I: Integer;                          // hulp var;
   
begin
  {TODO}

  Assert( not IsEmpty, 'TBoundedQueueOfInt.RemFirst.pre failed'); // de Array is leeg
  FHead := (FHead + 1) mod Length(FA);  // De kop van de string schuift 1 op, modulo de lengte van het array
  FCount := FCount - 1;                 // we hebben 1 element minder
end;
  
function TBoundedQueueOfInt.Size: Integer;
begin
  {TODO}
    Result := Length(FA);
end;

///////// ***** AANPASSEN ****** /////////// eigen result chekker! Wat is de Head resp. Tail ?
function TBoundedQueueOfInt.First: Integer;
begin
  {TODO}
    Assert( not IsEmpty, 'TBoundedQueueOfInt.Top.pre failed');
  Result := FA[FHead];
end;

function IsValid(AIndex: Integer; AQueue: TBoundedQueueOfInt): Boolean; // Zit het I'de getal in de AQueue
// Ter verduidelijking maak ik gebriuk van illustratieve gevallen
// H = FHead, T = FTail, I = AIndex, is het I'de getal waar we nu naar kijken.
// __ betekent een array element wat niet mag worden weergegeven
// x is de waarde van een array element \in [0 .. 100]
// voorbeeld:
// __ __ H xxIxxx T __ __ betekent, twee getallen die niet in de stack zitten, 5getallen wel, 
//  waarvan I er naar 1 van hun wijst, en dan komt de Tail. De Head wijst altijd een element aan,
// de Tail wijst het laatste toegevoegde element aan.  


begin
  with AQueue do begin                  // We gaan AQueue be(mis)handelen
    if (AIndex >= FHead) and (AIndex < (FTail)) then begin // Geval: __ __ H xIxxx T __ __
      Result := True;                   // dit mag
    end  // if
    else if (AIndex < FTail) and (FTail < FHead) then begin // Geval: xxIxx T __ __ H xxxx
      Result := True;                   // dit mag (tail is doorgeschoten)
    end // else if
    else if (FTail < FHead) and (FHead <= AIndex) then begin // Geval: xxxxT __ __ H xxIxx
      Result := True;
    end // else if
    else if IsFull then begin           // Is de Array vol? dus xxxTHxxx, waarbij T,H overlappen
      Result := True;
    end // else if
    else begin                          // alle andere gevallen is het niet een juiste uitkomst.
      Result := False;
    end; // else if

  end; //with
end;

// -----------------------------------------------------------------------------
function QueueInternally(AQueue: TBoundedQueueOfInt): String;

var
 I:Integer;                             // hulp var

begin
  with AQueue do                        // we be(mis)handelen AQueue
  begin
    {TODO}
    Result := '[';                      // we hebben om z'n minst een [

    for I := 0 to Length(FA) - 1 do begin // we gaan de hele array langs en kijken op voor ieder array element
      if isValid(I, AQueue) then begin    //  of deze in de queue zit
        Result := Result + Format('%3d',[ FA[I] ]); // als dat zo is schrijven we deze
      end
      else begin
        Result := Result + ' __';       // anders schrijven we iets anders
      end; // else
    end; //for

    Result := Result + ']';             // we eindigen met ]
  end{with};
end;




// voor test/controle doel einden schrijft hij de VOLLEDIGE ARRAY, en dus niet de werkelijke queue
// op het moment dat de internal werkt, wordt het makkelijk de external te maken
function QueueExternally(AQueue: TBoundedQueueOfInt): String;

var
   I:Integer;                           // hulp var

begin
  with AQueue do
  begin
    {TODO}
    Result := '[';                      // we hebben om z'n minst een [

    for I := 0 to Length(FA) - 1 do begin // we gaan de hele array langs en kijken op voor ieder array element
      if isValid(I, AQueue) then begin    //  of deze in de queue zit
        Result := Result + Format('%3d',[ FA[I] ]); // als dat zo is schrijven we deze
      end; // if
    end; //for

    Result := Result + ']';             // we eindigen met ]
  end{with};
end;

end. // einde programma
