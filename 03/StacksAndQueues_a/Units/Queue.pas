unit Queue;

// Etienne van Delden
// 0618959
// 14-04-2007

// Deze unit bevat een volledige class header en een volledig contract voor
// de class TQueueOfInt. De bodies van de methods zijn echter nog niet
// ingevuld; in plaats daarvan staat slechts het commentaar {TODO}. Al deze
// dummies dienen vervangen te worden door echte code die voldoet aan het
// contract van de class.
//
// Ook van de twee hulpfuncties QueueInternally en QueueExternally is (vrijwel)
// geen code gegeven. Deze functies dienen een string op te leveren die de
// interne resp. externe inhoud van het queue object voorstelt. Van deze
// functies is geen specificatie gegeven, maar zie unit Stack voor
// vergelijkbare hulpfuncties voor stacks.


interface

  // ---------------------------------------------------------------------------
  // Class TQueueOfInt implements an *unbounded* queue of integers.
  // It provides the following public operations:
  // - Create        create queue, initially empty
  // - Count         number of elements in the queue
  // - First         return first element of queue
  // - IsEmpty       return whether queue is empty
  // - Put(AValue)   append AValue to the end of the queue
  // - RemFirst      remove first element of the queue
  //
  // The implementation is based on a dynamic array FA
  // The element count is maintained in FCount
  // When insufficient space is available, FA is extended.
  // When there are too many unused elements, FA is shortened.
  // The array is used cyclically. The head and tail positions are maintained
  // in fields FHead and FTail, respectively.
  // If the queue is not empty, FHead indicates the first used position.
  // FTail indicates the first free position.
  //----------------------------------------------------------------------------

type
  TIntArray = array of Integer;
  // giving this type a name is necessary to achieve type compatibility inside
  // procedure ShrinkArray.

type
  TQueueOfInt =
  class(TObject)
  private
    // fields -------------------------
    FA: TIntArray;
    FCount: Integer;
    FHead: Integer;
    FTail: Integer;

    // auxiliary methods --------------
    procedure ShrinkArray; // copy left-adjusted to new shorter dynamic array

    // pre: FCount < Length(FA) div 2
    // post: Length(FA) = Length(old FA) div 2, FCount = old FCount
    //       FHead = 0, FTail = FCount, Abstr = old Abstr

    // private invariants -------------
    // Pri0: coMinQueueSize <= Length(FA) {N.B. 0 < coMinQueueSize)}
    // Pri1: 0 <= FCount < Length(FA) {N.B. at least one empty position}
    // Pri2: 0 <= FHead < Length(FA)
    // Pri3: 0 <= FTail < Length(FA)

    // representation -----------------
    // if FHead < =FTail -> Abstr = FA[FHead..FTail)
    // [] FTail < FHead  -> Abstr = FA[Head..L) ++ FA[0..FTail),
    // fi
    // Count = FCount

    procedure ExtendArray; // copy left-adjusted to new longer dynamic array
    // pre: FCount = Length(FA)
    // post: Length(FA) = 2 * Length(old FA), FCount = old FCount
    //       FHead = 0, FTail = FCount, Abstr = old Abstr


  public
    // construction -------------------
    constructor Create;
    // pre: true
    // post: Abstr = []

    // queries ------------------------
    function Count: Integer;
    // pre: true
    // ret: |Abstr|

    function First: Integer;
    // pre: not IsEmpty, Abstr :: [X] ++ S
    // ret: X

    function IsHalfEmpty: Boolean;
    // pre: true
    // ret: Count = 0.5*Length(FA)

    function IsHalfFull: Boolean;
    // pre: true
    // ret: Count = 0.5*Length(FA)

    function IsEmpty: Boolean;
    // pre: true
    // ret: Count = 0

    function IsFull: Boolean;
    // pre: true
    // ret: Count = Size

    // commands -----------------------
    procedure Put(AValue: Integer);
    // pre: Abstr = S
    // post: Abstr = S ++ [AValue]

    procedure RemFirst;
    // pre: not IsEmpty, Abstr :: [X] ++ S
    // post: Abstr = S

    // model variables ----------------
    // Abstr: sequence of Integer

    // public invariants --------------
    // none
  end;

function QueueInternally(AQueue: TQueueOfInt): String;
function QueueExternally(AQueue: TQueueOfInt): String;
function IsValid(AIndex: Integer; AQueue: TQueueOfInt): Boolean;  // Is het I'de getal een getal in de array?

implementation // ==============================================================

uses
  SysUtils, Math; // for Format function


const
  coMinQueueSize = 8;


{ TQueueOfInt }

function TQueueOfInt.Count: Integer;
begin
  {TODO}
      Result := FCount;
end;

constructor TQueueOfInt.Create;
begin
  {TODO}
    Assert(0 < coMinQueueSize,
    Format('TStackOfInt.Create.pre failed; ASize = %d', [coMinQueueSize] ));
  inherited Create;
  SetLength(FA, coMinQueueSize);        // Minimale lengte is coMinQueueSize
  FCount := 0;                          // Initieel zijn er geen elementen
  FHead := 0;                           // Initieel staat de header op nul (vooraan)
  FTail := 0;                           // Initieel staat de tail op nul

end;

procedure TQueueOfInt.ExtendArray;

var
  I, J: Integer;                        // hulp var
  TempArray: Array of Integer;          // hulp array

begin
  SetLength(TempArray, FCount);         // tijdelijke array
  J := FHead;                           // hulp var gaat helpen om te tellen van FHead naar FTail - 1
  for I := 0 to FCount - 1 do begin     // copy to temp array
    TempArray[I] := FA[J];
    J := (J + 1) mod Length(FA);
  end; // for

  SetLength(FA, 2*Length(FA));          // increase lenth array

  for I := 0 to FCount - 1 do begin     // copy from temp array to new array
    FA[I] := TempArray[I];
    FTail := I + 1;                     // dynamically set the new FTail
  end;//for
  FHead := 0;                           // set the new Head

end;

procedure TQueueOfInt.ShrinkArray;
var
  I,J,K:Integer;                        // hulp vars
  TempArray: Array of Integer;          // hulp array

begin
  {TODO}
  SetLength(TempArray, FCount);         // init hulp array
  J := FHead;                           // hulp var gaat helpen om te tellen van FHead naar FTail - 1
  for I := 0 to FCount - 1 do begin     // copy to temp array
    TempArray[I] := FA[J];
    J := (J + 1) mod Length(FA);
  end; // for

  if (Length(FA) div 2) <= coMinQueueSize then begin  // even chekken of we niet te klein gaan maken
    K := coMinQueueSize;                // ow nooo's! We mogen niet kleiner dan dit
  end
  else begin
    K := Length(FA) div 2;              // gefeliciflapstaart, u kunt veilig verkleinen
  end;

  SetLength(FA, K);          // increase lenth array

  for I := 0 to FCount - 1 do begin     // copy from temp array to new array
    FA[I] := TempArray[I];
    FTail := I + 1;                     // dynamically set the new FTail
  end;//for

  FHead := 0;                           // set the new Head

end;

function TQueueOfInt.First: Integer;

begin
  {TODO}
  Assert( not IsEmpty, 'TBQueueOfInt.Top.pre failed');
  Result := FA[FHead];
end;

function TQueueOfInt.IsEmpty: Boolean;
begin
  {TODO}
    Result := Count = 0;                // Een array is leeg als ie geen elementen bevat
end;

function TQueueOfInt.IsHalfEmpty: Boolean;
begin
  {TODO}
    Result := Count = Length(FA) div 2; // Een array is half leeg als de Count kleiner is dan de helft van de array
end;

function TQueueOfInt.IsHalfFull: Boolean;
begin
  {TODO}
    Result := Count = Length(FA) div 2; // Een array is half vol als de Count kleiner is dan de helft (okay okay, hij is net ietjses minder vol)
end;


function TQueueOfInt.IsFull: Boolean;
begin
  {TODO}
    Result := Count = Length(FA);       // Een array is vol als ie geen elementen bevat
end;

procedure TQueueOfInt.Put(AValue: Integer);
var
  I: Integer;                           // hulp var
  TempArray: TQueueOfInt;               // hulp array
begin
  {TODO}

  FA[FTail] := AValue;                  // De tail wijst normaal na de laatste element aan,
  FCount := FCount + 1;                 // Er is een element bijgekomen
  FTail := (FTail + 1) mod length(FA);  // Tail verplaatst 1 plek (mod Length(FA))

  if IsFull then begin                  // is onze array nu vol? Zo ja, Extenden!
    ExtendArray;
  end;

end;

procedure TQueueOfInt.RemFirst;
begin
  {TODO}
  Assert( not IsEmpty, 'TBoundedQueueOfInt.RemFirst.pre failed'); // de Array is leeg

  FHead := (FHead + 1) mod Length(FA);  // De kop van de string schuift 1 op, modulo de lengte van het array
  FCount := FCount - 1;                 // we hebben 1 element minder

  if IsHalfEmpty or IsHalfFull then begin             // Is de array nu te half leef óf half vol?
    ShrinkArray;                        // verkleinen die handel!
  end;
end;



function IsValid(AIndex: Integer; AQueue: TQueueOfInt): Boolean; // Zit het I'de getal in de AQueue
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
function QueueInternally(AQueue: TQueueOfInt): String;
var
 I:Integer;                             // hulp var
begin
  with AQueue do
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
  end // with
end;

function QueueExternally(AQueue: TQueueOfInt): String;
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
  end //with
end;


end.
