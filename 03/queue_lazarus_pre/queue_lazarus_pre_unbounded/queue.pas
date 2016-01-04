unit Queue;

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
    // pre: FCount = Length(FA)
    // post: Length(FA) = 2 * Length(old FA), FCount = old FCount
    //       FHead = 0, FTail = FCount, Abstr = old Abstr

    procedure ExtendArray; // copy left-adjusted to new longer dynamic array
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

    function IsEmpty: Boolean;
    // pre: true
    // ret: Count = 0

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

implementation // ==============================================================

uses
  SysUtils; // for Format function

const
  coMinQueueSize = 8;


{ TQueueOfInt }

function TQueueOfInt.Count: Integer;
begin
  {TODO}
end;

constructor TQueueOfInt.Create;
begin
  {TODO}
end;

procedure TQueueOfInt.ExtendArray;
begin
  {TODO}
end;

function TQueueOfInt.First: Integer;
begin
  {TODO}
end;

function TQueueOfInt.IsEmpty: Boolean;
begin
  {TODO}
end;

procedure TQueueOfInt.Put(AValue: Integer);
begin
  {TODO}
end;

procedure TQueueOfInt.RemFirst;
begin
  {TODO}
end;

procedure TQueueOfInt.ShrinkArray;
begin
  {TODO}
end;

// -----------------------------------------------------------------------------
function QueueInternally(AQueue: TQueueOfInt): String;
begin
  with AQueue do
  begin
    {TODO}
  end
end;

function QueueExternally(AQueue: TQueueOfInt): String;
begin
  with AQueue do
  begin
    {TODO}
  end
end;


end.
