unit Queue_Array;

interface

uses
  AbsQueue;

  // ---------------------------------------------------------------------------
  // Class TIntQueue_Array implements the abstract class TIntQueue by means
  // of an unbounded array. It works in essentially the same way as the class
  // TQueueOfInt.
  //
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
  TIntQueue_Array =
  class(TIntQueue)
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

    procedure ExtendArray; // copy left-adjusted to new longer dynamic array
    // pre: true
    // post: Length(FA) = 2 * Length(old FA), FCount = old FCount
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
    function Count: Integer; override;
    // pre: true
    // ret: |Abstr|

    function First: Integer; override;
    // pre: not IsEmpty, Abstr :: [X] ++ S
    // ret: X

    function IsEmpty: Boolean; override;
    // pre: true
    // ret: Count = 0

    // commands -----------------------
    procedure Put(AValue: Integer); override;
    // pre: Abstr = S
    // post: Abstr = S ++ [AValue]

    procedure RemFirst; override;
    // pre: not IsEmpty, Abstr :: [X] ++ S
    // post: Abstr = S

    // model variables ----------------
    // Abstr: sequence of Integer

    // public invariants --------------
    // none
  end;

function IntQueueArrayInternally(AQueue: TIntQueue_Array): String;
function IntQueueArrayExternally(AQueue: TIntQueue_Array): String;

implementation // ==============================================================

uses
  SysUtils; // for Format function

const
  coMinQueueSize = 8;


{ TIntQueue_Array }

function TIntQueue_Array.Count: Integer;
begin
  Result := FCount;
end;

constructor TIntQueue_Array.Create;
begin
  inherited Create;
  SetLength(FA, coMinQueueSize);
  FCount := 0;
  FHead := 0;
  FTail := 0;
end;

procedure TIntQueue_Array.ExtendArray;
var
  VA: TIntArray;
  VHead: Integer;
  I: Integer;
begin
  // create new array of appropriate length
  SetLength(VA, Length(FA) * 2);

  // copy FA left-adjusted to VA
  VHead := FHead;
  for I := 0 to FCount - 1 do
  begin
    VA[I] := FA[VHead];
    VHead := (VHead + 1) mod Length(FA);
  end;
  FHead := 0;
  FTail := FCount;

  // assign VA to FA; old value of FA will be disposed
  FA := VA;
end;

function TIntQueue_Array.First: Integer;
begin
  Assert( not IsEmpty, 'TIntQueue_Array.First.pre failed');
  Result := FA[FHead];
end;

function TIntQueue_Array.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TIntQueue_Array.Put(AValue: Integer);
begin
  FA[FTail] := AValue;
  FTail := (FTail + 1) mod Length(FA);
  FCount := FCount + 1;
  if FCount = Length(FA)
  then ExtendArray;
  {FCount < Length(FA)}
end;

procedure TIntQueue_Array.RemFirst;
begin
  Assert( not IsEmpty, 'TIntQueue_Array.RemFirst.pre failed');
  FCount := FCount - 1;
  FHead := (FHead + 1) mod Length(FA);
  if (FCount < Length(FA) div 2) and (coMinQueueSize <= Length(FA) div 2)
  then ShrinkArray;
end;

procedure TIntQueue_Array.ShrinkArray;
var
  VA: TIntArray;
  VHead: Integer;
  I: Integer;
begin
  // create new array of appropriate length
  SetLength(VA, Length(FA) div 2);

  // copy FA left-adjusted to VA
  VHead := FHead;
  for I := 0 to FCount - 1 do
  begin
    VA[I] := FA[VHead];
    VHead := (VHead + 1) mod Length(FA);
  end;
  FHead := 0;
  FTail := FCount;

  // assign VA to FA; old value of FA will be disposed
  FA := VA;
end;

// -----------------------------------------------------------------------------
function IntQueueArrayInternally(AQueue: TIntQueue_Array): String;
var
  I: integer;
begin
  with AQueue do
  begin
    Result := '[';
    if FHead <= FTail then
      begin {Abstr = FA[FHead..FTail)}
        for I := 0 to FHead - 1 do Result := Result + ' __';
        for I := FHead to FTail - 1 do Result := Result + Format('%3d',[ FA[I] ]);
        for I := FTail to Length(FA) - 1 do Result := Result + ' __';
      end
    else
      begin {Abstr = FA[Head..L) ++ FA[0..FTail)}
        for I := 0 to FTail - 1 do Result := Result + Format('%3d',[ FA[I] ]);
        for I := FTail to FHead - 1 do Result := Result + ' __';
        for I := FHead to Length(FA) - 1 do Result := Result + Format('%3d',[ FA[I] ]);
      end;
    Result := Result + ']';
  end{with}
end;

function IntQueueArrayExternally(AQueue: TIntQueue_Array): String;
var
  I: integer;
begin
  with AQueue do
  begin
    Result := '[';
    if FHead <= FTail then
      begin {Abstr = FA[FHead..FTail)}
        for I := FHead to FTail - 1 do Result := Result + Format('%3d',[ FA[I] ]);
      end
    else
      begin {Abstr = FA[Head..L) ++ FA[0..FTail)}
        for I := FHead to Length(FA) - 1 do Result := Result + Format('%3d',[ FA[I] ]);
        for I := 0 to FTail - 1 do Result := Result + Format('%3d',[ FA[I] ]);
      end;
    Result := Result + ']';
  end{with}
end;


end.
