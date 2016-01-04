unit BoundedQueue;


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


var
  limit: Integer; //max limiet


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
function IsValid(AIndex: Integer; AQueue: TBoundedQueueOfInt): Boolean;


implementation // ==============================================================

uses
  SysUtils, Math; // for Format function

{ TBoundedQueueOfInt }

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
  FCount := 0;
end;

function TBoundedQueueOfInt.IsEmpty: Boolean;
begin
  {TODO}
    Result := Count = 0;
end;

function TBoundedQueueOfInt.IsFull: Boolean;
begin
  {TODO}
    Result := Count = 20;
end;

procedure TBoundedQueueOfInt.Put(AValue: Integer);
begin
  {TODO}
     Assert( not IsFull, 'TBoundedStackOfInt.Push.pre failed');

     FCount := FCount + 1;

     FA[FTail] := AValue;
     FTail := FTail + 1;
     if FTail >= Length(FA) then begin
        FTail := 0;
     end

end;

procedure TBoundedQueueOfInt.RemFirst;

var
   I: Integer;
   
begin
  {TODO}

  Assert( not IsEmpty, 'TBoundedQueueOfInt.RemFirst.pre failed');

  FCount := FCount - 1;
  FHead := FHead + 1;
  if FHead >= Length(FA) then begin
    FHead := 0;
  end

end;
  
function TBoundedQueueOfInt.Size: Integer;
begin
  {TODO}
    Result := Length(FA);
end;

function TBoundedQueueOfInt.First: Integer;
begin
  {TODO}
    Assert( not IsEmpty, 'TBoundedStackOfInt.Top.pre failed');
  Result := FHead * 100 + FTail;
end;

function IsValid(AIndex: Integer; AQueue: TBoundedQueueOfInt): Boolean;
begin
Result := False;
  with AQueue do begin
    if (AIndex >= FHead) and (AIndex < (FTail)) then begin
      Result := True;
    end  // if
    else if (AIndex < FTail) and (FTail < FHead) then begin
      Result := True;
    end // else if
    else if (FTail < FHead) and (FHead < AIndex) then begin
      Result := True;
    end // else if
    else if IsFull then begin
      Result := True;
    end // else if
 {   else begin
      Result := False;
    end; // else if
  }
  end; //with
end;

// -----------------------------------------------------------------------------
function QueueInternally(AQueue: TBoundedQueueOfInt): String;

var
 I:Integer;

begin
  with AQueue do
  begin
    {TODO}
    Result := '[';

    for I := 0 to Length(FA) - 1 do begin
      if isValid(I, AQueue) then begin
        Result := Result + Format('%3d',[ FA[I] ]); 
      end
      else begin
        Result := Result + ' __';
      end;
    end;
   

    Result := Result + ']';
  end{with};

 
end;

function QueueExternally(AQueue: TBoundedQueueOfInt): String;

var
   I:Integer;

begin
  with AQueue do
  begin
    {TODO}
    Result := '[';
    for I := 0 to Length(FA) - 1 do Result := Result + Format('%3d',[ FA[I] ]);
    Result := Result + ']';
  end{with};
end;

end.
