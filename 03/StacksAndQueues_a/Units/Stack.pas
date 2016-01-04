unit Stack;

interface

  // ---------------------------------------------------------------------------
  // Class TStackOfInt implements an *unbounded* stack of integers.
  // It provides the following public operations:
  // - Create        create stack, initially empty
  // - Count         number of elements in the stack
  // - Top           return topmost element
  // - IsEmpty       return whether stack is empty
  // - Push(AValue)  push AValue on the stack
  // - Pop           remove top element of the stack
  //
  // The implementation is based on a dynamic array FA.
  // The element count is maintained in FCount.
  // The stack elements are stored in FA at positions 0 .. FCount - 1.
  // When insufficient space is available, FA is extended.
  // When there are too many unused elements, FA is shortened.
  //----------------------------------------------------------------------------

type
  TStackOfInt =
  class(TObject)
  private
    // fields -------------------------
    FA: array of Integer; // N.B.: dynamic array
    FCount: Integer;

    // private invariants -------------
    // Pri0: coMinStackSize <= Length(FA) {N.B. 0 < coMinStackSize}
    // Pri1: 0 <= FCount <= Length(FA)

    // representation -----------------
    // Abstr = FA[0..FCount)
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

    function Top: Integer;
    // pre: not isEmpty, Abstr :: S ++ [X]
    // ret: X

    function IsEmpty: Boolean;
    // pre: true
    // ret: Count = 0

    // commands -----------------------
    procedure Push(AValue: Integer);
    // pre: Abstr = S
    // post: Abstr = S ++ [AValue]

    procedure Pop;
    // pre: not IsEmpty, Abstr :: S ++ [X]
    // post: Abstr = S

    // model variables ----------------
    // Abstr: sequence of Integer

    // public invariants ---------
    // none
  end;

function StackInternally(AStack: TStackOfInt): String;
function StackExternally(AStack: TStackOfInt): String;

implementation // ==============================================================

uses
  SysUtils; // for Format function

const
  coMinStackSize = 8;

{ TStackOfInt }

function TStackOfInt.Count: Integer;
begin
  Result := FCount;
end;

constructor TStackOfInt.Create;
begin
  inherited Create;
  SetLength(FA, coMinStackSize);
  FCount := 0;
end;

function TStackOfInt.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TStackOfInt.Pop;
begin
  Assert( not IsEmpty, 'TStackOfInt.Pop.pre failed');
  FCount := FCount - 1;
  if (FCount <= Length(FA) div 2) and (coMinStackSize <= Length(FA) div 2) then
  begin {only one half of array in use; halve its length}
    SetLength(FA, Length(FA) div 2);
  end;
  {FCount <= Length(FA)}
end;

procedure TStackOfInt.Push(AValue: Integer);
begin
  if FCount = Length(FA) then
  begin {array is full; double its length}
    SetLength(FA, 2 * Length(FA));
  end;
  {FCount < Length(FA)}
  FA[FCount] := AValue;
  FCount := FCount + 1;
  {FCount <= Length(FA)}
end;

function TStackOfInt.Top: Integer;
begin
  Assert( not IsEmpty, 'TStackOfInt.Top.pre failed');
  Result := FA[FCount - 1];
end;

// -----------------------------------------------------------------------------
function StackInternally(AStack: TStackOfInt): String;
var
  I: integer;
begin
  with AStack do
  begin
    Result := '[';
    for I := 0 to FCount - 1 do Result := Result + Format('%3d',[ FA[I] ]);
    for I := FCount to Length(FA) - 1 do Result := Result + ' __';
    Result := Result + ']';
  end{with};
end;

function StackExternally(AStack: TStackOfInt): String;
var
  I: integer;
begin
  with AStack do
  begin
    Result := '[';
    for I := 0 to FCount - 1 do Result := Result + Format('%3d',[ FA[I] ]);
    Result := Result + ']';
  end{with};
end;

end.





