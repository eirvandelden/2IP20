unit BoundedStack;

interface

  // ---------------------------------------------------------------------------
  // Class TBoundedStackOfInt implements a *bounded* stack of integers.
  // It provides the following public operations:
  // - Create(ASize) create stack, initially empty, with length bounded by ASize
  // - Count         number of elements in the stack
  // - Size          upper bound for Count
  // - Top           return topmost element
  // - IsEmpty       return whether stack is empty
  // - IsFull        return whether stack is full
  // - Push(AValue)  push AValue on the stack
  // - Pop           remove top element of the stack
  //
  // The implementation is based on a dynamic array FA of which the length
  // is set by Create; the length remains constant.
  // The element count is maintained in FCount.
  // The stack elements are stored in FA at positions 0 .. FCount - 1
  //----------------------------------------------------------------------------

type
  TBoundedStackOfInt =
  class(TObject)
  private
    // fields -------------------------
    FA: array of Integer; // N.B.: dynamic array
    FCount: Integer;

    // private invariants -------------
    // Pri0: 0 < Length(FA)
    // Pri1: 0 <= FCount <= Length(FA)

    // representation -----------------
    // Abstr = FA[0..FCount)
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

    function Top: Integer;
    // pre: not isEmpty, Abstr :: S ++ [X]
    // ret: X

    function IsEmpty: Boolean;
    // pre: true
    // ret: Count = 0

    function IsFull: Boolean;
    // pre: true
    // ret: Count = Size

    // commands -----------------------
    procedure Push(AValue: Integer);
    // pre: not IsFull, Abstr = S
    // post: Abstr := S ++ [AValue]

    procedure Pop;
    // pre: not IsEmpty, Abstr :: S ++ [X]
    // post: Abstr = S

    // model variables ----------------
    // Abstr: sequence of Integer

    // public invariants --------------
    // I0: 0 < Size
    // I1: 0 <= Count <= Size
  end;


function StackInternally(AStack: TBoundedStackOfInt): String;
function StackExternally(AStack: TBoundedStackOfInt): String;

implementation // ==============================================================

uses
  SysUtils; // for Format function

{ TBoundedStackOfInt }

function TBoundedStackOfInt.Count: Integer;
begin
  Result := FCount;
end;

constructor TBoundedStackOfInt.Create(ASize: Integer);
begin
  Assert(0 < ASize,
    Format('TBoundedStackOfInt.Create.pre failed; ASize = %d', [ASize] ));
  inherited Create;
  SetLength(FA, ASize);
  FCount := 0;
end;

function TBoundedStackOfInt.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

function TBoundedStackOfInt.IsFull: Boolean;
begin
  Result := Count = Size;
end;

procedure TBoundedStackOfInt.Pop;
begin
  Assert( not IsEmpty, 'TBoundedStackOfInt.Pop.pre failed');
  FCount := FCount - 1;
end;

procedure TBoundedStackOfInt.Push(AValue: Integer);
begin
  Assert( not IsFull, 'TBoundedStackOfInt.Push.pre failed');
  FA[FCount] := AValue;
  FCount := FCount + 1;
end;

function TBoundedStackOfInt.Size: Integer;
begin
  Result := Length(FA);
end;

function TBoundedStackOfInt.Top: Integer;
begin
  Assert( not IsEmpty, 'TBoundedStackOfInt.Top.pre failed');
  Result := FA[FCount - 1];
end;

// -----------------------------------------------------------------------------
function StackInternally(AStack: TBoundedStackOfInt): String;
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

function StackExternally(AStack: TBoundedStackOfInt): String;
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
