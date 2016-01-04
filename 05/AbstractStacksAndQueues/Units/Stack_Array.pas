unit Stack_Array;

interface

uses
  AbsStack;

  // ---------------------------------------------------------------------------
  // Class TIntStack_Array implements the abstract class TIntStack by means
  // of an unbounded array. It works in essentially the same way as the class
  // TStackOfInt.
  //
  // The implementation is based on a dynamic array FA.
  // The element count is maintained in FCount.
  // The stack elements are stored in FA at positions 0 .. FCount - 1.
  // When insufficient space is available, FA is extended.
  // When there are too many unused elements, FA is shortened.
  //----------------------------------------------------------------------------

type
  TIntStack_Array =
  class(TIntStack)
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
    function Count: Integer; override;
    // pre: true
    // ret: |Abstr|

    function Top: Integer; override;
    // pre: not isEmpty, Abstr :: S ++ [X]
    // ret: X

    function IsEmpty: Boolean; override;
    // pre: true
    // ret: Count = 0

    // commands -----------------------
    procedure Push(AValue: Integer); override;
    // pre: Abstr = S
    // post: Abstr = S ++ [AValue]

    procedure Pop; override;
    // pre: not IsEmpty, Abstr :: S ++ [X]
    // post: Abstr = S

    // model variables ----------------
    // Abstr: sequence of Integer

    // public invariants ---------
    // none
  end;

function IntStackArrayInternally(AStack: TIntStack_Array): String;
function IntStackArrayExternally(AStack: TIntStack_Array): String;

implementation // ==============================================================

uses
  SysUtils; // for Format function

const
  coMinStackSize = 8;

{ TIntStack_Array }

function TIntStack_Array.Count: Integer;
begin
  Result := FCount;
end;

constructor TIntStack_Array.Create;
begin
  inherited Create;
  SetLength(FA, coMinStackSize);
  FCount := 0;
end;

function TIntStack_Array.IsEmpty: Boolean;
begin
  Result := Count = 0;
end;

procedure TIntStack_Array.Pop;
begin
  Assert( not IsEmpty, 'TIntStack_Array.Pop.pre failed');
  FCount := FCount - 1;
  if (FCount <= Length(FA) div 2) and (coMinStackSize <= Length(FA) div 2) then
  begin {only one half of array in use; halve its length}
    SetLength(FA, Length(FA) div 2);
  end;
  {FCount <= Length(FA)}
end;

procedure TIntStack_Array.Push(AValue: Integer);
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

function TIntStack_Array.Top: Integer;
begin
  Assert( not IsEmpty, 'TIntStack_Array.Top.pre failed');
  Result := FA[FCount - 1];
end;

// -----------------------------------------------------------------------------
function IntStackArrayInternally(AStack: TIntStack_Array): String;
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

function IntStackArrayExternally(AStack: TIntStack_Array): String;
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





