unit Stack_Linked;

interface

uses
  AbsStack;

  //----------------------------------------------------------------------------
  //  Class TIntStack_Linked implements the abstract class by means of a single-
  //  linked list.
  //
  //  FList points to the first cell (containing the top value), if any
  //  FCount maintains the number of cells in the linked list.
  //----------------------------------------------------------------------------

type
  PCell = ^TCell;
  TCell =
  record
    FVal: Integer;
    FNext: PCell;
  end;

type
  TIntStack_Linked =
  class(TIntStack)
  protected
    FList: PCell;    // points to first cell (containing the top value), if any
    FCount: Integer; // number of cells in linked list

    // auxiliary procedures -----------
    procedure DisposeList;
    // pre: FList -> C1 -> ... -> Cn
    // post: C1,...,Cn have been disposed;

    // protected invariants -----------
    // FList(^.FNext)^^FCount = nil , where (A)^^B stands for B repetitions of A
    // N.B. in particular, if FCount = 0, then FList = nil

    // representation -----------------
    // Abstr = [FList(^.FNext)^^(FCount-1).FVal, ..., FList^.FVal]
    // N.B. in particular, if FCount = 0, then Abstr = []

  public
    // construction and destruction
    constructor Create;
    // pre: true
    // post: Abstr = []

    destructor Destroy; override;

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

    // public invariants --------------
    // none
  end;

function IntStackLinkedExternally(AStack: TIntStack_Linked): String;


implementation //===============================================================

uses
  SysUtils; // for Format function

{ TIntStack_Linked }

function TIntStack_Linked.Count: Integer;
begin
  Result := FCount;
end;

constructor TIntStack_Linked.Create;
begin
  inherited Create;
  FList := nil;
  FCount := 0;
end;

destructor TIntStack_Linked.Destroy;
begin
  DisposeList;
  inherited Destroy;
end;

procedure TIntStack_Linked.DisposeList;
var
  P, H: PCell;
begin
  P := FList;
  while P <> nil do
  begin
    H := P;
    P := P^.FNext;
    Dispose(H);
  end;
end;

function TIntStack_Linked.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

procedure TIntStack_Linked.Pop;
var
  H: Pcell;
begin
  Assert( not IsEmpty, 'TIntStack_Linked.Pop.pre failed');
  H := FList;
  FList := FList^.FNext;
  Dispose(H);
  FCount := FCount - 1;
end;

procedure TIntStack_Linked.Push(AValue: Integer);
var
  H: PCell;
begin
  New(H);
  H^.FVal := AValue;
  H^.FNext := FList;
  FList := H;
  FCount := FCount + 1;
end;

function TIntStack_Linked.Top: Integer;
begin
  Assert( not IsEmpty, 'TIntStack_Linked.Pop.pre failed');
  Result := FList^.FVal;
end;

function IntStackLinkedExternally(AStack: TIntStack_Linked): String;
var
  H: PCell;
begin
  // N.B.: stack is shown with top on the right
  with AStack do
  begin
    H := FList;
    Result := ']';
    while H <> nil do
    begin
      Result := Format('%3d',[ H^.FVal ]) + Result;
      H := H^.FNext;
    end;
    Result := '[' + Result;
  end{with}
end;


end.
