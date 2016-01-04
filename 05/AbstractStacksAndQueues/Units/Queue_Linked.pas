unit Queue_Linked;

interface

uses
  AbsQueue;

  //----------------------------------------------------------------------------
  //  Class TIntQueue_Linked implements the abstract class TIntQueue by means of
  //  a single-linked list.
  //
  //  FHead points to the first cell, if any
  //  FTail points to the last cell, if any
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
  TIntQueue_Linked =
  class(TIntQueue)
  protected
    FHead: PCell;
    FTail: PCell;
    FCount: Integer;

    // auxiliary procedures -----------
    procedure DisposeList;
    // pre: FHead -> C1 -> ... -> Cn
    // post: C1,...,Cn have been disposed;

    // protected invariants -----------
    // ((FCount = 0) and (FHead = nil) and (FTail = nil)) or
    //   ((FCount > 0) and (FHead(^.FNext)^^(FCount - 1) = FTail))

    // representation -----------------
    // Abstr = [ (FHead(^.FNext)^^I)^.FVal | 0 <= I < FCount]

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

function IntQueueLinkedExternally(AQueue: TIntQueue_Linked): String;

implementation //===============================================================

uses
  SysUtils; // for Format function

{ TIntQueue_Linked }

function TIntQueue_Linked.Count: Integer;
begin
  Result := FCount;
end;

constructor TIntQueue_Linked.Create;
begin
  inherited Create;
  FHead := nil;
  FTail := nil;
end;

destructor TIntQueue_Linked.Destroy;
begin
  DisposeList;
  inherited Destroy;
end;

procedure TIntQueue_Linked.DisposeList;
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
end;

function TIntQueue_Linked.First: Integer;
begin
  Assert( not IsEmpty, 'TIntQueue_Linked.First.pre failed');
  Result := FHead^.FVal;
end;

function TIntQueue_Linked.IsEmpty: Boolean;
begin
  Result := FCount = 0;
end;

procedure TIntQueue_Linked.Put(AValue: Integer);
var
  H: PCell;
begin
  // create and fill new cell
  New(H);
  H^.FVal := AValue;
  H^.FNext := nil;

  // append cell to queue
  if FHead = nil
  then FHead := H
  else FTail^.FNext := H;
  FTail := H;
  FCount := FCount + 1;
end;

procedure TIntQueue_Linked.RemFirst;
var
  H: PCell;
begin
  // check pre-condition
  Assert( not IsEmpty, 'TIntQueue_Linked.RemFirst.pre failed');

  // detach first cell
  H := FHead;
  FHead := FHead^.FNext;
  if FHead = nil
  then FTail := nil;
  FCount := FCount - 1;

  // dispose detached cell
  Dispose(H);
end;

function IntQueueLinkedExternally(AQueue: TIntQueue_Linked): String;
var
  I: integer;
  H: PCell;
begin
  with AQueue do
  begin
    Result := '[';
    H := FHead;
    while H <> nil do
    begin
      Result := Result + Format('%3d',[ H^.FVal ]);
      H := H^.FNext;
    end{while};
    Result := Result + ']';
  end{with}
end;


end.
 