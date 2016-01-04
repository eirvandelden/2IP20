unit AbsStack;

interface

  //----------------------------------------------------------------------------
  // TIntStack is an abstract class which defines an interface for stack
  // operations.
  // It provides the following public operations:
  // - Create        create stack, initially empty
  // - Count         number of elements in the stack
  // - Top           return topmost element
  // - IsEmpty       return whether stack is empty
  // - Push(AValue)  push AValue on the stack
  // - Pop           remove top element of the stack
  //----------------------------------------------------------------------------


type
  TIntStack =
  class(TObject)
  public
    // queries ------------------------
    function Count: Integer; virtual; abstract;
    // pre: true
    // ret: |Abstr|

    function Top: Integer; virtual; abstract;
    // pre: not isEmpty, Abstr :: S ++ [X]
    // ret: X

    function IsEmpty: Boolean; virtual; abstract;
    // pre: true
    // ret: Count = 0

    // commands -----------------------
    procedure Push(AValue: Integer); virtual; abstract;
    // pre: Abstr = S
    // post: Abstr = S ++ [AValue]

    procedure Pop; virtual; abstract;
    // pre: not IsEmpty, Abstr :: S ++ [X]
    // post: Abstr = S

    // model variables ----------------
    // Abstr: sequence of Integer

    // public invariants ---------
    // none
  end;


implementation

end.
