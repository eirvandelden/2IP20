unit AbsQueue;

interface

type
  TIntQueue =
  class(TObject)
  public
    // queries ------------------------
    function Count: Integer; virtual; abstract;
    // pre: true
    // ret: |Abstr|

    function First: Integer; virtual; abstract;
    // pre: not IsEmpty, Abstr :: [X] ++ S
    // ret: X

    function IsEmpty: Boolean; virtual; abstract;
    // pre: true
    // ret: Count = 0

    // commands -----------------------
    procedure Put(AValue: Integer); virtual; abstract;
    // pre: Abstr = S
    // post: Abstr = S ++ [AValue]

    procedure RemFirst; virtual; abstract;
    // pre: not IsEmpty, Abstr :: [X] ++ S
    // post: Abstr = S

    // model variables ----------------
    // Abstr: sequence of Integer

    // public invariants --------------
    // none
  end;


implementation

end.
