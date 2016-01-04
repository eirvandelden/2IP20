unit PersonBase;

interface

uses
  SysUtils;

type
  TPersonData =
  class(TObject)
    FamilyName: String;
    Prefix: String;
    Initials: String;
    FirstName: String;
    RoomNumber: String;
    PhoneNumber: String;
  end;

  TPersonList =
  class(TObject)
  public
    // primitive queries ---------------
    function Count: Integer; virtual; abstract;
    // pre: true
    // ret: |(Abstr)|

    function Indexof(AFamilyName: String): Integer; virtual; abstract;
    // pre: true
    // ret: I s.t. Abstr[I].s = AFamilyName, otherwise -1

    function Get(I: Integer): TPersonData; virtual; abstract;
    // pre: 0 <= I < |Abstr|
    // ret: Abstr[I].d

    // derived queries ----------------

    function Has(AFamilyName: String): Boolean; virtual;
    // pre: true
    // ret: Indexof(AFamilyName) <> -1

    function GetDataOf(AFamilyName: string): TPersonData; virtual;
    // pre: Has(AFamilyName)
    // ret: Abstr[I].d, where I = IndexOf(AFamilyName)

    // commands ------------------------
    procedure Clear; virtual; abstract;
    // pre: true
    // post: Abstr = []

    procedure Add(AData: TPersonData); virtual; abstract;
    // pre: not Has(AData.FamilyName)
    // effect: Abstr := old Abstr ++ [(AData.FamilyName, Adata)]

    procedure Change(AFamilyName: String; ANewData: TPersonData); virtual; abstract;
    // pre: Abstr :: S1 ++ [(AFamilyName, d)] ++ S2
    //      AFamilyName = ANewData.FamilyName
    // post: Abstr = S1 ++ [(AFamilyName, ANewData)] ++ S2

    procedure Delete(AFamilyName: String); virtual; abstract;
    // pre: Abstr :: S1 ++ [(AFamilyName, d)] ++ S2
    // post: Abstr = S1 ++ S2


    // model variables -----------------
    // Abstr: finite list of pairs (s,d), where s in String and d in TPersonData

    // invariants ----------------------
    // IsMap: (forall I, J: 0 <= I < J < |Abstr|: Abstr[I}.s <> Abstr[J].s)
    // Consistent: (forall I: 0 <= I < |Abstr|: Abstr[I].s = Abstr[I].d.FamilyName)
    //
  end;

implementation //===============================================================

{ TPersonList }

function TPersonList.GetDataOf(AFamilyName: string): TPersonData;
var
  I: Integer;
begin
  I := Indexof(AFamilyName);
  Assert(I <> -1,
    Format('TPersonList.GetDataOf.pre failed; AFamilyName = %s', [AFamilyName]));
  Result := Get(I);
end;

function TPersonList.Has(AFamilyName: String): Boolean;
begin
  Result := IndexOf(AFamilyName) <> -1;
end;

end.
 