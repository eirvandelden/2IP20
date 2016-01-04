unit PersonArray;

interface

uses
  PersonBase;

const
  coDefaultArraySize = 8;

type
  TPersonList_Array =
  class(TPersonList)
  protected
    FA: array of TPersonData; // N.B.: dynamic array
    FCount: Integer;

    // invariants ----------------------
    // 0 <= FCount <= Length(FA)

    // representation ------------------
    // Count = FCount
    // Abstr = [(FA[I].FamilyName, FA[I]) | 0 <= I <F Count]

  public
    // construction and destruction
    constructor Create;
    // pre: true
    // post: Abstr = []
    //       Length(FA) = coDefaultArraySize

    destructor Destroy;override;

    // primitive queries ---------------
    function Count: Integer; override;
    // pre: true
    // ret: |(Abstr)|

    function Indexof(AFamilyName: String): Integer; override;
    // pre: true
    // ret: I s.t. Abstr[I].s = AFamilyName, otherwise -1

    function Get(I: Integer): TPersonData; override;
    // pre: 0 <= I < |Abstr|
    // ret: Abstr[I].d

    // commands ------------------------
    procedure Clear; override;
    // pre: true
    // post: Abstr = []

    procedure Add(AData: TPersonData); override;
    // pre: not Has(AData.FamilyName)
    // effect: Abstr := old Abstr ++ [(AData.FamilyName, Adata)]

    procedure Change(AFamilyName: String; ANewData: TPersonData); override;
    // pre: Abstr :: S1 ++ [(AFamilyName, d)] ++ S2
    //      AFamilyName = ANewData.FamilyName
    // post: Abstr = S1 ++ [(AFamilyName, ANewData)] ++ S2

    procedure Delete(AFamilyName: String); override;
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

uses
  SysUtils;

{ TPersonList_Array }

procedure TPersonList_Array.Add(AData: TPersonData);
begin
  // check predonsition
  Assert( not Has(AData.FamilyName),
    Format('TPersonList_Array.Add.pre failed; FamilyName = %s',
      [AData.FamilyName]));

  // extend FA if necessary
  if FCount = Length(FA) then
  begin
    SetLength(FA, 2 * Length(FA))
  end;

  {FCount < Length(FA)}
  FA[FCount] := AData;
  FCount := Fcount + 1;
end;

procedure TPersonList_Array.Change(AFamilyName: String; ANewData: TPersonData);
var
  I: Integer;
begin
  I := IndexOf(AFamilyName);
  // check pre-condition
  Assert(I <> -1,
    Format('TPersonList_Array.Change.pre1 failed; FamilyName = %s',
      [AFamilyName]));
  Assert(AFamilyName = ANewData.FamilyName,
    Format('TPersonList_Array.Change.pre2 failed; FamilyName = %s, ANewData.FamilyName = %s',
      [AFamilyName, ANewdata.FamilyName]));

  // replace data
  FA[I].Free;
  FA[I] := ANewData;
end;

procedure TPersonList_Array.Clear;
var
  I: Integer;
begin
  for I := 0 to FCount - 1
  do FA[I].Free;
  FCount := 0;
  SetLength(FA, coDefaultArraySize);
end;

function TPersonList_Array.Count: Integer;
begin
  Result := FCount;
end;

constructor TPersonList_Array.Create;
begin
  inherited Create;
  SetLength(FA, coDefaultArraySize);
  FCount := 0;
end;

procedure TPersonList_Array.Delete(AFamilyName: String);
var
  I, J: Integer;
begin
  // locate position
  I := Indexof(AFamilyName);

  // check pre-condition
  Assert(I <> -1,
    Format('TPersonList_Array.Delete.pre failed; FamilyName = %s',
      [AFamilyName]));

  // shift FA[I+1.. FCount) downward
  for J := I + 1 to FCount - 1
  do FA[J - 1] := FA[J];
  FCount := Fcount - 1;

  // contract FA, if necessary
  if (coDefaultArraySize <= FCount) and (FCount <= Length(FA) div 2)
  then SetLength(FA, Length(FA) div 2);
end;

destructor TPersonList_Array.Destroy;
begin
  Finalize(FA);
  inherited Destroy;
end;

function TPersonList_Array.Get(I: Integer): TPersonData;
begin
  // check pre-condition
  Assert((0 <= I) and (I < Count) ,
    Format('TPersonList_Array.Get.pre failed; I = %d',[I]));

  Result := FA[I];
end;

function TPersonList_Array.Indexof(AFamilyName: String): Integer;
var
  I: Integer;
begin
  // sloppy linear search
  Result := -1;
  for I := 0 to FCount - 1 do
  begin
    if FA[I].FamilyName = AFamilyName
    then Result := I;
  end;
end;

end.
