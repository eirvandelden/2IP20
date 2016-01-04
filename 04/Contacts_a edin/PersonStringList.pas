unit PersonStringList;

//------------------------------------------------------------------------------
// In deze unit wordt een class TPersonList_StringList gedefinieerd, die afstamt
// van de abstract base class TPersonList. De implementatie maakt gebruik van
// een protected field FList van het type TStringList (een Delphi container
// class).
//
// Van de te maken class zijn de header, het public contract en de lege method
// declarations gegeven. De interne invarianten en de code van de methods moeten
// nog ingevuld worden op de met {TODO} aangegeven plaatsen.
//------------------------------------------------------------------------------

interface

uses
  Classes,
  PersonBase;

type
  TPersonList_StringList =
  class(TPersonList)
  protected
    FList: TStringList;
    FCount: Integer;
    // invariants ----------------------
    {TODO}

    // representation ------------------
    {TODO}

  public
    // construction and destruction
    constructor Create;
    // pre: true
    // post: Abstr = []

    destructor Destroy;override;

    // primitive queries ---------------
    function Count: Integer; override;
    // pre: true
    // ret: |dom(Abstr)|

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

{ TPersonList_StringList }

procedure TPersonList_StringList.Add(AData: TPersonData);
begin
  //Assert( not Has(AData.FamilyName),
  //Format('TPersonList_Array.Add.pre failed; FamilyName = %s',
  //    [AData.FamilyName]));

end;

procedure TPersonList_StringList.Change(AFamilyName: String; ANewData: TPersonData);
begin
  {TODO}
end;

procedure TPersonList_StringList.Clear;
begin
  {TODO}
end;

function TPersonList_StringList.Count: Integer;
begin
  Result := FCount;
end;

constructor TPersonList_StringList.Create;
begin
  inherited Create;
  FCount := 0;

end;

procedure TPersonList_StringList.Delete(AFamilyName: String);
begin
  {TODO}
end;

destructor TPersonList_StringList.Destroy;
begin
  {TODO}
end;

function TPersonList_StringList.Get(I: Integer): TPersonData;
begin
  Result := FList.Get(I);
end;

function TPersonList_StringList.Indexof(AFamilyName: String): Integer;
begin

  Result := FList.Indexof(AFamilyName);

end;

end.

