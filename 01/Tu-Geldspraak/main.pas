{ Naam: Etienne van Delden
  Studentnummer: 0618959
  Datum: 01-04-2007}

unit main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, converteer;
  
const
  Digit = ['0' .. '9'];                 // De geldige char's voor invoer

type

  { TMainForm }

  TMainForm = class(TForm)
    cmb_convert: TButton;
    EditResultaat: TEdit;
    EditCenten: TEdit;
    EditEuro: TEdit;
    Label1: TLabel;
    LabelEuro: TLabel;
    procedure EditCentenChange(Sender: TObject);
    procedure EditEuroChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure LabelEuroClick(Sender: TObject);
    procedure cmb_convertClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    function ValidEuro: Boolean;
    function ValidCent: Boolean;
	function ValidBedrag: Boolean;
    
  end; 

var
  MainForm: TMainForm;

implementation

{ TMainForm }

 // Hebben we een geldig xx Euro bedrag? 
function TMainform.ValidEuro: Boolean;
{ pre: true
  retL 1<-Lenght(S) /\ (forall i:1<=i<=Lenght(S). S[i] in Digit),
       waarin S=EditEuro.text
 }
 
var
  S: String;
  I: Integer;

begin
  S := EditEuro.Text;
  Result := (1 <= Length(S)) and (Length(S) <= 6);
  if Result then
    for I := 1 to Length(S) do
      Result := Result and (S[I] in Digit);
end;

 // Hebben we een geldig xx Eurocent bedrag?
function TMainForm.ValidCent: Boolean;
      { pre : true
        ret : Length(S) = 2) /\ (S[1] in Digit) /\ (S[2] in Digit),
              waarin S=EditCent.text
	}

var
  S: String;

begin
  S := EditCenten.Text;
  Result := (Length(S) = 2) and (S[1] in Digit) and
    (S[2] in Digit);
end;
  
  // Is het bedrag wat we krijgen wel geldig?
function TMainForm.ValidBedrag: Boolean;
      { pre: true
        ret: ValidEuro /\ ValidCent
	}

begin
  Result := ValidEuro and ValidCent;
end;

  // Bij het veranderen van het euro bedrag
procedure TMainForm.EditEuroChange(Sender: TObject);

var S: ShortString;                     // var om het bedrag in woorden in te zetten

begin
  if ValidBedrag then                   // \o/ een geldig bedrag
  begin
      cmb_convert.Enabled := true;      // we mogen nu het bedrag omzetten
      S := ' ';                         // init van S
        // nu het bedrag om in woorden zetten
      TekstVanBedrag( StrToInt(EditEuro.Text), StrToInt(EditCenten.Text), S );
      EditResultaat.Text := S;          // print het bedrag
  end
  else cmb_convert.Enabled := false;    // NO! It's not a valid input!
       
end;

  // Bij het veranderen van het eurocent bedrag
procedure TMainForm.EditCentenChange(Sender: TObject);
var S: ShortString;                     // var om het bedrag in woorden in te zetten

begin
  if  ValidBedrag then                   // \o/ een geldig bedrag
  begin
    cmb_convert.Enabled := true;      // we mogen nu het bedrag omzetten
    S:= ' ';                         // init van S
        // nu het bedrag om in woorden zetten
    TekstVanBedrag( StrToInt(EditEuro.Text), StrToInt(EditCenten.Text), S );
    EditResultaat.Text := S;          // print het bedrag
  end
  else cmb_convert.Enabled := false;    // NO! It's not a valid input!


end;

  // De grote omzet knop staat bij het starten van het programma uit
procedure TMainForm.FormCreate(Sender: TObject);
begin
  cmb_convert.Enabled := false;
end;

procedure TMainForm.Label1Click(Sender: TObject);
begin

end;

procedure TMainForm.LabelEuroClick(Sender: TObject);
begin

end;

  // Als er geklikt word op de grote omzet knop
procedure TMainForm.cmb_convertClick(Sender: TObject);
var
   S: ShortString;                      // hulp var
begin
   S:= '';                              // init hulp var
      // zet het bedrag om in woorden
   TekstVanBedrag( StrToInt(EditEuro.Text), StrToInt(EditCenten.Text), S );
   EditResultaat.Text := S;             // print het bedrag in woorden
   
end;

initialization
  {$I main.lrs}

end.