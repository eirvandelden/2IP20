unit Main;

//------------------------------------------------------------------------------
// Deze Main unit moet nog in twee opzichten worden aangepast:
//
// 1. De user interface is onvolledig. De volgende elementen moeten nog worden
// toegevoegd en/of uitgewerkt:
// - Plaatsing van een StringGrid van de juiste dimensies op het tabblad Edit
//   en het instellen van de properties van dat StringGrid;
// - Uitwerking van de procedure UpdateStringGrid, die de weergave in het
//   StringGrid moet aanpassen aan de inhoud van FPersonList;
// - Uitwerking van de procedure UpdateButtonStates, die de waarde van de
//   Enabled property van de FindButton, AddButton en DeleteButton aanpast aan
//   de toestanden van de andere GUI-componenten en van FPersonList;
// - Toevoegen van OnClick event handlers voor de FindButton, AddButton en
//   DeleteButton.
//
// 2. Nadat de implementatie in de unit PersonStringList is uitgewerkt, moet in
// de onderstaande code het gebruik van de unit PersonArray vervangen worden
// door de unit PersonStringList. De twee plaatsen waar aanpassingen moeten
// worden uitgevoerd zijn gemerkt met commentaar van de vorm //*  .
//------------------------------------------------------------------------------

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ExtCtrls, StdCtrls, ComCtrls, Menus,
  PersonBase,
  PersonArray;
//*  PersonStringList;

type
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    PrintSetup1: TMenuItem;
    Print1: TMenuItem;
    N2: TMenuItem;
    SaveAs1: TMenuItem;
    Save1: TMenuItem;
    Open1: TMenuItem;
    New1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    FindButton: TButton;
    Memo1: TMemo;
    LabeledEdit1: TLabeledEdit;
    Panel1: TPanel;
    Panel2: TPanel;
    TabSheet3: TTabSheet;
    Memo2: TMemo;
    FamilyNameEdit: TLabeledEdit;
    PrefixEdit: TLabeledEdit;
    InitialsEdit: TLabeledEdit;
    FirstNameEdit: TLabeledEdit;
    RoomNumberEdit: TLabeledEdit;
    PhoneNumberEdit: TLabeledEdit;
    AddButton: TButton;
    DeleteButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure New1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure FamilyNameEditChange(Sender: TObject);
    procedure LabeledEdit1Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FPersonList: TPersonList;
    FCurrentFileName: String;
    FModified: Boolean;
    procedure DoNew;
    procedure DoOpen(AFileName: String);
    procedure DoSave(AFileName: String);
    procedure UpdateButtonStates;
    procedure UpdateStringGrid;
  end;

var
  Form1: TForm1;

implementation

uses
  IniFiles;

{$R *.dfm}

procedure TForm1.DoNew;
begin
  FPersonList.Clear;
  FCurrentFileName := 'Untitled';
  FModified := false;
  UpdateStringGrid;
  UpdateButtonStates;
end;

procedure TForm1.DoOpen(AFileName: String);
var
  VIniFile: TMemIniFile;
  VSections: TStringList;
  I: Integer;
  VPersonData: TPersonData;
begin
  // create inifile
  VIniFile := TMemIniFile.Create(AFileName);

  // show contents in Memo2;
  Memo2.Clear;
  VIniFile.GetStrings(Memo2.Lines);

  // assign contents to FPersonList;
  FPersonList.Clear;
  VSections := TStringList.Create;
  VIniFile.ReadSections(VSections);
  for I := 0 to VSections.Count - 1 do
  begin
    // create and fill VPersonData;
    VPersonData := TPersonData.Create;
    with VPersonData do
    begin
      FamilyName := VSections[I];
      Prefix := VIniFile.ReadString(VSections[I],'prefix', '');
      Initials := VIniFile.ReadString(VSections[I], 'initials', '');
      FirstName := VIniFile.ReadString(VSections[I], 'firstname', '');
      RoomNumber := VIniFile.ReadString(VSections[I], 'roomnumber', '');
      PhoneNumber := VIniFile.ReadString(VSections[I], 'phonenumber', '');
    end;
    FPersonList.Add(VPersonData);
  end;

  // free inifile
  VIniFile.Free;

end;

procedure TForm1.DoSave(AFileName: String);
var
  VIniFile: TMemIniFile;
  I: Integer;
begin
  // create inifile
  VIniFile := TMemIniFile.Create(AFileName);
  VIniFile.Clear;

  // write contents of FPersonList
  for I := 0 to FPersonList.Count - 1 do
  begin
    with FPersonList.Get(I) do
    begin
      VIniFile.WriteString(FamilyName, 'prefix', Prefix);
      VIniFile.WriteString(FamilyName, 'initials', Initials);
      VIniFile.WriteString(FamilyName, 'firstname', FirstName);
      VIniFile.WriteString(FamilyName, 'roomnumber', RoomNumber);
      VIniFile.WriteString(FamilyName, 'phonenumber', PhoneNumber);
    end{with};
  end;

  // update and free inifile
  VIniFile.UpdateFile;
  VIniFile.Free;
end;

procedure TForm1.UpdateButtonStates;
begin
  {TODO}
end;


procedure TForm1.UpdateStringGrid;
begin
  {TODO}
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPersonList := TPersonList_Array.Create;
//*  FPersonList := TPersonList_StringList.Create;

  DoNew;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FPersonList.Free;
end;

procedure TForm1.New1Click(Sender: TObject);
begin
  //  'Save current file?' dialog should be added here.

  DoNew;
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  //  'Save current file?' dialog should be added here.

  if OpenDialog1.Execute then
  begin
    FCurrentFileName := OpenDialog1.FileName;
    DoOpen(FCurrentFileName);
    FModified := false;
    UpdateStringGrid;
    UpdateButtonStates;
  end;
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  if FModified then
  begin
    DoSave(FCurrentFileName);
    FModified := false;
  end;
end;

procedure TForm1.SaveAs1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
  begin
    FCurrentFileName := SaveDialog1.FileName;
    DoSave(FCurrentFileName);
    FModified := false;
  end;
end;

procedure TForm1.FamilyNameEditChange(Sender: TObject);
begin
  UpdateButtonStates;
end;

procedure TForm1.LabeledEdit1Change(Sender: TObject);
begin
  UpdateButtonStates;
end;

end.
