unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ExtCtrls, StdCtrls, ComCtrls, Menus,
  PersonBase,
  PersonLinkedlist;

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
    StringGrid1: TStringGrid;
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
    procedure FindButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure FamilyNameEditChange(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure LabeledEdit1Change(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure StringGrid1Click(Sender: TObject);
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
  FindButton.Enabled := FPersonList.Has(LabeledEdit1.Text);
  AddButton.Enabled :=
    (FamilyNameEdit.Text <> '') and not FPersonList.Has(FamilyNameEdit.Text);
  DeleteButton.Enabled :=
    (1 <= StringGrid1.Row) and
    (StringGrid1.Row < StringGrid1.RowCount) and
    (StringGrid1.Cells[0,StringGrid1.Row] <> '');
end;


procedure TForm1.UpdateStringGrid;
var
  I: Integer;
  VPersonData: TPersonData;
begin
  with StringGrid1 do
  begin
    // set column headings
    with Rows[0] do
    begin
      Add('Family Name');
      Add('Prefix');
      Add('Initials');
      Add('First Name');
      Add('Room Number');
      Add('Phone Number');
    end{with};

    // clear contents
    for I := 1 to RowCount - 1
    do Rows[I].Clear;

    // fill with contents of FPersonList
    RowCount := FPersonList.Count + 2; // 2: fixed row at top, empty row at bottom
    for I := 0 to FPersonList.Count - 1 do
    begin
      VPersonData := FPersonList.Get(I);
      with Rows[I + 1] do
      begin
        Add(VPersonData.FamilyName);
        Add(VPersonData.Prefix);
        Add(VPersonData.Initials);
        Add(VPersonData.FirstName);
        Add(VPersonData.RoomNumber);
        Add(VPersonData.PhoneNumber);
      end{with Rows[I+1]};
    end{for};

  end{with Stringgrid1};
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPersonList := TPersonList_LinkedList.Create;
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

procedure TForm1.FindButtonClick(Sender: TObject);
var
  VName: String;
  VPersonData: TPersonData;
  VIndex: Integer;
begin
  VName := LabeledEdit1.Text;
  Memo1.Lines.Clear;
  VIndex := FPersonList.Indexof(VName);
  if VIndex = -1
  then Memo1.Lines.Add('Name not found')
  else
   begin
     VPersonData := FPersonList.Get(VIndex);
     with Memo1.Lines do
     begin
       Add('Family Name: ' + VPersonData.FamilyName);
       Add('Prefix: ' + VPersonData.Prefix);
       Add('Initials: ' + VPersonData.Initials);
       Add('First Name: ' + VPersondata.FirstName);
       Add('Room Number: ' + VPersonData.RoomNumber);
       Add('Phone Number: ' + VPersonData.PhoneNumber);
     end{with}
   end{else}
end;

procedure TForm1.AddButtonClick(Sender: TObject);
var
  VPersonData: TPersonData;
begin
  // create and fill VPersonData
  VPersonData := TPersonData.Create;
  with VPersonData do
  begin
    FamilyName := FamilyNameEdit.Text;
    Prefix := PrefixEdit.Text;
    Initials := InitialsEdit.Text;
    FirstName := FirstNameEdit.Text;
    RoomNumber := RoomNumberEdit.Text;
    PhoneNumber := PhoneNumberEdit.Text;
  end{with};

  // add VPersonData to FPersonList
  FPersonList.Add(VPersonData);
  FModified := true;
  UpdateStringGrid;
  UpdateButtonStates;
end;

procedure TForm1.FamilyNameEditChange(Sender: TObject);
begin
  UpdateButtonStates;
end;

procedure TForm1.DeleteButtonClick(Sender: TObject);
var
  VName: String;
begin
  VName := Stringgrid1.Cells[0, Stringgrid1.Row]; // family name in selected row
  FPersonList.Delete(VName);
  FModified := true;
  UpdateStringGrid;
  UpdateButtonStates;
end;

procedure TForm1.LabeledEdit1Change(Sender: TObject);
begin
  UpdateButtonStates;
end;

procedure TForm1.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  UpdateButtonStates;
end;

procedure TForm1.StringGrid1Click(Sender: TObject);
begin
  UpdateButtonStates;
end;

end.
