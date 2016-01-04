unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls,
  {DisplayTree, } TestTree, ComCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    ScrollBox1: TScrollBox;
    Image1: TImage;
    HorTrackBar: TTrackBar;
    VerTrackBar: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    ValueEdit: TEdit;
    AddButton: TButton;
    DeleteButton: TButton;
    Label3: TLabel;
    CheckBox1: TCheckBox;
    RadioGroup1: TRadioGroup;
    CheckBox2: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure HorTrackBarChange(Sender: TObject);
    procedure VerTrackBarChange(Sender: TObject);
    procedure ValueEditChange(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
    FTree: TTree;
    function  CanConvertToInt(AString: String): Boolean;
    procedure UpdateButtons;
    procedure UpdateViews;
  end;

var
  Form1: TForm1;

implementation

uses
  Math;

{$R *.dfm}

function TForm1.CanConvertToInt(AString: String): Boolean;
var
  Dummy: Integer;
begin
  Result := TryStrToInt(ValueEdit.Text, Dummy) and (0 <= Dummy) and (Dummy <100);
end;

procedure TForm1.UpdateButtons;
begin
  AddButton.Enabled :=
    CanConvertToInt(ValueEdit.Text) and not FTree.Occurs(StrToInt(ValueEdit.Text));
  DeleteButton.Enabled :=
    CanConvertToInt(ValueEdit.Text) and FTree.Occurs(StrToInt(ValueEdit.Text));
end;

procedure TForm1.UpdateViews;
begin
  FTree.CalculateLayout(CheckBox2.Checked, RadioGroup1.ItemIndex);
  with Image1 do
  begin
    Left := 0;
    Top := 0;
    Picture.Graphic.Width := Max(100, FTree.Width);
    Picture.Graphic.Height := Max(100, FTree.Height + 30);
  end;
  FTree.Draw(Image1.Canvas, CheckBox1.Checked);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  VBitmap: TBitMap;
begin
  // create tree and add some numbers
  FTree := TTree.Create;
  FTree.Add(50);
  FTree.Add(30);
  FTree.Add(70);
  FTree.Add(60);
  FTree.Add(40);
  FTree.Add(20);
  FTree.Add(10);
  FTree.Add(25);

  // set layout parameters
  FTree.HSpace := HorTrackBar.Position;
  FTree.VSpace := 50;
  FTree.NodeSize := 10;

  VBitmap := TBitmap.Create;
  VBitmap.Width := 200;
  VBitmap.Height := 200;
  Image1.Picture.Graphic := VBitmap;

  UpdateButtons;
  UpdateViews;
end;

procedure TForm1.HorTrackBarChange(Sender: TObject);
begin
  FTree.HSpace := HorTrackBar.Position;
  UpdateViews;
end;

procedure TForm1.VerTrackBarChange(Sender: TObject);
begin
  FTree.VSpace := VerTrackBar.Position;
  UpdateViews;
end;

procedure TForm1.ValueEditChange(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TForm1.AddButtonClick(Sender: TObject);
begin
  FTree.Add(StrToInt(ValueEdit.Text));
  UpdateButtons;
  UpdateViews;
end;

procedure TForm1.DeleteButtonClick(Sender: TObject);
begin
  FTree.Remove(StrToInt(ValueEdit.Text));
  UpdateButtons;
  UpdateViews;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  UpdateViews;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  UpdateViews;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin

  if Checkbox2.Enabled then begin
    // ChangeDrawEmptyTrees(FTree, True);
  end
  else begin
    // ChangeDrawEmptyTrees(FTree, False);

  end;


  UpdateViews;
end;

end.
