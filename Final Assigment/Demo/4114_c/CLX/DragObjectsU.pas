unit DragObjectsU;

interface

uses
  SysUtils, Types, Classes, QGraphics, QControls, QForms, QDialogs,
  QExtCtrls, QStdCtrls;

type
  TTextDragObject = class(TDragObject)
  public
    Data: String;
  end;

  TForm1 = class(TForm)
    Button1: TButton;
    ComboBox1: TComboBox;
    Memo1: TMemo;
    Label1: TLabel;
    Edit1: TEdit;
    ListBox1: TListBox;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure Button1StartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure Label1StartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure ComboBox1StartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure Edit1StartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure Memo1StartDrag(Sender: TObject; var DragObject: TDragObject);
    procedure ListBox1StartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure SharedEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    FDragObject: TTextDragObject;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.xfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  //Make sure an item is definitely selected,
  //otherwise first drag will fail (listbox
  //selects the item when mouse is released)
  ListBox1.ItemIndex := 0
end;

procedure TForm1.Button1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FDragObject := TTextDragObject.Create;
  FDragObject.Data := TButton(Sender).Caption;
  DragObject := FDragObject;
end;

procedure TForm1.Label1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FDragObject := TTextDragObject.Create;
  FDragObject.Data := TLabel(Sender).Caption;
  DragObject := FDragObject;
end;

procedure TForm1.ComboBox1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FDragObject := TTextDragObject.Create;
  FDragObject.Data := TComboBox(Sender).Text;
  DragObject := FDragObject;
end;

procedure TForm1.Edit1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FDragObject := TTextDragObject.Create;
  FDragObject.Data := TEdit(Sender).Text;
  DragObject := FDragObject;
end;

procedure TForm1.Memo1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FDragObject := TTextDragObject.Create;
  FDragObject.Data := TMemo(Sender).Text;
  DragObject := FDragObject;
end;

procedure TForm1.ListBox1StartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  FDragObject := TTextDragObject.Create;
  with TListBox(Sender) do
    FDragObject.Data := Items[ItemIndex];
  DragObject := FDragObject;
end;

procedure TForm1.SharedEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  //All draggable controls share this event handler
  FDragObject.Free;
  FDragObject := nil
end;

procedure TForm1.Panel1DragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := Source is TTextDragObject;
end;

procedure TForm1.Panel1DragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  Panel1.Caption := TTextDragObject(FDragObject).Data
end;

end.
