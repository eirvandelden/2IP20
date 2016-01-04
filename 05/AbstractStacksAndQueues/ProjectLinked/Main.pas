unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Mask,
  AbsStack, {Stack_Array,} Stack_Linked,
  AbsQueue, {Queue_Array,} Queue_Linked;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Splitter1: TSplitter;
    Panel2: TPanel;
    StackMemo: TMemo;
    QueueMemo: TMemo;
    Panel3: TPanel;
    ClearStackButton: TButton;
    PopStackButton: TButton;
    RandomStackButton: TButton;
    PushStackButton: TButton;
    MaskEdit1: TMaskEdit;
    Panel4: TPanel;
    ClearQueueButton: TButton;
    RemFirstButton: TButton;
    RandomQueueButton: TButton;
    PutQueueButton: TButton;
    MaskEdit2: TMaskEdit;
    procedure FormCreate(Sender: TObject);
    procedure ClearStackButtonClick(Sender: TObject);
    procedure RandomStackButtonClick(Sender: TObject);
    procedure PopStackButtonClick(Sender: TObject);
    procedure MaskEdit1Change(Sender: TObject);
    procedure PushStackButtonClick(Sender: TObject);
    procedure ClearQueueButtonClick(Sender: TObject);
    procedure RandomQueueButtonClick(Sender: TObject);
    procedure RemFirstButtonClick(Sender: TObject);
    procedure PutQueueButtonClick(Sender: TObject);
    procedure MaskEdit2Change(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FStack: TIntStack;
    FQueue: TIntQueue;
    procedure ClearStack;
    procedure RandomStack;
    procedure ClearQueue;
    procedure RandomQueue;
    procedure UpdateStackViews;
    procedure UpdateQueueViews;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.ClearQueue;
begin
  with FQueue do
  begin
    while not IsEmpty
    do RemFirst;
  end;
end;

procedure TForm1.ClearStack;
begin
  with FStack do
  begin
    while not IsEmpty
    do Pop;
  end;
end;

procedure TForm1.RandomQueue;
var
  VCount, VValue: Integer;
  I: Integer;
begin
  ClearQueue;
  VCount := Random(20);
  for I := 0 to VCount - 1 do
  begin
    VValue := Random(100);
    FQueue.Put(VValue);
  end;
end;

procedure TForm1.RandomStack;
var
  VCount, VValue: Integer;
  I: Integer;
begin
  ClearStack;
  VCount := Random(20);
  for I := 0 to VCount - 1 do
  begin
    VValue := Random(100);
    FStack.Push(VValue);
  end;
end;

procedure TForm1.UpdateQueueViews;
begin
//  QueueMemo.Lines.Add('Intern: ' + IntQueueArrayInternally(FQueue as TIntQueue_Array));
//  QueueMemo.Lines.Add('Extern: ' + IntQueueArrayExternally(FQueue as TIntQueue_Array));
  QueueMemo.Lines.Add('Extern: ' + IntQueueLinkedExternally(FQueue as TIntQueue_Linked));
  ClearQueueButton.Enabled := true;
  RandomQueueButton.Enabled := true;
  RemFirstButton.Enabled := not FQueue.IsEmpty;
  PutQueueButton.Enabled := MaskEdit2.Text <> '  ';
end;

procedure TForm1.UpdateStackViews;
begin
//  StackMemo.Lines.Add('Intern: ' + IntStackArrayInternally(FStack as TIntStack_Array));
//  StackMemo.Lines.Add('Extern: ' + IntStackArrayExternally(FStack as TIntStack_Array));
  StackMemo.Lines.Add('Extern: ' + IntStackLinkedExternally(FStack as TIntStack_Linked));
  ClearStackButton.Enabled := true;
  RandomStackButton.Enabled := true;
  PopStackButton.Enabled := not FStack.IsEmpty;
  PushStackButton.Enabled := MaskEdit1.Text <> '  ';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
//  FStack := TIntStack_Array.Create;
  FStack := TIntStack_Linked.Create;
  RandomStack;
  UpdateStackViews;
//  FQueue := TIntQueue_Array.Create;
  FQueue := TIntQueue_Linked.Create;
  RandomQueue;
  UpdateQueueViews;
end;

procedure TForm1.ClearStackButtonClick(Sender: TObject);
begin
  StackMemo.Lines.Add('Clear');
  ClearStack;
  UpdateStackViews;
end;

procedure TForm1.RandomStackButtonClick(Sender: TObject);
begin
  StackMemo.Lines.Add('Random');
  RandomStack;
  UpdateStackViews;
end;

procedure TForm1.PopStackButtonClick(Sender: TObject);
begin
  if not FStack.IsEmpty then
  begin
    StackMemo.Lines.Add('Pop');
    FStack.Pop;
    UpdateStackViews;
  end;
end;

procedure TForm1.PushStackButtonClick(Sender: TObject);
begin
  StackMemo.Lines.Add('Push ' + MaskEdit1.Text);
  FStack.Push(StrToInt(MaskEdit1.Text));
  UpdateStackViews;
end;

procedure TForm1.MaskEdit1Change(Sender: TObject);
begin
  PushStackButton.Enabled := MaskEdit1.Text <> '  ';
end;

procedure TForm1.ClearQueueButtonClick(Sender: TObject);
begin
  QueueMemo.Lines.Add('Clear');
  ClearQueue;
  UpdateQueueViews;
end;

procedure TForm1.RandomQueueButtonClick(Sender: TObject);
begin
  QueueMemo.Lines.Add('Random');
  RandomQueue;
  UpdateQueueViews;
end;

procedure TForm1.RemFirstButtonClick(Sender: TObject);
begin
  if not FQueue.IsEmpty then
  begin
    QueueMemo.Lines.Add('RemFirst');
    FQueue.RemFirst;
    UpdateQueueViews;
  end;
end;

procedure TForm1.PutQueueButtonClick(Sender: TObject);
begin
  QueueMemo.Lines.Add('Put ' + MaskEdit2.Text);
  FQueue.Put(StrToInt(MaskEdit2.Text));
  UpdateQueueViews;
end;

procedure TForm1.MaskEdit2Change(Sender: TObject);
begin
  PutQueueButton.Enabled := MaskEdit2.Text <> '  ';
end;

end.
