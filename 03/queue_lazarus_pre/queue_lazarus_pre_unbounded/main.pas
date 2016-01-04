unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Stack, Queue, ExtCtrls, StdCtrls, Buttons;

type

  { TForm1 }

  TForm1 = class(TForm)
    ClearStackButton: TButton;
    RandomStackButton: TButton;
    PopStackButton: TButton;
    PushStackButton: TButton;
    ClearQueueButton: TButton;
    RandomQueueButton: TButton;
    PutQueueButton: TButton;
    RemFirstQueueButton: TButton;
    MaskEdit1: TEdit;
    MaskEdit2: TEdit;
    QueueMemo: TMemo;
    StackMemo: TMemo;
    Splitter1: TSplitter;
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
    { private declarations }
  public
    { public declarations }
    FStack: TStackOfInt;
    FQueue: TQueueOfInt;
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

{ TForm1 }

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
  QueueMemo.Lines.Add('Intern: ' + QueueInternally(FQueue));
  QueueMemo.Lines.Add('Extern: ' + QueueExternally(FQueue));
  ClearQueueButton.Enabled := true;
  RandomQueueButton.Enabled := true;
  RemFirstQueueButton.Enabled := not FQueue.IsEmpty;
  PutQueueButton.Enabled := MaskEdit2.Text <> '  ';
end;

procedure TForm1.UpdateStackViews;
begin
  StackMemo.Lines.Add('Intern: ' + StackInternally(FStack));
  StackMemo.Lines.Add('Extern: ' + StackExternally(FStack));
  ClearStackButton.Enabled := true;
  RandomStackButton.Enabled := true;
  PopStackButton.Enabled := not FStack.IsEmpty;
  PushStackButton.Enabled := MaskEdit1.Text <> '  ';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  FStack := TStackOfInt.Create;
  RandomStack;
  UpdateStackViews;
  FQueue := TQueueOfInt.Create;
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


initialization
  {$I main.lrs}

end.

