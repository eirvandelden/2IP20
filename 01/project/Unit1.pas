unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdActns, ActnList, XPStyleActnCtrls, ActnMan, ComCtrls,
  StdCtrls, ImgList, ToolWin, ActnCtrls, ActnMenus, About;

type
  TForm1 = class(TForm)
    RichEdit1: TRichEdit;
    StatusBar1: TStatusBar;
    ActionManager1: TActionManager;
    FileNew: TAction;
    FileSave: TAction;
    HelpIndex: TAction;
    HelpAbout: TAction;
    EditCut1: TEditCut;
    EditCopy1: TEditCopy;
    EditPaste1: TEditPaste;
    FileOpen1: TFileOpen;
    FileSaveAs1: TFileSaveAs;
    FileExit1: TFileExit;
    HelpContents1: THelpContents;
    ImageList1: TImageList;
    ActionMainMenuBar1: TActionMainMenuBar;
    ActionToolBar1: TActionToolBar;
    procedure FileNewExecute(Sender: TObject);
    procedure FileOpen1Accept(Sender: TObject);
    procedure FileSaveExecute(Sender: TObject);
    procedure FileSaveAs1BeforeExecute(Sender: TObject);
    procedure FileSaveAs1Accept(Sender: TObject);
    procedure HelpContents1Execute(Sender: TObject);
    procedure HelpIndexExecute(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    FileName: String;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FileNewExecute(Sender: TObject);
begin
  RichEdit1.Clear;
  FileName := 'untitled.txt';
  StatusBar1.Panels[0].Text := FileName;
end;

procedure TForm1.FileOpen1Accept(Sender: TObject);
begin
  RichEdit1.Lines.LoadFromFile(FileOpen1.Dialog.FileName);
  FileName := FileOpen1.Dialog.FileName;
  StatusBar1.Panels[0].Text := FileName;
end;

procedure TForm1.FileSaveExecute(Sender: TObject);
begin
  if (FileName = 'untitled.txt') then
    FileSaveAs1.Execute
  else
    RichEdit1.Lines.SaveToFile(FileName);
end;

procedure TForm1.FileSaveAs1BeforeExecute(Sender: TObject);
begin
  FileSaveAs1.Dialog.InitialDir := ExtractFilePath(FileName);
end;

procedure TForm1.FileSaveAs1Accept(Sender: TObject);
begin
  RichEdit1.Lines.SaveToFile(FileSaveAs1.Dialog.FileName);
  FileName := FileSaveAs1.Dialog.FileName;
  StatusBar1.Panels[0].Text := FileName;
end;

procedure TForm1.HelpContents1Execute(Sender: TObject);

const
  HELP_TAB = 15;
  CONTENTS_ACTIVE = -3;

begin
  Application.HelpCommand( HELP_TAB, CONTENTS_ACTIVE);
end;

procedure TForm1.HelpIndexExecute(Sender: TObject);
Const
  HELP_TAB = 15;
  INDEX_ACTIVE = -2;

begin
  Application.HelpCommand(HELP_TAB, INDEX_ACTIVE);
end;

procedure TForm1.HelpAboutExecute(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Application.HelpFile := ExtractFilePath(Application.ExeName ) + 'TextEditor.hlp';
  FileNew.Execute
end;

end.
