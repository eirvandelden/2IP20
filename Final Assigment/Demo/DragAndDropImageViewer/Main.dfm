object Form1: TForm1
  Left = 193
  Top = 216
  Width = 1195
  Height = 656
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 120
  TextHeight = 16
  object Image1: TImage
    Left = 336
    Top = 56
    Width = 825
    Height = 537
    OnDragDrop = Image1DragDrop
    OnDragOver = Image1DragOver
  end
  object PathEdit: TEdit
    Left = 16
    Top = 16
    Width = 297
    Height = 24
    TabOrder = 0
    OnChange = PathEditChange
  end
  object BrowsePathButton: TButton
    Left = 328
    Top = 16
    Width = 81
    Height = 25
    Caption = 'Browse Path'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = BrowsePathButtonClick
  end
  object ListBox1: TListBox
    Left = 16
    Top = 64
    Width = 297
    Height = 529
    ItemHeight = 16
    TabOrder = 2
    OnDblClick = ListBox1DblClick
    OnMouseDown = ListBox1MouseDown
  end
end
