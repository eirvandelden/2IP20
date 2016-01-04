object Form1: TForm1
  Left = 229
  Top = 131
  Caption = 'Form1'
  ClientHeight = 610
  ClientWidth = 1063
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 20
    Top = 20
    Width = 32
    Height = 13
    Caption = '&Pieces'
  end
  object Label2: TLabel
    Left = 254
    Top = 228
    Width = 18
    Height = 13
    Caption = '&Box'
    FocusControl = BoxGrid
  end
  object BoxGrid: TDrawGrid
    Left = 254
    Top = 247
    Width = 260
    Height = 150
    DefaultColWidth = 16
    DefaultRowHeight = 16
    DefaultDrawing = False
    FixedCols = 0
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    TabOrder = 0
    OnDragDrop = BoxGridDragDrop
    OnDragOver = BoxGridDragOver
    OnDrawCell = BoxGridDrawCell
    OnEndDrag = BoxGridEndDrag
    OnMouseDown = BoxGridMouseDown
    OnStartDrag = BoxGridStartDrag
  end
  object PiecesGrid: TStringGrid
    Left = 20
    Top = 59
    Width = 813
    Height = 137
    ColCount = 50
    DefaultColWidth = 16
    DefaultRowHeight = 16
    FixedCols = 0
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    TabOrder = 1
    OnDragDrop = PiecesGridDragDrop
    OnDragOver = PiecesGridDragOver
    OnDrawCell = PiecesGridDrawCell
    OnEndDrag = PiecesGridEndDrag
    OnMouseDown = PiecesGridMouseDown
    OnStartDrag = PiecesGridStartDrag
  end
  object MainMenu1: TMainMenu
    Left = 904
    Top = 8
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New'
        Enabled = False
      end
      object Open1: TMenuItem
        Caption = '&Open...'
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = '&Save'
        Enabled = False
      end
      object SaveAs1: TMenuItem
        Caption = 'Save &As...'
        Enabled = False
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Caption = '&Print...'
        Enabled = False
      end
      object PrintSetup1: TMenuItem
        Caption = 'P&rint Setup...'
        Enabled = False
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
        OnClick = FormClose
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 816
    Top = 8
  end
  object SaveDialog1: TSaveDialog
    Left = 864
    Top = 8
  end
end
