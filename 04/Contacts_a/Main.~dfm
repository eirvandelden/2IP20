object Form1: TForm1
  Left = 252
  Top = 203
  Width = 800
  Height = 668
  Caption = 'Contacts'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 792
    Height = 622
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'Find'
      object FindButton: TButton
        Left = 312
        Top = 65
        Width = 61
        Height = 20
        Caption = 'Find'
        Enabled = False
        TabOrder = 0
      end
      object Memo1: TMemo
        Left = 0
        Top = 302
        Width = 784
        Height = 292
        Align = alBottom
        Lines.Strings = (
          
            'Choose File|New to create a new contacts list or File|Open to op' +
            'en an existing one.'
          ''
          'Enter a family name in the edit box above.'
          
            'If the name occurs in the current contacts list, the Find button' +
            ' will be enabled.'
          'Clicking it will show the associated data in this memo.'
          ''
          
            'The Edit tab shows a StringGrid with all entries and some Edit b' +
            'oxes for'
          'adding and deleting items.'
          ''
          'The File Contents tab show the file in so-called .ini format.')
        TabOrder = 1
      end
      object LabeledEdit1: TLabeledEdit
        Left = 293
        Top = 33
        Width = 98
        Height = 21
        EditLabel.Width = 60
        EditLabel.Height = 13
        EditLabel.Caption = 'Family Name'
        TabOrder = 2
        OnChange = LabeledEdit1Change
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Edit'
      ImageIndex = 1
      object Panel1: TPanel
        Left = 0
        Top = 490
        Width = 784
        Height = 104
        Align = alBottom
        TabOrder = 0
        object FamilyNameEdit: TLabeledEdit
          Left = 13
          Top = 26
          Width = 98
          Height = 21
          EditLabel.Width = 60
          EditLabel.Height = 13
          EditLabel.Caption = 'Family Name'
          TabOrder = 0
          OnChange = FamilyNameEditChange
        end
        object PrefixEdit: TLabeledEdit
          Left = 137
          Top = 26
          Width = 98
          Height = 21
          EditLabel.Width = 26
          EditLabel.Height = 13
          EditLabel.Caption = 'Prefix'
          TabOrder = 1
        end
        object InitialsEdit: TLabeledEdit
          Left = 260
          Top = 26
          Width = 98
          Height = 21
          EditLabel.Width = 29
          EditLabel.Height = 13
          EditLabel.Caption = 'Initials'
          TabOrder = 2
        end
        object FirstNameEdit: TLabeledEdit
          Left = 384
          Top = 26
          Width = 98
          Height = 21
          EditLabel.Width = 50
          EditLabel.Height = 13
          EditLabel.Caption = 'First Name'
          TabOrder = 3
        end
        object RoomNumberEdit: TLabeledEdit
          Left = 501
          Top = 26
          Width = 98
          Height = 21
          EditLabel.Width = 68
          EditLabel.Height = 13
          EditLabel.Caption = 'Room Number'
          TabOrder = 4
        end
        object PhoneNumberEdit: TLabeledEdit
          Left = 624
          Top = 26
          Width = 98
          Height = 21
          EditLabel.Width = 71
          EditLabel.Height = 13
          EditLabel.Caption = 'Phone Number'
          TabOrder = 5
        end
        object AddButton: TButton
          Left = 215
          Top = 72
          Width = 60
          Height = 20
          Caption = 'Add'
          TabOrder = 6
        end
        object DeleteButton: TButton
          Left = 410
          Top = 72
          Width = 60
          Height = 20
          Caption = 'Delete'
          TabOrder = 7
          OnClick = DeleteButtonClick
        end
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 784
        Height = 33
        Align = alTop
        TabOrder = 1
      end
      object StringGrid1: TStringGrid
        Left = 0
        Top = 0
        Width = 961
        Height = 489
        ColCount = 6
        DefaultColWidth = 160
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goRowSelect]
        ScrollBars = ssVertical
        TabOrder = 2
        ColWidths = (
          160
          160
          160
          160
          160
          153)
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'File Contents'
      ImageIndex = 2
      object Memo2: TMemo
        Left = 0
        Top = 0
        Width = 913
        Height = 627
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 0
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 808
    Top = 32
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Caption = '&New'
        OnClick = New1Click
      end
      object Open1: TMenuItem
        Caption = '&Open...'
        OnClick = Open1Click
      end
      object Save1: TMenuItem
        Caption = '&Save'
        OnClick = Save1Click
      end
      object SaveAs1: TMenuItem
        Caption = 'Save &As...'
        OnClick = SaveAs1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Print1: TMenuItem
        Caption = '&Print...'
      end
      object PrintSetup1: TMenuItem
        Caption = 'P&rint Setup...'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'E&xit'
      end
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'ctx'
    Filter = 'Contacts|*.ctx'
    Left = 856
    Top = 32
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'ctx'
    Filter = 'Contacts|*.ctx'
    Left = 904
    Top = 32
  end
end
