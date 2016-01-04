object Form1: TForm1
  Left = 63
  Top = 140
  Caption = 'Form1'
  ClientHeight = 647
  ClientWidth = 944
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -10
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 749
    Top = 0
    Width = 195
    Height = 647
    Align = alRight
    TabOrder = 0
    object Label1: TLabel
      Left = 98
      Top = 13
      Width = 81
      Height = 13
      Caption = '&Horizontal Space'
      FocusControl = HorTrackBar
    end
    object Label2: TLabel
      Left = 7
      Top = 13
      Width = 69
      Height = 13
      Caption = '&Vertical Space'
      FocusControl = VerTrackBar
    end
    object Label3: TLabel
      Left = 39
      Top = 163
      Width = 112
      Height = 13
      Caption = '&Element in range [0..99]'
      FocusControl = ValueEdit
    end
    object HorTrackBar: TTrackBar
      Left = 98
      Top = 33
      Width = 95
      Height = 36
      Max = 100
      Position = 50
      TabOrder = 0
      OnChange = HorTrackBarChange
    end
    object VerTrackBar: TTrackBar
      Left = 7
      Top = 26
      Width = 36
      Height = 92
      Max = 100
      Orientation = trVertical
      Position = 50
      TabOrder = 1
      OnChange = VerTrackBarChange
    end
    object ValueEdit: TEdit
      Left = 39
      Top = 182
      Width = 98
      Height = 21
      TabOrder = 2
      OnChange = ValueEditChange
    end
    object AddButton: TButton
      Left = 20
      Top = 228
      Width = 60
      Height = 20
      Caption = '&Add'
      TabOrder = 3
      OnClick = AddButtonClick
    end
    object DeleteButton: TButton
      Left = 111
      Top = 228
      Width = 60
      Height = 20
      Caption = '&Delete'
      TabOrder = 4
      OnClick = DeleteButtonClick
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 272
      Width = 129
      Height = 17
      Caption = 'Draw bounding boxes'
      TabOrder = 5
      OnClick = CheckBox1Click
    end
    object RadioGroup1: TRadioGroup
      Left = 8
      Top = 320
      Width = 185
      Height = 89
      Caption = 'Horizontal root position'
      ItemIndex = 0
      Items.Strings = (
        'Midposition of width'
        'Halfway roots of subtrees'
        'Halfway gap between subtrees')
      TabOrder = 6
      OnClick = RadioGroup1Click
    end
    object CheckBox2: TCheckBox
      Left = 16
      Top = 296
      Width = 129
      Height = 17
      Caption = 'Draw empty trees'
      TabOrder = 7
      OnClick = CheckBox2Click
    end
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 0
    Width = 749
    Height = 647
    HorzScrollBar.Smooth = True
    VertScrollBar.Smooth = True
    Align = alClient
    TabOrder = 1
    object Image1: TImage
      Left = 189
      Top = 111
      Width = 260
      Height = 267
      AutoSize = True
    end
  end
end
