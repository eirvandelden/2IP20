object Form1: TForm1
  Left = 208
  Top = 166
  Width = 952
  Height = 656
  Caption = 'Bounded Stacks and Queues'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 120
  TextHeight = 16
  object Splitter1: TSplitter
    Left = 465
    Top = 0
    Height = 616
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 465
    Height = 616
    Align = alLeft
    TabOrder = 0
    object StackMemo: TMemo
      Left = 1
      Top = 1
      Width = 463
      Height = 517
      Align = alClient
      Lines.Strings = (
        'Stack Contents:'
        '')
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object Panel3: TPanel
      Left = 1
      Top = 518
      Width = 463
      Height = 97
      Align = alBottom
      TabOrder = 1
      DesignSize = (
        463
        97)
      object ClearStackButton: TButton
        Left = 24
        Top = 16
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Clear'
        TabOrder = 0
        OnClick = ClearStackButtonClick
      end
      object PopStackButton: TButton
        Left = 24
        Top = 56
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Pop'
        TabOrder = 1
        OnClick = PopStackButtonClick
      end
      object RandomStackButton: TButton
        Left = 118
        Top = 16
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Random'
        TabOrder = 2
        OnClick = RandomStackButtonClick
      end
      object PushStackButton: TButton
        Left = 118
        Top = 56
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Push'
        TabOrder = 3
        OnClick = PushStackButtonClick
      end
      object MaskEdit1: TMaskEdit
        Left = 208
        Top = 57
        Width = 120
        Height = 24
        Anchors = [akLeft, akBottom]
        EditMask = '99;1;_'
        MaxLength = 2
        TabOrder = 4
        Text = '  '
        OnChange = MaskEdit1Change
      end
    end
  end
  object Panel2: TPanel
    Left = 468
    Top = 0
    Width = 476
    Height = 616
    Align = alClient
    TabOrder = 1
    object QueueMemo: TMemo
      Left = 1
      Top = 1
      Width = 474
      Height = 519
      Align = alClient
      Lines.Strings = (
        'Queue Contents:'
        '')
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object Panel4: TPanel
      Left = 1
      Top = 520
      Width = 474
      Height = 95
      Align = alBottom
      TabOrder = 1
      DesignSize = (
        474
        95)
      object ClearQueueButton: TButton
        Left = 16
        Top = 14
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Clear'
        TabOrder = 0
        OnClick = ClearQueueButtonClick
      end
      object RemFirstButton: TButton
        Left = 16
        Top = 54
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'RemFirst'
        TabOrder = 1
        OnClick = RemFirstButtonClick
      end
      object RandomQueueButton: TButton
        Left = 120
        Top = 14
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Random'
        TabOrder = 2
        OnClick = RandomQueueButtonClick
      end
      object PutQueueButton: TButton
        Left = 120
        Top = 54
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Put'
        TabOrder = 3
        OnClick = PutQueueButtonClick
      end
      object MaskEdit2: TMaskEdit
        Left = 216
        Top = 55
        Width = 120
        Height = 24
        Anchors = [akLeft, akBottom]
        EditMask = '99;1;_'
        MaxLength = 2
        TabOrder = 4
        Text = '  '
        OnChange = MaskEdit2Change
      end
    end
  end
end
