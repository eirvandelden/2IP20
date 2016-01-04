object Form1: TForm1
  Left = 207
  Top = 110
  Width = 952
  Height = 656
  Caption = 'Unbounded Stacks and Queues'
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
  object Splitter1: TSplitter
    Left = 378
    Top = 0
    Width = 2
    Height = 629
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 378
    Height = 629
    Align = alLeft
    TabOrder = 0
    object StackMemo: TMemo
      Left = 1
      Top = 1
      Width = 376
      Height = 548
      Align = alClient
      Lines.Strings = (
        'Stack Contents:'
        '')
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object Panel3: TPanel
      Left = 1
      Top = 549
      Width = 376
      Height = 79
      Align = alBottom
      TabOrder = 1
      DesignSize = (
        376
        79)
      object ClearStackButton: TButton
        Left = 20
        Top = 13
        Width = 60
        Height = 20
        Anchors = [akLeft, akBottom]
        Caption = 'Clear'
        TabOrder = 0
        OnClick = ClearStackButtonClick
      end
      object PopStackButton: TButton
        Left = 20
        Top = 46
        Width = 60
        Height = 20
        Anchors = [akLeft, akBottom]
        Caption = 'Pop'
        TabOrder = 1
        OnClick = PopStackButtonClick
      end
      object RandomStackButton: TButton
        Left = 96
        Top = 13
        Width = 61
        Height = 20
        Anchors = [akLeft, akBottom]
        Caption = 'Random'
        TabOrder = 2
        OnClick = RandomStackButtonClick
      end
      object PushStackButton: TButton
        Left = 96
        Top = 46
        Width = 61
        Height = 20
        Anchors = [akLeft, akBottom]
        Caption = 'Push'
        TabOrder = 3
        OnClick = PushStackButtonClick
      end
      object MaskEdit1: TMaskEdit
        Left = 169
        Top = 46
        Width = 98
        Height = 21
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
    Left = 380
    Top = 0
    Width = 564
    Height = 629
    Align = alClient
    TabOrder = 1
    object QueueMemo: TMemo
      Left = 1
      Top = 1
      Width = 562
      Height = 550
      Align = alClient
      Lines.Strings = (
        'Queue Contents:'
        '')
      ScrollBars = ssBoth
      TabOrder = 0
    end
    object Panel4: TPanel
      Left = 1
      Top = 551
      Width = 562
      Height = 77
      Align = alBottom
      TabOrder = 1
      DesignSize = (
        562
        77)
      object ClearQueueButton: TButton
        Left = 13
        Top = 11
        Width = 61
        Height = 21
        Anchors = [akLeft, akBottom]
        Caption = 'Clear'
        TabOrder = 0
        OnClick = ClearQueueButtonClick
      end
      object RemFirstButton: TButton
        Left = 13
        Top = 44
        Width = 61
        Height = 20
        Anchors = [akLeft, akBottom]
        Caption = 'RemFirst'
        TabOrder = 1
        OnClick = RemFirstButtonClick
      end
      object RandomQueueButton: TButton
        Left = 98
        Top = 11
        Width = 60
        Height = 21
        Anchors = [akLeft, akBottom]
        Caption = 'Random'
        TabOrder = 2
        OnClick = RandomQueueButtonClick
      end
      object PutQueueButton: TButton
        Left = 98
        Top = 44
        Width = 60
        Height = 20
        Anchors = [akLeft, akBottom]
        Caption = 'Put'
        TabOrder = 3
        OnClick = PutQueueButtonClick
      end
      object MaskEdit2: TMaskEdit
        Left = 176
        Top = 45
        Width = 97
        Height = 21
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
