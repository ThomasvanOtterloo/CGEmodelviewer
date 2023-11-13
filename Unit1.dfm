object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'FPS: '
  ClientHeight = 538
  ClientWidth = 967
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  TextHeight = 15
  object CastleControl: TCastleControl
    Left = 0
    Top = 0
    Width = 967
    Height = 538
    Container.DesignUrl = 'castle-data:/test_3d.castle-user-interface'
    Align = alClient
    ExplicitLeft = -8
    ExplicitTop = 7
  end
  object Button1: TButton
    Left = 843
    Top = 273
    Width = 116
    Height = 73
    Caption = 'Randomise color!'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 843
    Top = 368
    Width = 116
    Height = 65
    Caption = 'NewCamera'
    TabOrder = 2
    OnClick = NewCamera
  end
  object Button3: TButton
    Left = 843
    Top = 462
    Width = 116
    Height = 68
    Align = alCustom
    Caption = 'Set Error on/off'
    TabOrder = 3
    OnClick = Button3Click
  end
  object ListBox1: TListBox
    Left = 784
    Top = 32
    Width = 121
    Height = 97
    ItemHeight = 15
    TabOrder = 4
    OnClick = ListBoxPlayAnimation
  end
  object Button4: TButton
    Left = 838
    Top = 184
    Width = 121
    Height = 73
    Caption = 'start/stop animation'
    TabOrder = 5
    OnClick = StartStopAnimation
  end
  object StaticText1: TStaticText
    Left = 784
    Top = 8
    Width = 65
    Height = 19
    Caption = 'Animations'
    TabOrder = 6
  end
  object StaticText2: TStaticText
    Left = 640
    Top = 7
    Width = 44
    Height = 19
    Caption = 'Objects'
    TabOrder = 7
  end
  object StaticText3: TStaticText
    Left = 16
    Top = 32
    Width = 61
    Height = 19
    Caption = 'DebugLine'
    TabOrder = 8
  end
  object ListBox2: TListBox
    Left = 640
    Top = 32
    Width = 121
    Height = 97
    ItemHeight = 15
    TabOrder = 9
    OnClick = ListBox2Click
  end
  object FailureDection: TTimer
    Interval = 500
    OnTimer = FailureDetectionTimer
    Left = 88
    Top = 272
  end
end
