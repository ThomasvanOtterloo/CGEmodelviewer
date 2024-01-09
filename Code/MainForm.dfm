object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'FPS: '
  ClientHeight = 625
  ClientWidth = 1119
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    1119
    625)
  TextHeight = 15
  object CastleControl: TCastleControl
    Left = 0
    Top = 0
    Width = 1119
    Height = 625
    Container.Requirements.MultiSampling = 4
    Container.DesignUrl = 'castle-data:/test_3d.castle-user-interface'
    Align = alClient
    ExplicitWidth = 1113
    ExplicitHeight = 616
  end
  object SetErrorButton: TButton
    Left = 990
    Top = 549
    Width = 121
    Height = 68
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Set Error on/off'
    TabOrder = 1
    OnClick = SetErrorButtonClick
    ExplicitLeft = 984
    ExplicitTop = 540
  end
  object ListBox1: TListBox
    Left = 952
    Top = 351
    Width = 159
    Height = 113
    Anchors = [akRight, akBottom]
    ItemHeight = 15
    TabOrder = 2
    OnClick = ListBoxPlayAnimation
    ExplicitLeft = 946
    ExplicitTop = 342
  end
  object Button4: TButton
    Left = 990
    Top = 470
    Width = 121
    Height = 73
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'start/stop animation'
    TabOrder = 3
    OnClick = StartStopAnimation
    ExplicitLeft = 984
    ExplicitTop = 461
  end
  object FailureDetectionTTimer: TTimer
    Interval = 500
    OnTimer = FailureDetectionTimer
    Left = 152
    Top = 88
  end
  object FPS: TTimer
    OnTimer = FPSTimer
    Left = 768
    Top = 224
  end
end
