object HttpThreadForm: THttpThreadForm
  Left = 168
  Top = 120
  Caption = 'ICS HTTP Threaded Test - 6 Aug 2024'
  ClientHeight = 795
  ClientWidth = 850
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object Label14: TLabel
    Left = 24
    Top = 16
    Width = 20
    Height = 13
    Caption = 'URL'
  end
  object Label15: TLabel
    Left = 16
    Top = 40
    Width = 27
    Height = 13
    Caption = 'Proxy'
  end
  object URLEdit: TEdit
    Left = 64
    Top = 7
    Width = 379
    Height = 21
    TabOrder = 0
    Text = 'https://wiki.overbyte.eu/wiki/index.php/FAQ_Using_TSslHttpRest'
  end
  object ResultsMemo: TMemo
    Left = 8
    Top = 62
    Width = 563
    Height = 724
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'ResultsMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object DoItButton: TButton
    Left = 480
    Top = 15
    Width = 116
    Height = 25
    Caption = 'Start Threads'
    TabOrder = 2
    OnClick = DoItButtonClick
  end
  object ProxyEdit: TEdit
    Left = 64
    Top = 32
    Width = 378
    Height = 21
    TabOrder = 3
    Text = 'ProxyEdit'
  end
  object GroupBox1: TGroupBox
    Left = 588
    Top = 63
    Width = 198
    Height = 143
    Caption = 'Thread States'
    TabOrder = 4
    object Label1: TLabel
      Left = 15
      Top = 15
      Width = 45
      Height = 13
      Caption = 'Thread 0'
    end
    object Label2: TLabel
      Tag = 1
      Left = 15
      Top = 35
      Width = 45
      Height = 13
      Caption = 'Thread 1'
    end
    object Label3: TLabel
      Tag = 2
      Left = 15
      Top = 55
      Width = 45
      Height = 13
      Caption = 'Thread 2'
    end
    object Label4: TLabel
      Tag = 3
      Left = 15
      Top = 75
      Width = 45
      Height = 13
      Caption = 'Thread 3'
    end
    object Label5: TLabel
      Tag = 4
      Left = 15
      Top = 95
      Width = 45
      Height = 13
      Caption = 'Thread 4'
    end
    object Label6: TLabel
      Tag = 5
      Left = 15
      Top = 115
      Width = 45
      Height = 13
      Caption = 'Thread 5'
    end
    object Thread0Label: TLabel
      Left = 80
      Top = 15
      Width = 45
      Height = 13
      Caption = 'Thread 0'
    end
    object Thread1Label: TLabel
      Left = 80
      Top = 35
      Width = 45
      Height = 13
      Caption = 'Thread 1'
    end
    object Thread2Label: TLabel
      Left = 80
      Top = 55
      Width = 45
      Height = 13
      Caption = 'Thread 2'
    end
    object Thread3Label: TLabel
      Left = 80
      Top = 75
      Width = 45
      Height = 13
      Caption = 'Thread 3'
    end
    object Thread4Label: TLabel
      Left = 80
      Top = 95
      Width = 45
      Height = 13
      Caption = 'Thread 4'
    end
    object Thread5Label: TLabel
      Left = 80
      Top = 115
      Width = 45
      Height = 13
      Caption = 'Thread 5'
    end
  end
  object EditTotal: TEdit
    Left = 606
    Top = 18
    Width = 51
    Height = 21
    TabOrder = 5
    Text = '1'
  end
end
