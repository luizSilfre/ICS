object ClientForm: TClientForm
  Left = 139
  Top = 650
  Caption = 'ICS Socket Client - https://www.overbyte.be - 7 Aug 2024'
  ClientHeight = 302
  ClientWidth = 601
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDefault
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 73
    Width = 601
    Height = 229
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 601
    Height = 73
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 21
      Height = 13
      Caption = 'Port'
    end
    object Label2: TLabel
      Left = 104
      Top = 12
      Width = 31
      Height = 13
      Caption = 'Server'
    end
    object Label3: TLabel
      Left = 6
      Top = 43
      Width = 24
      Height = 13
      Caption = 'Data'
    end
    object SendEdit: TEdit
      Left = 32
      Top = 40
      Width = 185
      Height = 21
      TabOrder = 0
      Text = 'SendEdit'
    end
    object SendButton: TButton
      Left = 230
      Top = 40
      Width = 75
      Height = 21
      Caption = '&Send'
      Default = True
      TabOrder = 1
      OnClick = SendButtonClick
    end
    object DisconnectButton: TButton
      Left = 214
      Top = 8
      Width = 75
      Height = 21
      Caption = 'Disconnect'
      Enabled = False
      TabOrder = 2
      OnClick = DisconnectButtonClick
    end
    object PortEdit: TEdit
      Left = 32
      Top = 8
      Width = 65
      Height = 21
      TabOrder = 3
      Text = 'telnet'
    end
    object ServerEdit: TEdit
      Left = 144
      Top = 8
      Width = 65
      Height = 21
      TabOrder = 4
      Text = 'localhost'
    end
    object AllowBinaryCheckBox: TCheckBox
      Left = 320
      Top = 44
      Width = 85
      Height = 17
      Hint = 
        'Allow embedded binary data encoded as $XY (XY being a two digit ' +
        'hex value)'
      Caption = 'Allow &Binary'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
    end
    object AddCRLFCheckBox: TCheckBox
      Left = 320
      Top = 24
      Width = 97
      Height = 17
      Caption = 'Add CR/LF'
      Checked = True
      State = cbChecked
      TabOrder = 6
    end
  end
  object CliSocket: TWSocket
    LineEnd = #13#10
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    KeepAliveOnOff = wsKeepAliveOnSystem
    KeepAliveTime = 30000
    KeepAliveInterval = 1000
    SocksLevel = '5'
    ExclusiveAddr = False
    ComponentOptions = []
    ListenBacklog = 15
    ReqVerLow = 1
    ReqVerHigh = 1
    OnDataAvailable = CliSocketDataAvailable
    OnSessionClosed = CliSocketSessionClosed
    OnSessionConnected = CliSocketSessionConnected
    SocketErrs = wsErrTech
    Left = 240
    Top = 128
  end
end
