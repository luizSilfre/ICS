object SocksTestForm: TSocksTestForm
  Left = 175
  Top = 163
  Caption = 
    'ICS Socks and HTTP Tunnel Proxy Test - https://www.overbyte.be -' +
    ' 7 Aug 2024'
  ClientHeight = 483
  ClientWidth = 737
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object Label8: TLabel
    Left = 225
    Top = 215
    Width = 33
    Height = 13
    Caption = 'Label8'
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 196
    Width = 737
    Height = 287
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 737
    Height = 196
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 12
      Width = 87
      Height = 13
      Caption = 'Target Hostname'
    end
    object Label2: TLabel
      Left = 20
      Top = 40
      Width = 56
      Height = 13
      Caption = 'Target Port'
    end
    object Label3: TLabel
      Left = 255
      Top = 12
      Width = 61
      Height = 13
      Caption = 'Proxy Server'
    end
    object Label4: TLabel
      Left = 267
      Top = 36
      Width = 51
      Height = 13
      Caption = 'Proxy Port'
    end
    object Label5: TLabel
      Left = 260
      Top = 60
      Width = 53
      Height = 13
      Caption = 'Proxy User'
    end
    object Label6: TLabel
      Left = 240
      Top = 84
      Width = 79
      Height = 13
      Caption = 'Proxy Password'
    end
    object Label7: TLabel
      Left = 118
      Top = 107
      Width = 50
      Height = 13
      Caption = 'Proxy URL'
    end
    object Label9: TLabel
      Left = 331
      Top = 157
      Width = 199
      Height = 26
      Caption = 
        'socks5://[user:password@]host:port or'#13#10'http://[user:password@]ho' +
        'st:port'
    end
    object Label10: TLabel
      Left = 118
      Top = 133
      Width = 52
      Height = 13
      Caption = 'Command'
    end
    object ConnectButton: TButton
      Left = 8
      Top = 165
      Width = 65
      Height = 25
      Caption = '&Connect'
      TabOrder = 0
      OnClick = ConnectButtonClick
    end
    object DisconnectButton: TButton
      Left = 85
      Top = 165
      Width = 65
      Height = 25
      Caption = '&Disconnect'
      TabOrder = 1
      OnClick = DisconnectButtonClick
    end
    object TargetHostEdit: TEdit
      Left = 104
      Top = 8
      Width = 125
      Height = 21
      TabOrder = 2
      Text = 'TargetHostEdit'
    end
    object TargetPortEdit: TEdit
      Left = 104
      Top = 36
      Width = 125
      Height = 21
      TabOrder = 3
      Text = 'TargetPortEdit'
    end
    object SocksServerEdit: TEdit
      Left = 332
      Top = 8
      Width = 184
      Height = 21
      TabOrder = 4
      Text = 'SocksServerEdit'
    end
    object SocksPortEdit: TEdit
      Left = 332
      Top = 32
      Width = 184
      Height = 21
      TabOrder = 5
      Text = 'SocksPortEdit'
    end
    object SocksUsercodeEdit: TEdit
      Left = 332
      Top = 56
      Width = 184
      Height = 21
      TabOrder = 6
      Text = 'SocksUsercodeEdit'
    end
    object SocksPasswordEdit: TEdit
      Left = 332
      Top = 80
      Width = 184
      Height = 21
      TabOrder = 7
      Text = 'SocksPasswordEdit'
    end
    object SocksAuthCheckBox: TCheckBox
      Left = 106
      Top = 73
      Width = 90
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Authentication'
      TabOrder = 8
    end
    object ClearButton: TButton
      Left = 156
      Top = 165
      Width = 65
      Height = 25
      Caption = 'C&lear'
      TabOrder = 9
      OnClick = ClearButtonClick
    end
    object Socks4RadioButton: TRadioButton
      Left = 8
      Top = 59
      Width = 83
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Socks 4'
      TabOrder = 10
    end
    object Socks5RadioButton: TRadioButton
      Left = 8
      Top = 77
      Width = 83
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Socks 5'
      TabOrder = 11
    end
    object HttpTunnelRadioButton: TRadioButton
      Left = 8
      Top = 94
      Width = 83
      Height = 17
      Alignment = taLeftJustify
      Caption = 'HTTP Tunnel'
      Checked = True
      TabOrder = 12
      TabStop = True
    end
    object ProxyUrlEdit: TEdit
      Left = 175
      Top = 103
      Width = 341
      Height = 21
      TabOrder = 13
      Text = 'ProxyUrlEdit'
    end
    object CommandEdit: TEdit
      Left = 175
      Top = 130
      Width = 341
      Height = 21
      TabOrder = 14
      Text = 'CommandEdit'
    end
    object cmdButton: TButton
      Left = 227
      Top = 165
      Width = 65
      Height = 25
      Caption = 'Com&mand'
      TabOrder = 15
      OnClick = cmdButtonClick
    end
  end
  object WSocket1: TWSocket
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
    OnHttpTunnelError = WSocket1HttpTunnelError
    OnHttpTunnelConnected = WSocket1HttpTunnelConnected
    OnDataAvailable = WSocket1DataAvailable
    OnSessionClosed = WSocket1SessionClosed
    OnSessionConnected = WSocket1SessionConnected
    OnSocksConnected = WSocket1SocksConnected
    OnSocksError = WSocket1SocksError
    OnSocksAuthState = WSocket1SocksAuthState
    SocketErrs = wsErrTech
    Left = 78
    Top = 274
  end
end
