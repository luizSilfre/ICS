object Form1: TForm1
  Left = 221
  Top = 298
  Caption = 'Simple SSL Client https://www.overbyte.be - 7 Feb 2024'
  ClientHeight = 368
  ClientWidth = 609
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    609
    368)
  TextHeight = 13
  object Label1: TLabel
    Left = 15
    Top = 346
    Width = 26
    Height = 13
    Caption = 'Host:'
  end
  object Label2: TLabel
    Left = 164
    Top = 322
    Width = 24
    Height = 13
    Caption = 'Port:'
  end
  object Label3: TLabel
    Left = 232
    Top = 324
    Width = 23
    Height = 13
    Caption = 'URL:'
  end
  object Label4: TLabel
    Left = 24
    Top = 276
    Width = 31
    Height = 13
    Caption = 'Label4'
  end
  object Memo1: TMemo
    Left = 92
    Top = 0
    Width = 509
    Height = 338
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
    WordWrap = False
  end
  object Button1: TButton
    Left = 6
    Top = 58
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 132
    Width = 75
    Height = 25
    Caption = 'Close'
    Enabled = False
    TabOrder = 2
    OnClick = Button2Click
  end
  object EditHost: TEdit
    Left = 52
    Top = 343
    Width = 109
    Height = 21
    TabOrder = 3
    Text = '127.0.0.1'
  end
  object EditPort: TEdit
    Left = 192
    Top = 345
    Width = 29
    Height = 21
    TabOrder = 4
    Text = '443'
  end
  object EditUrl: TEdit
    Left = 257
    Top = 345
    Width = 259
    Height = 21
    TabOrder = 5
    Text = '/big5000.txt'
  end
  object Button3: TButton
    Left = 6
    Top = 94
    Width = 75
    Height = 25
    Caption = 'Get URL'
    Enabled = False
    TabOrder = 6
    OnClick = Button3Click
  end
  object Sock: TSslWSocket
    LineEnd = #13#10
    Port = '443'
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    SocksLevel = '5'
    ExclusiveAddr = False
    ComponentOptions = []
    ListenBacklog = 15
    OnDataAvailable = SockDataAvailable
    OnSessionClosed = SockSessionClosed
    OnSessionConnected = SockSessionConnected
    SocketErrs = wsErrTech
    SslContext = SslContext1
    SslEnable = False
    SslMode = sslModeClient
    OnSslHandshakeDone = SockSslHandshakeDone
    Left = 20
    Top = 24
  end
  object SslContext1: TSslContext
    SslDHParamLines.Strings = (
      '-----BEGIN DH PARAMETERS-----'
      'MIICCAKCAgEA45KZVdTCptcakXZb7jJvSuuOdMlUbl1tpncHbQcYbFhRbcFmmefp'
      'bOmZsTowlWHQpoYRRTe6NEvYox8J+44i/X5cJkMTlIgMb0ZBty7t76U9f6qAId/O'
      '6elE0gnk2ThER9nmBcUA0ZKgSXn0XCBu6j5lzZ0FS+bx9OVNhlzvIFBclRPXbI58'
      '71dRoTjOjfO1SIzV69T3FoKJcqur58l8b+no/TOQzekMzz4XJTRDefqvePhj7ULP'
      'Z/Zg7vtEh11h8gHR0/rlF378S05nRMq5hbbJeLxIbj9kxQunETSbwwy9qx0SyQgH'
      'g+90+iUCrKCJ9Fb7WKqtQLkQuzJIkkXkXUyuxUuyBOeeP9XBUAOQu+eYnRPYSmTH'
      'GkhyRbIRTPCDiBWDFOskdyGYYDrxiK7LYJQanqHlEFtjDv9t1XmyzDm0k7W9oP/J'
      'p0ox1+WIpFgkfv6nvihqCPHtAP5wevqXNIQADhDk5EyrR3XWRFaySeKcmREM9tbc'
      'bOvmsEp5MWCC81ZsnaPAcVpO66aOPojNiYQZUbmm70fJsr8BDzXGpcQ44+wmL4Ds'
      'k3+ldVWAXEXs9s1vfl4nLNXefYl74cV8E5Mtki9hCjUrUQ4dzbmNA5fg1CyQM/v7'
      'JuP6PBYFK7baFDjG1F5YJiO0uHo8sQx+SWdJnGsq8piI3w0ON9JhUvMCAQI='
      '-----END DH PARAMETERS-----')
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslVerifyFlags = []
    SslVerifyFlagsValue = 0
    SslCheckHostFlags = []
    SslCheckHostFlagsValue = 0
    SslSecLevel = sslSecLevel80bits
    SslOptions = []
    SslOptions2 = [sslOpt2_ALLOW_UNSAFE_LEGACY_RENEGOTIATION, SslOpt2_LEGACY_SERVER_CONNECT]
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslVerifyPeerModesValue = 1
    SslSessionCacheModes = []
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23
    SslMinVersion = sslVerSSL3
    SslMaxVersion = sslVerMax
    SslECDHMethod = sslECDHNone
    SslCryptoGroups = 'P-256:X25519:P-384:P-512'
    SslCliSecurity = sslCliSecIgnore
    SslOcspStatus = False
    UseSharedCAStore = False
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    AutoEnableBuiltinEngines = False
    Left = 48
    Top = 24
  end
end
