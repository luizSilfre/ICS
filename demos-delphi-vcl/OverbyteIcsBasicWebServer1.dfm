object WebServerForm: TWebServerForm
  Left = 0
  Top = 0
  Caption = 'ICS Basic Web Server  - https://www.overbyte.eu  - 7 Feb 2025'
  ClientHeight = 580
  ClientWidth = 802
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 15
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 802
    Height = 41
    Align = alTop
    TabOrder = 0
    object StartButton: TButton
      Left = 29
      Top = 8
      Width = 57
      Height = 21
      Caption = '&Start'
      Default = True
      TabOrder = 0
      OnClick = StartButtonClick
    end
    object StopButton: TButton
      Left = 111
      Top = 8
      Width = 57
      Height = 21
      Caption = 'S&top'
      Enabled = False
      TabOrder = 1
      OnClick = StopButtonClick
    end
    object DisplayHeaderCheckBox: TCheckBox
      Left = 500
      Top = 12
      Width = 134
      Height = 17
      Caption = 'Display HTTP Protocol'
      TabOrder = 2
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 41
    Width = 802
    Height = 539
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      
        'Please note that this basic sample has it'#39's setting hard coded i' +
        'n the program. '
      ''
      'Most web servers would take settings from an INI file.  '
      '')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object SslHttpAppSrv: TSslHttpAppSrv
    ListenBacklog = 15
    MultiListenSockets = <>
    Port = '80'
    Addr = '0.0.0.0'
    SocketFamily = sfIPv4
    MaxClients = 0
    DocDir = 'c:\wwwroot'
    TemplateDir = 'c:\wwwroot\templates'
    DefaultDoc = 'index.html'
    LingerOnOff = wsLingerNoSet
    LingerTimeout = 0
    Options = []
    KeepAliveTimeSec = 10
    KeepAliveTimeXferSec = 300
    MaxRequestsKeepAlive = 100
    SizeCompressMin = 5000
    SizeCompressMax = 5000000
    MaxBlkSize = 8192
    BandwidthLimit = 0
    BandwidthSampling = 1000
    ServerHeader = 'Server: ICS-HttpServer-V9.4'
    OnClientConnect = SslHttpAppSrvClientConnect
    OnGetDocument = SslHttpAppSrvProcDocument
    OnHeadDocument = SslHttpAppSrvProcDocument
    OnPostDocument = SslHttpAppSrvProcDocument
    OnPutDocument = SslHttpAppSrvProcDocument
    OnPatchDocument = SslHttpAppSrvProcDocument
    OnHttpRequestDone = SslHttpAppSrvHttpRequestDone
    OnBeforeProcessRequest = SslHttpAppSrvBeforeProcessRequest
    OnBeforeAnswer = SslHttpAppSrvBeforeAnswer
    OnAfterAnswer = SslHttpAppSrvAfterAnswer
    OnAuthGetPassword = SslHttpAppSrvAuthGetPassword
    OnAuthResult = SslHttpAppSrvAuthResult
    OnAuthGetType = SslHttpAppSrvAuthGetType
    AuthTypes = []
    AuthRealm = 'ics'
    OnBgException = SslHttpAppSrvBgException
    SocketErrs = wsErrTech
    ExclusiveAddr = True
    OnHttpRespHdr = SslHttpAppSrvHttpRespHdr
    SessionTimeout = 300
    MaxSessions = 100
    MaxUploadMB = 200
    MaxStreamMB = 50
    OnDisplay = SslHttpAppSrvDisplay
    SslEnable = False
    IcsHosts = <>
    SslCliCertMethod = sslCliCertNone
    SslCertAutoOrder = False
    CertExpireDays = 30
    OcspSrvStapling = False
    OnSslHandshakeDone = SslHttpAppSrvSslHandshakeDone
    OnSslServerName = SslHttpAppSrvSslServerName
    Left = 133
    Top = 170
  end
end
