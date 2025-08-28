object DDWebServiceSrv: TDDWebServiceSrv
  Left = 0
  Top = 0
  Caption = 'ICS DDService Web Server - https://www.overbyte.eu  - 7 Feb 2025'
  ClientHeight = 478
  ClientWidth = 852
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 437
    Width = 852
    Height = 41
    Align = alBottom
    TabOrder = 0
    object StopButton: TButton
      Left = 110
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Stop'
      Enabled = False
      TabOrder = 0
      OnClick = StopButtonClick
    end
    object StartButton: TButton
      Left = 20
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 1
      OnClick = StartButtonClick
    end
    object doClear: TButton
      Left = 205
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Clear Log'
      TabOrder = 2
      OnClick = doClearClick
    end
    object RecheckCertsButton: TButton
      Left = 300
      Top = 10
      Width = 81
      Height = 25
      Caption = 'Recheck Certs'
      TabOrder = 3
      OnClick = RecheckCertsButtonClick
    end
    object DisplayHeaderCheckBox: TCheckBox
      Left = 542
      Top = 15
      Width = 134
      Height = 17
      Caption = 'Display HTTP Protocol'
      TabOrder = 4
    end
    object DisplaySslInfo: TCheckBox
      Left = 397
      Top = 15
      Width = 129
      Height = 17
      Caption = 'Display SSL Client Info'
      Checked = True
      State = cbChecked
      TabOrder = 5
    end
    object doRestartApp: TButton
      Left = 696
      Top = 10
      Width = 100
      Height = 25
      Caption = 'Restart App'
      TabOrder = 6
      OnClick = doRestartAppClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 0
    Width = 852
    Height = 437
    Align = alClient
    Lines.Strings = (
      ''
      
        'Please note that this sample takes all it'#39's settings from an INI' +
        ' file.  '
      ''
      
        'The INI file needs to be manually edited to add multiple Host se' +
        'ctions to '
      
        'define the web server behaviour.  A default INI file is copied t' +
        'o user'
      'space when first run and the file to edit shown on start-up. ')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object IcsMailQueue: TIcsMailQueue
    MailServers = <>
    Active = False
    ArchiveSent = False
    DeleteFailed = True
    Debug = False
    BodyDebug = False
    RetryList = '5,5,10,10,30,30,60,90,300,300,300,300'
    QuStartDelay = 3
    SslVerMethod = MailSslVerNone
    SslRevocation = False
    SslReportChain = False
    SslRootFile = 'RootCaCertsBundle.pem'
    MailCliSecurity = sslCliSecTls11
    SmtpMethod = MailSmtpRelay
    FileQuSent = '"MailQuSent-"yyyymmdd".log'
    LogQuSent = False
    MxSrvUseSsl = False
    MxSocketFamily = sfIPv4
    LogEvent = IcsMailQueueLogEvent
    OATokenEvent = IcsMailQueueOATokenEvent
    OcspCacheFile = 'ocspmailqucache.recs'
    NoSSL = False
    Left = 50
    Top = 120
  end
  object IcsSslX509Certs: TSslX509Certs
    AcmeAccKeyType = PrivKeyRsa2048
    AutoOrderComplete = False
    CertSubAltNames = <>
    CertCsrOrigin = CsrOriginProps
    CertOutFmts = []
    CertSerNumType = SerNumRandom
    CertSignDigestType = Digest_sha256
    CertValidity = 365
    DebugLevel = DebugConn
    DomWebSrvIP = '0.0.0.0'
    DomWebSrvIP2 = '::'
    MsCertLoc = MsLocMachine
    KeepOldCA = False
    LogJson = False
    LogPkeys = False
    OAAuthType = OAuthTypeWeb
    OARefreshAuto = False
    OARefrMinsPrior = 120
    OAWebSrvIP = '127.0.0.1'
    OAWebSrvPort = '8080'
    PrivKeyCipher = PrivKeyEncNone
    PrivKeyType = PrivKeyRsa2048
    AutoAccountClose = False
    AccountTimeOutMins = 10
    SeqOrderNum = 0
    SocketFamily = sfAny
    SuppCertChallenge = ChallNone
    SupplierProto = SuppProtoNone
    OnCertProg = IcsSslX509CertsCertProg
    OnNewCert = IcsSslX509CertsNewCert
    OnOAuthAuthUrl = IcsSslX509CertsOAuthAuthUrl
    OnChallengeDNS = IcsSslX509CertsChallengeDNS
    Left = 140
    Top = 120
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 225
    Top = 120
  end
  object SslHttpAppSrv1: TSslHttpAppSrv
    ListenBacklog = 5
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
    ServerHeader = 'Server: ICS-HttpServer-8.69'
    OnServerStarted = SslHttpAppSrv1ServerStarted
    OnServerStopped = SslHttpAppSrv1ServerStopped
    OnClientConnect = SslHttpAppSrv1ClientConnect
    OnGetDocument = SslHttpAppSrv1GetDocument
    OnHeadDocument = SslHttpAppSrv1HeadDocument
    OnPostDocument = SslHttpAppSrv1PostDocument
    OnPostedData = SslHttpAppSrv1PostedData
    OnOptionsDocument = SslHttpAppSrv1OptionsDocument
    OnPutDocument = SslHttpAppSrv1PutDocument
    OnDeleteDocument = SslHttpAppSrv1DeleteDocument
    OnTraceDocument = SslHttpAppSrv1TraceDocument
    OnPatchDocument = SslHttpAppSrv1PatchDocument
    OnConnectDocument = SslHttpAppSrv1ConnectDocument
    OnHttpRequestDone = SslHttpAppSrv1HttpRequestDone
    OnBeforeProcessRequest = SslHttpAppSrv1BeforeProcessRequest
    OnAfterAnswer = SslHttpAppSrv1AfterAnswer
    OnHttpMimeContentType = SslHttpAppSrv1HttpMimeContentType
    OnAuthGetPassword = SslHttpAppSrv1AuthGetPassword
    OnAuthResult = SslHttpAppSrv1AuthResult
    OnAuthGetType = SslHttpAppSrv1AuthGetType
    OnAuthNtlmBeforeValidate = SslHttpAppSrv1AuthNtlmBeforeValidate
    AuthTypes = []
    AuthRealm = 'ics'
    OnBgException = SslHttpAppSrv1BgException
    SocketErrs = wsErrFriendly
    ExclusiveAddr = True
    onWellKnownDir = SslHttpAppSrv1WellKnownDir
    OnHttpRespHdr = SslHttpAppSrv1HttpRespHdr
    SessionTimeout = 300
    MaxSessions = 100
    MaxUploadMB = 100
    MaxStreamMB = 100
    OnDeleteSession = SslHttpAppSrv1DeleteSession
    OnVirtualException = SslHttpAppSrv1VirtualException
    OnDisplay = SslHttpAppSrv1Display
    SslEnable = False
    IcsHosts = <>
    SslCliCertMethod = sslCliCertNone
    SslCertAutoOrder = False
    CertExpireDays = 30
    SslX509Certs = IcsSslX509Certs
    OcspSrvStapling = False
    OnSslHandshakeDone = SslHttpAppSrv1SslHandshakeDone
    OnSslServerName = SslHttpAppSrv1SslServerName
    OnSslAlpnSelect = SslHttpAppSrv1SslAlpnSelect
    Left = 300
    Top = 120
  end
  object IcsRestEmail: TIcsRestEmail
    RestEmailType = RestEmailGoogle
    ForceLogin = False
    LoginTimeout = 120
    HdrFieldList = 'to,from,subject,date'
    DebugLevel = DebugNone
    OAAuthType = OAuthTypeWeb
    MsUserAuth = 'consumers'
    OnEmailProg = IcsRestEmailEmailProg
    OnEmailNewToken = IcsRestEmailEmailNewToken
    OnOAAuthUrl = IcsRestEmailOAAuthUrl
    Left = 395
    Top = 120
  end
  object IcsDomainNameCache1: TIcsDomainNameCache
    DNMethod = MethodWinsock
    DnsServerStrat = SrvStratOne
    DefTTL = 3600
    MaxLookups = 5
    QTimeout = 5
    AddLocalhost = False
    DBLANlookup = LanLookDef
    OnDNUpdateEvent = IcsDomainNameCache1DNUpdateEvent
    Left = 515
    Top = 122
  end
  object IcsAppMonCli: TIcsAppMonCli
    AppMonMode = AMModeMonitor
    AppHandle = 0
    AutoStart = False
    LANWide = False
    LogPackets = False
    PingMinSecs = 60
    PingMaxSecs = 180
    onLogEvent = IcsAppMonCliLogEvent
    Left = 641
    Top = 121
  end
end
