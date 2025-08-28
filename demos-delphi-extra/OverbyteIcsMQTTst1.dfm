object MainForm: TMainForm
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 
    'ICS MQTT Server / Client Demo - https://www.overbyte.eu  - 7 Aug' +
    ' 2024'
  ClientHeight = 574
  ClientWidth = 879
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object PixTxt: TLabel
    Left = 618
    Top = 2
    Width = 193
    Height = 13
    Alignment = taCenter
    AutoSize = False
  end
  object GroupBoxClient: TGroupBox
    Left = 5
    Top = 270
    Width = 862
    Height = 298
    Caption = 'MQTT Client'
    TabOrder = 0
    object Label6: TLabel
      Left = 587
      Top = 128
      Width = 42
      Height = 13
      Caption = 'Message'
    end
    object Label5: TLabel
      Left = 436
      Top = 93
      Width = 91
      Height = 13
      Caption = 'Subscription Topics'
    end
    object Label18: TLabel
      Left = 586
      Top = 92
      Width = 25
      Height = 13
      Caption = 'Topic'
    end
    object Label13: TLabel
      Left = 735
      Top = 55
      Width = 20
      Height = 13
      Caption = 'Port'
    end
    object COnlineTxt: TLabel
      Left = 411
      Top = 21
      Width = 15
      Height = 13
      Caption = 'NO'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label4: TLabel
      Left = 375
      Top = 21
      Width = 30
      Height = 13
      Caption = 'Online'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object CEnableTxt: TLabel
      Left = 353
      Top = 21
      Width = 15
      Height = 13
      Caption = 'NO'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label3: TLabel
      Left = 309
      Top = 21
      Width = 38
      Height = 13
      Caption = 'Enabled'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label16: TLabel
      Left = 178
      Top = 21
      Width = 19
      Height = 13
      Caption = 'Qos'
    end
    object CMsgTxt: TLabel
      Left = 53
      Top = 21
      Width = 40
      Height = 13
      Caption = '<none>'
    end
    object Label8: TLabel
      Left = 5
      Top = 21
      Width = 42
      Height = 13
      Caption = 'Last Msg'
    end
    object Label12: TLabel
      Left = 436
      Top = 52
      Width = 68
      Height = 26
      Caption = 'Client Host or Address'
      WordWrap = True
    end
    object ClientIDTxt: TLabel
      Left = 436
      Top = 255
      Width = 38
      Height = 13
      Caption = 'ClientID'
    end
    object CQosTxt: TLabel
      Left = 205
      Top = 21
      Width = 33
      Height = 13
      Caption = '<qos>'
    end
    object ClientMemo3: TMemo
      Left = 297
      Top = 39
      Width = 133
      Height = 249
      TabOrder = 0
      OnDblClick = ClientMemo3DblClick
    end
    object ButtonCliOff: TButton
      Left = 774
      Top = 249
      Width = 50
      Height = 25
      Caption = 'Off'
      TabOrder = 24
      OnClick = ButtonCliOffClick
    end
    object ButtonCliOn: TButton
      Left = 718
      Top = 249
      Width = 50
      Height = 25
      Caption = 'On'
      TabOrder = 23
      OnClick = ButtonCliOnClick
    end
    object MsgBox: TMemo
      Left = 586
      Top = 143
      Width = 145
      Height = 69
      Lines.Strings = (
        'WARNING'
        'Coolant Leak.'
        'Primary Cooling System.'
        'Reactor 5.')
      TabOrder = 12
    end
    object CliJTxt: TEdit
      Left = 567
      Top = 249
      Width = 33
      Height = 21
      TabOrder = 21
      Text = '3'
    end
    object CMsgId: TEdit
      Left = 623
      Top = 249
      Width = 89
      Height = 21
      TabOrder = 22
      Text = '3'
    end
    object ButtonCliSend: TButton
      Left = 511
      Top = 249
      Width = 50
      Height = 25
      Caption = 'Send'
      TabOrder = 20
      OnClick = ButtonCliSendClick
    end
    object ButtonCliPublish: TButton
      Left = 586
      Top = 218
      Width = 91
      Height = 25
      Caption = 'Publish Message'
      TabOrder = 15
      OnClick = ButtonCliPublishClick
    end
    object ButtonCliUnsub: TButton
      Left = 511
      Top = 218
      Width = 69
      Height = 25
      Caption = 'Unsubscribe'
      TabOrder = 14
      OnClick = ButtonCliUnsubClick
    end
    object ButtonCliShow: TButton
      Left = 662
      Top = 19
      Width = 69
      Height = 25
      Caption = 'Show'
      TabOrder = 4
      OnClick = ButtonCliShowClick
    end
    object ButtonCliSubs: TButton
      Left = 436
      Top = 218
      Width = 69
      Height = 25
      Caption = 'Subscribe'
      TabOrder = 13
      OnClick = ButtonCliSubsClick
    end
    object TopicsTxt: TMemo
      Left = 436
      Top = 108
      Width = 144
      Height = 104
      Lines.Strings = (
        'update/memo'
        'update/png/+'
        'will/#')
      TabOrder = 10
    end
    object TopicTxt: TEdit
      Left = 586
      Top = 108
      Width = 146
      Height = 21
      TabOrder = 11
      Text = 'update/memo'
    end
    object CMBoxCli: TCheckBox
      Left = 735
      Top = 220
      Width = 108
      Height = 17
      Caption = 'Client Monitor'
      Checked = True
      State = cbChecked
      TabOrder = 19
    end
    object BounceBoxCli: TCheckBox
      Left = 735
      Top = 200
      Width = 97
      Height = 17
      Caption = ' Local Bounce'
      TabOrder = 18
    end
    object RetainBox: TCheckBox
      Left = 735
      Top = 180
      Width = 97
      Height = 17
      Caption = 'Retain'
      TabOrder = 17
      OnClick = RetainBoxClick
    end
    object CleanBox2: TCheckBox
      Left = 737
      Top = 160
      Width = 97
      Height = 17
      Caption = 'Clean'
      TabOrder = 16
    end
    object ButtonCliUpdate: TButton
      Left = 779
      Top = 19
      Width = 53
      Height = 25
      Caption = 'Update'
      TabOrder = 5
      OnClick = ButtonCliUpdateClick
    end
    object CPortTxt: TEdit
      Left = 774
      Top = 50
      Width = 44
      Height = 21
      TabOrder = 7
      Text = '1883'
    end
    object ButtonCliKill: TButton
      Left = 587
      Top = 19
      Width = 69
      Height = 27
      Caption = 'Kill'
      TabOrder = 3
      OnClick = ButtonCliKillClick
    end
    object ButtonCliStop: TButton
      Left = 511
      Top = 19
      Width = 69
      Height = 27
      Caption = 'Stop'
      TabOrder = 2
      OnClick = ButtonCliStopClick
    end
    object ButtonCliStart: TButton
      Left = 436
      Top = 19
      Width = 69
      Height = 27
      Caption = 'Start'
      TabOrder = 1
      OnClick = ButtonCliStartClick
    end
    object ClientMemo: TMemo
      Left = 5
      Top = 39
      Width = 279
      Height = 249
      ScrollBars = ssVertical
      TabOrder = 25
      OnDblClick = ClientMemoDblClick
    end
    object ClientHost: TEdit
      Left = 513
      Top = 50
      Width = 208
      Height = 21
      TabOrder = 6
      Text = 'localhost'
    end
    object VerifyCertChain: TCheckBox
      Left = 513
      Top = 77
      Width = 164
      Height = 17
      Caption = 'Verify Certificate Chain'
      Checked = True
      State = cbChecked
      TabOrder = 8
    end
    object ClientQOS: TRadioGroup
      Left = 738
      Top = 83
      Width = 113
      Height = 71
      Caption = 'Quality of Service'
      ItemIndex = 1
      Items.Strings = (
        'A Most Once'
        'At Least Once'
        'Exactly Once')
      TabOrder = 9
    end
  end
  object GroupBoxServer: TGroupBox
    Left = 8
    Top = 8
    Width = 862
    Height = 262
    Caption = 'MQTT Server'
    TabOrder = 1
    object Label11: TLabel
      Left = 5
      Top = 21
      Width = 42
      Height = 13
      Caption = 'Last Msg'
    end
    object SMsgTxt: TLabel
      Left = 61
      Top = 21
      Width = 40
      Height = 13
      Caption = '<none>'
    end
    object Label14: TLabel
      Left = 180
      Top = 21
      Width = 19
      Height = 13
      Caption = 'Qos'
    end
    object Label15: TLabel
      Left = 311
      Top = 21
      Width = 38
      Height = 13
      Caption = 'Enabled'
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentColor = False
      ParentFont = False
    end
    object SEnableTxt: TLabel
      Left = 355
      Top = 21
      Width = 15
      Height = 13
      Caption = 'NO'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label9: TLabel
      Left = 376
      Top = 21
      Width = 32
      Height = 13
      Caption = 'Clients'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object SClientsTxt: TLabel
      Left = 414
      Top = 21
      Width = 6
      Height = 13
      Caption = '0'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object Label10: TLabel
      Left = 440
      Top = 100
      Width = 110
      Height = 13
      Caption = 'Server Ports:  Non-SSL'
    end
    object CIDTxt: TLabel
      Left = 373
      Top = 8
      Width = 17
      Height = 13
      AutoSize = False
    end
    object SQosTxt: TLabel
      Left = 205
      Top = 21
      Width = 33
      Height = 13
      Caption = '<qos>'
    end
    object Label1: TLabel
      Left = 440
      Top = 75
      Width = 102
      Height = 13
      Caption = 'Server Listen IP Addr'
    end
    object Label2: TLabel
      Left = 615
      Top = 100
      Width = 40
      Height = 13
      Caption = 'SSL Port'
    end
    object Label17: TLabel
      Left = 441
      Top = 125
      Width = 105
      Height = 13
      Caption = 'Certificate Host Name'
    end
    object Label19: TLabel
      Left = 440
      Top = 150
      Width = 104
      Height = 13
      Caption = 'Certificate Bundle File'
    end
    object Label20: TLabel
      Left = 440
      Top = 175
      Width = 59
      Height = 13
      Caption = 'Root CA File'
    end
    object ServerMemo: TMemo
      Left = 5
      Top = 40
      Width = 421
      Height = 208
      ScrollBars = ssVertical
      TabOrder = 0
      OnDblClick = ServerMemoDblClick
    end
    object ButtonSrvStart: TButton
      Left = 433
      Top = 30
      Width = 69
      Height = 27
      Caption = 'Start'
      TabOrder = 1
      OnClick = ButtonSrvStartClick
    end
    object ButtonSrvStop: TButton
      Left = 508
      Top = 30
      Width = 69
      Height = 27
      Caption = 'Stop'
      TabOrder = 2
      OnClick = ButtonSrvStopClick
    end
    object ButtonSrvShowCli: TButton
      Left = 583
      Top = 30
      Width = 80
      Height = 27
      Caption = 'Show Clients'
      TabOrder = 3
      OnClick = ButtonSrvShowCliClick
    end
    object BounceBoxSrv: TCheckBox
      Left = 591
      Top = 197
      Width = 90
      Height = 17
      Caption = ' Local Bounce'
      TabOrder = 12
    end
    object CMBoxSrvr: TCheckBox
      Left = 441
      Top = 199
      Width = 137
      Height = 17
      Caption = 'Client Monitor for Server'
      Checked = True
      State = cbChecked
      TabOrder = 11
    end
    object ButtonSrvBrokers: TButton
      Left = 669
      Top = 30
      Width = 96
      Height = 27
      Caption = 'Brokers Windows'
      TabOrder = 4
      OnClick = ButtonSrvBrokersClick
    end
    object ServerNonPort: TEdit
      Left = 561
      Top = 95
      Width = 38
      Height = 21
      TabOrder = 6
      Text = '1883'
    end
    object ServerIpAddr: TComboBox
      Left = 561
      Top = 70
      Width = 179
      Height = 21
      TabOrder = 5
    end
    object ServerSSLPort: TEdit
      Left = 674
      Top = 95
      Width = 38
      Height = 21
      TabOrder = 7
      Text = '8883'
    end
    object CertHostName: TEdit
      Left = 561
      Top = 120
      Width = 226
      Height = 21
      TabOrder = 8
      Text = 'localhost'
    end
    object CertBundleFile: TEdit
      Left = 561
      Top = 145
      Width = 293
      Height = 21
      TabOrder = 9
      Text = 'localhost.pem'
    end
    object RootCAFile: TEdit
      Left = 561
      Top = 170
      Width = 293
      Height = 21
      TabOrder = 10
      Text = 'TrustedCABundle.pem'
    end
    object CertAutoOrder: TCheckBox
      Left = 687
      Top = 197
      Width = 161
      Height = 17
      Caption = 'Auto Order SSL Certificate'
      Enabled = False
      TabOrder = 13
    end
  end
  object MQTTServer: TIcsMQTTServer
    MaxRetries = 4
    RetryTime = 60
    LocalBounce = False
    IcsHosts = <
      item
        HostNames.Strings = (
          '*')
        HostEnabled = True
        BindIpAddr = '0.0.0.0'
        BindSslPort = 8883
        BindNonPort = 1883
        HostTag = 'HostTag'
        ForwardProxy = False
        WebLogIdx = 0
        SslSrvSecurity = sslSrvSecBack
        AuthSslCmd = False
        AuthForceSsl = False
        WebRedirectStat = 0
        CliCertMethod = sslCliCertNone
        SslLoadSource = CertLoadFile
        CertSupplierProto = SuppProtoNone
        CertChallenge = ChallNone
        CertPKeyType = PrivKeyRsa1024
        CertSignDigest = Digest_md5
      end>
    SslCertAutoOrder = False
    CertExpireDays = 30
    SslX509Certs = SslX509Certs1
    OcspSrvStapling = False
    OnFailure = MQTTServerFailure
    OnStoreSession = MQTTServerStoreSession
    OnRestoreSession = MQTTServerRestoreSession
    OnDeleteSession = MQTTServerDeleteSession
    OnBrokerOnline = MQTTServerBrokerOnline
    OnBrokerOffline = MQTTServerBrokerOffline
    OnBrokerEnableChange = MQTTServerBrokerEnableChange
    OnEnableChange = MQTTServerEnableChange
    OnSubscription = MQTTServerSubscription
    OnClientsChange = MQTTServerClientsChange
    OnCheckUser = MQTTServerCheckUser
    OnObituary = MQTTServerObituary
    OnMon = MQTTServerMon
    Left = 131
    Top = 125
  end
  object MQTTClient: TIcsMQTTClient
    KeepAlive = 10
    MaxRetries = 8
    RetryTime = 60
    Clean = True
    Broker = False
    AutoSubscribe = False
    Username = 'admin'
    Password = 'password'
    Host = 'localhost'
    Port = 1883
    LocalBounce = False
    SslVerifyCerts = False
    SslReportChain = False
    SslRevocation = False
    SslCliSecurity = sslCliSecIgnore
    OnClientID = MQTTClientClientID
    OnMon = MQTTClientMon
    OnOnline = MQTTClientOnline
    OnOffline = MQTTClientOffline
    OnEnableChange = MQTTClientEnableChange
    OnFailure = MQTTClientFailure
    OnMsg = MQTTClientMsg
    Left = 150
    Top = 397
  end
  object SslX509Certs1: TSslX509Certs
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
    Left = 220
    Top = 131
  end
end
