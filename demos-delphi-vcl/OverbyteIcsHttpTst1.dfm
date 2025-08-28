object HttpTestForm: THttpTestForm
  Left = 115
  Top = 165
  Caption = 'ICS Http Test - https://www.overbyte.be - 6 Aug 2024'
  ClientHeight = 676
  ClientWidth = 781
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 781
    Height = 128
    Align = alTop
    ParentBackground = False
    TabOrder = 0
    OnResize = Panel1Resize
    object Label1: TLabel
      Left = 8
      Top = 11
      Width = 20
      Height = 13
      Caption = 'URL'
    end
    object Label2: TLabel
      Left = 8
      Top = 59
      Width = 27
      Height = 13
      Caption = 'Proxy'
    end
    object Label3: TLabel
      Left = 8
      Top = 35
      Width = 24
      Height = 13
      Caption = 'Data'
    end
    object Label4: TLabel
      Left = 8
      Top = 83
      Width = 132
      Height = 13
      Caption = 'Modified Since Date/Time'
    end
    object Label5: TLabel
      Left = 136
      Top = 66
      Width = 21
      Height = 13
      Caption = 'Port'
    end
    object Label6: TLabel
      Left = 136
      Top = 54
      Width = 27
      Height = 13
      Caption = 'Proxy'
    end
    object Label7: TLabel
      Left = 244
      Top = 66
      Width = 38
      Height = 13
      Caption = 'Version'
    end
    object Label8: TLabel
      Left = 244
      Top = 54
      Width = 23
      Height = 13
      Caption = 'Http'
    end
    object Label9: TLabel
      Left = 375
      Top = 54
      Width = 49
      Height = 13
      Caption = 'Post / Put'
    end
    object Label10: TLabel
      Left = 342
      Top = 80
      Width = 108
      Height = 26
      Caption = 'Maximum Bandwidth (bytes/sec)'
      WordWrap = True
    end
    object Label11: TLabel
      Left = 374
      Top = 66
      Width = 69
      Height = 13
      Caption = 'Content-Type'
    end
    object URLEdit: TEdit
      Left = 40
      Top = 8
      Width = 329
      Height = 21
      Hint = 'protocol://[user[:password]@]server[:port]/path'
      ImeName = 'Portuguese'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Text = 'http://www.overbyte.be'
    end
    object ProxyHostEdit: TEdit
      Left = 40
      Top = 56
      Width = 89
      Height = 21
      ImeName = 'Portuguese'
      TabOrder = 2
      Text = 'ProxyHostEdit'
    end
    object ProxyPortEdit: TEdit
      Left = 168
      Top = 56
      Width = 65
      Height = 21
      ImeName = 'Portuguese'
      TabOrder = 3
      Text = 'ProxyPortEdit'
    end
    object DataEdit: TEdit
      Left = 40
      Top = 32
      Width = 329
      Height = 21
      ImeName = 'Portuguese'
      TabOrder = 1
      Text = 'DataEdit'
    end
    object DateTimeEdit: TEdit
      Left = 136
      Top = 78
      Width = 97
      Height = 21
      ImeName = 'Portuguese'
      TabOrder = 4
      Text = 'DateTimeEdit'
    end
    object HttpVersionComboBox: TComboBox
      Left = 284
      Top = 56
      Width = 85
      Height = 21
      Style = csDropDownList
      ImeName = 'Portuguese'
      TabOrder = 7
      Items.Strings = (
        'HTTP/1.0'
        'HTTP/1.1')
    end
    object Panel2: TPanel
      Left = 619
      Top = 1
      Width = 161
      Height = 126
      Align = alRight
      ParentBackground = False
      TabOrder = 11
      object GetButton: TButton
        Left = 4
        Top = 8
        Width = 49
        Height = 21
        Caption = '&Get'
        Default = True
        TabOrder = 0
        OnClick = GetButtonClick
      end
      object HeadButton: TButton
        Left = 4
        Top = 56
        Width = 49
        Height = 21
        Caption = '&Head'
        TabOrder = 2
        OnClick = HeadButtonClick
      end
      object PostButton: TButton
        Left = 4
        Top = 32
        Width = 49
        Height = 21
        Caption = '&Post'
        TabOrder = 1
        OnClick = PostButtonClick
      end
      object AbortButton: TButton
        Left = 4
        Top = 80
        Width = 49
        Height = 21
        Caption = '&Abort'
        TabOrder = 3
        OnClick = AbortButtonClick
      end
      object ParseButton: TButton
        Left = 56
        Top = 80
        Width = 49
        Height = 21
        Caption = 'Par&se'
        TabOrder = 7
        OnClick = ParseButtonClick
      end
      object PutButton: TButton
        Left = 56
        Top = 32
        Width = 49
        Height = 21
        Caption = 'P&ut'
        TabOrder = 5
        OnClick = PutButtonClick
      end
      object CloseButton: TButton
        Left = 56
        Top = 8
        Width = 49
        Height = 21
        Caption = '&Close'
        TabOrder = 4
        OnClick = CloseButtonClick
      end
      object ClearButton: TButton
        Left = 56
        Top = 56
        Width = 49
        Height = 21
        Caption = 'Clea&r'
        TabOrder = 6
        OnClick = ClearButtonClick
      end
      object PatchButton: TButton
        Left = 108
        Top = 8
        Width = 49
        Height = 21
        Caption = '&Patch'
        TabOrder = 8
        OnClick = PatchButtonClick
      end
      object TraceButton: TButton
        Left = 108
        Top = 56
        Width = 49
        Height = 21
        Caption = '&Trace'
        TabOrder = 10
        OnClick = TraceButtonClick
      end
      object OptionsButton: TButton
        Left = 108
        Top = 32
        Width = 49
        Height = 21
        Caption = '&Options'
        TabOrder = 9
        OnClick = OptionsButtonClick
      end
      object DeleteButton: TButton
        Left = 108
        Top = 80
        Width = 49
        Height = 21
        Caption = '&Delete'
        TabOrder = 11
        OnClick = DeleteButtonClick
      end
    end
    object DisplayHeaderCheckBox: TCheckBox
      Left = 238
      Top = 84
      Width = 97
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Display Header'
      TabOrder = 8
    end
    object PostContentTypeEdit: TEdit
      Left = 458
      Top = 56
      Width = 83
      Height = 21
      AutoSize = False
      ImeName = 'Portuguese'
      TabOrder = 9
      Text = 'PostContentTypeEdit'
    end
    object BandwidthLimitEdit: TEdit
      Left = 458
      Top = 83
      Width = 83
      Height = 21
      AutoSize = False
      ImeName = 'Portuguese'
      TabOrder = 10
      Text = 'BandwidthLimitEdit'
    end
    object ContentEncodingCheckBox: TCheckBox
      Left = 8
      Top = 100
      Width = 131
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Content Encoding Gzip'
      TabOrder = 5
    end
    object NoRecvStreamCheckBox: TCheckBox
      Left = 158
      Top = 100
      Width = 113
      Height = 17
      Alignment = taLeftJustify
      Caption = 'No Receive Stream'
      TabOrder = 6
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 128
    Width = 781
    Height = 148
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ImeName = 'Portuguese'
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object DocumentMemo: TMemo
    Left = 0
    Top = 276
    Width = 781
    Height = 400
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ImeName = 'Portuguese'
    Lines.Strings = (
      'DocumentMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object HttpCli1: THttpCli
    URL = 'http://www.rtfm.be'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    Proxy = 'intsrv02'
    ProxyPort = '80'
    Agent = 'Mozilla/3.0 (compatible)'
    Accept = 'image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*'
    NoCache = False
    ResponseNoException = False
    ContentTypePost = 'application/x-www-form-urlencoded'
    LmCompatLevel = 0
    RequestVer = '1.0'
    FollowRelocation = True
    LocationChangeMaxCount = 5
    ServerAuth = httpAuthNone
    ProxyAuth = httpAuthNone
    BandwidthLimit = 10000
    BandwidthSampling = 1000
    Options = []
    IcsLogger = IcsLogger1
    Timeout = 30
    OnHeaderData = HttpCli1HeaderData
    OnCommand = HttpCli1Command
    OnDocBegin = HttpCli1DocBegin
    OnDocData = HttpCli1DocData
    OnDocEnd = HttpCli1DocEnd
    OnRequestDone = HttpCli1RequestDone
    OnLocationChange = HttpCli1LocationChange
    OnCookie = HttpCli1Cookie
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    SocketFamily = sfAny
    SocketErrs = wsErrTech
    OnSelectDns = HttpCli1SelectDns
    Left = 16
    Top = 136
  end
  object IcsLogger1: TIcsLogger
    TimeStampFormatString = 'hh:nn:ss:zzz'
    TimeStampSeparator = ' '
    LogFileOption = lfoAppend
    LogFileEncoding = lfeUtf8
    LogOptions = [loDestOutDebug, loProtSpecErr, loProtSpecInfo]
    Left = 136
    Top = 136
  end
  object IcsCookies: TIcsCookies
    AutoSave = False
    OnNewCookie = IcsCookiesNewCookie
    Left = 225
    Top = 135
  end
end
