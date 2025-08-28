object NsLookupForm: TNsLookupForm
  Left = 257
  Top = 327
  Caption = 
    'ICS Domain Name Server Lookup - https://www.overbyte.be - 7 Feb ' +
    '2024'
  ClientHeight = 644
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
  object DisplayMemo: TMemo
    Left = 0
    Top = 121
    Width = 737
    Height = 523
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 737
    Height = 121
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 417
      Top = 15
      Width = 80
      Height = 13
      Caption = 'Host to Lookup'
    end
    object Label2: TLabel
      Left = 10
      Top = 10
      Width = 95
      Height = 14
      AutoSize = False
      Caption = 'Single DNS Server'
      WordWrap = True
    end
    object Label3: TLabel
      Left = 393
      Top = 45
      Width = 98
      Height = 13
      Caption = 'Lookup Query Type'
    end
    object Label4: TLabel
      Left = 137
      Top = 35
      Width = 95
      Height = 14
      AutoSize = False
      Caption = 'DNS Server List'
      WordWrap = True
    end
    object ClearDisplayBitBtn: TBitBtn
      Left = 677
      Top = 80
      Width = 25
      Height = 25
      Hint = 'Clear Log'
      Glyph.Data = {
        F6000000424DF600000000000000760000002800000010000000100000000100
        0400000000008000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        777700000000000777770FFFFFFFFF0777770FFFFFFFFF0777770FFFFFFFFF07
        77770FFFFFFFF00777770F0F00FF0B0777770FFFFFF0BFB077770FFFFF0BFBFB
        07770F00F08FBFBFB0770FFFFFF8FBFBFB070FF00F0F8FBFBFB00FFFFFFFF8FB
        FBF80F00F0FF0F8FBF870FFFFFFFFF08F8770000000000078777}
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = ClearDisplayBitBtnClick
    end
    object LookupButton: TButton
      Left = 415
      Top = 81
      Width = 75
      Height = 21
      Caption = '&Start Lookup'
      TabOrder = 5
      OnClick = LookupButtonClick
    end
    object TcpRadioButton: TRadioButton
      Left = 322
      Top = 8
      Width = 49
      Height = 17
      Caption = 'TCP'
      TabOrder = 3
    end
    object UdpRadioButton: TRadioButton
      Left = 323
      Top = 28
      Width = 46
      Height = 17
      Caption = 'UDP'
      Checked = True
      TabOrder = 4
      TabStop = True
    end
    object DnsEdit: TComboBox
      Left = 112
      Top = 7
      Width = 190
      Height = 21
      TabOrder = 2
      Text = '8.8.8.8'
      Items.Strings = (
        '')
    end
    object NameEdit: TComboBox
      Left = 504
      Top = 10
      Width = 201
      Height = 21
      TabOrder = 0
      Text = 'pool.ntp.org'
      Items.Strings = (
        'pool.ntp.org'
        'www.google.com'
        'google.com'
        'smtp.google.com'
        'www.overbyte.eu'
        'overbyte.eu'
        'wiki.overbyte.eu'
        'magsys.co.uk'
        'www.magsys.co.uk'
        'mail.magsys.co.uk'
        'embarcadero.com'
        'www.embarcadero.com'
        'str'#248'm.no'
        #233'x'#224'mpl'#234'.ftptest.co.uk'
        'scr'#250'd'#250'.ftptest.co.uk'
        'pr'#248've.ftptest.co.uk'
        ''
        '')
    end
    object DnsQueryType: TComboBox
      Left = 504
      Top = 40
      Width = 201
      Height = 21
      Style = csDropDownList
      TabOrder = 1
    end
    object AllButton: TButton
      Left = 500
      Top = 80
      Width = 75
      Height = 21
      Caption = '&Lookup All '
      TabOrder = 6
      OnClick = AllButtonClick
    end
    object AbortButton: TButton
      Left = 590
      Top = 80
      Width = 75
      Height = 21
      Caption = '&Abort '
      TabOrder = 8
      OnClick = AbortButtonClick
    end
    object RequestSync: TCheckBox
      Left = 313
      Top = 81
      Width = 90
      Height = 17
      Caption = 'Sync Request'
      TabOrder = 9
    end
    object ServerStratagy: TRadioGroup
      Left = 8
      Top = 38
      Width = 117
      Height = 66
      Caption = 'DNS Server Stratagy'
      ItemIndex = 0
      Items.Strings = (
        'Single Server'
        'Multiple Servers'
        'Public Servers')
      TabOrder = 10
    end
    object ServerList: TMemo
      Left = 136
      Top = 54
      Width = 168
      Height = 55
      Lines.Strings = (
        '1.1.1.1')
      ScrollBars = ssVertical
      TabOrder = 11
    end
  end
  object DnsQuery1: TDnsQuery
    Port = '53'
    Proto = 'udp'
    MultiThreaded = False
    Timeout = 5
    ServerStrat = SrvStratOne
    ServerCur = 0
    ServerMax = 4
    OnRequestDone = DnsQuery1RequestDone
    OnLogEvent = DnsQuery1LogEvent
    Left = 253
    Top = 163
  end
  object Timer1: TTimer
    Enabled = False
    Interval = 30000
    OnTimer = Timer1Timer
    Left = 342
    Top = 164
  end
end
