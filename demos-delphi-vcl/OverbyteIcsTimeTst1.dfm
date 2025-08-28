object TimeDemoForm: TTimeDemoForm
  Left = 220
  Top = 122
  Caption = 
    'ICS Simple Network Time Protocol Client and Server Demo - https:' +
    '//www.overbyte.be - 7 Aug 2024'
  ClientHeight = 517
  ClientWidth = 886
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000008888
    88888888888999998888000000008888888888888998FFF89988000000008888
    8888888899FFFCFFF998000000008888888888FF9FFFFFFFCF98000000008888
    88888FFFFFF0FFFFFF8900000000888000000FFFFFFF0FFFFFF9000000008877
    777777FFFCFFF90000F900000000878888888889FFFFFFFFFFF9000000007F87
    088888798FFFFFFFFF89000000007F87088888709FCFFFFFCF98000000007F87
    0888887099FFFCFFF998000000007F87088888708998FCF89988000000007F87
    78888877887999998888000000007F8888700888887088888888000000007F88
    88700888887088888888000000007F88887778888870888888880000000087F8
    8888888887088888888800000000887FFFFFFFFFF08888888888000000008887
    7777777778888888888800000000000000000000000000000000000000000888
    8077777777777777777000000000080080777777777777777770000000000888
    807777777777777777700000000000000000000000000000000000000000FFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
    00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
    00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
    00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF}
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 151
    Width = 886
    Height = 366
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 886
    Height = 151
    Align = alTop
    TabOrder = 0
    object doClose: TButton
      Left = 793
      Top = 13
      Width = 75
      Height = 21
      Cancel = True
      Caption = 'C&lose'
      TabOrder = 0
      OnClick = doCloseClick
    end
    object GroupBox1: TGroupBox
      Left = 10
      Top = 5
      Width = 301
      Height = 140
      Caption = 'Time Server'
      TabOrder = 1
      object Label1: TLabel
        Left = 5
        Top = 20
        Width = 87
        Height = 13
        Caption = 'Server IP Address'
      end
      object ServerIP: TComboBox
        Left = 105
        Top = 15
        Width = 171
        Height = 21
        TabOrder = 0
        Text = '127.0.0.1'
      end
      object ServerProtoSntp: TCheckBox
        Left = 10
        Top = 40
        Width = 181
        Height = 17
        Caption = 'Simple Network Time Protocol'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object ServerProtoTCP: TCheckBox
        Left = 10
        Top = 60
        Width = 97
        Height = 17
        Caption = 'TIME TCP'
        TabOrder = 2
      end
      object ServerProtoUdp: TCheckBox
        Left = 10
        Top = 80
        Width = 97
        Height = 17
        Caption = 'TIME UDP'
        TabOrder = 3
      end
      object doServerStart: TButton
        Left = 125
        Top = 65
        Width = 75
        Height = 25
        Caption = 'Start'
        TabOrder = 4
        OnClick = doServerStartClick
      end
      object doServerStop: TButton
        Left = 211
        Top = 65
        Width = 75
        Height = 25
        Caption = 'Sop'
        TabOrder = 5
        OnClick = doServerStopClick
      end
    end
    object GroupBox2: TGroupBox
      Left = 320
      Top = 5
      Width = 415
      Height = 140
      Caption = 'Time Client'
      TabOrder = 2
      object Label2: TLabel
        Left = 10
        Top = 21
        Width = 58
        Height = 13
        Caption = 'Time Server'
      end
      object LabelCorrection: TLabel
        Left = 15
        Top = 112
        Width = 95
        Height = 13
        Caption = 'Correction Needed?'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        ParentFont = False
      end
      object ClientTimeServer: TComboBox
        Left = 90
        Top = 15
        Width = 210
        Height = 21
        TabOrder = 0
        Text = 'time.google.com'
        Items.Strings = (
          'time.google.com'
          'time1.google.com'
          'time2.google.com'
          'time.cloudflare.com'
          'time.facebook.com'
          'time1.facebook.com'
          'time2.facebook.com'
          'time.apple.com'
          'time.euro.apple.com'
          'pool.ntp.org'
          '0.pool.ntp.org'
          '1.pool.ntp.org'
          '2.pool.ntp.org'
          '3.pool.ntp.org'
          'europe.pool.ntp.org'
          'uk.pool.ntp.org'
          'north-america.pool.ntp.org'
          'south-america.pool.ntp.org'
          'africa.pool.ntp.org'
          'asia.pool.ntp.org'
          'oceania.pool.ntp.org'
          '127.0.0.1')
      end
      object ClientProtocol: TRadioGroup
        Left = 10
        Top = 40
        Width = 91
        Height = 66
        Caption = 'Protocol'
        ItemIndex = 0
        Items.Strings = (
          'SNTP'
          'TIME TCP'
          'TIME UDP')
        TabOrder = 1
      end
      object doClientStart: TButton
        Left = 315
        Top = 15
        Width = 74
        Height = 25
        Caption = 'Get Time'
        TabOrder = 2
        OnClick = doClientStartClick
      end
      object doCorrect: TButton
        Left = 315
        Top = 46
        Width = 74
        Height = 25
        Caption = 'Correct Time'
        Enabled = False
        TabOrder = 3
        OnClick = doCorrectClick
      end
      object SockFamily: TRadioGroup
        Left = 123
        Top = 40
        Width = 90
        Height = 92
        Caption = 'Socket Family'
        ItemIndex = 1
        Items.Strings = (
          'Any'
          'Prefer IPv4'
          'Prefer IPv6'
          'Only IPv4 '
          'Only IPv6')
        TabOrder = 4
      end
    end
    object doClientAbort: TButton
      Left = 794
      Top = 51
      Width = 75
      Height = 25
      Caption = 'Abort'
      TabOrder = 3
      OnClick = doClientAbortClick
    end
    object doTestConv: TButton
      Left = 793
      Top = 99
      Width = 75
      Height = 25
      Caption = 'Test NTP Conv'
      TabOrder = 4
      OnClick = doTestConvClick
    end
  end
  object IcsTimeServer1: TIcsTimeServer
    Addr = '0.0.0.0'
    TimeProtocol = [tpSNTP, tpTCP, tpUDP]
    OnStop = IcsTimeServer1Stop
    OnStart = IcsTimeServer1Start
    OnQueryDone = IcsTimeServer1QueryDone
    OnQuery = IcsTimeServer1Query
    Left = 235
    Top = 220
  end
  object IcsTimeClient1: TIcsTimeClient
    ServerAddr = 'time.google.com'
    SocketFamily = sfIPv4
    TimeProtocol = tpSNTP
    TimeoutSecs = 5
    OnTime = IcsTimeClient1Time
    OnTimeInfo = IcsTimeClient1TimeInfo
    Left = 285
    Top = 220
  end
end
