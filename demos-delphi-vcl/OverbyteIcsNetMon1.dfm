object MonForm: TMonForm
  Left = 44
  Top = 96
  Caption = 
    'ICS Internet Packet Monitoring Components - Display Raw Packets ' +
    '- https://www.overbyte.be - 7 Aug 2024'
  ClientHeight = 736
  ClientWidth = 1028
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
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FFFFFFFFFFFFFFFFFFFFFFFFFFFFF0F
    F0FFFFFFFFFFFFFFFFFFFFFFFFFFF0888F00FFFFFFFFFFFFFFFFF0FFFFFF0888
    88FF0FFFFFFFFFFFFFFF000FFFFF08888888F00FFFFFFFFFFFF00000FFFFF088
    88888FF0FFFFFFFFFF00000FFFFFFF088888888F00FFFFFFF00000FFFFFFFFF0
    88888888FF0FFFFF00000FFFFFFFFFFF0888888888F0FFF00000FFFFFFFFFFFF
    F0888888888F0000000FFFFFFFFFFFFFFF0888888888F00000FFFFFFFFFFFFFF
    FFF08888888800000FFFFFFFFFFFFFFFFFFF088888800000F0FFFFFFFFFFFFFF
    FFFFF088886000088F00FFFFFFFFFFFFFFFFFF0886660088888F0FFFFFFFFFFF
    FFFFFFF0666688888888F0FFFFFFFFFFFFFFFFF6666888888888811FFFFFFFFF
    FFFFFF666688888888111111FFFFFFFFFFFFFF666F088888811111111FFFFFFF
    FFFFF666FFF088881111111111FFFFFFFFFF666FFFFF08811111111F11FFFF7F
    FFF666FFFFFFF081111111FF11FFF777F7666FFFFFFFFF0111111FFF11FF7777
    7776FFFFFFFFFFF11111FFF111FFF7777777FFFFFFFFFFF1111FFF1111FFFF77
    777FFFFFFFFFFFF1111FF1111FFFFFF7777FFFFFFFFFFFFF11111111FFFFFFFF
    7777FFFFFFFFFFFFF111111FFFFFFFFFFF77777FFFFFFFFFFFF111FFFFFFFFFF
    FFFFFFFFFFFFFFFFFFF11FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 1028
    Height = 736
    ActivePage = TabSettings
    Align = alClient
    TabOrder = 0
    object TabSettings: TTabSheet
      Caption = 'Monitor Settings'
      object GroupBox2: TGroupBox
        Left = 5
        Top = 5
        Width = 415
        Height = 452
        Caption = 'Monitoring Adaptor or IP Address'
        TabOrder = 0
        object Label3: TLabel
          Left = 10
          Top = 198
          Width = 217
          Height = 13
          Caption = 'IP Address to Monitor (Socket Monitoring)'
        end
        object Label4: TLabel
          Left = 10
          Top = 45
          Width = 168
          Height = 13
          Caption = 'Adapter to Monitor (NPCap only)'
        end
        object LabelAdmin: TLabel
          Left = 10
          Top = 357
          Width = 310
          Height = 13
          Caption = 'Program does not have Administrator Rights, no socket monitoring'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object Label2: TLabel
          Left = 9
          Top = 385
          Width = 371
          Height = 39
          Caption = 
            'Note: Socket Monitoring may not capture from some network adapto' +
            'rs, or may be one way ony.'#13#10'Socket Monitoring  IPv6 traffic deco' +
            'ding is partial.  '
          WordWrap = True
        end
        object MonIpList: TListBox
          Left = 10
          Top = 218
          Width = 254
          Height = 126
          ItemHeight = 13
          TabOrder = 0
        end
        object UseWinNCap: TCheckBox
          Left = 10
          Top = 22
          Width = 188
          Height = 17
          Caption = 'Use NPCap Driver'
          Enabled = False
          TabOrder = 1
          OnClick = UseWinNCapClick
        end
        object MonAdptList: TListBox
          Left = 10
          Top = 71
          Width = 390
          Height = 121
          ItemHeight = 13
          TabOrder = 2
          OnClick = MonAdptListClick
        end
        object ShowFullData: TCheckBox
          Left = 275
          Top = 222
          Width = 101
          Height = 17
          Caption = 'Display Full Data'
          TabOrder = 3
        end
        object ShowIgnoreData: TCheckBox
          Left = 275
          Top = 245
          Width = 81
          Height = 17
          Caption = 'Ignore Data'
          TabOrder = 4
        end
        object MonPromiscuous: TCheckBox
          Left = 189
          Top = 45
          Width = 81
          Height = 17
          Caption = 'Promiscuous (Sockets)'
          Checked = True
          Enabled = False
          State = cbChecked
          TabOrder = 5
        end
        object doExit: TButton
          Left = 273
          Top = 18
          Width = 75
          Height = 25
          Caption = 'Exit'
          TabOrder = 6
          OnClick = doExitClick
        end
      end
      object GroupBox3: TGroupBox
        Left = 668
        Top = 4
        Width = 300
        Height = 452
        Caption = 'Filter by IP Address'
        TabOrder = 1
        object IpAddrList: TMemo
          Left = 14
          Top = 108
          Width = 226
          Height = 333
          Lines.Strings = (
            '192.168.1.4'
            '192.168.1.255')
          ScrollBars = ssVertical
          TabOrder = 0
          OnExit = IpAddrListExit
        end
        object FilterIpAddr: TRadioGroup
          Left = 10
          Top = 27
          Width = 131
          Height = 75
          Caption = 'Filter Method'
          ItemIndex = 0
          Items.Strings = (
            'No Filter'
            'Ignore Selections'
            'Only Selections')
          TabOrder = 1
          OnClick = FilterIpAddrClick
        end
        object IpAddrLocal: TCheckBox
          Left = 161
          Top = 35
          Width = 120
          Height = 17
          Caption = 'Local Addresses'
          TabOrder = 2
          OnClick = IpAddrLocalClick
        end
      end
      object GroupBox1: TGroupBox
        Left = 426
        Top = 5
        Width = 236
        Height = 452
        Caption = 'Filter by Protocol and Service'
        TabOrder = 2
        object LaberlPorts: TLabel
          Left = 154
          Top = 22
          Width = 55
          Height = 13
          Caption = 'Filter Ports'
        end
        object FilterProtocol: TRadioGroup
          Left = 5
          Top = 22
          Width = 131
          Height = 75
          Caption = 'Filter Method'
          ItemIndex = 0
          Items.Strings = (
            'No Filter'
            'Ignore Selections'
            'Only Selections')
          TabOrder = 0
          OnClick = FilterProtocolClick
        end
        object ProtocolIPv4: TCheckBox
          Left = 5
          Top = 105
          Width = 120
          Height = 17
          Caption = 'IPv4 Traffic'
          TabOrder = 1
          OnClick = ProtocolIPv4Click
        end
        object ProtocolIPv6: TCheckBox
          Left = 5
          Top = 125
          Width = 120
          Height = 17
          Caption = 'IPv6 Traffic'
          TabOrder = 2
          OnClick = ProtocolIPv6Click
        end
        object ProtocolUdp: TCheckBox
          Left = 5
          Top = 145
          Width = 120
          Height = 17
          Caption = 'UDP Traffic'
          TabOrder = 3
          OnClick = ProtocolUdpClick
        end
        object ProtocolARP: TCheckBox
          Left = 5
          Top = 185
          Width = 137
          Height = 17
          Caption = 'Addr Resolution Traffic'
          TabOrder = 4
          OnClick = ProtocolARPClick
        end
        object ProtocolICMP: TCheckBox
          Left = 5
          Top = 365
          Width = 120
          Height = 17
          Caption = 'ICMP Traffic'
          TabOrder = 5
          OnClick = ProtocolICMPClick
        end
        object ProtocolDns: TCheckBox
          Left = 5
          Top = 325
          Width = 138
          Height = 17
          Caption = 'Name Resolution Traffic'
          TabOrder = 6
          OnClick = ProtocolDnsClick
        end
        object ProtocolNonIp: TCheckBox
          Left = 5
          Top = 225
          Width = 120
          Height = 17
          Caption = 'Non-IP Traffic'
          TabOrder = 7
          OnClick = ProtocolNonIpClick
        end
        object ProtocolSyslog: TCheckBox
          Left = 5
          Top = 245
          Width = 120
          Height = 17
          Caption = 'Syslog Traffic'
          TabOrder = 8
          OnClick = ProtocolSyslogClick
        end
        object ProtocolHttp: TCheckBox
          Left = 5
          Top = 285
          Width = 120
          Height = 17
          Caption = 'HTTP Traffic'
          TabOrder = 9
          OnClick = ProtocolHttpClick
        end
        object ProtocolSNMP: TCheckBox
          Left = 5
          Top = 205
          Width = 120
          Height = 17
          Caption = 'SNMP Traffic'
          TabOrder = 10
          OnClick = ProtocolSNMPClick
        end
        object ProtocolUPnP: TCheckBox
          Left = 5
          Top = 305
          Width = 120
          Height = 17
          Caption = 'UPnP Traffic'
          TabOrder = 11
          OnClick = ProtocolUPnPClick
        end
        object ProtocolIRC: TCheckBox
          Left = 5
          Top = 265
          Width = 120
          Height = 17
          Caption = 'IRC Traffic'
          TabOrder = 12
          OnClick = ProtocolIRCClick
        end
        object PortsList: TMemo
          Left = 154
          Top = 41
          Width = 73
          Height = 214
          ScrollBars = ssVertical
          TabOrder = 13
          OnChange = PortsListChange
        end
        object ProtocolBroadcast: TCheckBox
          Left = 5
          Top = 345
          Width = 138
          Height = 17
          Caption = 'Broadcast Traffic'
          TabOrder = 14
          OnClick = ProtocolBroadcastClick
        end
        object ProtocolTCP: TCheckBox
          Left = 5
          Top = 165
          Width = 86
          Height = 17
          Caption = 'TCP Traffic'
          TabOrder = 15
          OnClick = ProtocolTCPClick
        end
      end
      object GroupBox4: TGroupBox
        Left = 5
        Top = 462
        Width = 238
        Height = 105
        Caption = 'DNS Cache'
        TabOrder = 3
        object DnsCacheLog: TCheckBox
          Left = 15
          Top = 21
          Width = 130
          Height = 17
          Caption = 'Log Activity'
          TabOrder = 0
        end
        object doDnsCacheClear: TButton
          Left = 27
          Top = 55
          Width = 75
          Height = 25
          Caption = 'Clear Cache'
          TabOrder = 1
          OnClick = doDnsCacheClearClick
        end
        object doDnsCacheList: TButton
          Left = 122
          Top = 55
          Width = 75
          Height = 25
          Caption = 'List Cache'
          TabOrder = 2
          OnClick = doDnsCacheListClick
        end
      end
    end
    object TabPacketsLog: TTabSheet
      Caption = 'Packets (Log)'
      ImageIndex = 1
      object MonLogWin: TMemo
        Left = 0
        Top = 41
        Width = 1016
        Height = 651
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -10
        Font.Name = 'Lucida Console'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 1016
        Height = 41
        Align = alTop
        TabOrder = 1
        object LabelTrafficLog: TLabel
          Left = 305
          Top = 5
          Width = 33
          Height = 13
          Caption = 'Traffic:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object doPackLogStart: TButton
          Left = 5
          Top = 5
          Width = 91
          Height = 25
          Caption = 'Start Monitor'
          TabOrder = 0
          OnClick = doPackLogStartClick
        end
        object doClearLog: TButton
          Left = 209
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Clear'
          TabOrder = 1
          OnClick = doClearLogClick
        end
        object doPackLogStop: TButton
          Left = 107
          Top = 5
          Width = 91
          Height = 25
          Caption = 'Stop Monitor'
          TabOrder = 2
          OnClick = doPackLogStopClick
        end
      end
    end
    object TabPacketsGrid: TTabSheet
      Caption = 'Packets (Grid)'
      ImageIndex = 2
      object MonGridWin: TListView
        Left = 0
        Top = 41
        Width = 1016
        Height = 651
        Align = alClient
        Columns = <
          item
            Caption = 'Time'
            Width = 80
          end
          item
            Caption = 'Prot'
          end
          item
            Caption = 'PLen'
            Width = 40
          end
          item
            Caption = 'Source IP and Host Name'
            Width = 200
          end
          item
            Caption = 'Port'
          end
          item
            Caption = 'Destination IP and Host Name'
            Width = 200
          end
          item
            Caption = 'Port'
          end
          item
            Caption = 'Service'
            Width = 80
          end
          item
            Caption = 'DLen'
            Width = 40
          end
          item
            Caption = 'Packet Data'
            Width = 2000
          end
          item
          end>
        GridLines = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnData = MonGridWinData
        OnSelectItem = MonGridWinSelectItem
      end
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 1016
        Height = 41
        Align = alTop
        TabOrder = 1
        object LabelTrafficGrid: TLabel
          Left = 500
          Top = 5
          Width = 33
          Height = 13
          Caption = 'Traffic:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object doPackGridStart: TButton
          Left = 5
          Top = 4
          Width = 91
          Height = 25
          Caption = 'Start Monitor'
          TabOrder = 0
          OnClick = doPackGridStartClick
        end
        object doPackGridStop: TButton
          Left = 105
          Top = 5
          Width = 91
          Height = 25
          Caption = 'Stop Monitor'
          TabOrder = 1
          OnClick = doPackGridStopClick
        end
        object doPackGridClear: TButton
          Left = 209
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Clear'
          TabOrder = 2
          OnClick = doPackGridClearClick
        end
        object GridAutoScroll: TCheckBox
          Left = 304
          Top = 7
          Width = 97
          Height = 17
          Caption = 'Auto Scroll'
          TabOrder = 3
        end
      end
    end
    object TabTraffic: TTabSheet
      Caption = 'Traffic Totals Only'
      ImageIndex = 3
      object MonTrafficWin: TListView
        Left = 0
        Top = 41
        Width = 1016
        Height = 651
        Align = alClient
        Columns = <
          item
            Caption = 'Source IP/Host'
            Width = 200
          end
          item
            Caption = 'Target IP/Host'
            Width = 200
          end
          item
            Caption = 'Service'
            Width = 100
          end
          item
            Caption = 'Sent '
            Width = 70
          end
          item
            Caption = 'Packets'
            Width = 70
          end
          item
            Caption = 'Received'
            Width = 70
          end
          item
            Caption = 'Packets'
            Width = 70
          end
          item
            Caption = 'First Time'
            Width = 100
          end
          item
            Caption = 'Last Time'
            Width = 100
          end
          item
            Width = 100
          end>
        GridLines = True
        OwnerData = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
        OnData = MonTrafficWinData
        OnSelectItem = MonGridWinSelectItem
      end
      object Panel1: TPanel
        Left = 0
        Top = 0
        Width = 1016
        Height = 41
        Align = alTop
        TabOrder = 1
        object LabelTrafficTotals: TLabel
          Left = 606
          Top = 5
          Width = 33
          Height = 13
          Caption = 'Traffic:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
        end
        object doTrafficStart: TButton
          Left = 5
          Top = 5
          Width = 91
          Height = 25
          Caption = 'Start Monitor'
          TabOrder = 0
          OnClick = doTrafficStartClick
        end
        object doTrafficStop: TButton
          Left = 106
          Top = 5
          Width = 91
          Height = 25
          Caption = 'Stop Monitor'
          TabOrder = 1
          OnClick = doTrafficStopClick
        end
        object doTrafficClear: TButton
          Left = 209
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Clear'
          TabOrder = 2
          OnClick = doTrafficClearClick
        end
        object doTrafficLogTotals: TButton
          Left = 296
          Top = 5
          Width = 151
          Height = 25
          Caption = 'Log Totals (in Log window)'
          TabOrder = 3
          OnClick = doTrafficLogTotalsClick
        end
        object TrafficHostNames: TCheckBox
          Left = 460
          Top = 5
          Width = 125
          Height = 17
          Caption = 'Show Host Names'
          TabOrder = 4
        end
      end
    end
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 124
    Top = 126
  end
  object IcsDomainNameCache1: TIcsDomainNameCache
    DNMethod = MethodWinsock
    DnsServerStrat = SrvStratOne
    DefTTL = 3600
    MaxLookups = 5
    QTimeout = 5
    AddLocalhost = False
    DBLANlookup = LanLookDef
    OnDNLogEvent = IcsDomainNameCache1DNLogEvent
    Left = 232
    Top = 131
  end
end
