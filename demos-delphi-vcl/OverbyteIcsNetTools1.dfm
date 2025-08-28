object ToolsForm: TToolsForm
  Left = 0
  Top = 0
  Caption = 'ICS Network Tools Demo - https://www.overbyte.be - 7 Aug 2024'
  ClientHeight = 839
  ClientWidth = 796
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFF0
    00000000000000000000000FFFFFFF000000000000000000000000000FFF7999
    99999999999999999977700000FF7999999999999999999999799770000F799B
    99BBB9BBB9BB9BBB99799997700F799B99B9B9B9B9B9999B99799999700F799B
    99B9B9B9B9B99BBB99799999700F799B99B9B9B9B9B99B9999799999700F79BB
    B9BBB9BBB9B99BBB99790099700F7999999999999999999999799099700F7999
    999999999999999999799009700F7999999999009999999999799999700F7999
    999990000999999999799999700F7777777777007777777777799999700FF711
    111111111111111111779999700FFF71111111111111111111177999700FFFF7
    71111111111111111111779970FFFFFF7711111111111111111117777FFFFFFF
    FFF799999999999999999977FFFFFFFFFF799999999999999999997FFFFFFFFF
    F7799999999999999999977FFFFFFFFFF799999999999999999977FFFFFFFFFF
    799999999999999999997FFFFFFFFFF7999999909999999999977FFFFFFFFF79
    99999990999999999997FFFFFFFFF79999999900099999999977FFFFFFFFF777
    7777777777777777777FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
    FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 15
  object Label8: TLabel
    Left = 21
    Top = 60
    Width = 139
    Height = 15
    Caption = 'Scan Starting IPv4 Address'
  end
  object Label9: TLabel
    Left = 26
    Top = 65
    Width = 139
    Height = 15
    Caption = 'Scan Starting IPv4 Address'
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 796
    Height = 49
    Align = alTop
    TabOrder = 0
    object Label2: TLabel
      Left = 83
      Top = 15
      Width = 53
      Height = 15
      Caption = 'Local IPv4'
    end
    object Label4: TLabel
      Left = 299
      Top = 15
      Width = 53
      Height = 15
      Caption = 'Local IPv6'
    end
    object Label11: TLabel
      Left = 587
      Top = 15
      Width = 35
      Height = 15
      Caption = 'Family'
    end
    object doExit: TButton
      Left = 13
      Top = 10
      Width = 51
      Height = 25
      Caption = 'Exit'
      TabOrder = 0
      OnClick = doExitClick
    end
    object LocalIPv4: TComboBox
      Left = 151
      Top = 10
      Width = 124
      Height = 23
      TabOrder = 1
    end
    object LocalIPv6: TComboBox
      Left = 361
      Top = 10
      Width = 212
      Height = 23
      TabOrder = 2
    end
    object IpFamily: TComboBox
      Left = 642
      Top = 10
      Width = 134
      Height = 23
      Style = csDropDownList
      TabOrder = 3
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 49
    Width = 796
    Height = 790
    ActivePage = TabConnections
    Align = alClient
    MultiLine = True
    TabOrder = 1
    object TabConnections: TTabSheet
      Caption = 'IP Connections'
      object BoxIpConnections: TGroupBox
        Left = 0
        Top = 0
        Width = 788
        Height = 90
        Align = alTop
        Caption = 'IP Connections'
        TabOrder = 0
        object LabelInterval: TLabel
          Left = 180
          Top = 60
          Width = 113
          Height = 15
          Caption = 'Update Interval (secs)'
        end
        object doLogIpConns: TButton
          Left = 472
          Top = 21
          Width = 120
          Height = 25
          Caption = 'Log Connections'
          TabOrder = 0
          OnClick = doLogIpConnsClick
        end
        object IpConnType: TRadioGroup
          Left = 620
          Top = 10
          Width = 111
          Height = 72
          Caption = 'Type'
          ItemIndex = 0
          Items.Strings = (
            'TCP and UDP'
            'TCP Only'
            'UDP Only')
          TabOrder = 1
        end
        object doIpConnStop: TButton
          Left = 304
          Top = 19
          Width = 120
          Height = 25
          Caption = 'Stop Updating'
          Enabled = False
          TabOrder = 2
          OnClick = doIpConnStopClick
        end
        object doIpConnUpdate: TButton
          Left = 10
          Top = 20
          Width = 120
          Height = 25
          Caption = 'Update Once'
          TabOrder = 3
          OnClick = doIpConnUpdateClick
        end
        object doIpConnStart: TButton
          Left = 177
          Top = 20
          Width = 120
          Height = 25
          Caption = 'Start Updating'
          TabOrder = 4
          OnClick = doIpConnStartClick
        end
        object IpConnsSecs: TEdit
          Left = 310
          Top = 55
          Width = 65
          Height = 23
          TabOrder = 5
          Text = '10'
        end
      end
      object IpConnsListV: TListView
        Left = 0
        Top = 90
        Width = 788
        Height = 650
        Align = alClient
        Columns = <
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end>
        OwnerData = True
        ReadOnly = True
        TabOrder = 1
        ViewStyle = vsReport
        OnData = IpConnsListVData
      end
    end
    object TabNeighbDev: TTabSheet
      Caption = 'LAN Devices'
      ImageIndex = 2
      object BoxNeigbDevs: TGroupBox
        Left = 0
        Top = 0
        Width = 788
        Height = 115
        Align = alTop
        Caption = 'Locate Neigbourhood LAN Devices and IPs'
        TabOrder = 0
        object Label3: TLabel
          Left = 516
          Top = 55
          Width = 98
          Height = 15
          Caption = 'Total IPs (max 253)'
        end
        object Label5: TLabel
          Left = 10
          Top = 55
          Width = 139
          Height = 15
          Caption = 'Scan Starting IPv4 Address'
        end
        object Label6: TLabel
          Left = 10
          Top = 25
          Width = 120
          Height = 15
          Caption = 'Check PC Cache (secs)'
        end
        object Label7: TLabel
          Left = 224
          Top = 25
          Width = 121
          Height = 15
          Caption = 'Scan for New IPs (secs)'
        end
        object Label10: TLabel
          Left = 304
          Top = 55
          Width = 67
          Height = 15
          Caption = 'IPv6 Address'
        end
        object doNeigbDevStart: TButton
          Left = 10
          Top = 80
          Width = 148
          Height = 25
          Caption = 'Start Locating Devices'
          TabOrder = 0
          OnClick = doNeigbDevStartClick
        end
        object NeigbDevIPv4: TEdit
          Left = 170
          Top = 50
          Width = 116
          Height = 23
          TabOrder = 1
          Text = '192.168.1.1'
        end
        object NeigbDevIPv6: TEdit
          Left = 388
          Top = 50
          Width = 116
          Height = 23
          TabOrder = 2
        end
        object doNeigbDevStop: TButton
          Left = 179
          Top = 80
          Width = 148
          Height = 25
          Caption = 'Stop Locating Devices'
          Enabled = False
          TabOrder = 3
          OnClick = doNeigbDevStopClick
        end
        object doDeviceLogMac: TButton
          Left = 391
          Top = 80
          Width = 128
          Height = 25
          Caption = 'Log Devices by Mac'
          TabOrder = 4
          OnClick = doDeviceLogMacClick
        end
        object doDeviceLogIp: TButton
          Left = 539
          Top = 80
          Width = 107
          Height = 25
          Caption = 'Log Devices by IP'
          TabOrder = 5
          OnClick = doDeviceLogIpClick
        end
        object doDeviceEmptyCache: TButton
          Left = 433
          Top = 17
          Width = 124
          Height = 25
          Caption = 'Device Empty Cache'
          TabOrder = 6
          OnClick = doDeviceEmptyCacheClick
        end
        object doDevicesClear: TButton
          Left = 573
          Top = 17
          Width = 124
          Height = 25
          Caption = 'Devices Clear'
          TabOrder = 7
          OnClick = doDevicesClearClick
        end
        object NeigbDevCache: TEdit
          Left = 142
          Top = 20
          Width = 65
          Height = 23
          TabOrder = 8
          Text = '60'
        end
        object NeigbDevScan: TEdit
          Left = 355
          Top = 20
          Width = 65
          Height = 23
          TabOrder = 9
          Text = '120'
        end
        object NeigbDevTot: TEdit
          Left = 633
          Top = 50
          Width = 65
          Height = 23
          TabOrder = 10
          Text = '16'
        end
      end
      object NeigbDevListV: TListView
        Left = 0
        Top = 115
        Width = 788
        Height = 625
        Align = alClient
        Columns = <
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end>
        OwnerData = True
        ReadOnly = True
        TabOrder = 1
        ViewStyle = vsReport
        OnData = NeigbDevListVData
      end
    end
    object TabAdaptors: TTabSheet
      Caption = 'Network Adaptors'
      ImageIndex = 1
      object BoxNetAdapt: TGroupBox
        Left = 0
        Top = 0
        Width = 788
        Height = 56
        Align = alTop
        Caption = 'Network Adatptors and Params'
        TabOrder = 0
        object doLogAdaptors: TButton
          Left = 522
          Top = 20
          Width = 109
          Height = 25
          Caption = 'Log Adaptor List'
          TabOrder = 0
          OnClick = doLogAdaptorsClick
        end
        object doLogNetParams: TButton
          Left = 651
          Top = 20
          Width = 124
          Height = 25
          Caption = 'Log Network Params'
          TabOrder = 1
          OnClick = doLogNetParamsClick
        end
        object doUpdAdaptors: TButton
          Left = 10
          Top = 20
          Width = 109
          Height = 25
          Caption = 'Update'
          TabOrder = 2
          OnClick = doUpdAdaptorsClick
        end
      end
      object AdaptorListV: TListView
        Left = 0
        Top = 56
        Width = 788
        Height = 484
        Align = alClient
        Columns = <
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end>
        ReadOnly = True
        TabOrder = 1
        ViewStyle = vsReport
      end
      object NetParamsListV: TListView
        Left = 0
        Top = 540
        Width = 788
        Height = 200
        Align = alBottom
        Columns = <
          item
            Caption = 'Title'
            Width = 150
          end
          item
            Caption = 'Value'
            Width = 500
          end>
        ReadOnly = True
        TabOrder = 2
        ViewStyle = vsReport
      end
    end
    object TabInterfaces: TTabSheet
      Caption = 'Network Interfaces'
      ImageIndex = 3
      object BoxNetInterfaces: TGroupBox
        Left = 0
        Top = 0
        Width = 788
        Height = 56
        Align = alTop
        Caption = 'Network Interfaces'
        TabOrder = 0
        object doLogInterfaces: TButton
          Left = 606
          Top = 20
          Width = 110
          Height = 25
          Caption = 'Log Interface List'
          TabOrder = 0
          OnClick = doLogInterfacesClick
        end
        object doUpdInterfaces: TButton
          Left = 10
          Top = 19
          Width = 109
          Height = 25
          Caption = 'Update'
          TabOrder = 1
          OnClick = doUpdInterfacesClick
        end
      end
      object InterfacesListV: TListView
        Left = 0
        Top = 56
        Width = 788
        Height = 684
        Align = alClient
        Columns = <
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end>
        ReadOnly = True
        TabOrder = 1
        ViewStyle = vsReport
      end
    end
    object TabIpAddr: TTabSheet
      Caption = 'IP Addresses'
      ImageIndex = 4
      object BoxIpAddresses: TGroupBox
        Left = 0
        Top = 0
        Width = 788
        Height = 56
        Align = alTop
        Caption = 'IP Addresses'
        TabOrder = 0
        object doLogIpAddr: TButton
          Left = 633
          Top = 20
          Width = 121
          Height = 25
          Caption = 'Log IP Addresses'
          TabOrder = 0
          OnClick = doLogIpAddrClick
        end
        object doUpdIpAddr: TButton
          Left = 10
          Top = 20
          Width = 109
          Height = 25
          Caption = 'Update'
          TabOrder = 1
          OnClick = doUpdIpAddrClick
        end
        object MonIpAddrChanges: TCheckBox
          Left = 149
          Top = 25
          Width = 193
          Height = 17
          Caption = 'Monitor IP Address Changes'
          Checked = True
          State = cbChecked
          TabOrder = 2
          OnClick = MonIpAddrChangesClick
        end
      end
      object IpAddressListV: TListView
        Left = 0
        Top = 56
        Width = 788
        Height = 684
        Align = alClient
        Columns = <
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end>
        ReadOnly = True
        TabOrder = 1
        ViewStyle = vsReport
      end
    end
    object TabIpRouting: TTabSheet
      Caption = 'IP Routing and Paths'
      ImageIndex = 5
      object BoxIpRouting: TGroupBox
        Left = 0
        Top = 0
        Width = 788
        Height = 61
        Align = alTop
        Caption = 'IP Routing and Current Paths'
        TabOrder = 0
        object Label12: TLabel
          Left = 259
          Top = 7
          Width = 274
          Height = 45
          Caption = 
            'Note: Routing is essentially fixed, Paths are very dynamic chang' +
            'ing every second as new connectins are opened and closed.'
          WordWrap = True
        end
        object doLogIpRouting: TButton
          Left = 557
          Top = 20
          Width = 100
          Height = 25
          Caption = 'Log IP Routing'
          TabOrder = 0
          OnClick = doLogIpRoutingClick
        end
        object doLogIpPath: TButton
          Left = 674
          Top = 20
          Width = 94
          Height = 25
          Caption = 'Log IP Paths'
          TabOrder = 1
          OnClick = doLogIpPathClick
        end
        object dpUpdIpRouting: TButton
          Left = 135
          Top = 20
          Width = 109
          Height = 25
          Caption = 'Update Routing'
          TabOrder = 2
          OnClick = dpUpdIpRoutingClick
        end
        object doUpdIpPaths: TButton
          Left = 10
          Top = 20
          Width = 109
          Height = 25
          Caption = 'Update Paths'
          TabOrder = 3
          OnClick = doUpdIpPathsClick
        end
      end
      object IpRoutingListV: TListView
        Left = 0
        Top = 465
        Width = 788
        Height = 275
        Align = alBottom
        Columns = <
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end>
        ReadOnly = True
        TabOrder = 1
        ViewStyle = vsReport
      end
      object IpPathsListV: TListView
        Left = 0
        Top = 61
        Width = 788
        Height = 404
        Align = alClient
        Columns = <
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end>
        ReadOnly = True
        TabOrder = 2
        ViewStyle = vsReport
      end
    end
    object TabArp: TTabSheet
      Caption = 'ARP and LAN IPs'
      ImageIndex = 6
      object BoxArp: TGroupBox
        Left = 0
        Top = 0
        Width = 788
        Height = 81
        Align = alTop
        Caption = 'ARP and LAN IPs'
        TabOrder = 0
        object Label1: TLabel
          Left = 483
          Top = 24
          Width = 54
          Height = 15
          Caption = 'Remote IP'
        end
        object doResolveIP: TButton
          Left = 375
          Top = 19
          Width = 93
          Height = 25
          Caption = 'Resolve IP'
          TabOrder = 0
          OnClick = doResolveIPClick
        end
        object ResolveIpRem: TEdit
          Left = 550
          Top = 20
          Width = 183
          Height = 23
          TabOrder = 1
          Text = '192.168.1.20'
        end
        object doLogARP: TButton
          Left = 500
          Top = 47
          Width = 105
          Height = 25
          Caption = 'Log ARP List'
          TabOrder = 2
          OnClick = doLogARPClick
        end
        object doLogNeighbIPs: TButton
          Left = 625
          Top = 47
          Width = 148
          Height = 25
          Caption = 'Log Neighbourhood IPs'
          TabOrder = 3
          OnClick = doLogNeighbIPsClick
        end
        object doUpdNeigbIps: TButton
          Left = 10
          Top = 20
          Width = 125
          Height = 25
          Caption = 'Update Neighb IPs'
          TabOrder = 4
          OnClick = doUpdNeigbIpsClick
        end
        object NeighbPermAddr: TCheckBox
          Left = 162
          Top = 23
          Width = 174
          Height = 17
          Caption = 'Show Permanent IP Addresses'
          TabOrder = 5
        end
        object doNetBIOS: TButton
          Left = 375
          Top = 47
          Width = 93
          Height = 25
          Caption = 'NetBIOS Name'
          TabOrder = 6
          OnClick = doNetBIOSClick
        end
      end
      object NeighbListV: TListView
        Left = 0
        Top = 81
        Width = 788
        Height = 659
        Align = alClient
        Columns = <
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end
          item
          end>
        ReadOnly = True
        TabOrder = 1
        ViewStyle = vsReport
      end
    end
    object TabStats: TTabSheet
      Caption = 'Statistics'
      ImageIndex = 8
      object BoxNetStats: TGroupBox
        Left = 0
        Top = 0
        Width = 788
        Height = 56
        Align = alTop
        Caption = 'Network Statistics'
        TabOrder = 0
        object doListStats: TButton
          Left = 662
          Top = 16
          Width = 104
          Height = 25
          Caption = 'Network Statistics'
          TabOrder = 0
          OnClick = doListStatsClick
        end
        object doUpdStats: TButton
          Left = 10
          Top = 20
          Width = 109
          Height = 25
          Caption = 'Update'
          TabOrder = 1
          OnClick = doUpdStatsClick
        end
      end
      object ListStatsTCP: TListBox
        Left = 5
        Top = 65
        Width = 291
        Height = 273
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 15
        ParentFont = False
        TabOrder = 1
      end
      object ListStatsUDP: TListBox
        Left = 330
        Top = 65
        Width = 285
        Height = 137
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 15
        ParentFont = False
        TabOrder = 2
      end
      object ListStatsIcmpIn: TListBox
        Left = 5
        Top = 355
        Width = 297
        Height = 269
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 15
        ParentFont = False
        TabOrder = 3
      end
      object ListStatsIcmpOut: TListBox
        Left = 330
        Top = 355
        Width = 261
        Height = 270
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ItemHeight = 15
        ParentFont = False
        TabOrder = 4
      end
    end
    object TabWhois: TTabSheet
      Caption = 'Whois'
      ImageIndex = 9
      object BoxWhois: TGroupBox
        Left = 0
        Top = 0
        Width = 788
        Height = 105
        Align = alTop
        Caption = 'Whois Domain Lookup'
        TabOrder = 0
        object Label13: TLabel
          Left = 5
          Top = 55
          Width = 68
          Height = 15
          Caption = 'Whois Server'
        end
        object Label14: TLabel
          Left = 5
          Top = 25
          Width = 102
          Height = 15
          Caption = 'Domain/IP Address'
        end
        object Label15: TLabel
          Left = 5
          Top = 81
          Width = 494
          Height = 15
          Caption = 
            'Note: each top domain (com, uk, eu, etc) has it'#39's own whois serv' +
            'er, auto uses an internal table'
        end
        object WhoisQueryName: TComboBox
          Left = 115
          Top = 20
          Width = 257
          Height = 23
          TabOrder = 0
          Text = 'magsys.co.uk'
          Items.Strings = (
            'magsys.co.uk'
            'ftptest.org'
            'ftptest.co.uk'
            'overbyte.be'
            'overbyte.eu'
            'google.com'
            'google.co.uk'
            '217.146.102.139'
            '2a00:1940:2:2::139'
            '1.1.1.1')
        end
        object WhoisServer: TComboBox
          Left = 115
          Top = 50
          Width = 257
          Height = 23
          TabOrder = 1
        end
        object WhoisMethod: TRadioGroup
          Left = 438
          Top = 16
          Width = 116
          Height = 60
          Caption = 'Whois Method'
          ItemIndex = 0
          Items.Strings = (
            'Automatic'
            'Specific Server')
          TabOrder = 2
        end
        object doWhoisQuery: TButton
          Left = 638
          Top = 21
          Width = 92
          Height = 25
          Caption = 'Start Query'
          Default = True
          TabOrder = 3
          OnClick = doWhoisQueryClick
        end
        object doWhoisAbort: TButton
          Left = 636
          Top = 60
          Width = 92
          Height = 25
          Caption = 'Abort Query'
          Default = True
          TabOrder = 4
          OnClick = doWhoisAbortClick
        end
        object doWhoisClear: TBitBtn
          Left = 392
          Top = 21
          Width = 22
          Height = 22
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777777777777777777777777777777777779977777997777777997777799
            7777777799777997777777777997997777777777779997777777777777999777
            7777777779979977777777779977799777777779977777997777777997777799
            7777777777777777777777777777777777777777777777777777}
          TabOrder = 5
          TabStop = False
          OnClick = doWhoisClearClick
        end
      end
      object WhoisMemo: TMemo
        Left = 0
        Top = 105
        Width = 788
        Height = 635
        Align = alClient
        Lines.Strings = (
          '')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
    object TabPing: TTabSheet
      Caption = 'Ping and Trace Route'
      ImageIndex = 10
      object BoxPingTrace: TGroupBox
        Left = 0
        Top = 0
        Width = 788
        Height = 109
        Align = alTop
        Caption = 'Ping and Traceroute'
        TabOrder = 0
        object Label17: TLabel
          Left = 175
          Top = 80
          Width = 100
          Height = 15
          Caption = 'Timeout (millisecs)'
        end
        object Label16: TLabel
          Left = 5
          Top = 79
          Width = 97
          Height = 15
          Caption = 'Ping Repeat Times'
        end
        object Labe26: TLabel
          Left = 364
          Top = 72
          Width = 88
          Height = 30
          Caption = 'Maximum Traceroute Errors'
          WordWrap = True
        end
        object Label18: TLabel
          Left = 5
          Top = 20
          Width = 132
          Height = 15
          Caption = 'Host Name or IP Address'
        end
        object doPingSync: TButton
          Left = 315
          Top = 45
          Width = 98
          Height = 25
          Caption = 'Ping (&Sync)'
          Default = True
          TabOrder = 0
          OnClick = doPingSyncClick
        end
        object doPingThread: TButton
          Left = 315
          Top = 15
          Width = 98
          Height = 25
          Caption = 'Ping (&Thread)'
          Default = True
          TabOrder = 1
          OnClick = doPingThreadClick
        end
        object doTraceRoute: TButton
          Left = 431
          Top = 15
          Width = 98
          Height = 25
          Caption = '&Trace Route'
          Default = True
          TabOrder = 2
          OnClick = doTraceRouteClick
        end
        object doPingAbort: TButton
          Left = 430
          Top = 44
          Width = 98
          Height = 25
          Cancel = True
          Caption = 'A&bort'
          Enabled = False
          TabOrder = 3
          OnClick = doPingAbortClick
        end
        object PingQueryName: TComboBox
          Left = 5
          Top = 40
          Width = 251
          Height = 23
          ItemIndex = 0
          TabOrder = 4
          Text = 'www.magsys.co.uk'
          Items.Strings = (
            'www.magsys.co.uk'
            '217.146.102.139'
            '2a00:1940:2:2::139'
            'svn.overbyte.be'
            'www.overbyte.be'
            'www.google.com'
            'www.embarcadero.com'
            'ipv6.google.com'
            #233'x'#224'mpl'#234'.ftptest.co.uk'
            'str'#248'm.no'
            '')
        end
        object doPingClear: TBitBtn
          Left = 276
          Top = 40
          Width = 22
          Height = 22
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777777777777777777777777777777777779977777997777777997777799
            7777777799777997777777777997997777777777779997777777777777999777
            7777777779979977777777779977799777777779977777997777777997777799
            7777777777777777777777777777777777777777777777777777}
          TabOrder = 5
          TabStop = False
          OnClick = doPingClearClick
        end
        object PingTimeoutMs: TEdit
          Left = 287
          Top = 75
          Width = 57
          Height = 23
          TabOrder = 6
          Text = '2000'
        end
        object PingCount: TEdit
          Left = 113
          Top = 75
          Width = 48
          Height = 23
          TabOrder = 7
          Text = '3'
        end
        object TraceErrors: TEdit
          Left = 479
          Top = 75
          Width = 48
          Height = 23
          TabOrder = 8
          Text = '4'
        end
        object PingLog: TCheckBox
          Left = 549
          Top = 80
          Width = 113
          Height = 17
          Caption = 'Log Results'
          TabOrder = 9
        end
      end
      object PingListView: TListView
        Left = 0
        Top = 109
        Width = 788
        Height = 631
        Align = alClient
        Columns = <
          item
            Caption = 'Count'
            Width = 60
          end
          item
            Caption = 'Duration'
            Width = 60
          end
          item
            Caption = 'Destination'
            Width = 200
          end
          item
            Caption = 'Comment'
            Width = 600
          end>
        ReadOnly = True
        TabOrder = 1
        ViewStyle = vsReport
      end
    end
    object TabDNSLookup: TTabSheet
      Caption = 'DNS Look-up'
      ImageIndex = 11
      object BoxDNSLookup: TGroupBox
        Left = 0
        Top = 0
        Width = 788
        Height = 151
        Align = alTop
        Caption = 'Doman Name Server Look-Up'
        TabOrder = 0
        object Label20: TLabel
          Left = 5
          Top = 80
          Width = 85
          Height = 15
          Caption = 'DNS Query Type'
        end
        object Label26: TLabel
          Left = 5
          Top = 20
          Width = 132
          Height = 15
          Caption = 'Host Name or IP Address'
        end
        object NameServerHttps: TComboBox
          Left = 491
          Top = 105
          Width = 258
          Height = 23
          Hint = 'DNS over HTTPS (DOH) Server URL '
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Items.Strings = (
            'A - Host Address               '
            'NS - Authoritative NS '
            'MD - obsolete'
            'MF - obsolete'
            'CNAME - Alias            '
            'SOA - Zone of Authority   '
            'MB - experimental              '
            'MG - experimental        '
            'MR - experimental       '
            'NULL - experimental                   '
            'WKS - Well Known Service '
            'PTR - Domain Name Pointer            '
            'HINFO - Host Information               '
            'MINFO - Mailbox information            '
            'MX - Mail Exchange    '
            'TXT - Text Strings                   '
            'RP     '
            'AFSDB  '
            'X25    '
            'ISDN   '
            'RT     '
            'NSAP   '
            'NSAPPTR'
            'SIG    '
            'KEY    '
            'PX     '
            'GPOS   '
            'AAAA - IP6   '
            'LOC - Location   '
            'NXT    '
            'spare'
            'spare'
            'SRV                                     '
            'NAPTR                                   '
            'KX   ')
        end
        object doDnsRev: TButton
          Left = 250
          Top = 110
          Width = 66
          Height = 25
          Caption = 'Reverse'
          TabOrder = 1
          OnClick = doDnsRevClick
        end
        object doDNSAll: TButton
          Left = 165
          Top = 110
          Width = 76
          Height = 25
          Caption = 'Full Query'
          TabOrder = 2
          OnClick = doDNSAllClick
        end
        object doDNSQuery: TButton
          Left = 5
          Top = 110
          Width = 75
          Height = 25
          Caption = 'DNS Query'
          TabOrder = 3
          OnClick = doDNSQueryClick
        end
        object NameProtoTCP: TCheckBox
          Left = 335
          Top = 120
          Width = 69
          Height = 17
          Caption = 'TCP Proto'
          TabOrder = 4
        end
        object NameQueryType: TComboBox
          Left = 113
          Top = 75
          Width = 197
          Height = 23
          Hint = 'DNS Query Type'
          Style = csDropDownList
          ParentShowHint = False
          ShowHint = True
          TabOrder = 5
          Items.Strings = (
            'A - Host Address               '
            'NS - Authoritative NS '
            'MD - obsolete'
            'MF - obsolete'
            'CNAME - Alias            '
            'SOA - Zone of Authority   '
            'MB - experimental              '
            'MG - experimental        '
            'MR - experimental       '
            'NULL - experimental                   '
            'WKS - Well Known Service '
            'PTR - Domain Name Pointer            '
            'HINFO - Host Information               '
            'MINFO - Mailbox information            '
            'MX - Mail Exchange    '
            'TXT - Text Strings                   '
            'RP     '
            'AFSDB  '
            'X25    '
            'ISDN   '
            'RT     '
            'NSAP   '
            'NSAPPTR'
            'SIG    '
            'KEY    '
            'PX     '
            'GPOS   '
            'AAAA - IP6   '
            'LOC - Location   '
            'NXT    '
            'spare'
            'spare'
            'SRV                                     '
            'NAPTR                                   '
            'KX   ')
        end
        object NameServerPublic: TComboBox
          Left = 490
          Top = 75
          Width = 258
          Height = 23
          Hint = 'Public DNS Server IP Address'
          Style = csDropDownList
          ParentShowHint = False
          ShowHint = True
          TabOrder = 6
        end
        object NameServerIP: TComboBox
          Left = 490
          Top = 45
          Width = 258
          Height = 23
          Hint = 'Specific DNS Server IP Address'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 7
        end
        object NameDnsOpt: TRadioGroup
          Left = 336
          Top = 15
          Width = 137
          Height = 98
          Caption = 'Domain Name Server'
          ItemIndex = 0
          Items.Strings = (
            'Automatic'
            'Specific'
            'Public'
            'HTTPS ')
          TabOrder = 8
        end
        object doDelNameHost: TBitBtn
          Left = 268
          Top = 39
          Width = 22
          Height = 22
          Glyph.Data = {
            F6000000424DF600000000000000760000002800000010000000100000000100
            0400000000008000000000000000000000001000000000000000000000000000
            80000080000000808000800000008000800080800000C0C0C000808080000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
            7777777777777777777777777777777777777779977777997777777997777799
            7777777799777997777777777997997777777777779997777777777777999777
            7777777779979977777777779977799777777779977777997777777997777799
            7777777777777777777777777777777777777777777777777777}
          TabOrder = 9
          TabStop = False
          OnClick = doDelNameHostClick
        end
        object NameHostName: TComboBox
          Left = 5
          Top = 40
          Width = 251
          Height = 23
          TabOrder = 10
          Text = 'magsys.co.uk'
        end
        object NameServerAuto: TComboBox
          Left = 490
          Top = 15
          Width = 258
          Height = 23
          Hint = 'Specific DNS Server IP Address'
          Style = csDropDownList
          ParentShowHint = False
          ShowHint = True
          TabOrder = 11
        end
        object doDnsBothA: TButton
          Left = 85
          Top = 110
          Width = 75
          Height = 25
          Caption = 'A and AAAA'
          TabOrder = 12
          OnClick = doDnsBothAClick
        end
      end
      object NameLookupMemo: TMemo
        Left = 0
        Top = 151
        Width = 788
        Height = 589
        Align = alClient
        Lines.Strings = (
          '')
        ReadOnly = True
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
    object TabAddrLookup: TTabSheet
      Caption = 'IP Address Look-up'
      ImageIndex = 12
      object BoxAddrLookup: TGroupBox
        Left = 0
        Top = 0
        Width = 788
        Height = 739
        Align = alTop
        Caption = 'IP Address Look-up'
        TabOrder = 0
        object BoxAddrForward: TGroupBox
          Left = 10
          Top = 20
          Width = 282
          Height = 188
          Caption = 'Forward Look-up'
          TabOrder = 0
          object Label19: TLabel
            Left = 5
            Top = 100
            Width = 101
            Height = 15
            Caption = 'Official Host Name'
          end
          object Label21: TLabel
            Left = 5
            Top = 74
            Width = 55
            Height = 15
            Caption = 'IP Address'
          end
          object Label22: TLabel
            Left = 5
            Top = 20
            Width = 113
            Height = 15
            Caption = 'Host Name (not URL)'
          end
          object LookupAddr: TEdit
            Left = 70
            Top = 70
            Width = 196
            Height = 23
            ReadOnly = True
            TabOrder = 0
          end
          object doAddrLookUp: TButton
            Left = 20
            Top = 150
            Width = 113
            Height = 27
            Caption = 'Forward Look-Up'
            TabOrder = 1
            OnClick = doAddrLookUpClick
          end
          object LookupOfficial: TEdit
            Left = 5
            Top = 119
            Width = 242
            Height = 23
            ReadOnly = True
            TabOrder = 2
          end
          object doDelAddrLook: TBitBtn
            Left = 252
            Top = 40
            Width = 22
            Height = 22
            Glyph.Data = {
              F6000000424DF600000000000000760000002800000010000000100000000100
              0400000000008000000000000000000000001000000000000000000000000000
              80000080000000808000800000008000800080800000C0C0C000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
              7777777777777777777777777777777777777779977777997777777997777799
              7777777799777997777777777997997777777777779997777777777777999777
              7777777779979977777777779977799777777779977777997777777997777799
              7777777777777777777777777777777777777777777777777777}
            TabOrder = 3
            TabStop = False
            OnClick = doDelAddrLookClick
          end
          object LookupHost: TComboBox
            Left = 5
            Top = 40
            Width = 242
            Height = 23
            TabOrder = 4
            Text = 'www.magsys.co.uk'
          end
        end
        object BoxAddrRev: TGroupBox
          Left = 307
          Top = 19
          Width = 249
          Height = 188
          Caption = 'Reverse Look-up'
          TabOrder = 1
          object Label23: TLabel
            Left = 5
            Top = 125
            Width = 148
            Height = 15
            Caption = 'Extra following IP Addresses'
          end
          object Label24: TLabel
            Left = 5
            Top = 70
            Width = 60
            Height = 15
            Caption = 'Host Name'
          end
          object Label25: TLabel
            Left = 5
            Top = 20
            Width = 55
            Height = 15
            Caption = 'IP Address'
          end
          object doAddrRevLook: TButton
            Left = 20
            Top = 150
            Width = 113
            Height = 27
            Caption = 'Reverse Look-Up'
            TabOrder = 0
            OnClick = doAddrRevLookClick
          end
          object RevExtra: TEdit
            Left = 181
            Top = 120
            Width = 51
            Height = 23
            TabOrder = 1
            Text = '12'
          end
          object RevLookHost: TEdit
            Left = 5
            Top = 90
            Width = 220
            Height = 23
            ReadOnly = True
            TabOrder = 2
          end
          object DelRevLookAddr: TBitBtn
            Left = 217
            Top = 40
            Width = 22
            Height = 22
            Glyph.Data = {
              F6000000424DF600000000000000760000002800000010000000100000000100
              0400000000008000000000000000000000001000000000000000000000000000
              80000080000000808000800000008000800080800000C0C0C000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
              7777777777777777777777777777777777777779977777997777777997777799
              7777777799777997777777777997997777777777779997777777777777999777
              7777777779979977777777779977799777777779977777997777777997777799
              7777777777777777777777777777777777777777777777777777}
            TabOrder = 3
            TabStop = False
            OnClick = DelRevLookAddrClick
          end
          object RevLookAddr: TComboBox
            Left = 5
            Top = 40
            Width = 205
            Height = 23
            TabOrder = 4
            Text = '217.146.102.130'
          end
        end
        object BoxBulkDns: TGroupBox
          Left = 5
          Top = 210
          Width = 776
          Height = 512
          Caption = 'Bulk IP Address Look-up'
          TabOrder = 2
          DesignSize = (
            776
            512)
          object Label29: TLabel
            Left = 9
            Top = 22
            Width = 65
            Height = 15
            Anchors = []
            AutoSize = False
            Caption = 'Host Names'
            ExplicitLeft = 5
            ExplicitTop = 20
          end
          object Label28: TLabel
            Left = 195
            Top = 20
            Width = 141
            Height = 15
            Caption = 'IPv4 or IPv6 Address Result'
          end
          object BulkNamesMemo: TMemo
            Left = 6
            Top = 39
            Width = 168
            Height = 462
            Hint = 'Double click to clear list, defaults will be reloaded on restart'
            ParentShowHint = False
            ScrollBars = ssBoth
            ShowHint = True
            TabOrder = 0
            WordWrap = False
            OnDblClick = BulkNamesMemoDblClick
          end
          object BulkResultMemo: TMemo
            Left = 195
            Top = 39
            Width = 422
            Height = 462
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 1
            WordWrap = False
          end
          object doBulkDnsLookup: TButton
            Left = 628
            Top = 39
            Width = 124
            Height = 27
            Caption = 'Forward Look-Up All'
            TabOrder = 2
            OnClick = doBulkDnsLookupClick
          end
        end
        object BoxDnsCache: TGroupBox
          Left = 566
          Top = 20
          Width = 212
          Height = 188
          Caption = 'DNS Cache Control'
          TabOrder = 3
          object Label27: TLabel
            Left = 110
            Top = 94
            Width = 91
            Height = 15
            Caption = 'Parallel Look-ups'
          end
          object DNCacheMethod: TRadioGroup
            Left = 10
            Top = 19
            Width = 85
            Height = 91
            Caption = 'Method'
            ItemIndex = 0
            Items.Strings = (
              'Winsock'
              'UDP'
              'TCP'
              'HTTPS')
            TabOrder = 0
            OnClick = DNCacheChangeClick
          end
          object DNCacheMax: TEdit
            Left = 110
            Top = 115
            Width = 51
            Height = 23
            TabOrder = 1
            Text = '5'
            OnChange = DNCacheChangeClick
          end
          object doDNCacheList: TButton
            Left = 20
            Top = 150
            Width = 75
            Height = 25
            Caption = 'List Cache'
            TabOrder = 2
            OnClick = doDNCacheListClick
          end
          object doDNCacheClear: TButton
            Left = 115
            Top = 150
            Width = 75
            Height = 25
            Caption = 'Clear Cache'
            TabOrder = 3
            OnClick = doDNCacheClearClick
          end
          object DNCacheLog: TCheckBox
            Left = 10
            Top = 115
            Width = 93
            Height = 17
            Caption = 'Log Access'
            TabOrder = 4
          end
          object DNCacheLAN: TRadioGroup
            Left = 111
            Top = 18
            Width = 95
            Height = 76
            Caption = 'LAN Lookup'
            ItemIndex = 0
            Items.Strings = (
              'Default'
              'Winsock'
              'NetBios')
            TabOrder = 5
            OnClick = DNCacheChangeClick
          end
        end
      end
    end
  end
  object CommonTimer: TTimer
    Enabled = False
    OnTimer = CommonTimerTimer
    Left = 85
    Top = 662
  end
  object IcsWhoisCli1: TIcsWhoisCli
    Host = 'whois.ripe.net'
    WhoisServers.Strings = (
      '?? whois.iana.org'
      '00 whois.ripe.net'
      '00 whois.arin.net'
      '00 whois.apnic.net'
      'com whois.verisign-grs.com'
      'org whois.pir.org'
      'net whois.verisign-grs.com'
      'uk whois.nic.uk'
      'ac whois.nic.ac'
      'ad whois.ripe.net'
      'ae whois.aeda.net.ae'
      'aero whois.aero'
      'af whois.nic.af'
      'ag whois.nic.ag'
      'ai whois.ai'
      'al whois.ripe.net'
      'am whois.amnic.net'
      'as whois.nic.as'
      'asia whois.nic.asia'
      'at whois.nic.at'
      'au whois.aunic.net'
      'aw whois.nic.aw'
      'ax whois.ax '
      'az whois.ripe.net'
      'ba whois.ripe.net'
      'bar whois.nic.bar'
      'be whois.dns.be'
      'berlin whois.nic.berlin'
      'best whois.nic.best'
      'bg whois.register.bg'
      'bi whois.nic.bi'
      'biz whois.neulevel.biz'
      'bj www.nic.bj'
      'bo whois.nic.bo'
      'br whois.nic.br'
      'br.com whois.centralnic.com'
      'bt whois.netnames.net'
      'bw whois.nic.net.bw'
      'by whois.cctld.by'
      'bz whois.belizenic.bz'
      'bzh whois-bzh.nic.fr'
      'ca whois.cira.ca'
      'cat whois.cat'
      'cc whois.nic.cc'
      'cd whois.nic.cd'
      'ceo whois.nic.ceo'
      'cf whois.dot.cf'
      'ch whois.nic.ch '
      'ci whois.nic.ci'
      'ck whois.nic.ck'
      'cl whois.nic.cl'
      'cloud whois.nic.cloud'
      'club whois.nic.club'
      'cn whois.cnnic.net.cn'
      'cn.com whois.centralnic.com'
      'co whois.nic.co'
      'co.nl whois.co.nl'
      'coop whois.nic.coop'
      'cx whois.nic.cx'
      'cy whois.ripe.net'
      'cz whois.nic.cz'
      'de whois.denic.de'
      'dk whois.dk-hostmaster.dk'
      'dm whois.nic.cx'
      'dz whois.nic.dz'
      'ec whois.nic.ec'
      'edu whois.educause.net'
      'ee whois.tld.ee'
      'eg whois.ripe.net'
      'es whois.nic.es'
      'eu whois.eu'
      'eu.com whois.centralnic.com'
      'eus whois.nic.eus'
      'fi whois.fi'
      'fo whois.nic.fo'
      'fr whois.nic.fr'
      'gb whois.ripe.net'
      'gb.com whois.centralnic.com'
      'gb.net whois.centralnic.com'
      'qc.com whois.centralnic.com'
      'ge whois.ripe.net'
      'gg whois.gg'
      'gi whois2.afilias-grs.net'
      'gl whois.nic.gl'
      'gm whois.ripe.net'
      'gov whois.nic.gov'
      'gr whois.ripe.net'
      'gs whois.nic.gs'
      'gy whois.registry.gy'
      'hamburg whois.nic.hamburg'
      'hiphop whois.uniregistry.net'
      'hk whois.hknic.net.hk'
      'hm whois.registry.hm'
      'hn whois2.afilias-grs.net'
      'host whois.nic.host'
      'hr whois.dns.hr'
      'ht whois.nic.ht'
      'hu whois.nic.hu'
      'hu.com whois.centralnic.com'
      'id whois.pandi.or.id'
      'ie whois.domainregistry.ie'
      'il whois.isoc.org.il'
      'im whois.nic.im'
      'in whois.inregistry.net'
      'info whois.afilias.info'
      'ing domain-registry-whois.l.google.com'
      'ink whois.centralnic.com'
      'int whois.isi.edu'
      'io whois.nic.io'
      'iq whois.cmc.iq'
      'ir whois.nic.ir'
      'is whois.isnic.is'
      'it whois.nic.it'
      'je whois.je'
      'jobs jobswhois.verisign-grs.com'
      'jp whois.jprs.jp'
      'ke whois.kenic.or.ke'
      'kg whois.domain.kg'
      'ki whois.nic.ki'
      'kr whois.kr'
      'kz whois.nic.kz'
      'la whois2.afilias-grs.net'
      'li whois.nic.li'
      'london whois.nic.london'
      'lt whois.domreg.lt'
      'lu whois.restena.lu'
      'lv whois.nic.lv'
      'ly whois.lydomains.com'
      'ma whois.iam.net.ma'
      'mc whois.ripe.net'
      'md whois.nic.md'
      'me whois.nic.me'
      'mg whois.nic.mg'
      'mil whois.nic.mil'
      'mk whois.ripe.net'
      'ml whois.dot.ml'
      'mo whois.monic.mo'
      'mobi whois.dotmobiregistry.net'
      'ms whois.nic.ms'
      'mt whois.ripe.net'
      'mu whois.nic.mu'
      'museum whois.museum'
      'mx whois.nic.mx'
      'my whois.mynic.net.my'
      'mz whois.nic.mz'
      'na whois.na-nic.com.na'
      'name whois.nic.name'
      'nc whois.nc'
      'nf whois.nic.cx'
      'ng whois.nic.net.ng'
      'nl whois.domain-registry.nl'
      'no whois.norid.no'
      'no.com whois.centralnic.com'
      'nu whois.nic.nu'
      'nz whois.srs.net.nz'
      'om whois.registry.om'
      'ong whois.publicinterestregistry.net'
      'ooo whois.nic.ooo'
      'paris whois-paris.nic.fr'
      'pe kero.yachay.pe'
      'pf whois.registry.pf'
      'pics whois.uniregistry.net'
      'pl whois.dns.pl'
      'pm whois.nic.pm'
      'pr whois.nic.pr'
      'press whois.nic.press'
      'pro whois.registrypro.pro'
      'pt whois.dns.pt'
      'pub whois.unitedtld.com'
      'pw whois.nic.pw'
      'qa whois.registry.qa'
      're whois.nic.re'
      'ro whois.rotld.ro'
      'rs whois.rnids.rs'
      'ru whois.tcinet.ru'
      'sa saudinic.net.sa'
      'sa.com whois.centralnic.com'
      'sb whois.nic.net.sb'
      'sc whois2.afilias-grs.net'
      'se whois.nic-se.se'
      'se.com whois.centralnic.com'
      'se.net whois.centralnic.com'
      'sg whois.nic.net.sg'
      'sh whois.nic.sh'
      'si whois.arnes.si'
      'sk whois.sk-nic.sk'
      'sm whois.nic.sm'
      'st whois.nic.st'
      'so whois.nic.so'
      'su whois.tcinet.ru'
      'sx whois.sx'
      'sy whois.tld.sy'
      'tc whois.adamsnames.tc'
      'tel whois.nic.tel'
      'tf whois.nic.tf'
      'th whois.thnic.net'
      'tj whois.nic.tj'
      'tk whois.nic.tk'
      'tl whois.domains.tl'
      'tm whois.nic.tm'
      'tn whois.ati.tn'
      'to whois.tonic.to'
      'top whois.nic.top'
      'tp whois.domains.tl'
      'tr whois.nic.tr'
      'travel whois.nic.travel'
      'tw whois.twnic.net.tw'
      'tv whois.nic.tv'
      'tz whois.tznic.or.tz'
      'ua whois.ua'
      'ug whois.co.ug'
      'uk.com whois.centralnic.com'
      'uk.net whois.centralnic.com'
      'ac.uk whois.ja.net'
      'gov.uk whois.ja.net'
      'us whois.nic.us'
      'us.com whois.centralnic.com'
      'uy nic.uy'
      'uy.com whois.centralnic.com'
      'uz whois.cctld.uz'
      'va whois.ripe.net'
      'vc whois2.afilias-grs.net'
      've whois.nic.ve'
      'vg ccwhois.ksregistry.net'
      'vu vunic.vu'
      'wang whois.nic.wang'
      'wf whois.nic.wf'
      'wiki whois.nic.wiki'
      'ws whois.website.ws'
      'xxx whois.nic.xxx'
      'xyz whois.nic.xyz'
      'yu whois.ripe.net'
      'za.com whois.centralnic.com')
    OnQueryDone = IcsWhoisCli1QueryDone
    Left = 180
    Top = 664
  end
  object IcsPing1: TPing
    Address = 'abc'
    SocketFamily = sfIPv4
    PingMsg = 'Pinging from Delphi code written by F. Piette'
    Size = 56
    Timeout = 4000
    TTL = 64
    Flags = 0
    OnEchoRequest = IcsPing1EchoRequest
    OnEchoReply = IcsPing1EchoReply
    OnDnsLookupDone = IcsPing1DnsLookupDone
    Left = 263
    Top = 665
  end
  object IcsDnsQuery1: TDnsQuery
    Port = '53'
    Proto = 'udp'
    MultiThreaded = False
    Timeout = 5
    ServerStrat = SrvStratOne
    ServerCur = 0
    ServerMax = 4
    OnRequestDone = IcsDnsQuery1RequestDone
    OnLogEvent = IcsDnsQuery1LogEvent
    Left = 204
    Top = 596
  end
  object IcsDnsQueryHttps1: TDnsQueryHttps
    Port = '53'
    Proto = 'udp'
    MultiThreaded = False
    Timeout = 5
    ServerStrat = SrvStratOne
    ServerCur = 0
    ServerMax = 4
    OnRequestDone = IcsDnsQuery1RequestDone
    OnLogEvent = IcsDnsQuery1LogEvent
    DnsSrvUrl = 'https://cloudflare-dns.com/dns-query'
    DebugLevel = DebugNone
    Left = 88
    Top = 595
  end
  object IcsNeighbDevices1: TIcsNeighbDevices
    DevFamily = 0
    LocalIps = False
    ScanTotIps = 1
    UpdScanSecs = 60
    UpdCacheSecs = 600
    Left = 315
    Top = 590
  end
  object IcsDomainNameCache1: TIcsDomNameCacheHttps
    DNMethod = MethodWinsock
    DnsServerStrat = SrvStratOne
    DefTTL = 3600
    MaxLookups = 5
    QTimeout = 5
    AddLocalhost = True
    DBLANlookup = LanLookDef
    OnDNLogEvent = IcsDomainNameCachex1DNLogEvent
    Left = 473
    Top = 583
  end
  object IcsIpChanges1: TIcsIpChanges
    DevFamily = 0
    OnIpChangesEvent = IcsIpChanges1IpChangesEvent
    Left = 350
    Top = 665
  end
end
