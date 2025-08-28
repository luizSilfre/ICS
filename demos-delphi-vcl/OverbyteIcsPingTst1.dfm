object PingTstForm: TPingTstForm
  Left = 125
  Top = 232
  Caption = 'ICS Ping - https://www.overbyte.be - 7 Aug 2024'
  ClientHeight = 603
  ClientWidth = 722
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 181
    Width = 722
    Height = 422
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 722
    Height = 181
    Align = alTop
    TabOrder = 1
    object Label2: TLabel
      Left = 370
      Top = 10
      Width = 32
      Height = 13
      Caption = 'Family'
    end
    object Label3: TLabel
      Left = 370
      Top = 40
      Width = 58
      Height = 13
      Caption = 'Source IPv4'
    end
    object Label4: TLabel
      Left = 370
      Top = 70
      Width = 58
      Height = 13
      Caption = 'Source IPv6'
    end
    object doPingSync: TButton
      Left = 255
      Top = 3
      Width = 98
      Height = 21
      Caption = 'Ping 1 (&Sync)'
      Default = True
      TabOrder = 1
      OnClick = doPingSyncClick
    end
    object doAbort: TButton
      Left = 255
      Top = 132
      Width = 98
      Height = 21
      Cancel = True
      Caption = 'A&bort'
      Enabled = False
      TabOrder = 5
      OnClick = doAbortClick
    end
    object doPingThread: TButton
      Left = 255
      Top = 55
      Width = 98
      Height = 21
      Caption = 'Ping 1 (&Thread)'
      Default = True
      TabOrder = 2
      OnClick = doPingThreadClick
    end
    object doPingAllThread: TButton
      Left = 255
      Top = 80
      Width = 98
      Height = 21
      Caption = 'Ping &All (Thread)'
      Default = True
      TabOrder = 3
      OnClick = doPingThreadClick
    end
    object doTraceRoute: TButton
      Left = 255
      Top = 105
      Width = 98
      Height = 21
      Caption = '&Trace Route 1'
      Default = True
      TabOrder = 4
      OnClick = doTraceRouteClick
    end
    object SockFamily: TComboBox
      Left = 450
      Top = 5
      Width = 145
      Height = 21
      TabOrder = 6
      Text = 'Any'
    end
    object SrcIpV4: TComboBox
      Left = 450
      Top = 35
      Width = 211
      Height = 21
      TabOrder = 7
    end
    object SrcIpV6: TComboBox
      Left = 450
      Top = 63
      Width = 211
      Height = 21
      TabOrder = 8
    end
    object HostNames: TListBox
      Left = 9
      Top = 0
      Width = 221
      Height = 171
      ItemHeight = 13
      Items.Strings = (
        '217.146.102.140'
        'svn.overbyte.be'
        'www.overbyte.be'
        'www.google.com'
        'www.embarcadero.com'
        'ipv6.google.com'
        #233'x'#224'mpl'#234'.ftptest.co.uk'
        'str'#248'm.no'
        '2607:f8b0:400c:c01::93'
        '2001:470:1f05:36a::3'
        '2222:22:22:22::22')
      TabOrder = 0
      OnDblClick = HostNamesDblClick
    end
    object NewHost: TEdit
      Left = 480
      Top = 104
      Width = 210
      Height = 21
      TabOrder = 10
    end
    object doNewHost: TButton
      Left = 370
      Top = 105
      Width = 93
      Height = 21
      Cancel = True
      Caption = 'Add New  Host'
      TabOrder = 9
      OnClick = doNewHostClick
    end
    object doExit: TButton
      Left = 370
      Top = 132
      Width = 93
      Height = 21
      Cancel = True
      Caption = 'E&xit'
      TabOrder = 11
      OnClick = doExitClick
    end
  end
  object Ping1: TPing
    Address = 'abc'
    SocketFamily = sfIPv4
    PingMsg = 'Pinging from Delphi code written by F. Piette'
    Size = 56
    Timeout = 4000
    TTL = 64
    Flags = 0
    OnEchoRequest = Ping1EchoRequest
    OnEchoReply = Ping1EchoReply
    OnDnsLookupDone = Ping1DnsLookupDone
    Left = 34
    Top = 258
  end
end
