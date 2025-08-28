object ThrdSrvForm: TThrdSrvForm
  Left = 295
  Top = 249
  Caption = 'ICS ThrdSrvForm - https://www.overbyte.be - 7 Aug 2024'
  ClientHeight = 270
  ClientWidth = 581
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  TextHeight = 13
  object ToolPanel: TPanel
    Left = 0
    Top = 0
    Width = 581
    Height = 57
    Align = alTop
    TabOrder = 0
    DesignSize = (
      581
      57)
    object Label1: TLabel
      Left = 8
      Top = 12
      Width = 96
      Height = 13
      Caption = 'Clients Per Thread:'
    end
    object ClientsPerThreadEdit: TEdit
      Left = 100
      Top = 8
      Width = 39
      Height = 21
      TabOrder = 0
      Text = '1'
      OnChange = ClientsPerThreadEditChange
    end
    object DisconnectAllButton: TButton
      Left = 496
      Top = 6
      Width = 81
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'Disconnect All'
      TabOrder = 1
      OnClick = DisconnectAllButtonClick
    end
    object ClearMemoButton: TButton
      Left = 496
      Top = 30
      Width = 81
      Height = 22
      Anchors = [akTop, akRight]
      Caption = 'Clear'
      TabOrder = 2
      OnClick = ClearMemoButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 57
    Width = 581
    Height = 213
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
  object WSocketThrdServer1: TWSocketThrdServer
    LineEnd = #13#10
    Port = 'telnet'
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    LocalPort = '0'
    KeepAliveOnOff = wsKeepAliveOnSystem
    KeepAliveTime = 30000
    KeepAliveInterval = 1000
    SocksLevel = '5'
    ExclusiveAddr = False
    ComponentOptions = []
    ListenBacklog = 15
    ReqVerLow = 1
    ReqVerHigh = 1
    OnBgException = WSocketThrdServer1BgException
    SocketErrs = wsErrTech
    Banner = 'Welcome to TcpSrv'
    OnClientDisconnect = WSocketThrdServer1ClientDisconnect
    OnClientConnect = WSocketThrdServer1ClientConnect
    OnClientCreate = WSocketThrdServer1ClientCreate
    MultiListenSockets = <>
    ClientsPerThread = 1
    OnThreadException = WSocketThrdServer1ThreadException
    Left = 72
    Top = 120
  end
end
