object BatchDnsLookupForm: TBatchDnsLookupForm
  Left = 202
  Top = 118
  BorderStyle = bsToolWindow
  Caption = 
    'ICS batch DNS lookup (IPv6 and IPv4) - https://www.overbyte.be -' +
    ' 7 Aug 2024'
  ClientHeight = 621
  ClientWidth = 786
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
  DesignSize = (
    786
    621)
  TextHeight = 13
  object Label4: TLabel
    Left = 10
    Top = 525
    Width = 148
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Number of Lookup instances'
  end
  object Label5: TLabel
    Left = 14
    Top = 551
    Width = 66
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'SocketFamily'
  end
  object Label7: TLabel
    Left = 8
    Top = 8
    Width = 147
    Height = 13
    Caption = 'International Domain Names'
  end
  object Label8: TLabel
    Left = 190
    Top = 8
    Width = 136
    Height = 13
    Caption = 'IPv4 or IPv6 Address Result'
  end
  object Label9: TLabel
    Left = 381
    Top = 8
    Width = 127
    Height = 13
    Caption = 'Punycode ASCII Encoded'
  end
  object Label10: TLabel
    Left = 592
    Top = 8
    Width = 92
    Height = 13
    Caption = 'Unicode Decoded'
  end
  object StartButton: TButton
    Left = 350
    Top = 534
    Width = 88
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Start Lookup'
    TabOrder = 4
    OnClick = StartButtonClick
  end
  object DnsNamesMemo: TMemo
    Left = 12
    Top = 23
    Width = 168
    Height = 481
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'DnsNamesMemo')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    OnDblClick = DnsNamesMemoDblClick
  end
  object ResultMemo: TMemo
    Left = 191
    Top = 23
    Width = 185
    Height = 481
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'ResultMemo')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object InstancesEdit: TEdit
    Left = 169
    Top = 520
    Width = 36
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    Text = '10'
  end
  object SocketFamilyComboBox: TComboBox
    Left = 13
    Top = 571
    Width = 157
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemIndex = 0
    TabOrder = 3
    Text = 'sfAny'
    Items.Strings = (
      'sfAny'
      'sfAnyIPv4'
      'sfAnyIPv6'
      'sfIPv4 (old API)'
      'sfIPv6')
  end
  object IDNMemo: TMemo
    Left = 383
    Top = 23
    Width = 205
    Height = 481
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 5
    WordWrap = False
  end
  object doIDNEncode: TButton
    Left = 350
    Top = 567
    Width = 86
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'IDN Encode'
    TabOrder = 6
    OnClick = doIDNEncodeClick
  end
  object DecodeMemo: TMemo
    Left = 593
    Top = 23
    Width = 184
    Height = 481
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      '')
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 7
    WordWrap = False
  end
  object DNCacheMethod: TRadioGroup
    Left = 228
    Top = 520
    Width = 85
    Height = 79
    Caption = 'Lookup Method'
    ItemIndex = 0
    Items.Strings = (
      'Winsock'
      'UDP'
      'TCP')
    TabOrder = 8
    OnClick = DNCacheMethodClick
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
    Left = 541
    Top = 545
  end
end
