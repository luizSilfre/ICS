{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  ICS Network Tools Demo, includes demos for all the main IP Helper
              functions in OverbyteIcsIpHlpApi, and several demos for several
              other components, including Trace Route, Ping Host, Name Server
              Look-up, Reverse DNS, Address Lookup, Domain Name Cache and Whois.
              Illustrates use of components, TIcsNeighbDevices, TIcsDomainNameCache,
              IcsDnsQueuy, TDnsQueryHttps, TIcsWhoisCli, TIcsIpChanges, TPing and
              TPingThread.
Creation:     May 2023
Updated:      Aug 2024
Version:      V9.3
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2024 by Angus Robertson, Magenta Systems Ltd,
              Croydon, England. delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Aug 10, 2023 - V8.71 Baseline
Aug 08, 2023 V9.0  Updated version to major release 9.
Jan 11, 2024 V9.1  Log start/stop monitor IP changes.
                   Added resource file nmap-mac-prefixes.RES to avoid distributing
                      file nmap-mac-prefixes.txt to display MAC vendor names.
                   Added resource icsportlist.RES to avoid distributing file
                     icsportlist.txt to display common port/service names.
Aug 07, 2024 V9.3  Using OverbyteIcsTypes for consolidated types and constants, allowing
                     other import units to be removed.


}

unit OverbyteIcsNetTools1;

{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{ If you use Delphi 7, you may wants to disable warnings for unsage type,   }
{ unsafe code and unsafe typecast in the project options. Those warning are }
{ intended for .NET programs. You may also want to turn off deprecated      }
{ symbol and platform symbol warnings.                                      }

interface

{$R nmap-mac-prefixes.RES}   { V9.1 link nmap-mac-prefixes.txt list to avoid loading from file }
{$R icsportlist.RES}         { V9.1 link icsportlist.txt list to avoid loading from file }

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, Buttons,
//OverbyteIcsWinsock,
//OverbyteIcsWndControl,
  OverbyteIcsWsocket,
  OverbyteIcsUtils,
  OverbyteIcsIniFiles,
  OverbyteIcsIpHlpApi,
  OverbyteIcsIpUtils,
  OverbyteIcsTicks64,
  OverbyteIcsWhoisCli,
  OverbyteIcsPing,
  OverbyteIcsDnsQuery,
  OverbyteIcsDnsHttps,
  OverbyteIcsTypes, OverbyteIcsWndControl, OverbyteIcsSslBase;  { V9.3 consolidated types and constants }

const
    SectionMainWindow    = 'MainWindow';
    SectionLogWindow     = 'LogWindow';
    KeyTop               = 'Top';
    KeyLeft              = 'Left';
    KeyWidth             = 'Width';
    KeyHeight            = 'Height';
    SectionData          = 'Data';


type
  TToolsForm = class(TForm)
    DNCacheLAN: TRadioGroup;
    DNCacheLog: TCheckBox;
    DNCacheMax: TEdit;
    DNCacheMethod: TRadioGroup;
    IpConnType: TRadioGroup;
    IpConnsSecs: TEdit;
    IpFamily: TComboBox;
    LocalIPv4: TComboBox;
    LocalIPv6: TComboBox;
    LookupHost: TComboBox;
    MonIpAddrChanges: TCheckBox;
    NameDnsOpt: TRadioGroup;
    NameHostName: TComboBox;
    NameProtoTCP: TCheckBox;
    NameQueryType: TComboBox;
    NameServerIP: TComboBox;
    NameServerPublic: TComboBox;
    NeigbDevCache: TEdit;
    NeigbDevIPv4: TEdit;
    NeigbDevIPv6: TEdit;
    NeigbDevScan: TEdit;
    NeigbDevTot: TEdit;
    NeighbPermAddr: TCheckBox;
    PingCount: TEdit;
    PingLog: TCheckBox;
    PingQueryName: TComboBox;
    PingTimeoutMs: TEdit;
    ResolveIpRem: TEdit;
    RevExtra: TEdit;
    RevLookAddr: TComboBox;
    TraceErrors: TEdit;
    WhoisMethod: TRadioGroup;
    WhoisQueryName: TComboBox;
    WhoisServer: TComboBox;


// following not saved
    Panel1: TPanel;
    doExit: TButton;
    PageControl1: TPageControl;
    TabConnections: TTabSheet;
    TabAdaptors: TTabSheet;
    TabNeighbDev: TTabSheet;
    TabInterfaces: TTabSheet;
    Label2: TLabel;
    Label4: TLabel;
    BoxNeigbDevs: TGroupBox;
    doNeigbDevStart: TButton;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    doNeigbDevStop: TButton;
    doDeviceLogMac: TButton;
    doDeviceLogIp: TButton;
    doDeviceEmptyCache: TButton;
    doDevicesClear: TButton;
    NeigbDevListV: TListView;
    TabIpAddr: TTabSheet;
    TabIpRouting: TTabSheet;
    TabArp: TTabSheet;
    TabStats: TTabSheet;
    TabWhois: TTabSheet;
    TabPing: TTabSheet;
    TabDNSLookup: TTabSheet;
    TabAddrLookup: TTabSheet;
    Label11: TLabel;
    BoxArp: TGroupBox;
    BoxNetAdapt: TGroupBox;
    BoxIpAddresses: TGroupBox;
    BoxWhois: TGroupBox;
    BoxDNSLookup: TGroupBox;
    BoxNetStats: TGroupBox;
    BoxPingTrace: TGroupBox;
    BoxAddrLookup: TGroupBox;
    BoxIpRouting: TGroupBox;
    BoxNetInterfaces: TGroupBox;
    doResolveIP: TButton;
    Label1: TLabel;
    doLogARP: TButton;
    doLogNeighbIPs: TButton;
    doLogAdaptors: TButton;
    doLogInterfaces: TButton;
    doLogNetParams: TButton;
    doLogIpRouting: TButton;
    doLogIpPath: TButton;
    doListStats: TButton;
    doLogIpAddr: TButton;
    BoxIpConnections: TGroupBox;
    doLogIpConns: TButton;
    IpConnsListV: TListView;
    doIpConnStop: TButton;
    doIpConnUpdate: TButton;
    doIpConnStart: TButton;
    LabelInterval: TLabel;
    CommonTimer: TTimer;
    AdaptorListV: TListView;
    doUpdAdaptors: TButton;
    doUpdInterfaces: TButton;
    doUpdIpAddr: TButton;
    dpUpdIpRouting: TButton;
    doUpdIpPaths: TButton;
    doUpdNeigbIps: TButton;
    doUpdStats: TButton;
    InterfacesListV: TListView;
    IpAddressListV: TListView;
    IpRoutingListV: TListView;
    NeighbListV: TListView;
    IpPathsListV: TListView;
    NetParamsListV: TListView;
    Label12: TLabel;
    ListStatsTCP: TListBox;
    ListStatsUDP: TListBox;
    ListStatsIcmpIn: TListBox;
    ListStatsIcmpOut: TListBox;
    Label13: TLabel;
    Label14: TLabel;
    doWhoisQuery: TButton;
    IcsWhoisCli1: TIcsWhoisCli;
    doWhoisAbort: TButton;
    WhoisMemo: TMemo;
    Label15: TLabel;
    doPingSync: TButton;
    doPingThread: TButton;
    doTraceRoute: TButton;
    doPingAbort: TButton;
    doPingClear: TBitBtn;
    doWhoisClear: TBitBtn;
    Label17: TLabel;
    Label16: TLabel;
    IcsPing1: TPing;
    Labe26: TLabel;
    Label18: TLabel;
    PingListView: TListView;
    NameServerHttps: TComboBox;
    doDnsRev: TButton;
    doDNSAll: TButton;
    doDNSQuery: TButton;
    doDelNameHost: TBitBtn;
    Label20: TLabel;
    NameLookupMemo: TMemo;
    BoxAddrForward: TGroupBox;
    BoxAddrRev: TGroupBox;
    doAddrLookUp: TButton;
    LookupOfficial: TEdit;
    Label19: TLabel;
    Label21: TLabel;
    doDelAddrLook: TBitBtn;
    LookupAddr: TEdit;
    Label22: TLabel;
    doAddrRevLook: TButton;
    Label23: TLabel;
    RevLookHost: TEdit;
    Label24: TLabel;
    DelRevLookAddr: TBitBtn;
    Label25: TLabel;
    Label26: TLabel;
    IcsDnsQuery1: TDnsQuery;
    IcsDnsQueryHttps1: TDnsQueryHttps;
    NameServerAuto: TComboBox;
    BoxBulkDns: TGroupBox;
    BulkNamesMemo: TMemo;
    Label29: TLabel;
    Label28: TLabel;
    BulkResultMemo: TMemo;
    doBulkDnsLookup: TButton;
    BoxDnsCache: TGroupBox;
    Label27: TLabel;
    doDNCacheList: TButton;
    doDNCacheClear: TButton;
    IcsNeighbDevices1: TIcsNeighbDevices;
    doNetBIOS: TButton;
    doDnsBothA: TButton;
    IcsDomainNameCache1: TIcsDomNameCacheHttps;
    IcsIpChanges1: TIcsIpChanges;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure doLogIpConnsClick(Sender: TObject);
    procedure doLogARPClick(Sender: TObject);
    procedure doLogAdaptorsClick(Sender: TObject);
    procedure doLogNetParamsClick(Sender: TObject);
    procedure doLogInterfacesClick(Sender: TObject);
    procedure doLogIpAddrClick(Sender: TObject);
    procedure doLogIpRoutingClick(Sender: TObject);
    procedure doListStatsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doLogNeighbIPsClick(Sender: TObject);
    procedure doLogIpPathClick(Sender: TObject);
    procedure doResolveIPClick(Sender: TObject);
    procedure doDeviceLogMacClick(Sender: TObject);
    procedure doDeviceLogIpClick(Sender: TObject);
    procedure doDeviceEmptyCacheClick(Sender: TObject);
    procedure doDevicesClearClick(Sender: TObject);
    procedure doNeigbDevStartClick(Sender: TObject);
    procedure doNeigbDevStopClick(Sender: TObject);
    procedure NeigbDevListVData(Sender: TObject; Item: TListItem);
    procedure IpConnsListVData(Sender: TObject; Item: TListItem);
    procedure doIpConnUpdateClick(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure CommonTimerTimer(Sender: TObject);
    procedure doIpConnStartClick(Sender: TObject);
    procedure doIpConnStopClick(Sender: TObject);
    procedure doUpdAdaptorsClick(Sender: TObject);
    procedure doUpdInterfacesClick(Sender: TObject);
    procedure doUpdIpAddrClick(Sender: TObject);
    procedure doUpdIpPathsClick(Sender: TObject);
    procedure dpUpdIpRoutingClick(Sender: TObject);
    procedure doUpdNeigbIpsClick(Sender: TObject);
    procedure doUpdStatsClick(Sender: TObject);
    procedure doWhoisQueryClick(Sender: TObject);
    procedure IcsWhoisCli1QueryDone(Sender: TObject; ErrCode: Word);
    procedure doWhoisAbortClick(Sender: TObject);
    procedure doWhoisClearClick(Sender: TObject);
    procedure doPingClearClick(Sender: TObject);
    procedure doPingThreadClick(Sender: TObject);
    procedure doPingSyncClick(Sender: TObject);
    procedure doPingAbortClick(Sender: TObject);
    procedure doTraceRouteClick(Sender: TObject);
    procedure IcsPing1DnsLookupDone(Sender: TObject; Error: Word);
    procedure IcsPing1EchoReply(Sender, Icmp: TObject; Status: Integer);
    procedure IcsPing1EchoRequest(Sender, Icmp: TObject);
    procedure doDNSQueryClick(Sender: TObject);
    procedure doDNSAllClick(Sender: TObject);
    procedure doDnsRevClick(Sender: TObject);
    procedure doAddrRevLookClick(Sender: TObject);
    procedure doAddrLookUpClick(Sender: TObject);
    procedure doDelNameHostClick(Sender: TObject);
    procedure doDelAddrLookClick(Sender: TObject);
    procedure DelRevLookAddrClick(Sender: TObject);
    procedure IcsDnsQuery1LogEvent(Sender: TObject; const Msg: string);
    procedure IcsDnsQuery1RequestDone(Sender: TObject; Error: Word);
    procedure doBulkDnsLookupClick(Sender: TObject);
    procedure IcsDomainNameCachex1DNLogEvent(Sender: TObject; const Msg: string);
    procedure BulkNamesMemoDblClick(Sender: TObject);
    procedure CommonCacheUpdateEvent(Sender: TObject; ItemNr: Integer);
    procedure BulkNameUpdateEvent(Sender: TObject; ItemNr: Integer);
    procedure NameLookUpdateEvent(Sender: TObject; ItemNr: Integer);
    procedure PingNameUpdateEvent(Sender: TObject; ItemNr: Integer);
    procedure IpConnNameUpdateEvent(Sender: TObject; ItemNr: Integer);
    procedure doDNCacheListClick(Sender: TObject);
    procedure doDNCacheClearClick(Sender: TObject);
    procedure DNCacheChangeClick(Sender: TObject);
    procedure MonIpAddrChangesClick(Sender: TObject);
    procedure doNetBIOSClick(Sender: TObject);
    procedure doDnsBothAClick(Sender: TObject);
    procedure IcsIpChanges1IpChangesEvent(IpAddrInfo: TIpAddrInfo; NotificationType: TMibNoticationType);
  private
    { Private declarations }
    procedure AddLogLine(const S: String);
    procedure AddLogLines(SL: TStrings);
    procedure OnNeigbDevDevUpd(Sender: TObject);
    procedure OnNeigbDevLogEvent(Sender: TObject; const Msg: string);
    function GetIpFamilyAF: Integer;
    procedure PingSetButtons (Enable: Boolean);
    procedure PingThreadTermPing(Sender: TObject);
    procedure PingThreadTermTrace (Sender: TObject);
    procedure SetupNameServer;
    procedure SetDnsButtons(Value: Boolean);
    procedure SetLocalIPs;
  public
    { Public declarations }
  end;

  const
    TraceMax = 32 ;
    MaxErrors = 8 ;
    MaskResponse1 = 'Ping of %d bytes took %d msecs' ;
    MaskResponse2 = '%2d  %4dms  %-24s  %s' ;
    MaskResponse3 = '%2s  %4s  %-24s  %s' ;

var
  ToolsForm: TToolsForm;
  BuffLogLines: String;
  WorkStrings: TStringList;
  FIniFileName: String;
  LocalIpLuid: TNetLuid;
  IcsNetworkParams: TNetworkParams;
  IpConnRows: TConnRows;
  IpConnUpdateTrg: Int64;
  NameServerTrg: Int64;
  TraceAddr: array [1..TraceMax] of string;   // IP reached in trace route
  ThreadIds: array [-1..TraceMax] of integer; // threaded pings so we can cancel them
  TraceErrs: integer;
  TraceSocketFamily: TSocketFamily;
  TraceIPAddr: string;
  TraceDoneFlag: boolean;
  StopFlag: boolean;
  PendingPings: integer;
  DNSReqId: Integer;


implementation

{$R *.dfm}

Uses OverbyteIcsNetTools2;

// functions to update drop down lists of TComboBox from Text
procedure AddRecentSList (SList: TStrings; S: string; Max: integer);
begin
    if SList.IndexOf (S) < 0 then begin
        if SList.Count > Max then
            SList.Delete (SList.Count - 1);
        SList.Insert (0, S);
    end ;
end ;

procedure DelRecentSList (SList: TComboBox);
var
    I: Integer;
begin
    I := SList.Items.IndexOf (SList.Text);
    if I >= 0 then begin
        SList.Items.Delete (I);
        if I = 0 then
           SList.ItemIndex := 0;
    end;
end ;

function CleanDomain (const domain: string): string ;
begin
    result := Trim (domain) ;
    if Pos ('http://', LowerCase(Result)) = 1 then
        result := Copy (Result, 8, 99) ;
    if Pos ('https://', LowerCase(Result)) = 1 then
        result := Copy (Result, 9, 99) ;
    If Result [Length (Result)] = '/' then
        SetLength (Result, Length (Result) - 1) ;
end;

procedure TToolsForm.AddLogLine(const S: String);
begin
    BuffLogLines := BuffLogLines + S + IcsCRLF;
end;

procedure TToolsForm.AddLogLines(SL: TStrings);
begin
    BuffLogLines := BuffLogLines + SL.Text;
end;

procedure TToolsForm.BulkNamesMemoDblClick(Sender: TObject);
begin
    BulkNamesMemo.Lines.Clear;
end;

procedure TToolsForm.CommonTimerTimer(Sender: TObject);
var
    displen: integer ;
begin
    displen := Length(BuffLogLines);
    if displen > 0 then begin
        try
            SetLength(BuffLogLines, displen - 2) ;  // remove CRLF
            if NOT FormLog.Visible then
                FormLog.Show;
            FormLog.Log.Lines.Add(BuffLogLines);
            SendMessage(FormLog.Log.Handle, EM_LINESCROLL, 0, 999999);
        except
        end ;
        BuffLogLines := '';
    end;

  // update IP connections list
    if IcsTestTrgTick64(IpConnUpdateTrg) then begin
        IpConnUpdateTrg := Trigger64Disabled;
        doIpConnUpdateClick(Self);
        IpConnUpdateTrg := IcsGetTrgSecs64(atoi(IpConnsSecs.Text));
    end;

 // DNS lookup, timeout
    if IcsTestTrgTick64(NameServerTrg) then begin
        NameServerTrg := Trigger64Disabled;
        if NameDnsOpt.ItemIndex = 3 then
            IcsDnsQueryHttps1.AbortQuery
        else
            IcsDnsQuery1.AbortQuery;
        SetDnsButtons(True);
    end;
end;


procedure TToolsForm.FormCreate(Sender: TObject);
begin
    WorkStrings := TStringList.Create;
    IpConnUpdateTrg := Trigger64Disabled;
    NameServerTrg := Trigger64Disabled;
end;

procedure TToolsForm.FormDestroy(Sender: TObject);
begin
    FreeAndNil(WorkStrings);
end;

// set local IP lists from winsock, hopefully first IPv4 is the one we need
procedure TToolsForm.SetLocalIPs;
var
    I: Integer;
begin
    LocalIPv4.Items.Assign(LocalIPList(sfIPv4, IPPROTO_TCP));
    if LocalIPv4.Text = '' then
        LocalIPv4.ItemIndex := 0;
    LocalIPv6.Items.Assign(LocalIPList(sfIPv6, IPPROTO_TCP));
    if LocalIPv6.Text = '' then begin
        if LocalIPv6.Items.Count > 0 then begin  // look for first public IPv6 with 2xxx
            for I := 0 to LocalIPv6.Items.Count - 1 do begin
                if Pos ('2', LocalIPv6.Items[I]) = 1 then begin
                    LocalIPv6.ItemIndex := I;
                    break;
                end;
            end;
        end;
    end;
end;


procedure TToolsForm.FormShow(Sender: TObject);
var
    IniFile: TIcsIniFile;
    section, S1, S2: String;
    I, J: Integer;
    SF: TSocketFamily;
begin
    for SF := Low(SocketFamilyNames) to High(SocketFamilyNames) do
        IpFamily.Items.Add(SocketFamilyNames[SF]);
    IpFamily.ItemIndex := 0;

    FIniFileName := GetIcsIniFileName;
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do begin
    // form positions
        Width := ReadInteger(SectionMainWindow, KeyWidth,  Width);
        Height := ReadInteger(SectionMainWindow, KeyHeight, Height);
        Top := ReadInteger(SectionMainWindow, KeyTop, (Screen.Height - Height) div 2);
        Left := ReadInteger(SectionMainWindow, KeyLeft, (Screen.Width  - Width) div 2);
        section := SectionData;
  DNCacheLAN.ItemIndex := ReadInteger (section, 'DNCacheLAN_ItemIndex', DNCacheLAN.ItemIndex) ;
  if ReadString (section, 'DNCacheLog_Checked', 'False') = 'True' then DNCacheLog.Checked := true else DNCacheLog.Checked := false ;
  DNCacheMax.Text := ReadString (section, 'DNCacheMax_Text', DNCacheMax.Text) ;
  DNCacheMethod.ItemIndex := ReadInteger (section, 'DNCacheMethod_ItemIndex', DNCacheMethod.ItemIndex) ;
  IpConnType.ItemIndex := ReadInteger (section, 'IpConnType_ItemIndex', IpConnType.ItemIndex) ;
  IpConnsSecs.Text := ReadString (section, 'IpConnsSecs_Text', IpConnsSecs.Text) ;
  IpFamily.ItemIndex := ReadInteger (section, 'IpFamily_ItemIndex', IpFamily.ItemIndex) ;
  LocalIPv4.Text := ReadString (section, 'LocalIPv4_Text', LocalIPv4.Text) ;
  LocalIPv6.Text := ReadString (section, 'LocalIPv6_Text', LocalIPv6.Text) ;
  LookupHost.Text := ReadString (section, 'LookupHost_Text', LookupHost.Text) ;
  LookupHost.Items.CommaText := ReadString (section, 'LookupHost_Items', LookupHost.Items.CommaText) ;
  if ReadString (section, 'MonIpAddrChanges_Checked', 'True') = 'True' then MonIpAddrChanges.Checked := true else MonIpAddrChanges.Checked := false ;  // edited
  NameDnsOpt.ItemIndex := ReadInteger (section, 'NameDnsOpt_ItemIndex', NameDnsOpt.ItemIndex) ;
  NameHostName.Text := ReadString (section, 'NameHostName_Text', NameHostName.Text) ;
  NameHostName.Items.CommaText := ReadString (section, 'NameHostName_Items', NameHostName.Items.CommaText) ;
  NameQueryType.ItemIndex := ReadInteger (section, 'NameQueryType_ItemIndex', NameQueryType.ItemIndex) ;
  NameHostName.Items.CommaText := ReadString (section, 'NameHostName_Items', NameHostName.Items.CommaText) ;
  if ReadString (section, 'NameProtoTCP_Checked', 'False') = 'True' then NameProtoTCP.Checked := true else NameProtoTCP.Checked := false ;
  NameServerIP.Text := ReadString (section, 'NameServerIP_Text', NameServerIP.Text) ;
  NameServerIP.Items.CommaText := ReadString (section, 'NameServerIP_Items', NameServerIP.Items.CommaText) ;
  NameServerPublic.ItemIndex := ReadInteger (section, 'NameServerPublic_ItemIndex', NameServerPublic.ItemIndex) ;
  NeigbDevCache.Text := ReadString (section, 'NeigbDevCache_Text', NeigbDevCache.Text) ;
  NeigbDevIPv4.Text := ReadString (section, 'NeigbDevIPv4_Text', NeigbDevIPv4.Text) ;
  NeigbDevIPv6.Text := ReadString (section, 'NeigbDevIPv6_Text', NeigbDevIPv6.Text) ;
  NeigbDevScan.Text := ReadString (section, 'NeigbDevScan_Text', NeigbDevScan.Text) ;
  NeigbDevTot.Text := ReadString (section, 'NeigbDevTot_Text', NeigbDevTot.Text) ;
  if ReadString (section, 'NeighbPermAddr_Checked', 'False') = 'True' then NeighbPermAddr.Checked := true else NeighbPermAddr.Checked := false ;
  PingCount.Text := ReadString (section, 'PingCount_Text', PingCount.Text) ;
  if ReadString (section, 'PingLog_Checked', 'False') = 'True' then PingLog.Checked := true else PingLog.Checked := false ;
  PingQueryName.Text := ReadString (section, 'PingQueryName_Text', PingQueryName.Text) ;
  PingQueryName.Items.CommaText := ReadString (section, 'PingQueryName_Items', PingQueryName.Items.CommaText) ;
  PingTimeoutMs.Text := ReadString (section, 'PingTimeoutMs_Text', PingTimeoutMs.Text) ;
  ResolveIpRem.Text := ReadString (section, 'ResolveIpRem_Text', ResolveIpRem.Text) ;
  RevExtra.Text := ReadString (section, 'RevExtra_Text', RevExtra.Text) ;
  RevLookAddr.Text := ReadString (section, 'RevLookAddr_Text', RevLookAddr.Text) ;
  RevLookAddr.Items.CommaText := ReadString (section, 'RevLookAddr_Items', RevLookAddr.Items.CommaText) ;
  TraceErrors.Text := ReadString (section, 'TraceErrors_Text', TraceErrors.Text) ;
  WhoisQueryName.Text := ReadString (section, 'WhoisQueryName_Text', WhoisQueryName.Text) ;
  WhoisQueryName.Items.CommaText := ReadString (section, 'WhoisQueryName_Items', WhoisQueryName.Items.CommaText) ;
  WhoisServer.Text := ReadString (section, 'WhoisServer_Text', WhoisServer.Text) ;
  WhoisServer.Items.CommaText := ReadString (section, 'WhoisServer_Items', WhoisServer.Items.CommaText) ;
    end;
    IniFile.Free;

// set local IP lists from winsock, hopefully first IPv4 is the one we need
    SetLocalIPs;

// prepare TListView columns from constants in OverbyteIcsIpHlpApi
    NeigbDevListV.Columns.Clear;
    for I := Low(NeighbDevRowHdrs) to High(NeighbDevRowHdrs) do begin
        NeigbDevListV.Columns.Add;
        with NeigbDevListV.Columns[NeigbDevListV.Columns.Count - 1] do begin
            Caption := NeighbDevRowHdrs[I];
            Width := NeighbDevRowWid[I];
        end;
    end;
    IpConnsListV.Columns.Clear;
    for I := Low(IpConnsRowHdrs) to High(IpConnsRowHdrs) do begin
        IpConnsListV.Columns.Add;
        with IpConnsListV.Columns[IpConnsListV.Columns.Count - 1] do begin
            Caption := IpConnsRowHdrs[I];
            Width := IpConnsRowWid[I];
        end;
    end;
    AdaptorListV.Columns.Clear;
    for I := Low(IpAdptRowHdrs) to High(IpAdptRowHdrs) do begin
        AdaptorListV.Columns.Add;
        with AdaptorListV.Columns[AdaptorListV.Columns.Count - 1] do begin
            Caption := IpAdptRowHdrs[I];
            Width := IpAdptRowWid[I];
        end;
    end;

    InterfacesListV.Columns.Clear;
    for I := Low(IpIfaceRowHdrs) to High(IpIfaceRowHdrs) do begin
        InterfacesListV.Columns.Add;
        with InterfacesListV.Columns[InterfacesListV.Columns.Count - 1] do begin
            Caption := IpIfaceRowHdrs[I];
            Width := IpIfaceRowWid[I];
        end;
    end;

    IpAddressListV.Columns.Clear;
    for I := Low(IpAddrRowHdrs) to High(IpAddrRowHdrs) do begin
        IpAddressListV.Columns.Add;
        with IpAddressListV.Columns[IpAddressListV.Columns.Count - 1] do begin
            Caption := IpAddrRowHdrs[I];
            Width := IpAddrRowWid[I];
        end;
    end;

    IpPathsListV.Columns.Clear;
    for I := Low(IpPathRowHdrs) to High(IpPathRowHdrs) do begin
        IpPathsListV.Columns.Add;
        with IpPathsListV.Columns[IpPathsListV.Columns.Count - 1] do begin
            Caption := IpPathRowHdrs[I];
            Width := IpPathRowWid[I];
        end;
    end;

    IpRoutingListV.Columns.Clear;
    for I := Low(IpRoutingRowHdrs) to High(IpRoutingRowHdrs) do begin
        IpRoutingListV.Columns.Add;
        with IpRoutingListV.Columns[IpRoutingListV.Columns.Count - 1] do begin
            Caption := IpRoutingRowHdrs[I];
            Width := IpRoutingRowWid[I];
        end;
    end;

    NeighbListV.Columns.Clear;
    for I := Low(NeighbRowHdrs) to High(NeighbRowHdrs) do begin
        NeighbListV.Columns.Add;
        with NeighbListV.Columns[NeighbListV.Columns.Count - 1] do begin
            Caption := NeighbRowHdrs[I];
            Width := NeighbRowWid[I];
        end;
    end;

  // build drop down list of Whois servers, not needed if only automatic queries are being done
    if IcsWhoisCli1.WhoisServers.Count > 0 then begin
        WhoisServer.Items.Clear;
        for I := 0 to IcsWhoisCli1.WhoisServers.Count - 1 do begin
            S1 := IcsWhoisCli1.WhoisServers[I];
            J := Pos(IcsSpace, S1);
            if J = 0 then
                WhoisServer.Items.Add(S1)
            else begin
                S2 := Copy(S1, 1, J);
                if S2 = '00 ' then
                    S2 := 'IP Addresses]'
                else if S2 = '?? ' then
                    S2 := 'Top Level Domains]'
                else
                    S2 := S2 + 'Domains]';
                WhoisServer.Items.Add(Copy(S1, J + 1, 99) + ' [' + S2);
            end;
        end;
        WhoisServer.ItemIndex := 0;
    end;

// fill Dns Query drop down
    NameQueryType.Items.Clear;
    for I := Low(DnsReqTable) to High(DnsReqTable) do
         NameQueryType.Items.Add (DnsReqTable[I].Asc + ' [' + DnsReqTable[I].Desc + ']');
    NameQueryType.ItemIndex := 0;

 // fill DNS server drop down
    NameServerPublic.Items.Clear;
    for I := Low(DnsPublicServerTable) to High(DnsPublicServerTable) do
         NameServerPublic.Items.Add (DnsPublicServerTable[I]);
    NameServerPublic.ItemIndex := 0;

 // fill DOH server drop down edit
    NameServerHttps.Items.Clear;
    for I := Low(DnsPublicHttpsTable) to High(DnsPublicHttpsTable) do
         NameServerHttps.Items.Add (DnsPublicHttpsTable[I]);
    if NameServerHttps.Text = '' then
        NameServerHttps.Text := NameServerHttps.Items[0];

// get Network Params, DNS servers in particular, ignore error, Auto DNS drop
    IpHlpNetworkParams(IcsNetworkParams);
    if (IcsNetworkParams.DnsServerTot > 0) then begin

 // update IcsDomainNameCache server list and NameServerAuto drop down list
        IpHlpGetDnsServers(IcsDomainNameCache1.DnsServerList);
        NameServerAuto.Items.Assign(IcsDomainNameCache1.DnsServerList);
        NameServerAuto.ItemIndex := 0;
        IcsDomainNameCache1.DnsServerStrat := SrvStratList;
    end;

   { beware these names are stored in INI file so only refreshed if the INI lines are removed }
    if BulkNamesMemo.Lines.Count = 0 then begin
        BulkNamesMemo.Text :=
        'pool.ntp.org'#13#10 +
        'uk.pool.ntp.org'#13#10 +
        'www.overbyte.eu'#13#10 +
        'svn.overbyte.be'#13#10 +
        'wiki.overbyte.eu'#13#10 +
        'mail.magsys.co.uk'#13#10 +
        'ipv4.magsys.co.uk'#13#10 +
        'ipv6.magsys.co.uk'#13#10 +
        'www.embarcardero.com'#13#10 +
        'www.microsoft.com'#13#10 +
        'www.msftconnecttest.com'#13#10 +
        'ipv6.msftconnecttest.com'#13#10 +
        'ipv6.google.com'#13#10 +
        'www.google.com'#13#10 +
        'sourceforge.net'#13#10 +
        'msdn.microsoft.com'#13#10 +
        'localhost'#13#10 +
        'strøm.no'#13#10 +
        'www.mâgsÿstést.eu'#13#10 +
        'www.háčkyčárky.cz'#13#10 +         { needs Unicode }
        'stránky.háčkyčárky.cz'#13#10 +     { needs Unicode }
        'мособлеирц.рф'#13#10 +             { needs Unicode }
        '例子.测试'#13#10 +                 { needs Unicode, lookup fails }
        'www.-badname.com'#13#10;
    end;

// reset page control
   PageControl1.ActivePage := TabConnections;

// start update timer
    IpConnUpdateTrg := Trigger64Disabled;
    NameServerTrg := Trigger64Disabled;
    CommonTimer.Interval := 1000;
    CommonTimer.Enabled := True;
    MonIpAddrChangesClick(Self);    // start IP address change monitor
end;

procedure TToolsForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile: TIcsIniFile;
    section, temp: String;
begin
    CommonTimer.Enabled := False;
//    IcsIpChanges1.StopMonitor;
    if IcsNeighbDevices1.Active then
        IcsNeighbDevices1.StopMonitor;
    if FormLog.Visible then
        FormLog.FormClose(Self, Action);
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do begin
    // form positions
        WriteInteger(SectionMainWindow, KeyTop, Top);
        WriteInteger(SectionMainWindow, KeyLeft, Left);
        WriteInteger(SectionMainWindow, KeyWidth, Width);
        WriteInteger(SectionMainWindow, KeyHeight, Height);
        section := SectionData;
  WriteInteger (section, 'DNCacheLAN_ItemIndex', DNCacheLAN.ItemIndex) ;
  if DNCacheLog.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'DNCacheLog_Checked', temp) ;
  WriteString (section, 'DNCacheMax_Text', DNCacheMax.Text) ;
  WriteInteger (section, 'DNCacheMethod_ItemIndex', DNCacheMethod.ItemIndex) ;
  WriteInteger (section, 'IpConnType_ItemIndex', IpConnType.ItemIndex) ;
  WriteString (section, 'IpConnsSecs_Text', IpConnsSecs.Text) ;
  WriteInteger (section, 'IpFamily_ItemIndex', IpFamily.ItemIndex) ;
  WriteString (section, 'LocalIPv4_Text', LocalIPv4.Text) ;
  WriteString (section, 'LocalIPv6_Text', LocalIPv6.Text) ;
  WriteString (section, 'LookupHost_Text', LookupHost.Text) ;
  WriteString (section, 'LookupHost_Items', LookupHost.Items.CommaText) ;
  if MonIpAddrChanges.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'MonIpAddrChanges_Checked', temp) ;
  WriteInteger (section, 'NameDnsOpt_ItemIndex', NameDnsOpt.ItemIndex) ;
  WriteString (section, 'NameHostName_Text', NameHostName.Text) ;
  WriteString (section, 'NameHostName_Items', NameHostName.Items.CommaText) ;
  WriteInteger (section, 'NameQueryType_ItemIndex', NameQueryType.ItemIndex) ;
  WriteString (section, 'NameHostName_Items', NameHostName.Items.CommaText) ;
  if NameProtoTCP.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'NameProtoTCP_Checked', temp) ;
  WriteString (section, 'NameServerIP_Text', NameServerIP.Text) ;
  WriteString (section, 'NameServerIP_Items', NameServerIP.Items.CommaText) ;
  WriteInteger (section, 'NameServerPublic_ItemIndex', NameServerPublic.ItemIndex) ;
  WriteString (section, 'NeigbDevCache_Text', NeigbDevCache.Text) ;
  WriteString (section, 'NeigbDevIPv4_Text', NeigbDevIPv4.Text) ;
  WriteString (section, 'NeigbDevIPv6_Text', NeigbDevIPv6.Text) ;
  WriteString (section, 'NeigbDevScan_Text', NeigbDevScan.Text) ;
  WriteString (section, 'NeigbDevTot_Text', NeigbDevTot.Text) ;
  if NeighbPermAddr.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'NeighbPermAddr_Checked', temp) ;
  WriteString (section, 'PingCount_Text', PingCount.Text) ;
  if PingLog.Checked then temp := 'True' else temp := 'False' ; WriteString (section, 'PingLog_Checked', temp) ;
  WriteString (section, 'PingQueryName_Text', PingQueryName.Text) ;
  WriteString (section, 'PingQueryName_Items', PingQueryName.Items.CommaText) ;
  WriteString (section, 'PingTimeoutMs_Text', PingTimeoutMs.Text) ;
  WriteString (section, 'ResolveIpRem_Text', ResolveIpRem.Text) ;
  WriteString (section, 'RevExtra_Text', RevExtra.Text) ;
  WriteString (section, 'RevLookAddr_Text', RevLookAddr.Text) ;
  WriteString (section, 'RevLookAddr_Items', RevLookAddr.Items.CommaText) ;
  WriteString (section, 'TraceErrors_Text', TraceErrors.Text) ;
  WriteString (section, 'WhoisQueryName_Text', WhoisQueryName.Text) ;
  WriteString (section, 'WhoisQueryName_Items', WhoisQueryName.Items.CommaText) ;
  WriteString (section, 'WhoisServer_Text', WhoisServer.Text) ;
  WriteString (section, 'WhoisServer_Items', WhoisServer.Items.CommaText) ;
        UpdateFile;
    end;
    IniFile.Free;
//
end;


procedure TToolsForm.OnNeigbDevLogEvent(Sender: TObject; const Msg: string);
begin
    AddLogLine(Msg);
end;


procedure TToolsForm.OnNeigbDevDevUpd(Sender: TObject);
begin
    NeigbDevListV.Items.Count := IcsNeighbDevices1.IpTot;
    NeigbDevListV.Refresh;
 //   NeigbDevListV.UpdateItems(GridCount, GridCount) ;
end;


// virtual listView data event, get and display a row of data
procedure TToolsForm.NeigbDevListVData(Sender: TObject; Item: TListItem);
var
    ItemNr, Idx, AddrNr: Integer;
begin
    ItemNr := Item.Index ;  // line for which data is required
    if ItemNr >= IcsNeighbDevices1.IpTot then
        Exit;
    Idx := IcsNeighbDevices1.IpRow[ItemNr, AddrNr];
    if Idx >=  IcsNeighbDevices1.MacTot then
        Exit;
    with IcsNeighbDevices1.NeighbDevices[Idx] do begin
        if (AddrNr < 0) or (AddrNr >= TotAddrNr) then
            AddrNr := 0;
        Item.Caption := AddrList[AddrNr].IpAddr;
        Item.SubItems.Add(AddrList[AddrNr].HostName);
        Item.SubItems.Add(MacAddress);
        Item.SubItems.Add(MacVendor);
        Item.SubItems.Add(StateDesc);
        Item.SubItems.Add(DateTimetoStr(FirstDT));
        Item.SubItems.Add(DateTimetoStr(LastDT));
        Item.SubItems.Add(InttoStr(Detections));
    end;
end;


procedure TToolsForm.doNeigbDevStartClick(Sender: TObject);
begin
    if LocalIpLuid.Value = 0  then
             LocalIpLuid := IpHlpAdpforIP (LocalIPv4.Text);  // assume IPv6 same interface
    if LocalIpLuid.Value = 0 then
        Exit;
    doNeigbDevStart.Enabled := False;
    doNeigbDevStop.Enabled := True;
    IcsNeighbDevices1.LocalIps := True;
    IcsNeighbDevices1.InterfaceLuid := LocalIpLuid;
    IcsNeighbDevices1.LocIpv4 := LocalIPv4.Text;
    IcsNeighbDevices1.LocIpv6 := LocalIPv6.Text;
    IcsNeighbDevices1.ScanStartIPv4 := NeigbDevIPv4.Text;
    IcsNeighbDevices1.ScanStartIPv6 := NeigbDevIPv4.Text;
    IcsNeighbDevices1.ScanTotIps := atoi(NeigbDevTot.Text);
    IcsNeighbDevices1.UpdCacheSecs := atoi(NeigbDevCache.Text);
    IcsNeighbDevices1.UpdScanSecs := atoi(NeigbDevScan.Text);
    IcsNeighbDevices1.OnDevUpd := OnNeigbDevDevUpd;
    IcsNeighbDevices1.OnLogEvent := OnNeigbDevLogEvent;
    IcsNeighbDevices1.StartMonitor;
end;


procedure TToolsForm.doNeigbDevStopClick(Sender: TObject);
begin
    doNeigbDevStart.Enabled := True;
    doNeigbDevStop.Enabled := False;
    IcsNeighbDevices1.StopMonitor;
end;



procedure TToolsForm.doDeviceEmptyCacheClick(Sender: TObject);
begin
    if NOT IcsIsProgAdmin then begin
        AddLogLine('This function requires program administrator rights');
        Exit;
    end;
    IcsNeighbDevices1.EmptyCache;
    AddLogLine('Neighbour Cache Emptied');

end;

procedure TToolsForm.doDeviceLogIpClick(Sender: TObject);
begin
    IcsNeighbDevices1.DispIpDevs(WorkStrings);
    AddLogLines(WorkStrings);
end;

procedure TToolsForm.doDeviceLogMacClick(Sender: TObject);
begin
    IcsNeighbDevices1.DispMacDevs(WorkStrings);
    AddLogLines(WorkStrings);
end;

procedure TToolsForm.doDevicesClearClick(Sender: TObject);
begin
    IcsNeighbDevices1.Clear;
    AddLogLine('Neighbour Devices Cleared');
end;

procedure TToolsForm.doExitClick(Sender: TObject);
begin
    Close;
end;

function TToolsForm.GetIpFamilyAF: Integer;
begin
    Result := WSocketFamilyToAF(TSocketFamily(IpFamily.ItemIndex));
end;

procedure TToolsForm.doIpConnStartClick(Sender: TObject);
begin
    if atoi(IpConnsSecs.Text) < 2 then
        Exit;  // interval sanity check
    doIpConnStart.Enabled := False;
    doIpConnStop.Enabled := True;
    IpConnUpdateTrg := Trigger64Immediate;
end;

procedure TToolsForm.doIpConnStopClick(Sender: TObject);
begin
    IpConnUpdateTrg := Trigger64Disabled;
    doIpConnStop.Enabled := False;
    doIpConnStart.Enabled := True;
end;

procedure TToolsForm.IpConnNameUpdateEvent(Sender: TObject; ItemNr: Integer);
var
    Row: Integer;
begin
// IP connections, report reverse lookup of remote IP address
    with IcsDomainNameCache1.GetDNItem(ItemNr) do begin
        Row := ReqTag;
        if Row >= Length(IpConnRows) then
            Exit;
        IpConnRows[Row].RemoteHost := IcsDomainNameCache1.BuildRespList(ItemNr);
    end;
end;

procedure TToolsForm.doIpConnUpdateClick(Sender: TObject);
var
    ErrorCode, I, Tot: Integer;
begin
  ErrorCode := IpHlpConnsTable(IpConnRows, GetIpFamilyAF, TTransProt(IpConnType.ItemIndex));
  if ErrorCode <> NO_ERROR then begin
     AddLogLine(SysErrorMessage (ErrorCode));
     exit ;
  end;
  Tot := Length(IpConnRows);
  if Tot = 0 then
    Exit;
  for I := 0 to Tot - 1 do begin
    with IpConnRows[I] do begin
        if (RemoteAddr <> '') and (RemoteAddr <> '0.0.0.0') then begin
         // if not cached, event is called to complete RemoteHost
            RemoteHost := IcsDomainNameCache1.LookupIPOne(RemoteAddr, I, sfAny, IpConnNameUpdateEvent);
            Application.ProcessMessages;
        end;
    end;
  end;
  IpConnsListV.Items.Count := Tot;
  IpConnsListV.Refresh;
end;


procedure TToolsForm.IpConnsListVData(Sender: TObject; Item: TListItem);
var
    ItemNr: Integer;
    DispTime: String;
begin
    ItemNr := Item.Index ;  // line for which data is required
    if ItemNr >= Length(IpConnRows) then
        Exit;
    with IpConnRows[ItemNr] do begin
        DispTime := '' ;
        if CreateDT > 0 then
            DispTime := DateTimeToStr (CreateDT) ;
        Item.Caption := Protocol;
        Item.SubItems.Add(LocalAddr + ':' + Lowercase(LocalPortDesc));
        if State >= 0 then begin  // TCP
            if State <> MIB_TCP_STATE_LISTEN then begin
                Item.SubItems.Add(RemoteAddr + ':' + Lowercase(RemotePortDesc));
                Item.SubItems.Add(RemoteHost);
            end
            else begin
                Item.SubItems.Add('');
                Item.SubItems.Add('');
            end;
            Item.SubItems.Add(TCPConnState[State])
        end
        else begin
            Item.SubItems.Add('');
            Item.SubItems.Add('');
            Item.SubItems.Add('');
        end;
        Item.SubItems.Add(IntToStr(ProcessId) + ':' + ExtractFileName (ProcName));
        Item.SubItems.Add(DispTime);
    end;
end;

// event called when TIcsIpChanges component detects IP addresses added or deleted
procedure TToolsForm.IcsIpChanges1IpChangesEvent(IpAddrInfo: TIpAddrInfo; NotificationType: TMibNoticationType);
begin
    if NotificationType = MibInitialNotification then
    //    AddLogLine('Waiting for IP address notification change events')
    else if  NotificationType = MibAddInstance then begin
        AddLogLine('New IP address added: ' + IpAddrInfo.IpAddress);
        doUpdIpAddrClick(Self);
        SetLocalIPs;
    end
    else if  NotificationType = MibDeleteInstance then begin
        AddLogLine('Old IP address removed: ' + IpAddrInfo.IpAddress);
        doUpdIpAddrClick(Self);
        SetLocalIPs;
    end;
end;


// start TIcsIpChanges component to detects IP addresses added or deleted
procedure TToolsForm.MonIpAddrChangesClick(Sender: TObject);
begin
    if MonIpAddrChanges.Checked then begin
        IcsIpChanges1.StartMonitor;
        AddLogLine('Started Monitoring IP Address Changes');  { V9.1 }
    end
    else begin
        IcsIpChanges1.StopMonitor;
        AddLogLine('Stopped Monitoring IP Address Changes');  { V9.1 }
    end;
end;

procedure TToolsForm.doLogIpConnsClick(Sender: TObject);
begin
    Get_ConnsTable(WorkStrings, GetIpFamilyAF, TTransProt(IpConnType.ItemIndex));
    AddLogLines(WorkStrings);
end;

procedure TToolsForm.doUpdAdaptorsClick(Sender: TObject);
var
  AdpTot: integer;
  AdpRows: TAdaptorRows;
  Error: DWORD ;
  I, J: integer ;
  ListIps, ListPref, ListDns, ListGate: string;
begin
    SetLength (AdpRows, 0) ;
    AdpTot := 0 ;
    Error := IpHlpAdaptersAddr(GetIpFamilyAF, AdpTot, AdpRows) ;
    if (Error <> 0) then begin
        AddLogLine(SysErrorMessage(GetLastError));
    end;
    if AdpTot > 0 then begin
        AdaptorListV.Items.Clear;
        for I := 0 to AdpTot - 1 do begin
            ListIps := '';
            ListPref := '';
            ListDns := '';
            ListGate := '';
            with AdpRows[I] do begin
                if IPAddressTot <> 0 then begin
                    for J := 0 to Pred (IPAddressTot) do begin
                        ListIps := ListIps + IPAddressList [J] ;
                        if IPMaskList [J] <> '' then
                            ListIps := ListIps + '/' + IPMaskList [J] + ' | '
                        else
                            ListIps := ListIps + ' | ';
                    end;
                end ;
                if PrefixTot <> 0 then begin
                    for J := 0 to Pred (PrefixTot) do begin
                        ListPref := ListPref + PrefixIPAddrList [J] ;
                        if PrefixMaskList [J] <> '' then
                            ListPref := ListPref + '=' + PrefixMaskList [J] + ' | '
                        else
                            ListPref := ListPref + ' | ';
                    end;
                end ;
                if DNSServerTot <> 0 then begin
                    for J := 0 to Pred (DNSServerTot) do
                        ListDns := ListDns + DNSServerList [J] + ' | ';
                end ;
                if GatewayTot <> 0 then begin
                    for J := 0 to Pred (GatewayTot) do
                        ListGate := ListGate + GatewayList [J] + ' | ';
                end ;
//  IpAdptRowHdrs: array[0..21] of String =
//     ('Index','Interface','Description','Friendly Name','Type', 'MAC Address','Vendor','DHCP',
//     'Tot IPs','IP Addresse(s)','IP Prefixe(s)','DNS Server(s)','Gateway(s)','DHCP Server', 'WINS Server',
//     'Metric','Op Status','DND Suffix','Xmit Speed','Conn Type', 'Tunnel', 'GUID'  );
                with AdaptorListV.Items.Add do begin
                    Caption := IntToStr(Index);
                    SubItems.Add(InterfaceName);
                    SubItems.Add(Description);
                    SubItems.Add(FriendlyName);
                    SubItems.Add(AdaptTypes [aType]);
                    SubItems.Add(MacAddress);
                    SubItems.Add(MacVendor);
                    SubItems.Add(IcsGetYN(DHCPEnabled));
                    SubItems.Add(IntToStr(IPAddressTot));
                    SubItems.Add(ListIps);
                    SubItems.Add(ListPref);
                    SubItems.Add(ListDns);
                    SubItems.Add(ListGate);
                    SubItems.Add(DHCPServer [0]);
                    SubItems.Add(PrimWINSServer [0]);
                    SubItems.Add(IntToStr(Ipv4Metric));
                    SubItems.Add(IfOperStatuses [Ord (OperStatus)]);
                    SubItems.Add(DnsSuffix);
                    SubItems.Add(IntToStr(XmitLinkSpeed));
                    SubItems.Add(NetIfConnectionTypes [Ord (ConnectionType)]);
                    SubItems.Add(TunnelTypes [Ord (TunnelType)]);
                    SubItems.Add(AdapterName) ;
                end;
            end ;
        end;
    end;

// network params
    NetParamsListV.Items.Clear;
    with IcsNetworkParams do begin
        with NetParamsListV.Items.Add do begin
            Caption := 'Host Name';
            SubItems.Add(HostName) ;
        end;
        with NetParamsListV.Items.Add do begin
            Caption := 'Domain Name';
            SubItems.Add(DomainName) ;
        end;
        if DnsServerTot <> 0 then begin
            for I := 0 to Pred (DnsServerTot) do begin
                with NetParamsListV.Items.Add do begin
                    Caption := 'DNS Server ' + IntToStr(I);
                    SubItems.Add(DnsServerNames [I]);
                end;
            end;
        end;
        with NetParamsListV.Items.Add do begin
            Caption := 'DHCP Scope';
            SubItems.Add(ScopeID) ;
        end;
        with NetParamsListV.Items.Add do begin
            Caption := 'NetBios Node Type';
            SubItems.Add(NETBIOSTypes[NodeType]) ;
        end;
        with NetParamsListV.Items.Add do begin
            Caption := 'Routing Enabled';
            SubItems.Add(IcsGetYN(EnableRouting)) ;
        end;
        with NetParamsListV.Items.Add do begin
            Caption := 'Proxy Enabled';
            SubItems.Add(IcsGetYN(EnableProxy));
        end;
        with NetParamsListV.Items.Add do begin
            Caption := 'DNS Enabled';
            SubItems.Add(IcsGetYN(EnableDNS));
        end;
    end;
    SetLength (AdpRows, 0) ;
end;

procedure TToolsForm.doLogAdaptorsClick(Sender: TObject);
begin
    Get_AdaptersInfo(WorkStrings);
    AddLogLines(WorkStrings);
end;

procedure TToolsForm.doLogNetParamsClick(Sender: TObject);
begin
    Get_NetworkParams(WorkStrings);
    AddLogLines(WorkStrings);
end;

procedure TToolsForm.doUpdInterfacesClick(Sender: TObject);
var
    IfRows2: TIfRows2 ;
    Error, I, J, NumEntries: integer ;
    IpAddrInfos: TIpAddrInfos ;
    ListIps: string ;
begin
    SetLength (IfRows2, 0) ;
    Error := IpHlpIfTable2 (NumEntries, IfRows2) ;
    if (Error <> 0) then
        AddLogLine(SysErrorMessage( GetLastError ))
    else if NumEntries > 0 then begin
       InterfacesListV.Items.Clear;
        for I := 0 to Pred (NumEntries) do begin
            with IfRows2 [I] do
            begin
            ListIps := '';
                if IpHlpIpAddrTable (IpAddrInfos, GetIpFamilyAF, True, false, Mib.InterfaceIndex) = 0 then
                begin
                    if Length (IpAddrInfos) <> 0 then
                    begin
                        for J := 0 to Pred (Length (IpAddrInfos)) do
                        begin
                            with IpAddrInfos [J] do
                            begin
                            ListIps := ListIps + IpAddress ;
                            if IPMask <> '' then
                                ListIps := ListIps + '=' + IPMask + ' | '
                            else
                                ListIps := ListIps + ' | ';
                            end;
                        end;
                    end ;
                end;
//  IpIfaceRowHdrs: array[0..22] of String =
//     ('Index', 'Interface', 'Description', 'Friendly Name', 'Type', 'MAC Address','Vendor',
//     'Tot IPs','IP Addresse(s)', 'MTU', 'Speed', 'Admin St', 'Oper Status', 'Media Type',  'Phys Medium','Access Type',
//     'Direction', 'Interface/Oper Status', 'Conn State', 'Conn Type', 'In Octets', 'Out Octets', 'Tunnel' );
                with InterfacesListV.Items.Add do begin
                    Caption := IntToStr(Index);
                    SubItems.Add(InterfaceName);
                    SubItems.Add(Description);
                    SubItems.Add(FriendlyName);
                    SubItems.Add(AdaptTypes [Mib.IfType]);
                    SubItems.Add(MacAddress);
                    SubItems.Add(MacVendor);
                    SubItems.Add(IntToStr(Length (IpAddrInfos)));
                    SubItems.Add(ListIps);
                    SubItems.Add(IntToStr(Mib.MTU));
                    SubItems.Add(IntToStr(Mib.TransmitLinkSpeed));
                    SubItems.Add(AdminStatuses [Ord (Mib.AdminStatus)]);
                    SubItems.Add(IfOperStatuses [Ord (Mib.OperStatus)]);
                    SubItems.Add(NdisMediums [Ord (Mib.MediaType)]);
                    SubItems.Add(NdisPhysicalMediums [Ord (Mib.PhysicalMediumType)]);
                    SubItems.Add(NetIfAccessTtypes [Ord (Mib.AccessType)]);
                    SubItems.Add(NetIfDirectionTypes [Ord (Mib.DirectionType)]);
                    SubItems.Add(GetIfoFlags (Mib.InterfaceAndOperStatusFlags));
                    SubItems.Add(NetIfMediaConnectStates [Ord (Mib.MediaConnectState)]);
                    SubItems.Add(NetIfConnectionTypes  [Ord (Mib.ConnectionType)]);
                    SubItems.Add(IntToStr(Mib.InOctets));
                    SubItems.Add(IntToStr(Mib.OutOctets));
                    SubItems.Add(TunnelTypes [Ord (Mib.TunnelType)]);
                end;
                SetLength (IpAddrInfos, 0) ;  // free memory
            end;
        end ;
    end ;
    SetLength (IfRows2, 0) ;
end;

procedure TToolsForm.doLogInterfacesClick(Sender: TObject);
begin
    Get_IfTable2(WorkStrings);
    AddLogLines(WorkStrings);
end;

procedure TToolsForm.doUpdIpAddrClick(Sender: TObject);
var
    IpAddrInfos: TIpAddrInfos ;
    ErrorCode, NumEntries, I: integer;
begin
    ErrorCode := IpHlpIpAddrTable (IpAddrInfos, GetIpFamilyAF) ;
    if ErrorCode <> NO_ERROR then begin
        AddLogLine(SysErrorMessage (ErrorCode));
    end
    else begin
        NumEntries := Length (IpAddrInfos) ;
        IpAddressListV.Items.Clear;
        if NumEntries > 0 then begin
//  IpAddrRowHdrs: array[0..7] of String =
//    ('IP Address', 'IP Mask', 'Type', 'Interface', 'MAC Address','Vendor','Description', 'Friendly Name');
            for I:= 0 to Pred (NumEntries) do begin
                with IpAddrInfos [I] do begin
                    with IpAddressListV.Items.Add do begin
                        Caption := IpAddress;
                        SubItems.Add(IpMask);
                        SubItems.Add(TypeStr);
                        SubItems.Add(InterfaceName);
                        SubItems.Add(MacAddress);
                        SubItems.Add(MacVendor);
                        SubItems.Add(Description);
                        SubItems.Add(FriendlyName);
                    end;
                end;
            end;
        end;
    end ;
    SetLength (IpAddrInfos, 0) ;
end;

procedure TToolsForm.doLogIpAddrClick(Sender: TObject);
begin
    Get_IPAddrTable(WorkStrings);
    AddLogLines(WorkStrings);
end;

procedure TToolsForm.dpUpdIpRoutingClick(Sender: TObject);
var
    IpRouteRows: TIpRouteRows;
    I, NumEntries, ErrorCode: Integer;
begin
    ErrorCode := IpHlpIPForwardTable(IpRouteRows, GetIpFamilyAF);
    if ErrorCode <> NO_ERROR then begin
        AddLogLine(SysErrorMessage (ErrorCode));
    end
    else begin
        NumEntries := Length(IpRouteRows);
        if NumEntries > 0 then begin
            IpRoutingListV.Items.Clear;
            for I := 0 to NumEntries - 1 do begin
                with IpRouteRows[I] do begin
                    with IpRoutingListV.Items.Add do begin
                        Caption := DestIpAddr;
                        SubItems.Add(DestMask);
                        SubItems.Add(NextIpAddr);
                        SubItems.Add(ProtDesc);
                        SubItems.Add(IntToStr(Metric));
                        SubItems.Add(OrigDesc);
                        SubItems.Add(InterfaceName);
                        SubItems.Add(IfTypeDesc);
                    end;
                end ;
            end;
        end;
    end;
    SetLength(IpRouteRows, 0);
end;

procedure TToolsForm.doLogIpRoutingClick(Sender: TObject);
begin
    Get_IPForwardTable(WorkStrings);
    AddLogLines(WorkStrings);
end;

procedure TToolsForm.doUpdIpPathsClick(Sender: TObject);
var
    IpPathRows: TIpPathRows;
    I, NumEntries, ErrorCode: Integer;
begin
    ErrorCode := IpHlpIPPathTable(IpPathRows, GetIpFamilyAF);
    if ErrorCode <> NO_ERROR then begin
        AddLogLine(SysErrorMessage (ErrorCode));
    end
    else begin
        IpPathsListV.Items.Clear;
        NumEntries := Length(IpPathRows);
//  IpPathRowHdrs: array[0..9] of String =
//    ('Source Address', 'Destination Address', 'Next Hop Address', 'MTU', 'RTT', 'Reach', 'Link Xmit', 'Link Recv', 'Interface','IF Type');
        if NumEntries > 0 then begin
            for I := 0 to NumEntries - 1 do begin
                with IpPathRows[I] do begin
                    with IpPathsListV.Items.Add do begin
                        Caption := SourceIpAddr;
                        SubItems.Add(DestIpAddr);
                        SubItems.Add(CurNextHopIpAddr);
                        SubItems.Add(IntToStr(PathMtu));
                        SubItems.Add(IntToStr(RttMean));
                        SubItems.Add(IcsGetYN(IsReachable));
                        SubItems.Add(IntToStr(LinkTransmitSpeed));
                        SubItems.Add(IntToStr(LinkReceiveSpeed));
                        SubItems.Add(InterfaceName);
                        SubItems.Add(IfTypeDesc);
                    end;
                end ;
            end;
        end
    end;
    SetLength(IpPathRows, 0);
end;

procedure TToolsForm.doLogIpPathClick(Sender: TObject);
begin
    Get_IpPathTable(WorkStrings);
    AddLogLines(WorkStrings);
end;

procedure TToolsForm.doUpdNeigbIpsClick(Sender: TObject);
var
    NeighbRows: TNeighbRows;
    ErrorCode: DWORD;
    NumEntries, I: Integer;
begin
    ErrorCode := IpHlpIpNeighbTable(NeighbRows, GetIpFamilyAF);
    if ErrorCode <> NO_ERROR then begin
        AddLogLine(SysErrorMessage (ErrorCode));
    end
    else begin
        NeighbListV.Items.Clear;
        NumEntries := Length (NeighbRows) ;
        if NumEntries > 0 then begin
            for I := 0 to Pred (NumEntries) do begin
                with NeighbRows [I] do begin
                    if (NOT NeighbPermAddr.Checked) and (State = NlnsPermanent) then
                        continue;
                    with NeighbListV.Items.Add do begin
                        Caption := MacAddress;
                        SubItems.Add(MacVendor);
                        SubItems.Add(IpAddress);
                        SubItems.Add(StateDesc);
                        SubItems.Add(InterfaceName);
                        SubItems.Add(IfTypeDesc);
                        SubItems.Add(IntToStr(ReachSecs));
                    end;
                end;
            end;
        end;
    end;
    SetLength (NeighbRows, 0);
end;

procedure TToolsForm.doLogNeighbIPsClick(Sender: TObject);
begin
    Get_IPNeighbourTable(WorkStrings, NeighbPermAddr.Checked);
    AddLogLines(WorkStrings);
end;

procedure TToolsForm.doLogARPClick(Sender: TObject);
begin
    Get_ARPTable(WorkStrings);
    AddLogLines(WorkStrings);
end;

procedure TToolsForm.doUpdStatsClick(Sender: TObject);
begin
    Get_TCPStatistics(ListStatsTCP.Items);
    Get_UDPStatistics(ListStatsUDP.Items);
    Get_ICMPStats(ListStatsIcmpIn.Items, ListStatsIcmpOut.Items);
end;


procedure TToolsForm.doListStatsClick(Sender: TObject);
var
    WorkStrings2: TStringList;
begin
    Get_TCPStatistics(WorkStrings);
    AddLogLines(WorkStrings);
    Get_UDPStatistics(WorkStrings);
    AddLogLines(WorkStrings);
    WorkStrings2 := TStringList.Create;
    Get_ICMPStats(WorkStrings, WorkStrings2);
    AddLogLines(WorkStrings);
    AddLogLines(WorkStrings2);
    WorkStrings2.Free;
 end;


procedure TToolsForm.doResolveIPClick(Sender: TObject);
var
    Error: Integer;
    NeighbRow: TNeighbRow;
    RemIP: String;
begin
    RemIP := ResolveIpRem.Text;
    if LocalIpLuid.Value = 0  then
             LocalIpLuid := IpHlpAdpforIP(LocalIPv4.Text);
    if LocalIpLuid.Value = 0 then
        Exit;
    Error := IpHlpResolveIpNet(RemIP, LocalIPv4.Text, LocalIpLuid, NeighbRow);
    if Error <> NO_ERROR then begin
        AddLogLine('Failed to Resolve IP ' + RemIP + ' - ' + SysErrorMessage (Error));
    end
    else begin
        AddLogLine('Resolved IP ' + RemIP + ', MAC ' + NeighbRow.MacAddress + ' - ' + NeighbRow.MacVendor);
    end;

end;


procedure TToolsForm.doNetBIOSClick(Sender: TObject);
var
    RemIP, Host: String;
begin
    RemIP := ResolveIpRem.Text;
    Host := GetNetBiosHostByAddr(RemIP);
    if Host = '' then
        AddLogLine('Failed to Find ' + RemIP)
    else
        AddLogLine('NetBios Name for IP ' + RemIP + ' - ' + Host);
end;


procedure TToolsForm.PingSetButtons (Enable: Boolean);
begin
//    StopFlag := False ;
    doPingSync.Enabled := Enable;
    doPingThread.Enabled := Enable;
    doTraceRoute.Enabled := Enable;
    doPingAbort.Enabled := NOT Enable;
end;


procedure TToolsForm.PingNameUpdateEvent(Sender: TObject; ItemNr: Integer);
var
    Row: Integer;
    S: String;
begin
// trace route, report reverse lookup of IP address for each hop
    with IcsDomainNameCache1.GetDNItem(ItemNr) do begin
        Row := ReqTag;
        if Row >= PingListView.Items.Count then
            Exit;
        S := IcsDomainNameCache1.BuildRespList(ItemNr);
        if Row >= 0 then
            PingListView.Items[row].SubItems[2] := S;
    end;
    if PendingPings > 0 then
        dec (PendingPings);
end;


procedure TToolsForm.PingThreadTermPing(Sender: TObject);
const
    response1 = 'Thread %d for %s, %s [%d]';
    response2 = 'Thread %d for %s, received %d bytes from %s in %dms';
begin
    if PendingPings > 0 then
        dec (PendingPings);
    if StopFlag then
        exit;
    if Application.Terminated then
        exit ;
 // this event is thread safe, all publics from the thread are available here
    with Sender as TPingThread do begin
        ThreadIds [PingId] := 0;  // clear id so we don't try and kill it later

    // update ListView columns
        if PingId <= PingListView.Items.Count then begin
            with PingListView.Items [PingId - 1] do begin
                Caption := IntToStr(PingId);
                if ReplyTotal <> 0 then begin
                    SubItems.Add(IntToStr(ReplyRTT) + 'ms');
                    SubItems.Add(PingHostName);
                    SubItems.Add(ReplyIPAddr);
                end
                else begin
                    SubItems.Add('');
                    SubItems.Add(PingHostName);
                    SubItems.Add(ErrString);
                end;
                if (ReplyIPAddr = '') or (ErrCode = 11001) then
                    StopFlag := True;
            end;
        end;
    end ;
end;


procedure TToolsForm.PingThreadTermTrace (Sender: TObject);
var
    logline, addrstr: string;
    I, row: integer;
begin
    if StopFlag then
        exit;
    if Application.Terminated then
        exit;
    with Sender as TPingThread do begin
        ThreadIds [PingId] := 0;  // clear id so we don't try and kill it later
        row := PingId;
        if row < 0 then
           row := 0;
        PingListView.Items[row].Caption := IntToStr(PingId + 1);
        if ErrCode <> 0 then begin
            if PendingPings > 0 then
               dec (PendingPings);
            if TraceDoneFlag then
                exit;
            logline := Format (MaskResponse2, [PingId + 1, 0, ' ', 'Request timed out']);
            PingListView.Items[row].SubItems.Add('0ms');
            PingListView.Items[row].SubItems.Add('');
            PingListView.Items[row].SubItems.Add('Request timed out');
            inc (TraceErrs);
            if TraceErrs >= MaxErrors then begin
                if PingLog.Checked then
                    AddLogLine('Stopped Due to Excessive Errors');
                TraceDoneFlag := true;
            end ;
        end
        else begin
            addrstr := ReplyIPAddr;  // subsequent pings build route
            if addrstr <> '' then begin
                if TraceIPAddr = addrstr then
                    TraceDoneFlag := true;                  // finished, reach destination
                for I := 1 to TraceMax do begin
                    if TraceAddr [I] = addrstr then
                        exit;    // don't repeat ping to same address
                end;
            end;
            TraceAddr [PingId] := addrstr;
            PingListView.Items[row].SubItems.Add(IntToStr(ReplyRTT) + 'ms');
            PingListView.Items[row].SubItems.Add(addrstr);

        // start reverse lookup of IP from DNS cache, updates ListView in event if not in cache
            PingListView.Items[row].SubItems.Add(IcsDomainNameCache1.LookupIPOne(addrstr, row, sfAny, PingNameUpdateEvent));
        end ;
    end;
end;


procedure TToolsForm.IcsPing1DnsLookupDone(Sender: TObject; Error: Word);
var
    retvalue: integer;
begin
    with PingListView.Items.Add do begin
        Caption := '';
        SubItems.Add('');
        if Error <> 0 then begin
            if PingLog.Checked then
                AddLogLine('DNS lookup error - ' + SysErrorMessage(Error));
            SubItems.Add('DNS lookup error');
            SubItems.Add(SysErrorMessage(Error));
            PingSetButtons (true);
            Exit;
        end;
        if PingLog.Checked then
            AddLogLine('Host ''' + String(IcsPing1.PunycodeHost) + ''' is ' + IcsPing1.DnsResult);
        SubItems.Add('Host ''' + String(IcsPing1.PunycodeHost));
        SubItems.Add(IcsPing1.DnsResult);
        IcsPing1.Address := IcsPing1.DnsResult;
        retvalue := IcsPing1.Ping;
        if retvalue = 0 then
           AddLogLine('Ping failed: ' + IcsPing1.LastErrStr)
    end;
    PingSetButtons (true);
end;

procedure TToolsForm.IcsPing1EchoReply(Sender, Icmp: TObject; Status: Integer);
begin
    with PingListView.Items.Add do begin
        Caption := '';
        if Status <> 0 then begin  { Success }
            if PingLog.Checked then
                AddLogLine('Received ' + IntToStr(IcsPing1.ReplySize) + ' bytes from ' +
                                IcsPing1.ReplyIP + ' in ' + IntToStr(IcsPing1.ReplyRTT) + ' msecs');
            SubItems.Add(IntToStr(IcsPing1.ReplyRTT) + 'ms');
            SubItems.Add(IcsPing1.ReplyIP);
            SubItems.Add('');
        end
        else { Failure }
            if PingLog.Checked then
                AddLogLine('Cannot ping host (' + IcsPing1.HostIP + ') : ' +
                    IcsPing1.LastErrStr + '. ReplyStatus = ' + IntToStr(IcsPing1.ReplyStatus));
            SubItems.Add('');
            SubItems.Add('Cannot ping host (' + IcsPing1.HostIP + ')');
            SubItems.Add(IcsPing1.LastErrStr + '. ReplyStatus = ' + IntToStr(IcsPing1.ReplyStatus));
    end;
    PingSetButtons (true);
end;

procedure TToolsForm.IcsPing1EchoRequest(Sender, Icmp: TObject);
begin
    if PingLog.Checked then
                AddLogLine('Sending ' + IntToStr(IcsPing1.Size) + ' bytes to ' +
                          IcsPing1.HostName + ' (' + IcsPing1.HostIP + ')');
end;

procedure TToolsForm.doPingAbortClick(Sender: TObject);
begin
    IcsPing1.CancelDnsLookup;
    StopFlag := true; // trace route
    PingSetButtons (true);
end;

procedure TToolsForm.doPingClearClick(Sender: TObject);
begin
    DelRecentSList(PingQueryName);
end;

procedure TToolsForm.doPingSyncClick(Sender: TObject);
begin
    if PingQueryName.Text = '' then exit;
    PingSetButtons (false);
    IcsPing1.SrcAddress := LocalIPv4.Text;
    IcsPing1.SrcAddress6 := LocalIPv6.Text;

    { first do async DNS lookup, sync ping from event handler }
    if PingLog.Checked then
          AddLogLine('Resolving host ''' + PingQueryName.Text + '''');
    IcsPing1.SocketFamily := TSocketFamily(IpFamily.ItemIndex);
    IcsPing1.DnsLookup(PingQueryName.Text);
    AddRecentSList(PingQueryName.Items, PingQueryName.Text, 99);
end;

procedure TToolsForm.doPingThreadClick(Sender: TObject);
var
    I: Integer;
    EndTimer: Int64;
begin
    if PingQueryName.Text = '' then exit;
    PingQueryName.Text := CleanDomain (PingQueryName.Text) ;
    PingSetButtons (false);
    AddRecentSList(PingQueryName.Items, PingQueryName.Text, 99);

 // start all pings
    try
        if PingLog.Checked then
           AddLogLine ('Pinging: ' + PingQueryName.Text) ;
        PendingPings := 0 ;
        PingListView.Items.Clear;
        for I := 1 to atoi(PingCount.Text) do    // one row for each ping
            PingListView.Items.Add.Caption := IntToStr(I);
        for I := 1 to atoi(PingCount.Text) do begin
            with TPingThread.Create (True) do begin  // create suspended
                FreeOnTerminate := True;
                PingId := I;            // keep track of the results
                ThreadIds [PingId] := ThreadId;
                OnTerminate := PingThreadTermPing ;    // where we get the response
                PingSrcAddress := LocalIPv4.Text;
                PingSrcAddress6 := LocalIPv6.Text;
                PingSocketFamily := TSocketFamily(IpFamily.ItemIndex);
                PingHostName := PingQueryName.Text ;  // host name or IP address to ping
                PingTimeout := atoi(PingTimeoutMs.Text);
                PingTTL := TraceMax ;                 // hops
                PingLookupReply := false ;      // don't need response host name lookup
                {$IF CompilerVersion < 21}
                    Resume;    // start it now
                {$ELSE}
                    Start;
                {$IFEND}
            end ;
            inc (PendingPings) ;
        // 100ms before next hop, unless ping finishes earlier
        // also restrict total threads, too many slows things down
            EndTimer := IcsGetTickCount64 + 100 ;
            while (PendingPings > 0) and (NOT StopFlag) do
            begin
                Application.ProcessMessages ;
                if (IcsGetTickCount64 > EndTimer) and (PendingPings < 6) then
                    break ;
            end ;

        end;

      // wait for pings to finish
        EndTimer := IcsGetTickCount64 + 30000 ;   // 30 seconds
        while (PendingPings > 0) and (NOT StopFlag) do
        begin
            Application.ProcessMessages ;
            if IcsGetTickCount64 > EndTimer then break ;
            if StopFlag then break ;
        end ;

    // not finished, terminate them cleanly
        if (PendingPings > 0) then
        begin
            for I := -1 to TraceMax do begin
                 if ThreadIds [I] > 0 then
                     PostThreadMessage (ThreadIds [I], WM_QUIT, 0, 0);  // terminate thread
            end ;
        end ;
        if PingLog.Checked then begin
          // copy ListView result to log
            for I := 0 to PingListView.Items.Count - 1 do begin
                with PingListView.Items[I] do begin
                    if (SubItems.Count >= 3) then
                        AddLogLine(Format (MaskResponse3, [Caption, SubItems[0], SubItems[1], SubItems[2]]));
                end;
            end;
            AddLogLine ('');
            if StopFlag then
                AddLogLine ('Stopped by User') ;
        end;
        beep ;
    finally
        PingSetButtons (true);
    end;
end;

procedure TToolsForm.doTraceRouteClick(Sender: TObject);
var
    Source: string;
    I, ItemNr: integer;
    EndTimer: Int64;
begin
    if PingQueryName.Text = '' then exit;
    PingSetButtons (false);
    try // finally
        try // except
            StopFlag := false ;
            TraceDoneFlag := false ;
            if PingLog.Checked then
                AddLogLine ('Trace Route to: ' + PingQueryName.Text) ;
            TraceErrs := 0 ;
            PendingPings := 0 ;
            for I := 1 to TraceMax do begin
                TraceAddr [I] := '';
                ThreadIds [I] := 0;
            end;
            TraceSocketFamily := TSocketFamily(IpFamily.ItemIndex);

        // prepare ListView for the maximum pings
            PingListView.Items.Clear;
            for I := 1 to TraceMax do
                PingListView.Items.Add.Caption := '';

        // look up host IP address first, from cache
            AddRecentSList(PingQueryName.Items, PingQueryName.Text, 99);
            ItemNr := IcsDomainNameCache1.LookupHostSync(PingQueryName.Text, 0, TraceSocketFamily);
            with IcsDomainNameCache1.GetDNItem(ItemNr) do begin
                if DNState = StateOK then begin
                    if TotResp4 > 0 then begin
                        TraceSocketFamily := sfIPv4;
                        TraceIPAddr := Responses4[0];
                        Source := LocalIPv4.Text;
                    end
                    else begin
                        TraceSocketFamily := sfIPv6;
                        TraceIPAddr := Responses6[0];
                        Source := LocalIPv6.Text;
                        if Source = '' then begin  // search first non-local address
                            for I := 1 to LocalIPv6.Items.Count - 1 do begin
                                if Pos ('fe', LocalIPv6.Items [I]) = 0 then begin
                                    Source := LocalIPv6.Items [I];
                                    break ;
                                end;
                            end;
                        end;
                    end;
                    if PingLog.Checked then
                        AddLogLine('Tracing to IP Address ' + TraceIPAddr);
                end
                else
                begin
                    if PingLog.Checked then
                        AddLogLine('Can Not Find Host ' + PingQueryName.Text);
                    PingListView.Items[0].SubItems.Add('');
                    PingListView.Items[0].SubItems.Add('Can Not Find Host ' + PingQueryName.Text);
                    Exit;
                end;
            end;
           PingListView.Items[0].Caption := '1';
           PingListView.Items[0].SubItems.Add('0ms');
           PingListView.Items[0].SubItems.Add(Source);
           PingListView.Items[0].SubItems.Add(Lowercase (String(LocalHostName)));

        // start all pings
            for I := 1 to TraceMax do
            begin
                Application.ProcessMessages;
                with TPingThread.Create (True) do
                begin
                    FreeOnTerminate := True;
                    PingId := I ;
                    ThreadIds [PingId] := ThreadId;
                    OnTerminate := PingThreadTermTrace ;
                    PingSrcAddress := LocalIPv4.Text;
                    PingSrcAddress6 := LocalIPv6.Text;
                    PingSocketFamily := TraceSocketFamily;
                    PingHostName := TraceIPAddr ;
                    PingLookupReply := false;
                    PingTimeout := atoi(PingTimeoutMs.Text);  // ms
                    PingTTL := I ;           //  increasing TTL for each hop
                    PingLookupReply := true ;
                    {$IF CompilerVersion < 21}
                        Resume;    // start it now
                    {$ELSE}
                        Start;
                    {$IFEND}
                    inc (PendingPings) ;

             // 100ns before next hop, unless ping finishes earlier
             // also restrict total threads, too many slows things down
                    EndTimer := IcsGetTrgMSecs64(100);
                    while (PendingPings > 0) and (NOT StopFlag) do
                    begin
                        Application.ProcessMessages ;
                        if IcsTestTrgTick64(EndTimer) and (PendingPings < 6) then
                            break ;
                    end ;
                    if TraceDoneFlag then
                        break ;  // reached host or too many errors
                end ;
            end ;

        // wait for pings and name lookups to finish
            AddLogLine('Pending Pings ' + IntToStr(PendingPings)) ;
            EndTimer := IcsGetTrgSecs(30);   // 30 seconds
            while (PendingPings > 0)  and (NOT StopFlag) do
            begin
                Application.ProcessMessages ;
                if IcsTestTrgTick64(EndTimer) then
                    break ;
                if StopFlag then
                    break ;
            end ;

        // not finished, terminate them cleanly
            if (PendingPings > 0) then
            begin
                for I := -1 to TraceMax do begin
                     if ThreadIds [I] > 0 then
                         PostThreadMessage (ThreadIds [I], WM_QUIT, 0, 0);  // terminate thread
                end ;
            end ;
            if PingLog.Checked then begin
              // copy ListView result to log
                for I := 0 to PingListView.Items.Count - 1 do begin
                    with PingListView.Items[I] do begin
                        if (SubItems.Count >= 3) then
                            AddLogLine(Format (MaskResponse3, [Caption, SubItems[0], SubItems[1], SubItems[2]]));
                    end;
                end;
                AddLogLine ('');
                if StopFlag then
                    AddLogLine('Stopped by User') ;
                AddLogLine('Trace Route Completed') ;
            end;
            beep ;
        except
            AddLogLine('Error Sending Pings') ;
            beep ;
        end ;
    finally
        PingSetButtons (true);
    end ;
end;

procedure TToolsForm.doWhoisAbortClick(Sender: TObject);
begin
     doWhoisQuery.Enabled := TRUE;
     doWhoisAbort.Enabled := FALSE;
     IcsWhoisCli1.Abort;
end;

procedure TToolsForm.doWhoisClearClick(Sender: TObject);
begin
    DelRecentSList (WhoisQueryName);
end;

procedure TToolsForm.doWhoisQueryClick(Sender: TObject);
begin
    doWhoisQuery.Enabled := FALSE;
    doWhoisAbort.Enabled := TRUE;
    WhoisMemo.Lines.Clear;
    IcsWhoisCli1.Query := WhoisQueryName.Text;
    IcsWhoisCli1.Host := WhoisServer.Text ;
    if WhoisMethod.ItemIndex = 0 then
        IcsWhoisCli1.StartAutoQuery
    else
        IcsWhoisCli1.StartQuery;
     AddRecentSList (WhoisQueryName.Items, WhoisQueryName.Text, 250) ;
     WhoisMemo.Lines.Add ('Query initiated for ' + IcsWhoisCli1.Query);
end;


procedure TToolsForm.IcsWhoisCli1QueryDone(Sender: TObject; ErrCode: Word);
begin
     doWhoisQuery.Enabled := TRUE;
     doWhoisAbort.Enabled := FALSE;
     WhoisMemo.Lines.Add (IcsWhoisCli1.WhoisResp);
     AddLogLine(IcsWhoisCli1.WhoisResp);
     WhoisMemo.Lines.Add ('Query completed');
     WhoisMemo.SelStart := 0;
     SendMessage(WhoisMemo.Handle, EM_SCROLLCARET, 0, 0);
end;


procedure TToolsForm.IcsDomainNameCachex1DNLogEvent(Sender: TObject; const Msg: string);
begin
    if DNCacheLog.Checked then
        AddLogLine(Msg);
end;


{ generally this event should not be called, instead each requst specifies it's own event }
procedure TToolsForm.CommonCacheUpdateEvent(Sender: TObject; ItemNr: Integer);
begin
    with IcsDomainNameCache1.GetDNItem(ItemNr) do begin
        AddLogLine('Domain Name Lookup Event for ' + Request + ' > ' + IcsDomainNameCache1.BuildRespList(ItemNr));
    end;
end;


procedure TToolsForm.BulkNameUpdateEvent(Sender: TObject; ItemNr: Integer);
var
    LineNr: Integer;
    S: String;
begin
    with IcsDomainNameCache1.GetDNItem(ItemNr) do begin
        LineNr := ReqTag;
        if LineNr >= BulkResultMemo.Lines.Count then
            Exit;
        S := IcsDomainNameCache1.BuildRespList(ItemNr);
        if LineNr >= 0 then
            BulkResultMemo.Lines[LineNr] := S;
    end;
end;

procedure TToolsForm.NameLookUpdateEvent(Sender: TObject; ItemNr: Integer);
var
    LineNr: Integer;
    S: String;
begin
    with IcsDomainNameCache1.GetDNItem(ItemNr) do begin
        LineNr := ReqTag;
        if LineNr >= BulkResultMemo.Lines.Count then
            Exit;
        S := IcsDomainNameCache1.BuildRespList(ItemNr);
        if LineNr = 0 then
            RevLookHost.Text := S;

    // reverse lookup
        if LineNr >= 0 then begin
            S := 'Reverse Lookup for ' + Request + ' > ' + S;
            BulkResultMemo.Lines[LineNr] := S;
        end

    // forward lookup, got IP address
        else if LineNr = -9 then begin
            LookupAddr.Text := S;
            AddLogLine('Domain Name Lookup Event for ' + Request + ' > ' + S);
        // now reverse lookup first IP address
            if TotResp4 > 0 then
                S := Responses4[0]
            else if TotResp6 > 0 then
                S := Responses6[0]
            else
                Exit;
           IcsDomainNameCache1.LookupIPAsync(S, -8, TSocketFamily(IpFamily.ItemIndex), NameLookUpdateEvent);
        end
    // forward lookup, got actual host
        else if LineNr = -8 then begin
            LookupOfficial.Text := S;
            AddLogLine('Reverse Lookup Event for ' + Request + ' > ' + S);
        end;
    end;
end;


procedure TToolsForm.doAddrLookUpClick(Sender: TObject);
begin
    AddLogLine('Starting Look-up of Domain Name ' + LookupHost.Text);
    AddRecentSList(LookupHost.Items, LookupHost.Text, 99);
    IcsDomainNameCache1.LookupHostAsync(LookupHost.Text, -9, TSocketFamily(IpFamily.ItemIndex), NameLookUpdateEvent);
end;


procedure TToolsForm.doAddrRevLookClick(Sender: TObject);
var
    ScanAddr: TSockAddrIn6;
    SockAddrIn: TSockAddrIn absolute ScanAddr;
    I, IpByte: Integer;
    NewIP: String;
begin
    AddLogLine('Starting Look-up of IP Address ' + RevLookAddr.Text);
    BulkResultMemo.Lines.Clear;
    BulkResultMemo.Lines.Add ('');  // waiting for result
    AddRecentSList(RevLookAddr.Items, RevLookAddr.Text, 99);
    IcsDomainNameCache1.LookupIPAsync(RevLookAddr.Text, 0, TSocketFamily(IpFamily.ItemIndex), NameLookUpdateEvent);
    if atoi(RevExtra.Text) > 0 then begin
        ScanAddr := WSocketIPAddrToSocAddr(RevLookAddr.Text);
        for I := 1 to atoi(RevExtra.Text) do begin
            Application.ProcessMessages;
        // increment IP address by one
            if ScanAddr.sin6_family = AF_INET then begin
                IpByte := Byte(SockAddrin.sin_addr.S_un_b.s_b4) + 1;
                if IpByte > 255 then begin  // handle wrapping to next byte
                    IpByte := 0;
                    SockAddrin.sin_addr.S_un_b.s_b3 := AnsiChar(Byte(SockAddrin.sin_addr.S_un_b.s_b3) + 1);
                end;
                SockAddrin.sin_addr.S_un_b.s_b4 := AnsiChar(IpByte);
            end
            else if ScanAddr.sin6_family = AF_INET6 then begin
                IpByte := ScanAddr.sin6_addr.S6_addr[15] + 1;
                if IpByte > 255 then begin  // handle wrapping to next byte, could use word but then need to swap bytes
                    IpByte := 0;
                    ScanAddr.sin6_addr.S6_addr[14] := ScanAddr.sin6_addr.S6_addr[14] + 1;
                end;
                ScanAddr.sin6_addr.S6_addr[15] := IpByte;
            end;
            NewIP := WSocketSockAddrToStr(ScanAddr);
            AddLogLine('Starting Look-up of IP Address ' + NewIP);
            BulkResultMemo.Lines.Add ('');  // waiting for result
            IcsDomainNameCache1.LookupIPAsync(NewIP, I, TSocketFamily(IpFamily.ItemIndex), NameLookUpdateEvent);
        end;
    end;
end;


procedure TToolsForm.doBulkDnsLookupClick(Sender: TObject);
var
    I: Integer;
begin
    if BulkNamesMemo.Lines.Count = 0 then
        Exit;
    BulkResultMemo.Lines.Clear;
    AddLogLine('Starting Look-up of ' + IntToStr(BulkNamesMemo.Lines.Count) + ' Domain Names');
    for I := 0 to BulkNamesMemo.Lines.Count - 1 do begin
        Application.ProcessMessages;
        if BulkNamesMemo.Lines[I] <> '' then begin
            BulkResultMemo.Lines.Add ('');  // waiting for result
            IcsDomainNameCache1.LookupHostAsync(BulkNamesMemo.Lines[I], I, TSocketFamily(IpFamily.ItemIndex), BulkNameUpdateEvent);
        end;
    end;
end;

procedure TToolsForm.DelRevLookAddrClick(Sender: TObject);
begin
    DelRecentSList(RevLookAddr);
end;


procedure TToolsForm.doDelAddrLookClick(Sender: TObject);
begin
    DelRecentSList(LookupHost);
end;


procedure TToolsForm.doDelNameHostClick(Sender: TObject);
begin
    DelRecentSList(NameHostName);
end;


procedure TToolsForm.doDNCacheClearClick(Sender: TObject);
begin
    IcsDomainNameCache1.MaintClearAll;
    AddLogLine('Cleared Name Server Cache');
end;


procedure TToolsForm.doDNCacheListClick(Sender: TObject);
begin
    AddLogLine(IcsDomainNameCache1.ListCache);
end;


procedure TToolsForm.DNCacheChangeClick(Sender: TObject);
begin
   IcsDomainNameCache1.DNMethod := TDNMethod(DNCacheMethod.ItemIndex);
   IcsDomainNameCache1.MaxLookups := atoi(DNCacheMax.Text);
   IcsDomainNameCache1.DBLANlookup := TDBLANlookup(DNCacheLAN.ItemIndex);
end;


procedure TToolsForm.IcsDnsQuery1LogEvent(Sender: TObject; const Msg: string);
begin
    AddLogLine(Msg);
end;


procedure TToolsForm.IcsDnsQuery1RequestDone(Sender: TObject; Error: Word);
var
    I: Integer;
    STTL, DispName, Resp: String;
    MyDnsQuery: TDnsQuery;
begin
    NameServerTrg := Trigger64Disabled;
    MyDnsQuery := Sender as TDnsQuery;
    SetDnsButtons(True);
    if Error <> 0 then begin
        Resp := 'Error #' + IntToStr(Error);
        NameLookupMemo.Lines.Add(Resp);
        AddLogLine(Resp);
        Exit;
    end;
    if MyDnsQuery.ResponseCode = DnsRCodeNoError then begin // success
        if MyDnsQuery.ResponseAuthoritative then
            Resp := 'Answers are Authoritative' + IcsCRLF
        else
            Resp := 'Answers are not Authoritative' + IcsCRLF;

        if MyDnsQuery.AnswerTotal > 0 then begin
            for I := 0 to MyDnsQuery.AnswerTotal - 1 do begin
                with  MyDnsQuery.AnswerRecord[I] do begin
                    STTL := ' (TTL ' + IntToStr(TTL) + ')';
                    if AnswerName = String(RRName) then     // May 2020
                        DispName := AnswerName + ': '
                    else
                        DispName := AnswerName + ' (' + String(RRName) + '): ';
                    case RRType of
                        DnsQueryMX: Resp := Resp + 'Mail Exchange Server for ' + DispName + HostName + ' (preference ' + IntToStr (Mxpref) + STTL + IcsCRLF;
                        DnsQueryA: Resp := Resp + 'DNS IPv4 Query for ' + DispName + String(RDData) + STTL + IcsCRLF;
                        DnsQueryAAAA: Resp := Resp + 'DNS IPv6 Query for ' + DispName + String(RDData) + STTL + IcsCRLF;
                        DnsQueryNS: Resp := Resp + 'Name Server for ' + DispName + HostName + STTL + IcsCRLF;
                        DnsQueryCNAME: Resp := Resp + 'Alias (CNAME) for ' + DispName+ HostName + IcsCRLF;
                        DnsQueryPTR: Resp := Resp + 'Domain Name Query for ' + DispName + HostName + IcsCRLF;
                        DnsQueryTXT: Resp := Resp + 'Text Query for ' + DispName + String(RDData) + IcsCRLF;
                        DnsQuerySOA:  begin
                            Resp := Resp + 'SOA - Zone of Authority - for ' + DispName + IcsCRLF;
                            Resp := Resp + 'Primary Name Server: ' + String(soa.mname) + IcsCRLF;
                            Resp := Resp + 'Contact: ' + String(soa.rname) + IcsCRLF;
                            Resp := Resp + 'Serial/version: ' + IntToStr(soa.serial) + IcsCRLF;
                            Resp := Resp + 'Refresh Time: ' +  IntToStr(soa.refresh) + IcsCRLF;
                            Resp := Resp + 'Failed Retry Time: ' + IntToStr(soa.retry) + IcsCRLF;
                            Resp := Resp + 'Expire Time: ' + IntToStr(soa.expire) + IcsCRLF;
                            Resp := Resp + 'Minimum TTL Time: ' + IntToStr(soa.minimum) + IcsCRLF;
                        end
                    else begin
                            Resp := Resp + FindDnsReqTypeName(RRType) + ' Result for ' + DispName + String(RDData) + IcsCRLF;
                         end;
                    end;
                end;
            end;
        end
        else
            Resp := 'DNS Record Not Found' + IcsCRLF;
    end
    else
        Resp := Resp + 'Query Failed: ' + DnsRCodeTable[MyDnsQuery.ResponseCode] + IcsCRLF;
    NameLookupMemo.Lines.Add(Resp);
    AddLogLine(Resp);
end;


procedure TToolsForm.SetupNameServer;
begin
    if NameDnsOpt.ItemIndex = 0 then begin
        if NameServerAuto.ItemIndex >= 0 then
            IcsDnsQuery1.Addr := NameServerAuto.Items[NameServerAuto.ItemIndex];
    end
    else if NameDnsOpt.ItemIndex = 1 then
    begin
        IcsDnsQuery1.Addr := NameServerIP.Text ;
        AddRecentSList (NameServerIP.Items, NameServerIP.Text, 250) ;
    end
    else if NameDnsOpt.ItemIndex = 2 then
         IcsDnsQuery1.Addr := NameServerPublic.Items[NameServerPublic.ItemIndex]
    else
        IcsDnsQueryHttps1.DnsSrvUrl := NameServerHttps.Text;
    if NameProtoTCP.Checked then
        IcsDnsQuery1.Proto := 'tcp'
    else
        IcsDnsQuery1.Proto := 'udp';
    NameHostName.Text := CleanDomain (NameHostName.Text) ;
    if NameHostName.Text = '' then exit ;
    AddRecentSList (NameHostName.Items, NameHostName.Text, 250) ;
    NameServerTrg := IcsGetTrgSecs64(3);   // three second timeout
    SetDnsButtons(False);
end;

procedure TToolsForm.SetDnsButtons(Value: Boolean);
begin
    doDNSAll.Enabled := Value;
    doDnsRev.Enabled := Value;
    doDnsQuery.Enabled := Value;
end;


procedure TToolsForm.doDNSAllClick(Sender: TObject);
begin
// UDP may ignore request
    SetupNameServer;
    NameLookupMemo.Lines.Clear;
    if NameDnsOpt.ItemIndex = 3 then begin
        IcsDnsQueryHttps1.DOHQueryAll (NameHostName.Text);
        NameLookupMemo.Lines.Add('ALL Query to: ' +  IcsDnsQueryHttps1.DnsSrvUrl + ' for: ' + NameHostName.Text) ;
    end
    else begin
        DNSReqId := IcsDnsQuery1.QueryAll (NameHostName.Text) ;
        NameLookupMemo.Lines.Add('ALL Query to: ' +  IcsDnsQuery1.Addr + ' for: ' + NameHostName.Text) ;
    end;
end;

procedure TToolsForm.doDnsBothAClick(Sender: TObject);
begin
// UDP may ignore request
    SetupNameServer;
    NameLookupMemo.Lines.Clear;
    if NameDnsOpt.ItemIndex = 3 then begin
        IcsDnsQueryHttps1.DOHQueryBothA (NameHostName.Text);
        NameLookupMemo.Lines.Add('Both A Query to: ' +  IcsDnsQueryHttps1.DnsSrvUrl + ' for: ' + NameHostName.Text) ;
    end
    else begin
        DNSReqId := IcsDnsQuery1.QueryBothA(NameHostName.Text) ;
        NameLookupMemo.Lines.Add('Both A Query to: ' +  IcsDnsQuery1.Addr + ' for: ' + NameHostName.Text) ;
    end;
end;

procedure TToolsForm.doDNSQueryClick(Sender: TObject);
begin
// UDP may ignore request
    SetupNameServer;
    NameLookupMemo.Lines.Clear;
    if NameDnsOpt.ItemIndex = 3 then begin
        IcsDnsQueryHttps1.DOHQueryAny (NameHostName.Text, DnsReqTable[NameQueryType.ItemIndex].Num);
        NameLookupMemo.Lines.Add('Sent Query to: ' +  IcsDnsQueryHttps1.DnsSrvUrl + ' for: ' + NameHostName.Text) ;
   end
    else begin
        DNSReqId := IcsDnsQuery1.QueryAny  (NameHostName.Text, DnsReqTable[NameQueryType.ItemIndex].Num) ;
        NameLookupMemo.Lines.Add('Sent Query to: ' +  IcsDnsQuery1.Addr + ' for: ' + NameHostName.Text) ;
     end;
end;

procedure TToolsForm.doDnsRevClick(Sender: TObject);
begin
// UDP may ignore request
    SetupNameServer;
    if NameDnsOpt.ItemIndex = 3 then begin
        IcsDnsQueryHttps1.DOHQueryAny (NameHostName.Text, DnsQueryPTR);
        NameLookupMemo.Lines.Add('PTR Query to: ' +  IcsDnsQueryHttps1.DnsSrvUrl + ' for: ' + NameHostName.Text) ;
    end
    else begin
        DNSReqId := IcsDnsQuery1.QueryAny (NameHostName.Text, DnsQueryPTR) ;
        NameLookupMemo.Lines.Add('PTR Query to: ' +  IcsDnsQuery1.Addr + ' for: ' + NameHostName.Text) ;
    end;
end;


end.
