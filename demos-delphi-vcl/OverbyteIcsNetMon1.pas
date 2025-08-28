{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  ICS Internet Packet Monitoring Components - Display Packets using Raw Sockets and Npcap Demo
Creation:     Oct 2005
Updated:      Apr 2024
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
8 Aug 2008  - 1.2  - updated to support ICS V6 and V7, and Delphi 2009
                     when stopping capture ignore any buffered data so it stops faster
9 Aug 2010  -  1.3 - fixed various cast warnings with Delphi 2009 and later
                     added Clear button
26 Nov 2018 - 1.4 -  comsmetics only for Npcap support, tested with ICS V8.
                     added program Admin rights check and force Wincap if missing
                     since socket monitoring won't work
July 21, 2023 V8.71 - Updated sample and units for main ICS library.
                     Added IPv6 support.
                     Added new protocol and IP address filtering, including improved
                      local address filtering so only external traffic is caught.
                     Added grid display for packet capture and window for one complete packet.
                     Added grid display for traffic capture.
Aug 08, 2023 V9.0  Updated version to major release 9.
Jan 11, 2024 V9.1  Moved two utility functions to OverbyteIcsUtils.
                   Added resource nmap-mac-prefixes.RES to avoid distributing file
                     nmap-mac-prefixes.txt to display MAC vendor names.
                   Added resource icsportlist.RES to avoid distributing file
                     icsportlist.txt to display common port/service names.
Apr 11, 2024 V9.2  When displaying captured data, escape CRLF to \n.
Aug 07, 2024 V9.3  Added OverbyteIcsTypes for consolidated types and constants, allowing
                     other import units to be removed.


Pending - NPCap does not seem to work on Win64, needs more testing

This unit requires the Npcap driver to be downloaded and installed from:
https://npcap.com/

}

unit OverbyteIcsNetMon1;

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
  Dialogs, StdCtrls, ExtCtrls, ComCtrls,
//OverbyteIcsWinsock,
  OverbyteIcsWsocket,
  OverbyteIcsUtils,
  OverbyteIcsIniFiles,
  OverbyteIcsIpUtils,
  OverbyteIcsMonCommon,
  OverbyteIcsMonSock,
  OverbyteIcsMonPcap,
  OverbyteIcsMonNdis,
//  OverbyteIcsWndControl,
  OverbyteIcsDnsQuery,
  OverbyteIcsTypes, OverbyteIcsWndControl;  { V9.3 consolidated types and constants }

const
    sPacketLine = '%-12s %-6s %4d  %-35s %6d  %-35s %6d  %-12s %4d  %s' ;
    sHeaderLine = 'Time         Prot   Plen  Source IP                      Source Port  Destination IP                   Dest Port  Service      Dlen  Packet Data' ;
//                 10:46:21:058 TCPv4    54  192.168.1.103                        29101  217.146.102.135                      61643  <61643>         0  ACK
//                 10:46:21:731 TCPv6   107  2a00:1940:2:2::137                     443  2a00:1940:1:2:141d:6c51:d01d:c273    36670  https          36  [!! DL=47 HD-L=107 offset=60 pay-L=53]
    SectionMainWindow    = 'MainWindow';
    SectionRow1Window    = 'Row1Window';
    KeyTop               = 'Top';
    KeyLeft              = 'Left';
    KeyWidth             = 'Width';
    KeyHeight            = 'Height';
    SectionData          = 'Data';


type
  TCaptureMode = (CapModeNone, CapModeLog, CapModeGrid, CapModeTraffic);

  TMonForm = class(TForm)
// components saved in INI file
    FilterIpAddr: TRadioGroup;
    FilterProtocol: TRadioGroup;
    IpAddrList: TMemo;
    IpAddrLocal: TCheckBox;
    MonAdptList: TListBox;
    MonIpList: TListBox;
    MonPromiscuous: TCheckBox;
    ProtocolIRC: TCheckBox;
    PortsList: TMemo;
    ProtocolARP: TCheckBox;
    ProtocolBroadcast: TCheckBox;
    ProtocolDns: TCheckBox;
    ProtocolHttp: TCheckBox;
    ProtocolICMP: TCheckBox;
    ProtocolIPv4: TCheckBox;
    ProtocolIPv6: TCheckBox;
    ProtocolNonIp: TCheckBox;
    ProtocolSNMP: TCheckBox;
    ProtocolSyslog: TCheckBox;
    ProtocolTCP: TCheckBox;
    ProtocolUPnP: TCheckBox;
    ProtocolUdp: TCheckBox;
    ShowFullData: TCheckBox;
    ShowIgnoreData: TCheckBox;
    UseWinNCap: TCheckBox;

// following not saved
    Timer: TTimer;
    PageControl1: TPageControl;
    TabSettings: TTabSheet;
    TabPacketsLog: TTabSheet;
    TabPacketsGrid: TTabSheet;
    MonLogWin: TMemo;
    Panel3: TPanel;
    MonGridWin: TListView;
    Panel2: TPanel;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    LabelAdmin: TLabel;
    Label2: TLabel;
    LabelTrafficLog: TLabel;
    doPackLogStart: TButton;
    doClearLog: TButton;
    doExit: TButton;
    GroupBox1: TGroupBox;
    doPackLogStop: TButton;
    TabTraffic: TTabSheet;
    LaberlPorts: TLabel;
    LabelTrafficGrid: TLabel;
    doPackGridStart: TButton;
    doPackGridStop: TButton;
    doPackGridClear: TButton;
    MonTrafficWin: TListView;
    Panel1: TPanel;
    LabelTrafficTotals: TLabel;
    doTrafficStart: TButton;
    doTrafficStop: TButton;
    doTrafficClear: TButton;
    doTrafficLogTotals: TButton;
    TrafficHostNames: TCheckBox;
    GridAutoScroll: TCheckBox;
    IcsDomainNameCache1: TIcsDomainNameCache;
    GroupBox4: TGroupBox;
    DnsCacheLog: TCheckBox;
    doDnsCacheClear: TButton;
    doDnsCacheList: TButton;
    procedure doExitClick(Sender: TObject);
    procedure doPackLogStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure MonAdptListClick(Sender: TObject);
    procedure UseWinNCapClick(Sender: TObject);
    procedure doClearLogClick(Sender: TObject);
    procedure doPackLogStopClick(Sender: TObject);
    procedure FilterProtocolClick(Sender: TObject);
    procedure PortsListChange(Sender: TObject);
    procedure ProtocolARPClick(Sender: TObject);
    procedure ProtocolBroadcastClick(Sender: TObject);
    procedure ProtocolDnsClick(Sender: TObject);
    procedure ProtocolHttpClick(Sender: TObject);
    procedure ProtocolICMPClick(Sender: TObject);
    procedure ProtocolIPv4Click(Sender: TObject);
    procedure ProtocolIPv6Click(Sender: TObject);
    procedure ProtocolIRCClick(Sender: TObject);
    procedure ProtocolNonIpClick(Sender: TObject);
    procedure ProtocolSNMPClick(Sender: TObject);
    procedure ProtocolSyslogClick(Sender: TObject);
    procedure ProtocolTCPClick(Sender: TObject);
    procedure ProtocolUdpClick(Sender: TObject);
    procedure ProtocolUPnPClick(Sender: TObject);
    procedure FilterIpAddrClick(Sender: TObject);
    procedure IpAddrLocalClick(Sender: TObject);
    procedure IpAddrListExit(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure doPackGridClearClick(Sender: TObject);
    procedure doPackGridStartClick(Sender: TObject);
    procedure doPackGridStopClick(Sender: TObject);
    procedure MonGridWinData(Sender: TObject; Item: TListItem);
    procedure MonGridWinSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure doTrafficStartClick(Sender: TObject);
    procedure doTrafficStopClick(Sender: TObject);
    procedure MonTrafficWinData(Sender: TObject; Item: TListItem);
    procedure doTrafficClearClick(Sender: TObject);
    procedure doTrafficLogTotalsClick(Sender: TObject);
    procedure doDnsCacheListClick(Sender: TObject);
    procedure doDnsCacheClearClick(Sender: TObject);
    procedure IcsDomainNameCache1DNLogEvent(Sender: TObject; const Msg: string);
  private
    { Private declarations }
    procedure PacketEvent (Sender: TObject; PacketInfo: TPacketInfo) ;
    procedure LogEvent (Sender: TObject; const Msg: String) ;
    procedure LoadIpAddrList;
    procedure CapButtons(Run: Boolean);
    procedure CaptureStart;
    procedure CaptureStop;
    procedure LogLine (const Msg: String) ;
  public
    { Public declarations }
  end;

var
  MonForm: TMonForm;
  MonitorSocket: TIcsMonSocket ;
  MonitorPcap: TIcsMonPcap ;
  MonitorFilter: TIcsMonFilterClass;
  TrafficClass: TIcsTrafficClass ;
  AdapterIPList: TStringList ;
  AdapterMaskList: TStringList ;
  AdapterBcastList: TStringList ;
  FIniFileName: String;
  BuffLogLines: String;
  CaptureMode: TCaptureMode;
  GridBuffer: array of TPacketInfo;
  GridCount: Integer;

implementation

{$R *.dfm}

Uses OverbyteIcsNetMon2;

procedure TMonForm.FormCreate(Sender: TObject);
var
    I: integer ;
begin
    MonitorFilter := TIcsMonFilterClass.Create;
    TrafficClass := TIcsTrafficClass.Create(Self) ;
    TrafficClass.IcsDNCache := IcsDomainNameCache1;

// raw sockets monitoring
    MonitorSocket := TIcsMonSocket.Create (self) ;
    MonitorSocket.onPacketEvent := PacketEvent ;
    MonitorSocket.OnLogEvent := LogEvent ;
    MonitorSocket.IcsDNCache := IcsDomainNameCache1;
    MonIpList.Items := LocalIPList (sfAny) ;
    if MonIpList.Items.Count > 0 then
        MonIpList.ItemIndex := 0 ;

// winpcap monitoring, needs packet.dll and drivers installed
    if LoadPacketDll then
    begin
        MonitorPcap := TIcsMonPcap.Create (self) ;
        MonitorPcap.onPacketEvent := PacketEvent ;
        MonitorPcap.OnLogEvent := LogEvent ;
        MonitorPcap.IcsDNCache := IcsDomainNameCache1;
        LogLine ('Npcap version: ' + Pcap_GetPacketVersion) ;
        MonAdptList.Items.Assign (MonitorPcap.AdapterDescList) ;
        if MonAdptList.Items.Count <> 0 then
        begin
            for I := 0 to MonAdptList.Items.Count - 1 do
            begin
                if MonAdptList.Items [I] = '' then
                    MonAdptList.Items [I] := MonitorPcap.AdapterNameList [I];   // device name
            end;
            MonAdptList.ItemIndex := 0 ;
            MonAdptList.Enabled := true ;
            UseWinNCap.Enabled := true ;
            MonPromiscuous.Enabled := true ;
            AdapterIPList := TStringList.Create ;
            AdapterMaskList := TStringList.Create ;
            AdapterBcastList := TStringList.Create ;
        end ;
    end ;
end;

procedure TMonForm.FormDestroy(Sender: TObject);
begin
    FreeAndNil (MonitorFilter);
    FreeAndNil(TrafficClass);
    FreeAndNil (MonitorSocket) ;
    FreeAndNil (MonitorPcap) ;
end;

procedure TMonForm.FormShow(Sender: TObject);
var
    IniFile: TIcsIniFile;
    section: String;
begin
    // Nov 2018 socket monitoring needs admin rights
    if IcsIsProgAdmin then
        LabelAdmin.Caption := 'Program has Administrator Rights'
    else
    begin
        LabelAdmin.Caption := 'Program does not have Administrator Rights, no socket monitoring';
        UseWinNCap.Checked := true ;
    end;

    CaptureMode := CapModeNone;
    FIniFileName := GetIcsIniFileName;
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do begin
    // form positions
        Width := ReadInteger(SectionMainWindow, KeyWidth,  Width);
        Height := ReadInteger(SectionMainWindow, KeyHeight, Height);
        Top := ReadInteger(SectionMainWindow, KeyTop, (Screen.Height - Height) div 2);
        Left := ReadInteger(SectionMainWindow, KeyLeft, (Screen.Width  - Width) div 2);

        section := SectionData;
     // first, to fill IP address list
        if ReadString (SectionData, 'UseWinNCap_Checked', 'False') = 'True' then UseWinNCap.Checked := true else UseWinNCap.Checked := false ;
        MonAdptList.ItemIndex := ReadInteger (SectionData, 'MonAdptList_ItemIndex', MonAdptList.ItemIndex) ;
        UseWinNCapClick(Self);
        MonIpList.ItemIndex := ReadInteger (SectionData, 'MonIpList_ItemIndex', MonIpList.ItemIndex) ;
        FilterIpAddr.ItemIndex := ReadInteger (SectionData, 'FilterIpAddr_ItemIndex', FilterIpAddr.ItemIndex) ;
        FilterProtocol.ItemIndex := ReadInteger (SectionData, 'FilterProtocol_ItemIndex', FilterProtocol.ItemIndex) ;
        IpAddrList.Lines.CommaText := ReadString (SectionData, 'IpAddrList_Lines', IpAddrList.Lines.CommaText) ;
        if ReadString (SectionData, 'IpAddrLocal_Checked', 'False') = 'True' then IpAddrLocal.Checked := true else IpAddrLocal.Checked := false ;
        if ReadString (SectionData, 'MonPromiscuous_Checked', 'False') = 'True' then MonPromiscuous.Checked := true else MonPromiscuous.Checked := false ;
        if ReadString (SectionData, 'ProtocolIRC_Checked', 'False') = 'True' then ProtocolIRC.Checked := true else ProtocolIRC.Checked := false ;
        PortsList.Lines.CommaText := ReadString (SectionData, 'PortsList_Lines', PortsList.Lines.CommaText) ;
        if ReadString (SectionData, 'ProtocolARP_Checked', 'False') = 'True' then ProtocolARP.Checked := true else ProtocolARP.Checked := false ;
        if ReadString (SectionData, 'ProtocolBroadcast_Checked', 'False') = 'True' then ProtocolBroadcast.Checked := true else ProtocolBroadcast.Checked := false ;
        if ReadString (SectionData, 'ProtocolDns_Checked', 'False') = 'True' then ProtocolDns.Checked := true else ProtocolDns.Checked := false ;
        if ReadString (SectionData, 'ProtocolHttp_Checked', 'False') = 'True' then ProtocolHttp.Checked := true else ProtocolHttp.Checked := false ;
        if ReadString (SectionData, 'ProtocolICMP_Checked', 'False') = 'True' then ProtocolICMP.Checked := true else ProtocolICMP.Checked := false ;
        if ReadString (SectionData, 'ProtocolIPv4_Checked', 'False') = 'True' then ProtocolIPv4.Checked := true else ProtocolIPv4.Checked := false ;
        if ReadString (SectionData, 'ProtocolIPv6_Checked', 'False') = 'True' then ProtocolIPv6.Checked := true else ProtocolIPv6.Checked := false ;
        if ReadString (SectionData, 'ProtocolNonIp_Checked', 'False') = 'True' then ProtocolNonIp.Checked := true else ProtocolNonIp.Checked := false ;
        if ReadString (SectionData, 'ProtocolSNMP_Checked', 'False') = 'True' then ProtocolSNMP.Checked := true else ProtocolSNMP.Checked := false ;
        if ReadString (SectionData, 'ProtocolSyslog_Checked', 'False') = 'True' then ProtocolSyslog.Checked := true else ProtocolSyslog.Checked := false ;
        if ReadString (SectionData, 'ProtocolTCP_Checked', 'False') = 'True' then ProtocolTCP.Checked := true else ProtocolTCP.Checked := false ;
        if ReadString (SectionData, 'ProtocolUPnP_Checked', 'False') = 'True' then ProtocolUPnP.Checked := true else ProtocolUPnP.Checked := false ;
        if ReadString (SectionData, 'ProtocolUdp_Checked', 'False') = 'True' then ProtocolUdp.Checked := true else ProtocolUdp.Checked := false ;
        if ReadString (SectionData, 'ShowFullData_Checked', 'False') = 'True' then ShowFullData.Checked := true else ShowFullData.Checked := false ;
        if ReadString (SectionData, 'ShowIgnoreData_Checked', 'False') = 'True' then ShowIgnoreData.Checked := true else ShowIgnoreData.Checked := false ;
    end;
    IniFile.Free;
    CapButtons(False);
end;

procedure TMonForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile: TIcsIniFile;
    section, temp: String;
begin
    if CaptureMode <> CapModeNone then
        CaptureStop;
    if FormOneRow.Visible then
        FormOneRow.FormClose(Self, Action);
    FIniFileName := GetIcsIniFileName;
    IniFile := TIcsIniFile.Create(FIniFileName);
    with IniFile do begin
    // form positions
        WriteInteger(SectionMainWindow, KeyTop, Top);
        WriteInteger(SectionMainWindow, KeyLeft, Left);
        WriteInteger(SectionMainWindow, KeyWidth, Width);
        WriteInteger(SectionMainWindow, KeyHeight, Height);
        section := SectionData;
        WriteInteger (SectionData, 'FilterIpAddr_ItemIndex', FilterIpAddr.ItemIndex) ;
        WriteInteger (SectionData, 'FilterProtocol_ItemIndex', FilterProtocol.ItemIndex) ;
        WriteString (SectionData, 'IpAddrList_Lines', IpAddrList.Lines.CommaText) ;
        if IpAddrLocal.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'IpAddrLocal_Checked', temp) ;
        WriteInteger (SectionData, 'MonAdptList_ItemIndex', MonAdptList.ItemIndex) ;
        WriteInteger (SectionData, 'MonIpList_ItemIndex', MonIpList.ItemIndex) ;
        if MonPromiscuous.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'MonPromiscuous_Checked', temp) ;
        if ProtocolIRC.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ProtocolIRC_Checked', temp) ;
        WriteString (SectionData, 'PortsList_Lines', PortsList.Lines.CommaText) ;
        if ProtocolARP.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ProtocolARP_Checked', temp) ;
        if ProtocolBroadcast.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ProtocolBroadcast_Checked', temp) ;
        if ProtocolDns.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ProtocolDns_Checked', temp) ;
        if ProtocolHttp.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ProtocolHttp_Checked', temp) ;
        if ProtocolICMP.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ProtocolICMP_Checked', temp) ;
        if ProtocolIPv4.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ProtocolIPv4_Checked', temp) ;
        if ProtocolIPv6.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ProtocolIPv6_Checked', temp) ;
        if ProtocolNonIp.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ProtocolNonIp_Checked', temp) ;
        if ProtocolSNMP.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ProtocolSNMP_Checked', temp) ;
        if ProtocolSyslog.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ProtocolSyslog_Checked', temp) ;
        if ProtocolTCP.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ProtocolTCP_Checked', temp) ;
        if ProtocolUPnP.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ProtocolUPnP_Checked', temp) ;
        if ProtocolUdp.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ProtocolUdp_Checked', temp) ;
        if ShowFullData.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ShowFullData_Checked', temp) ;
        if ShowIgnoreData.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ShowIgnoreData_Checked', temp) ;
        if UseWinNCap.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'UseWinNCap_Checked', temp) ;
        UpdateFile;
    end;
    IniFile.Free;
end;

procedure TMonForm.PacketEvent (Sender: TObject; PacketInfo: TPacketInfo) ;
var
    mydata: string ;
begin
    if CaptureMode = CapModeNone then
        Exit ;
    if NOT MonitorFilter.AllowPacket(PacketInfo) then
        Exit;
    if CaptureMode = CapModeLog then
    begin
        with PacketInfo do
        begin
            if (NOT ShowFullData.Checked) and (DataLen > 128) then
                SetLength (DataBuf, 128) ;
             if (DataLen = 0) and (ProtoType = IPPROTO_TCP) then
                mydata := IcsTCPFlags (TcpFlags)
             else
                mydata := IcsEscapeCRLF(IcsStrRemCntls('[' + String (DataBuf) + ']')); { V9.2 escape CRLF to \n }
             LogLine (Format (sPacketLine, [IcsTimeToZStr (PacketDT), DispProto, PacketLen, DispAddrSrc, PortSrc,
                                                                              DispAddrDest, PortDest, DispServ, DataLen, mydata])) ;
        end ;
    end
    else if CaptureMode = CapModeGrid then
    begin
        if GridCount >= Length(GridBuffer) then
            SetLength(GridBuffer, GridCount * 2);
        GridBuffer[GridCount] := PacketInfo;
        MonGridWin.Items.Count := GridCount + 1;  { virtual TListView, gets data from event }
        if GridAutoScroll.Checked then
            MonGridWin.Items [GridCount].MakeVisible (false) ;    // scroll
        MonGridWin.UpdateItems(GridCount, GridCount) ;
        GridCount := GridCount + 1;
    end
    else if CaptureMode = CapModeTraffic then
    begin
        TrafficClass.Add (PacketInfo) ;
    end;
end ;

procedure TMonForm.MonGridWinData(Sender: TObject; Item: TListItem);
  //Virtual TListView
  //OnData gets called once for each item for which the ListView needs
  //data. If the ListView is in Report View, be sure to add the subitems.
  //Item is a "dummy" item whose only valid data is it's index which
  //is used to index into the underlying data.
var
    ItemNr: Integer;
    mydata, dispsrc, dispdest: string ;
begin
    ItemNr := Item.Index ;  // line for which data is required
    if ItemNr >= GridCount then
        Exit;
    with GridBuffer[ItemNr] do
    begin
        if (DataLen = 0) and (ProtoType = IPPROTO_TCP) then
            mydata := IcsTCPFlags (TcpFlags)
        else begin
            if (NOT ShowFullData.Checked) and (DataLen > 128) then
                mydata := IcsEscapeCRLF(IcsStrRemCntls(Copy(String (DataBuf), 1, 128)))   { V9.2 escape CRLF to \n }
            else
                mydata := IcsEscapeCRLF(IcsStrRemCntls(String (DataBuf))) ;
        end;
        dispsrc := DispAddrSrc;
        if DispAddrSrc <> HostNameSrc then
            dispsrc := dispsrc + IcsSpace + HostNameSrc;
        dispdest := DispAddrDest;
        if DispAddrDest <> HostNameDest then
            dispdest := dispdest + IcsSpace + HostNameDest;
        Item.Caption := IcsTimeToZStr (PacketDT) ;
        Item.SubItems.Add(DispProto) ;
        Item.SubItems.Add(IntToStr(PacketLen));
        Item.SubItems.Add(dispsrc);
        Item.SubItems.Add(IntToStr (PortSrc));
        Item.SubItems.Add(dispdest);
        Item.SubItems.Add(IntToStr (PortDest));
        Item.SubItems.Add(DispServ);
        Item.SubItems.Add(IntToStr (DataLen));
        Item.SubItems.Add(mydata);
    end;
end;

procedure TMonForm.MonGridWinSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
    I, ItemNr, DTot, DRow, DOff, HRow: Integer;
const
    DataPerRow = 40;

    procedure AddRow;
    begin
        with FormOneRow.ListOneRow.Items.Add do
        begin
            Caption := '';
            SubItems.Add ('');
         end;
    end;

begin
    if NOT Selected then
        Exit;
    if NOT Assigned (Item) then
        Exit;
    ItemNr := Item.Index ;  // line selected, equals GridBuffer
    if ItemNr >= GridCount then
        Exit;
    if NOT FormOneRow.Visible then
        FormOneRow.Show;
    with FormOneRow.ListOneRow do
    begin
        if Items.Count < ColCapDataLast then
        begin
            Items.Clear;
            for I := 0 to ColCapDataLast do
            begin
                AddRow;
                Items[I].Caption := HdrsCapData[I];
            end;
        end;
     // clear old data
        for I := 0 to Items.Count - 1 do
            Items[I].SubItems[0] := '';

        with GridBuffer[ItemNr] do
        begin
            Items[ColCapDataTime].SubItems[0] := IcsTimeToZStr (PacketDT) ;
            Items[ColCapDataPlen].SubItems[0] := IntToStr(PacketLen);
            Items[ColCapDataEProto].SubItems[0] := IcsEtherProtoName(EtherProto);
            Items[ColCapDataMacAddSrc].SubItems[0] := IcsMacToStr (EtherSrc) ;
            Items[ColCapDataMacVenSrc].SubItems[0] := VendorSrc;
            Items[ColCapDataMacAddTar].SubItems[0] := IcsMacToStr (EtherDest) ;
            Items[ColCapDataMacVenTar].SubItems[0] := VendorDest;
            if ((EtherProto = ETHERNET_IP) or (EtherProto = ETHERNET_IPV6)) then
            begin
                Items[ColCapDataTrans ].SubItems[0] := IcsIPProtoName(ProtoType);
                Items[ColCapDataSocFam].SubItems[0] := SocketFamilyNames[SocketFamily];
                Items[ColCapDataAddrSrc].SubItems[0] := DispAddrSrc;
                Items[ColCapDataHostSrc].SubItems[0] := HostNameSrc;
                Items[ColCapDataAddrDes].SubItems[0] := DispAddrDest;
                Items[ColCapDataHostDes].SubItems[0] := HostNameDest;
                Items[ColCapDataIpTTL].SubItems[0] := IntToStr(IpTTL);
                if (ProtoType in [IPPROTO_TCP, IPPROTO_UDP]) then
                begin
                    Items[ColCapDataPortSrc].SubItems[0] := IntToStr (PortSrc);
                    Items[ColCapDataPortDes].SubItems[0] := IntToStr (PortDest);
                    Items[ColCapDataServ].SubItems[0] := DispServ;
                end;
                if (ProtoType = IPPROTO_TCP) then
                begin
                    Items[ColCapDataTFlag].SubItems[0] := IcsTCPFlags (TcpFlags);
                    Items[ColCapDataTSeq].SubItems[0] := IntToStr(TCPCurSeq);
                    Items[ColCapDataTASeq].SubItems[0] := IntToStr(TCPNextSeq);
                    Items[ColCapDataTWin].SubItems[0] := IntToStr(TCPWinSize);
                end;
                if ProtoType = IPPROTO_ICMP then
                begin
                    Items[ColCapDataIcmp].SubItems[0] := Lowercase (IcsICMPType (IcmpType));
                end ;
            end;
            Items[ColCapDataDLen].SubItems[0] := IntToStr(DataLen);
            if (DataLen > 0) then
            begin
            // ascii representation of data, 40 bytes/row
                DTot := (DataLen div DataPerRow) + 1;
                DOff := 1;
                for DRow := ColCapDataData to (ColCapDataData + DTot - 1) do
                begin
                    if Items.Count <= DRow then
                        AddRow;
                    Items[DRow].SubItems[0] := IcsEscapeCRLF(IcsStrRemCntls(Copy (String (DataBuf), DOff, DataPerRow))) ;   { V9.2 escape CRLF to \n }
                    DOff := DOff + DataPerRow;
                end;
                AddRow;
            // hex representation of data, 20 bytes/row
                HRow := ColCapDataData + DTot + 1;
                DTot := (DataLen div DataPerRow * 2) + 1;
                DOff := 1;
                for DRow := HRow to (HRow + DTot - 1) do
                begin
                    if Items.Count <= DRow then
                        AddRow;
                    Items[DRow].SubItems[0] := IcsBufferToHex(Copy (DataBuf, DOff, DataPerRow div 2)) ;
                    DOff := DOff + (DataPerRow div 2);
                end;
            end;
        end;
    end;
end;

procedure TMonForm.MonTrafficWinData(Sender: TObject; Item: TListItem);
var
    ItemNr: Integer;
    disploc, disprem: string ;
    TrafficRec: PTrafficInfo ;
begin
    ItemNr := Item.Index ;  // line for which data is required
    if ItemNr >= TrafficClass.TotTraffic then
        Exit ;
    TrafficRec := TrafficClass.GetSortedTraf (ItemNr) ;
    if NOT Assigned (TrafficRec) then
        exit ;  // sanity check
    with TrafficRec^ do
    begin
        disploc := SrcDispAddr;
        disprem :=  DestDispAddr;
        if TrafficHostNames.Checked then
        begin
            if SrcHostName <> '' then
                disploc := SrcHostName ;
            if DestHostName <> '' then
                disprem := DestHostName ;
        end;
        Item.Caption := disploc;
        Item.SubItems.Add(disprem) ;
        Item.SubItems.Add(ServName) ;
        Item.SubItems.Add(IntToKbyte (BytesSent)) ;
        Item.SubItems.Add(IntToKbyte (PacksSent)) ;
        Item.SubItems.Add(IntToKbyte (BytesRecv)) ;
        Item.SubItems.Add(IntToKbyte (PacksRecv)) ;
        Item.SubItems.Add(TimeToStr (FirstDT)) ;
        Item.SubItems.Add(TimeToStr (LastDT)) ;
    end ;
end;

procedure TMonForm.LoadIpAddrList;
var
    I: Integer;
begin
    if IpAddrList.Lines.Count = 0 then Exit;
    for I := 0 to IpAddrList.Lines.Count - 1 do
        MonitorFilter.SetIpAddr(IpAddrList.Lines[I]);
end;

procedure TMonForm.IcsDomainNameCache1DNLogEvent(Sender: TObject; const Msg: string);
begin
    if DnsCacheLog.Checked then
        BuffLogLines := BuffLogLines + Msg + IcsCRLF;
end;

procedure TMonForm.IpAddrListExit(Sender: TObject);
begin
    LoadIpAddrList;
end;

procedure TMonForm.IpAddrLocalClick(Sender: TObject);
begin
    MonitorFilter.LocalIpAddr := IpAddrLocal.Checked;
end;

procedure TMonForm.FilterIpAddrClick(Sender: TObject);
begin
    MonitorFilter.FilterIpAddr := TMonFilterMeth(FilterIpAddr.ItemIndex);
end;

procedure TMonForm.FilterProtocolClick(Sender: TObject);
begin
    MonitorFilter.FilterProtocol := TMonFilterMeth(FilterProtocol.ItemIndex);
end;

procedure TMonForm.PortsListChange(Sender: TObject);
var
    I: Integer;
begin
    if PortsList.Lines.Count = 0 then Exit;
    for I := 0 to PortsList.Lines.Count - 1 do
        MonitorFilter.SetPort(PortsList.Lines[I]);
end;

procedure TMonForm.ProtocolARPClick(Sender: TObject);
begin
    MonitorFilter.ProtoARP := ProtocolARP.Checked;
end;

procedure TMonForm.ProtocolBroadcastClick(Sender: TObject);
begin
    MonitorFilter.ProtoBroadcast := ProtocolBroadcast.Checked;
end;

procedure TMonForm.ProtocolDnsClick(Sender: TObject);
begin
    MonitorFilter.ProtoDNS := ProtocolDNS.Checked;
end;

procedure TMonForm.ProtocolHttpClick(Sender: TObject);
begin
    MonitorFilter.ProtoHttp := ProtocolHttp.Checked;
end;

procedure TMonForm.ProtocolICMPClick(Sender: TObject);
begin
    MonitorFilter.ProtoICMP := ProtocolICMP.Checked;
end;

procedure TMonForm.ProtocolIPv4Click(Sender: TObject);
begin
    MonitorFilter.ProtoIPv4 := ProtocolIPv4.Checked;
end;

procedure TMonForm.ProtocolIPv6Click(Sender: TObject);
begin
    MonitorFilter.ProtoIPv6 := ProtocolIPv6.Checked;
end;

procedure TMonForm.ProtocolIRCClick(Sender: TObject);
begin
    MonitorFilter.ProtoIRC := ProtocolIRC.Checked;
end;

procedure TMonForm.ProtocolNonIpClick(Sender: TObject);
begin
    MonitorFilter.ProtoNonIp := ProtocolNonIp.Checked;
end;

procedure TMonForm.ProtocolSNMPClick(Sender: TObject);
begin
    MonitorFilter.ProtoSNMP := ProtocolSNMP.Checked;
end;

procedure TMonForm.ProtocolSyslogClick(Sender: TObject);
begin
    MonitorFilter.ProtoSyslog := ProtocolSyslog.Checked;
end;

procedure TMonForm.ProtocolTCPClick(Sender: TObject);
begin
    MonitorFilter.ProtoTCP := ProtocolTCP.Checked;
end;

procedure TMonForm.ProtocolUdpClick(Sender: TObject);
begin
    MonitorFilter.ProtoUdp := ProtocolUdp.Checked;
end;

procedure TMonForm.ProtocolUPnPClick(Sender: TObject);
begin
    MonitorFilter.ProtoUPnP := ProtocolUPnP.Checked;
end;

procedure TMonForm.LogLine (const Msg: String) ;
begin
    BuffLogLines := BuffLogLines + Msg + IcsCRLF;
end;

procedure TMonForm.LogEvent (Sender: TObject; const Msg: String) ;
begin
    LogLine(Msg);
end;

procedure TMonForm.doClearLogClick(Sender: TObject);
begin
  MonLogWin.Lines.Clear ;
  BuffLogLines := '';
end;

procedure TMonForm.doDnsCacheClearClick(Sender: TObject);
begin
    IcsDomainNameCache1.MaintClearAll;
end;

procedure TMonForm.doDnsCacheListClick(Sender: TObject);
begin
    BuffLogLines := BuffLogLines + IcsDomainNameCache1.ListCache + IcsCRLF;
end;

procedure TMonForm.doExitClick(Sender: TObject);
begin
    if CaptureMode <> CapModeNone then
        CaptureStop;
    Close ;
end;

procedure TMonForm.doPackGridClearClick(Sender: TObject);
begin
    MonGridWin.Items.Clear;
    SetLength (GridBuffer, 0);
    GridCount := 0;
end;

procedure TMonForm.doPackGridStartClick(Sender: TObject);
begin
    if Length(GridBuffer) = 0 then
        SetLength(GridBuffer, 512);   // somewhere to store our data for grid
    CaptureMode := CapModeGrid;
    CaptureStart;
end;

procedure TMonForm.doPackGridStopClick(Sender: TObject);
begin
    CaptureStop;
end;

procedure TMonForm.doPackLogStartClick(Sender: TObject);
begin
    CaptureMode := CapModeLog;
    CaptureStart;
    MonLogWin.Lines.Add (IcsCRLF + sHeaderLine + IcsCRLF);
end;

procedure TMonForm.doPackLogStopClick(Sender: TObject);
begin
    CaptureStop;
end;

procedure TMonForm.doTrafficClearClick(Sender: TObject);
begin
    MonTrafficWin.Items.Clear;
    TrafficClass.Clear;
end;

procedure TMonForm.doTrafficLogTotalsClick(Sender: TObject);
var
    I: integer ;
    S: string ;
begin
    doClearLogClick(Self);
    PageControl1.ActivePage := TabPacketsLog;
    LogLine (sTrafficHdr) ;
    if TrafficClass.TotTraffic = 0 then
        exit ;
    doTrafficLogTotals.Enabled := False;
    try
        TrafficClass.UpdateService ;
        for I := 0 to Pred (TrafficClass.TotTraffic) do
        begin
            S := TrafficClass.FmtTrafStr (TrafficClass.GetSortedTraf (I), TrafficHostNames.Checked);
            if S = '' then
                continue ;  // sanity check
            LogLine (S) ;
        end ;
        MonLogWin.Lines.Add (IcsCRLF + sServiceHdr) ;
        if TrafficClass.TotService = 0 then
            exit ;
        for I := 0 to Pred (TrafficClass.TotService) do
        begin
            S := TrafficClass.GetFmtServStr (I) ;
            if S = '' then continue ;  // sanity check
            LogLine (S) ;
        end ;
    finally
       doTrafficLogTotals.Enabled := True;
    end;
end;

procedure TMonForm.doTrafficStartClick(Sender: TObject);
begin
    CaptureMode := CapModeTraffic;
    MonTrafficWin.Items.Clear;
    TrafficClass.Clear;
    CaptureStart;
end;

procedure TMonForm.doTrafficStopClick(Sender: TObject);
begin
    CaptureStop;
end;

procedure TMonForm.CapButtons(Run: Boolean);
begin
    doPackGridStart.Enabled := NOT Run;
    doPackLogStart.Enabled := NOT Run;
    doTrafficStart.Enabled := NOT Run;
    doPackGridStop.Enabled := Run;
    doPackLogStop.Enabled := Run;
    doTrafficStop.Enabled := Run;
end;

procedure TMonForm.CaptureStart;
var
    I: integer ;
begin
    if MonIpList.ItemIndex < 0 then
    begin
        CaptureMode := CapModeNone;
        exit ;
    end;
    try
        LoadIpAddrList;
        if UseWinNCap.Checked then
        begin
            MonitorPcap.MonAdapter := MonitorPcap.AdapterNameList [MonAdptList.ItemIndex] ;
            I := MonitorPcap.GetIPAddresses (MonitorPcap.MonAdapter, AdapterIPList, AdapterMaskList, AdapterBcastList) ;
            if I > 0 then
            begin
                MonitorPcap.Addr := AdapterIPList [0] ;
            end
            else
            begin
                MonitorPcap.Addr := MonIpList.Items [MonIpList.ItemIndex] ;
            end ;
            MonitorPcap.IgnoreData := ShowIgnoreData.Checked ;
            MonitorPcap.Promiscuous := MonPromiscuous.Checked ;
            MonitorPcap.StartMonitor ;
            if NOT MonitorPcap.Connected then
            begin
                LogLine (MonitorPcap.LastError) ;
                exit ;
            end ;
            LogLine ('Capture Started - ' + MonitorPcap.MonAdapter + ' on ' + MonitorPcap.Addr) ;
        end
        else
        begin
            MonitorSocket.Addr := MonIpList.Items [MonIpList.ItemIndex] ;
            MonitorSocket.IgnoreData := ShowIgnoreData.Checked ;
            MonitorSocket.StartMonitor ;
            LogLine ('Capture Started - Raw Sockets on ' + MonitorSocket.Addr) ;
        end ;
        CapButtons(True);
    except
        LogLine ('Failed to Start Monitor - ' + IcsGetExceptMess (ExceptObject)) ;
        if NOT UseWinNCap.Checked then
            MonitorSocket.StopMonitor ;
        CaptureMode := CapModeNone;
        CapButtons(False);
    end ;
end;

procedure TMonForm.CaptureStop;
begin
    if CaptureMode <> CapModeNone then
    begin
        LogLine ('Capture Stopped' + IcsCRLF) ;
        if UseWinNCap.Checked then
            MonitorPcap.StopMonitor
        else
            MonitorSocket.StopMonitor ;
    end;
    CaptureMode := CapModeNone;
    CapButtons(False);
end;

procedure TMonForm.TimerTimer(Sender: TObject);
var
    Traffic: String;
    displen: integer ;
begin
    displen := Length(BuffLogLines);
    if displen > 0 then begin
        try
            SetLength(BuffLogLines, displen - 2) ;  // remove CRLF
            MonLogWin.Lines.Add(BuffLogLines);
            SendMessage(MonLogWin.Handle, EM_LINESCROLL, 0, 999999);
        except
        end ;
        BuffLogLines := '';
    end;

 // update traffic captions
    if CaptureMode = CapModeNone then
        Exit;
    if UseWinNCap.Checked then
    begin
        with MonitorPcap do
          Traffic := 'Traffic: Sent ' + IntToKbyte (TotSendBytes) + ', Received ' + IntToKbyte (TotRecvBytes) + IcsCRLF +
                     'Packets Sent ' + IntToStr (TotSendPackets) + ', Received ' + IntToStr (TotRecvPackets) ;
    end
    else
    begin
        with MonitorSocket do
          Traffic := 'Traffic: Sent ' + IntToKbyte (TotSendBytes) + ', Received ' + IntToKbyte (TotRecvBytes) + IcsCRLF +
                     'Packets Sent ' + IntToStr (TotSendPackets) +  ', Received ' + IntToStr (TotRecvPackets) ;
    end ;
    LabelTrafficLog.Caption := Traffic;
    LabelTrafficGrid.Caption := Traffic;
    LabelTrafficTotals.Caption := Traffic;

 // for traffic, update TListView lines which causes OnData event to get new details
 // perhaps less often...
    if CaptureMode = CapModeTraffic then begin
        MonTrafficWin.Items.Count := TrafficClass.TotTraffic;  // lines of traffic to display
        MonTrafficWin.Refresh;
    end;
end;

procedure TMonForm.MonAdptListClick(Sender: TObject);
var
    I: integer ;
begin
    if MonAdptList.Items.Count = 0 then Exit;
    I := MonitorPcap.GetIPAddresses (MonitorPcap.AdapterNameList [MonAdptList.ItemIndex],
                                        AdapterIPList, AdapterMaskList, AdapterBcastList) ;
    if I = 0 then exit ;
    MonIpList.Items.Assign (AdapterIPList) ;
    if MonIpList.Items.Count > 0 then
        MonIpList.ItemIndex := 0 ;
end;


procedure TMonForm.UseWinNCapClick(Sender: TObject);
begin
    if UseWinNCap.Checked then
        MonAdptListClick(self)
    else
    begin
        MonIpList.Items := LocalIPList (sfAny);
        if MonIpList.Items.Count > 0 then
            MonIpList.ItemIndex := 0 ;
    end ;
end;


end.

