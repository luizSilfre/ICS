{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Description:  MQ Telemetry Transport is a lightweight, publish-subscribe, machine
              to machine network protocol for message queue/message queuing service.
              The MQTT protocol defines two types of network entities: a message broker
              and a number of clients. An MQTT broker is a server that receives all
              messages from the clients and then routes the messages to the appropriate
              destination clients. An MQTT client is any device (from a micro controller
              up to a fully-fledged server) that runs an MQTT library and connects to
              an MQTT broker over a network.
Creation:     March 2009
Updated:      Aug 2024
Version:      V9.3
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2008-2024 by pjde and Geoffrey Smith,
              https://github.com/pjde/delphi-mqtt
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
Feb 24th 2023 V8.71 Baseline, incorporated into ICS samples.
               Added SSL/TLS support for client and server.
               Try to give components meaningful names and save them properly,
                 uncertain about publishing parameter names...
Aug 08, 2023 V9.0  Updated version to major release 9.
Dec 19, 2023  V9.1 Added OverbyteIcsSslBas, IcsSslRootCAStore.Initialise and
                     LibeayLoadProviders since this sample needs the OpenSSL Legacy provider.
Aug 07, 2024 V9.3  Added OverbyteIcsTypes for consolidated types and constants, allowing
                     other import units to be removed.


WARNING - this sample needs the VirtualTree component installed from GetIt or
https://github.com/TurboPack/VirtualTreeView, otherwise remove the
OverbyteIcsMQTTBroker unit and all it's references.


SSL Notes
---------

For the MQTT server to support SSL/TLS on port 8883 a certificate host name and
certificate bundle file name are required, the file may be PFX or PEM containing
the SSL certificate and private key and optionally any intermediate files required
for public verification, use the private key password 'password' or change the
sample code. If host or file name are blank, SSL is ignored.  A self signed localhost
cerificate is supplied for IP address 127.0.0.1, but beware this will fail chain
verification by the client so uncheck 'Verify Certification Chain'.

Not yet implemented auto certificate ordering mor tested with on the public
internet.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMQTTst1;

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

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Types,
//  OverbyteIcsSsleay, OverbyteIcsLibeay,
  OverbyteIcsWndControl,
  OverbyteIcsWSocket,
  OverbyteIcsWSocketS,
  OverbyteIcsIniFiles,
  OverbyteIcsMQTT,
  OverbyteIcsSslX509Certs,
  OverbyteIcsSslBase,       { V9.1 TSslContext, TX509Bas, TX509List }
  OverbyteIcsUtils,         { V9.3 constants }
  OverbyteIcsTypes;  { V9.3 consolidated types and constants }

type
  TMainForm = class (TForm)
 // saved components
    BounceBoxCli: TCheckBox;
    BounceBoxSrv: TCheckBox;
    CMBoxCli: TCheckBox;
    CMBoxSrvr: TCheckBox;
    CMsgId: TEdit;
    CPortTxt: TEdit;
    CertAutoOrder: TCheckBox;
    CertBundleFile: TEdit;
    CertHostName: TEdit;
    CleanBox2: TCheckBox;
    CliJTxt: TEdit;
    ClientHost: TEdit;
    ClientQOS: TRadioGroup;
    MsgBox: TMemo;
    RetainBox: TCheckBox;
    RootCAFile: TEdit;
    ServerIpAddr: TComboBox;
    ServerNonPort: TEdit;
    ServerSSLPort: TEdit;
    TopicTxt: TEdit;
    TopicsTxt: TMemo;
    VerifyCertChain: TCheckBox;

 // unsaved components
    MQTTServer: TIcsMQTTServer;
    MQTTClient: TIcsMQTTClient;
    PixTxt: TLabel;
    GroupBoxClient: TGroupBox;
    ClientMemo3: TMemo;
    ButtonCliOff: TButton;
    ButtonCliOn: TButton;
    Label6: TLabel;
    ButtonCliSend: TButton;
    ButtonCliPublish: TButton;
    ButtonCliUnsub: TButton;
    ButtonCliShow: TButton;
    ButtonCliSubs: TButton;
    Label5: TLabel;
    Label18: TLabel;
    ButtonCliUpdate: TButton;
    Label13: TLabel;
    ButtonCliKill: TButton;
    ButtonCliStop: TButton;
    ButtonCliStart: TButton;
    COnlineTxt: TLabel;
    Label4: TLabel;
    CEnableTxt: TLabel;
    Label3: TLabel;
    Label16: TLabel;
    CMsgTxt: TLabel;
    Label8: TLabel;
    ClientMemo: TMemo;
    GroupBoxServer: TGroupBox;
    ServerMemo: TMemo;
    Label11: TLabel;
    SMsgTxt: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    SEnableTxt: TLabel;
    Label9: TLabel;
    SClientsTxt: TLabel;
    ButtonSrvStart: TButton;
    ButtonSrvStop: TButton;
    ButtonSrvShowCli: TButton;
    ButtonSrvBrokers: TButton;
    Label10: TLabel;
    Label12: TLabel;
    ClientIDTxt: TLabel;
    CIDTxt: TLabel;
    CQosTxt: TLabel;
    SQosTxt: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label17: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    SslX509Certs1: TSslX509Certs;

    procedure FormCreate (Sender : TObject);
    procedure ServerMemoDblClick(Sender: TObject);
    procedure ButtonSrvStartClick(Sender: TObject);
    procedure ButtonSrvStopClick(Sender: TObject);
    procedure ClientMemoDblClick(Sender: TObject);
    procedure ButtonCliStartClick(Sender: TObject);
    procedure ButtonCliStopClick(Sender: TObject);
    procedure ButtonSrvShowCliClick(Sender: TObject);
    procedure ButtonCliShowClick(Sender: TObject);
    procedure ButtonSrvBrokersClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ButtonCliKillClick(Sender: TObject);
    procedure ButtonCliSubsClick(Sender: TObject);
    procedure ButtonCliUnsubClick(Sender: TObject);
    procedure ButtonCliPublishClick(Sender: TObject);
    procedure MQTTClientFailure(Sender: TObject; aReason: Integer; var CloseClient: Boolean);
    procedure MQTTClientMon(Sender: TObject; aStr: string);
    procedure MQTTClientEnableChange(Sender: TObject);
    procedure MQTTClientMsg (Sender: TObject; aTopic: UTF8String;  aMessage: AnsiString; aQos : TMQTTQOSType; aRetained : Boolean);
    procedure MQTTClientOffline(Sender: TObject; Graceful: Boolean);
    procedure MQTTClientOnline(Sender: TObject);
    procedure MQTTServerCheckUser(Sender: TObject; aUser, aPass: UTF8String; var Allowed: Boolean);
    procedure MQTTServerClientsChange(Sender: TObject; anID: Word);
    procedure MQTTServerFailure(Sender: TObject; aReason: Integer; var CloseClient: Boolean);
    procedure MQTTServerEnableChange(Sender: TObject);
    procedure MQTTServerMon(Sender: TObject; aStr: string);
    procedure MQTTServerRestoreSession(Sender: TObject; aClientID: UTF8String);
    procedure MQTTServerStoreSession(Sender: TObject; aClientID: UTF8String);
    procedure MQTTServerDeleteSession(Sender: TObject; aClientID: UTF8String);
    procedure MQTTServerObituary(Sender: TObject; var aTopic, aMessage: UTF8String; var aQos: TMQTTQOSType);
    procedure MQTTServerSubscription(Sender: TObject; aTopic: UTF8String; var RequestedQos: TMQTTQOSType);
    procedure MQTTServerBrokerOffline(Sender: TObject; Graceful: Boolean);
    procedure MQTTServerBrokerOnline(Sender: TObject);
    procedure MQTTServerBrokerEnableChange(Sender: TObject);
    procedure ButtonCliUpdateClick(Sender: TObject);
    procedure ClientMemo3DblClick(Sender: TObject);
    procedure RetainBoxClick(Sender: TObject);
    procedure ButtonCliSendClick(Sender: TObject);
    procedure ButtonCliOffClick(Sender: TObject);
    procedure ButtonCliOnClick(Sender: TObject);
    procedure MQTTClientClientID(Sender: TObject; var aClientID: UTF8String);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    { Private declarations }
  public
    { Public declarations }
//    aQos : TMQTTQOSType;
    aRetain : Boolean;
    procedure StoreSettings;
    procedure LoadSettings;
    procedure SMonHeader (Sender : TObject; aMsgType: TMQTTMessageType; aDup: Boolean; aQos: TMQTTQOSType; aRetain: Boolean);
    procedure CMonHeader (Sender : TObject; aMsgType: TMQTTMessageType; aDup: Boolean; aQos: TMQTTQOSType; aRetain: Boolean);
  end;

var
  MainForm: TMainForm;
  FIniFileName: String;

implementation

uses OverbyteIcsMQTTBroker;

{$R *.dfm}
{ TForm1 }

procedure TMainForm.FormCreate (Sender: TObject);
begin
  aRetain := false;
  ServerIpAddr.Items.Assign(LocalIPList(sfAny));
  ServerIpAddr.Items.Insert(0, ICS_LOCAL_HOST_V4);
  ServerIpAddr.ItemIndex := 0;
  LoadSettings;
  // load retained messages
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  MQTTServer.Activate(false);
  MQTTClient.Activate (false);
  // store retained messages
  StoreSettings;
end;

procedure TMainForm.FormShow (Sender: TObject);
begin
  MQTTServer.FOnMonHdr := SMonHeader;
  MQTTClient.Parser.OnHeader := CMonHeader;
end;

procedure TMainForm.LoadSettings;
var
    SectionData: String;
begin
    FIniFileName := GetIcsIniFileName;
    SectionData := 'MQTT';
    with TIcsIniFile.Create (FIniFileName) do
    begin
        if ReadString (SectionData, 'BounceBoxCli_Checked', 'False') = 'True' then BounceBoxCli.Checked := true else BounceBoxCli.Checked := false ;
        if ReadString (SectionData, 'CMBoxCli_Checked', 'True') = 'True' then CMBoxCli.Checked := true else CMBoxCli.Checked := false ;    // edited true
        if ReadString (SectionData, 'CMBoxSrvr_Checked', 'True') = 'True' then CMBoxSrvr.Checked := true else CMBoxSrvr.Checked := false ; // edited true
        CMsgId.Text := ReadString (SectionData, 'CMsgId_Text', CMsgId.Text) ;
        CPortTxt.Text := ReadString (SectionData, 'CPortTxt_Text', CPortTxt.Text) ;
        if ReadString (SectionData, 'CertAutoOrder_Checked', 'False') = 'True' then CertAutoOrder.Checked := true else CertAutoOrder.Checked := false ;
        CertBundleFile.Text := ReadString (SectionData, 'CertBundleFile_Text', CertBundleFile.Text) ;
        CertHostName.Text := ReadString (SectionData, 'CertHostName_Text', CertHostName.Text) ;
        if ReadString (SectionData, 'CleanBox2_Checked', 'False') = 'True' then CleanBox2.Checked := true else CleanBox2.Checked := false ;
        CliJTxt.Text := ReadString (SectionData, 'CliJTxt_Text', CliJTxt.Text) ;
        ClientHost.Text := ReadString (SectionData, 'ClientHost_Text', ClientHost.Text) ;
        ClientQOS.ItemIndex := ReadInteger (SectionData, 'ClientQOS_ItemIndex', ClientQOS.ItemIndex) ;
        MsgBox.Lines.CommaText := ReadString (SectionData, 'MsgBox_Lines', MsgBox.Lines.CommaText) ;
        if ReadString (SectionData, 'RetainBox_Checked', 'False') = 'True' then RetainBox.Checked := true else RetainBox.Checked := false ;
        RootCAFile.Text := ReadString (SectionData, 'RootCAFile_Text', RootCAFile.Text) ;
        ServerIpAddr.Text := ReadString (SectionData, 'ServerIpAddr_Text', ServerIpAddr.Text) ;
        ServerNonPort.Text := ReadString (SectionData, 'ServerNonPort_Text', ServerNonPort.Text) ;
        ServerSSLPort.Text := ReadString (SectionData, 'ServerSSLPort_Text', ServerSSLPort.Text) ;
        TopicTxt.Text := ReadString (SectionData, 'TopicTxt_Text', TopicTxt.Text) ;
        TopicsTxt.Lines.CommaText := ReadString (SectionData, 'TopicsTxt_Lines', TopicsTxt.Lines.CommaText) ;
        if ReadString (SectionData, 'VerifyCertChain_Checked', 'True') = 'True' then VerifyCertChain.Checked := true else VerifyCertChain.Checked := false ;  // edited true
        Free;
    end;
end;

procedure TMainForm.StoreSettings;
var
    SectionData, temp: String;
begin
    FIniFileName := GetIcsIniFileName;
    SectionData := 'MQTT';
    with TIcsIniFile.Create (FIniFileName) do
    begin
        if BounceBoxCli.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'BounceBoxCli_Checked', temp) ;
        if CMBoxCli.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CMBoxCli_Checked', temp) ;
        if CMBoxSrvr.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CMBoxSrvr_Checked', temp) ;
        WriteString (SectionData, 'CMsgId_Text', CMsgId.Text) ;
        WriteString (SectionData, 'CPortTxt_Text', CPortTxt.Text) ;
        if CertAutoOrder.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertAutoOrder_Checked', temp) ;
        WriteString (SectionData, 'CertBundleFile_Text', CertBundleFile.Text) ;
        WriteString (SectionData, 'CertHostName_Text', CertHostName.Text) ;
        if CleanBox2.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CleanBox2_Checked', temp) ;
        WriteString (SectionData, 'CliJTxt_Text', CliJTxt.Text) ;
        WriteString (SectionData, 'ClientHost_Text', ClientHost.Text) ;
        WriteInteger (SectionData, 'ClientQOS_ItemIndex', ClientQOS.ItemIndex) ;
        WriteString (SectionData, 'MsgBox_Lines', MsgBox.Lines.CommaText) ;
        if RetainBox.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'RetainBox_Checked', temp) ;
        WriteString (SectionData, 'RootCAFile_Text', RootCAFile.Text) ;
        WriteString (SectionData, 'ServerIpAddr_Text', ServerIpAddr.Text) ;
        WriteString (SectionData, 'ServerNonPort_Text', ServerNonPort.Text) ;
        WriteString (SectionData, 'ServerSSLPort_Text', ServerSSLPort.Text) ;
        WriteString (SectionData, 'TopicTxt_Text', TopicTxt.Text) ;
        WriteString (SectionData, 'TopicsTxt_Lines', TopicsTxt.Lines.CommaText) ;
        if VerifyCertChain.Checked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'VerifyCertChain_Checked', temp) ;
        UpdateFile ;
        Free;
    end;
end;

procedure TMainForm.ButtonSrvStartClick(Sender: TObject);
begin
    IcsSslRootCAStore.Initialise;       { V9.1 if OpenSSL and internal not loaded, do it }
    IcsSslLoadProviders(True, False);   { V9.3 need legacy provider }
    MQTTServer.IcsHosts[0].BindIpAddr := ServerIpAddr.Text;
//  MQTTServer.IcsHosts[0].BindIpAddr2 // can listen on a second IPv6 address
    MQTTServer.IcsHosts[0].BindNonPort := StrToIntDef (ServerNonPort.Text, 1883);
    MQTTServer.IcsHosts[0].BindSslPort := 0;
    if (CertBundleFile.Text <> '') then begin   // no SSL unless we have a certificate and private key
      if FileExists(CertBundleFile.Text) and (CertHostName.Text <> '') then begin
          MQTTServer.IcsHosts[0].BindSslPort := StrToIntDef (ServerSslPort.Text, 8883);
          MQTTServer.IcsHosts[0].HostNames.Text := CertHostName.Text;
          MQTTServer.IcsHosts[0].SslCert := CertBundleFile.Text;
          MQTTServer.IcsHosts[0].SslPassword := 'password';
          MQTTServer.IcsHosts[0].SslSrvSecurity := sslSrvSecHigh;
          MQTTServer.IcsHosts[0].HostTag := 'MQTTServer' ;
          MQTTServer.IcsHosts[0].Descr := MQTTServer.IcsHosts[0].HostTag;
          MQTTServer.IcsHosts[0].CertSupplierProto := SuppProtoNone;
        //  MQTTServer.IcsHosts[0].CertDirWork
      end
      else
          ServerMemo.Lines.Add ('SSL/TLS certificate or host not found, SSL disabled');
    end;
    MQTTServer.CertExpireDays := 30;
  //  MQTTServer.RootCA := RootCAFile.Text;
    MQTTServer.SslCertAutoOrder := CertAutoOrder.Checked;
    MQTTServer.LocalBounce := BounceBoxSrv.Checked;
    MQTTServer.Activate (true);
end;

procedure TMainForm.ButtonSrvStopClick (Sender: TObject);
begin
  if MQTTServer.Enabled then
  begin
    MQTTServer.Activate(false);
    ServerMemo.Lines.Add ('Stopped server');
  end;
end;

procedure TMainForm.ButtonCliStopClick (Sender: TObject);
begin
  if MQTTClient.Enabled then
  begin
    MQTTClient.Activate (false);
    ClientMemo.Lines.Add ('Stopped client');
  end;
end;

procedure TMainForm.ButtonCliStartClick (Sender: TObject);
begin
  MQTTClient.Host := ClientHost.Text;
  MQTTClient.Port := StrToIntDef (CPortTxt.Text, 1883);
  MQTTClient.LocalBounce := BounceBoxCli.Checked;
  MQTTClient.Clean := CleanBox2.Checked;
  MQTTClient.SslCliSecurity := sslCliSecTls12;
  MQTTClient.SslVerifyCerts := VerifyCertChain.Checked;
  if VerifyCertChain.Checked then
  begin
        MQTTClient.SslRevocation := True;
        MQTTClient.SslReportChain := True;
//        MQTTClient.RootCA := RootCAFile.Text;
  end;
  MQTTClient.Activate (true);
end;

procedure TMainForm.ButtonCliShowClick (Sender: TObject);
var
  j : integer;
  x : cardinal;
begin
  ClientMemo.Lines.Add ('');
  ClientMemo.Lines.Add('------ Client ' + string (MQTTClient.Parser.ClientID) + ' -------');
  ClientMemo.Lines.Add (format ('Username "%s" Password "%s"', [MQTTClient.Parser.Username, MQTTClient.Parser.Password]));
  ClientMemo.Lines.Add (format ('Keep Alive "%d" Retry Time "%d" Max Retries "%d"', [MQTTClient.Parser.KeepAlive, MQTTClient.Parser.RetryTime, MQTTClient.Parser.MaxRetries]));
  ClientMemo.Lines.Add (format ('Will Topic "%s" Message "%s" @ %s', [MQTTClient.Parser.WillTopic, MQTTClient.Parser.WillMessage, QosNames [MQTTClient.Parser.WillQos]]));
  ClientMemo.Lines.Add ('Subscriptions ----');
  for j := 0 to MQTTClient.Subscriptions.Count - 1 do
    begin
      x := cardinal (MQTTClient.Subscriptions.Objects[j]) and $03;
      if (cardinal (MQTTClient.Subscriptions.Objects[j]) shr 8) and $ff = $ff then
        ClientMemo.Lines.Add ('  "' + MQTTClient.Subscriptions[j] + '" @ ' + QOSNames[TMQTTQOSType (x)] + ' Acked.')
      else
        ClientMemo.Lines.Add ('  "' + MQTTClient.Subscriptions[j] + '" @ ' + QOSNames[TMQTTQOSType (x)]);
    end;
end;

procedure TMainForm.ButtonSrvShowCliClick(Sender: TObject);
var
  i, j : integer;
  aClient : TSrvrClient;
  x : cardinal;
begin
  for i := 0 to MQTTServer.Server.ClientCount - 1 do
    begin
      aClient := TSrvrClient (MQTTServer.Server.Client[i]);
      ServerMemo.Lines.Add ('');
      ServerMemo.Lines.Add('------ Client ' + string (aClient.Parser.ClientID) + ' -------');
      ServerMemo.Lines.Add (format ('Username "%s" Password "%s"', [aClient.Parser.Username, aClient.Parser.Password]));
      ServerMemo.Lines.Add (format ('Keep Alive "%d" Retry Time "%d" Max Retries "%d"', [aClient.Parser.KeepAlive, aClient.Parser.RetryTime, aClient.Parser.MaxRetries]));
      ServerMemo.Lines.Add (format ('Will Topic "%s" Message "%s" @ %s', [aClient.Parser.WillTopic, aClient.Parser.WillMessage, QosNames [aClient.Parser.WillQos]]));
      ServerMemo.Lines.Add ('Subscriptions ----');
      for j := 0 to aClient.Subscriptions.Count - 1 do
        begin
          x := cardinal (aClient.Subscriptions.Objects[j]) and $03;
          ServerMemo.Lines.Add ('  "' + aClient.Subscriptions[j] + '" @ ' + QOSNames[TMQTTQOSType (x)]);
        end;
    end;
end;

procedure TMainForm.ButtonCliPublishClick (Sender: TObject);
var
  i, x : integer;
  aStr : AnsiString;
begin
  aStr := '';
  for i := 0 to MsgBox.Lines.Count - 1 do
    begin
      x := length (MsgBox.Lines[i]);
      aStr := aStr + AnsiChar (x div $100) + AnsiChar (x mod $100) + AnsiString (MsgBox.Lines[i]);
    end;
  MQTTClient.Publish (UTF8String (TopicTxt.Text), aStr, TMQTTQOSType(ClientQOS.ItemIndex), aRetain);
end;

procedure TMainForm.ButtonSrvBrokersClick (Sender: TObject);
begin
  BrokerForm.FServer := MQTTServer;
  BrokerForm.Show;
end;

procedure TMainForm.ButtonCliKillClick (Sender: TObject);
begin
  try
    MQTTClient.LinkSocket.Close;
  except
  end;
end;

procedure TMainForm.ButtonCliSubsClick (Sender: TObject);
var
  s : TStringlist;
  i : integer;
begin
  s := TStringList.Create;
  for i := 0 to TopicsTxt.Lines.Count - 1 do
    s.AddObject(TopicsTxt.Lines[i], TObject (TMQTTQOSType(ClientQOS.ItemIndex)));
  MQTTClient.Subscribe (s);
  s.Free;
end;

procedure TMainForm.ButtonCliUnsubClick(Sender: TObject);
var
  s : TStringlist;
  i : integer;
begin
  s := TStringList.Create;
  for i := 0 to TopicsTxt.Lines.Count - 1 do
    s.Add (TopicsTxt.Lines[i]);
  MQTTClient.Unsubscribe (s);
  s.Free;
end;

procedure TMainForm.ButtonCliUpdateClick(Sender: TObject);
begin
  MQTTClient.Publish ('request/png/' + MQTTClient.ClientID, '?', qtEXACTLY_ONCE);
end;

procedure TMainForm.ButtonCliOnClick(Sender: TObject);
begin
  MQTTClient.Publish ('csi/pnl/set/state/' + UTF8String (CliJTxt.Text), '1', qtAT_MOST_ONCE, false);
end;

procedure TMainForm.ButtonCliSendClick(Sender: TObject);
begin
  MQTTClient.Publish ('csi/pnl/set/text/' + UTF8String (CliJTxt.Text), AnsiString (CMsgId.Text), qtAT_MOST_ONCE, false);
end;

procedure TMainForm.ButtonCliOffClick(Sender: TObject);
begin
  MQTTClient.Publish ('csi/pnl/set/state/' + UTF8String (CliJTxt.Text), '0', qtAT_MOST_ONCE, false);
end;

procedure TMainForm.CMonHeader(Sender: TObject; aMsgType: TMQTTMessageType;  aDup: Boolean; aQos: TMQTTQOSType; aRetain: Boolean);
begin
  CMsgTxt.Caption := MsgNames[aMsgType];
  CQosTxt.Caption := QosNames[aQos];
end;

procedure TMainForm.MQTTClientClientID(Sender: TObject; var aClientID: UTF8String);
begin
  MQTTClient.SetWill ('will/' + aClientID, 'I''ve had it folks..', qtEXACTLY_ONCE);
end;

procedure TMainForm.MQTTClientEnableChange(Sender: TObject);
begin
  CEnableTxt.Caption := ny[TIcsMQTTClient (Sender).Enabled];
  if TIcsMQTTClient (Sender).Enabled then
    ClientMemo.Lines.Add ('Client is enabled.')
  else
    ClientMemo.Lines.Add ('Client is disabled.');
end;

procedure TMainForm.MQTTClientFailure(Sender: TObject; aReason: Integer; var CloseClient: Boolean);
begin
  ClientMemo.Lines.Add ('---- Failure Reported ' + MqttFailureNames (aReason));
end;

procedure TMainForm.MQTTClientMon (Sender: TObject; aStr: string);
begin
  if CMBoxCli.Checked then
    ClientMemo.Lines.Add (aStr);
end;

procedure TMainForm.MQTTClientMsg (Sender: TObject; aTopic: UTF8String; aMessage: AnsiString; aQos : TMQTTQOSType; aRetained : boolean);
var
  i, x : integer;
  aStr : string;
  ForMe : boolean;
  t : TStringList;
begin                                 // Sender TMQTTClient
  ClientMemo.Lines.Add ('MESSAGE "' + string (aTopic) + '".');
  ClientMemo.Lines.Add (IntToStr (length (aMessage)) + ' byte(s) @ ' + QOSNames[aQos]);
  if aRetained then
    ClientMemo.Lines.Add('This is a Retained message.');
  t := SubTopics (aTopic);
  if t[0] = 'will' then
    ClientMemo.Lines.Add (string (aMessage))
  else if t.Count >= 2 then
    begin
      if t[0] = 'update' then
        begin
          if t[1] = 'memo' then
            begin
              ForMe := true;
              if (t.Count > 2) then ForMe := (t[2] = string (MQTTClient.ClientID));
              if ForMe then
                begin
                  ClientMemo3.Lines.Clear;
                  i := 1;
                  while (i + 1) <= length (aMessage) do
                    begin
                      aStr := '';
                      x := ord (aMessage[i]) * $100 + ord (aMessage[i + 1]);
                      i := i + 2;
                      if (x > 0) then
                        begin
                          aStr := Copy (string (aMessage), i, x);
                          i := i + x;
                        end;
                      ClientMemo3.Lines.Add (aStr);
                    end;  // while
                  if aRetained then
                    ClientMemo3.Lines.Add ('(Retained)');
                end;  // forme
            end;   // [t[1]
        end;    // t[0]
    end;  // t.Count >= 2
  ClientMemo.Lines.Add ('MESSAGE END');
  t.Free;
end;

procedure TMainForm.MQTTClientOffline(Sender: TObject; Graceful: Boolean);
begin
  COnlineTxt.Caption := 'NO';
  ClientIDTxt.Caption := '';
  if Graceful then
    ClientMemo.Lines.Add ('Client Gracefully Disconnected.')
  else
    ClientMemo.Lines.Add ('Client Terminated Unexpectedly.');
end;

procedure TMainForm.MQTTClientOnline(Sender: TObject);
begin
  COnlineTxt.Caption := 'YES';
  ClientMemo.Lines.Add ('Client is online.');
  ClientIDTxt.Caption := string (MQTTClient.Parser.ClientID);
  ButtonCliSubsClick (ButtonCliSubs);
end;

procedure TMainForm.MQTTServerBrokerEnableChange(Sender: TObject);
begin
  if BrokerForm.Visible then
    BrokerForm.RefreshTree;
end;

procedure TMainForm.MQTTServerBrokerOffline(Sender: TObject; Graceful: Boolean);
begin
  if BrokerForm.Visible then
    BrokerForm.RefreshTree;
end;

procedure TMainForm.MQTTServerBrokerOnline(Sender: TObject);
begin
  if BrokerForm.Visible then
    BrokerForm.RefreshTree;
end;

procedure TMainForm.MQTTServerCheckUser(Sender: TObject; aUser, aPass: UTF8String; var Allowed: Boolean);
begin
  ServerMemo.Lines.Add ('Login Approval Username "' + string (aUser) + '" Password "' + string (aPass) + '".');
  Allowed := true;
end;

procedure TMainForm.MQTTServerClientsChange (Sender: TObject; anID: Word);
begin
  SClientsTxt.Caption := IntToStr (anID);
end;

procedure TMainForm.MQTTServerDeleteSession (Sender: TObject; aClientID: UTF8String);
begin
  ServerMemo.Lines.Add ('Delete Session for "' + string (aClientID) + '".');
end;

procedure TMainForm.MQTTServerEnableChange (Sender: TObject);
begin
  SEnableTxt.Caption := ny[MQTTServer.Enabled];
end;

procedure TMainForm.MQTTServerFailure (Sender: TObject; aReason: Integer; var CloseClient: Boolean);
begin
  ServerMemo.Lines.Add ('---- Failure Reported ' + MqttFailureNames (aReason));
end;

procedure TMainForm.MQTTServerMon(Sender: TObject; aStr: string);
begin
  if CMBoxSrvr.Checked then
    ServerMemo.Lines.Add (aStr);
end;

procedure TMainForm.MQTTServerObituary(Sender: TObject; var aTopic, aMessage: UTF8String; var aQos: TMQTTQOSType);
begin
  if not (Sender is TSrvrClient) then exit;
  ServerMemo.Lines.Add ('Obituary Approval "' + string (aTopic) + '" with message "' + string (aMessage) + '"');
  with TSrvrClient (Sender) do
    begin
      aMessage := Parser.ClientID + ' failed at ' + UTF8String (TimeToStr (Now)) + ' - ' + aMessage;
    end;
end;

procedure TMainForm.MQTTServerRestoreSession(Sender: TObject;
  aClientID: UTF8String);
begin
  ServerMemo.Lines.Add ('Restore Session for "' + string (aClientID) + '".');
end;

procedure TMainForm.MQTTServerStoreSession(Sender: TObject;
  aClientID: UTF8String);
begin
  ServerMemo.Lines.Add ('Store Session for "' + string (aClientID) + '".');
end;

procedure TMainForm.MQTTServerSubscription(Sender: TObject; aTopic: UTF8String; var RequestedQos: TMQTTQOSType);
begin
   ServerMemo.Lines.Add ('Subscription Approval "' + string (aTopic) + '" @ ' + QOSNames [RequestedQOS]);
end;

procedure TMainForm.ServerMemoDblClick(Sender: TObject);
begin
  ServerMemo.Lines.Clear;
end;

procedure TMainForm.ClientMemoDblClick(Sender: TObject);
begin
  ClientMemo.Lines.Clear;
end;

procedure TMainForm.ClientMemo3DblClick(Sender: TObject);
begin
  ClientMemo3.Lines.Clear;
end;

procedure TMainForm.RetainBoxClick(Sender: TObject);
begin
  aRetain := RetainBox.Checked;
end;

procedure TMainForm.SMonHeader(Sender: TObject; aMsgType: TMQTTMessageType;  aDup: Boolean; aQos: TMQTTQOSType; aRetain: Boolean);
begin
  SMsgTxt.Caption := MsgNames[aMsgType];
  SQosTxt.Caption := QosNames[aQos];
end;


end.
