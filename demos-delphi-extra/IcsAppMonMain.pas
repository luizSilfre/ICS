{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  The ICS Application Monitor server is designed to monitor
              ICS applications using the TIcsAppMonCli client component,
              and ensure they remain running, restarting the application
              if it stops or becomes non-responsive, or on demand.
              Primarily to keep ICS server Windows services running
              non-stop, but may also be used for network wide monitoring
              of ICS applications.
Creation:     Sept 2024
Updated:      Feb 2025
Version:      V9.4
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2025 by Angus Robertson, Magenta Systems Ltd,
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

BUILD WARNING

Because ICS builds exe files to different sub-directories depending on build
config and target, you'll need to copy the IcsAppMon.config and ServerStatus.htm
files into one or more build bin sub-directories, otherwise the sample will not
find it and will die on startup.



DD Service Application Framework Requirement
--------------------------------------------
DDService is an enhanced Windows NT service application framework for Delphi and C++
Builder based on the original VCL service framework. In addition to it also encapsulates
new Windows NT service APIs introduced since Windows 2000. Original author was the late
Arno Garrels of Duodata in Berlin. Now being maintained by Magenta Systems Ltd.

https://www.magsys.co.uk/delphi/ddservice.asp

DDService must be installed before opening this sample.  Once the sample is built, it
may be installed as a Windows service from an elevated command line prompt with the
command IcsAppMon.exe /install. But this sample is written so it may also be run as
a GUI for debugging.


Windows Service or GUI?
-----------------------
If this sample is only used for monitoring, it does not matter how it is used.
But for Non-Stop monitoring of applications that requires the stopping and
restarting of applications, administative level access is required to start
and stop Windows services, and Windows services can not easily start GUI
applications.  So effectively, this program needs to be installed as a
Windows service to monitor other Windows Services, which is how most ICS
server applications should be designed, for continual running.


Application Independent Monitoring
----------------------------------
The ICS Application Monitor server is designed to monitor any ICS applications
using the TIcsAppMonCli client component, no configuration of the server is
needed other than setting it's listening IP address and email account details,
and the server has no prior knowledge of applications that may connect to it.

The server broadcasts it's availability, IP address and port to clients by
three methods:  Windows HLM registry entries, a named Windows message, and
optionally a UDP broadcast message.  The Windows registry and named messages
are only valid for applications on the same PC as the server, UDP messages
can be broadcast to the local LAN.

When clients access the server by the broadcasted IP address, they call the
component Start method that sends a HELLO request, containing the required
operating mode, currently 'Monitor Only', 'Non-Stop Monitor' or 'Installation',
and provide all the information the server needs, such as application name,
executable file name, Windows Service name, Windows Handle, Process ID, etc,
and general information the client can add for information. The client then
keeps the server socket connection open.  Note Non-Stop and Installation
modes are only accepted it the application is running on the same PC as the
server, and the server has the administrator rights to control the application.

Once accepted for monitoring, clients behaving correctly should call the
Awake method regularly, usually from a timer every few seconds, which
causes the component to send a small PING packet once a minute so the
server knows it's running OK. The Awake method also allows a status
message to be sent for the server display application information, such
as how many web pages it's sent.

To stop monitoring, the component Stop method is called that sends a STOP
packet, usually saying this is deliberate and no restart is required.  If
the client wants to be deliberately restarted, for instance after an
unexpected exception or terminal error, use the RestartNow method which
sends a special STOP packet that initiates the application restart process.
Not sending Awake regularly will also initiate a restart.

If the client connection is closed without a STOP packet or the PING packets
stop for a while, the server will attempt to restart the client, by first
stopping the Windows service or program, waiting for it to stop, and then
starting the service or program again.  If the Windows service does not
stop cleanly due to being non-responsive, the server will attempt to
terminate the program by process ID to ensure it stops and then restart.

Warning, it is crucial that applications ensure the Stop method is called
before deliberately closing down, otherwise the AppMon server will simply
restart them, continually.

When sending packets, the client may add an email message that the server
will send to the admin email address, perhaps application start-up or close
down information, error information, anything useful really.  The server
also sends admin emails when clients start and stop monitoring.

Not implemented yet, but installation mode is similar to restarting, except
the exe file is replaced by a new version while being restarted, allowing
the application to update itself with a new version.  Longer term, there
may be application updating component to include downlaoding new versions
and support to install multiple files.

The server can potentially monitor an unlimited number of applications,
whose status is available from a continuously refreshed web page using
Websockets, and also as Json for Websocket client applications.

There is a USERINFO command allowing the client and server to exchange
application defined information, not implemented yet.

The web and websocket server supports these URLs:
http://iporhost:17779/ServerStatus.htm (or blank page)
http://iporhost:17779/ServerStatus.json
https://host:17780/ServerStatus.htm (or blank page)
https://host:17780/ServerStatus.json
ws://iporhost:17779/WebSocket/StatusWeb
ws://iporhost:17779/WebSocket/StatusJson
wss://iporhost:17780/WebSocket/StatusWeb
wss://iporhost:17780/WebSocket/StatusJson

There is another sample ICS Application Monitor - Remote Manager, IcsAppMonMan.dpr
that provides remote monitoring of multiple ICS Application Monitor servers using
the Json web and websocket requests.


Sep 2024 V9.3 - Baseline
Feb 11, 2025 - V9.4 - split server methods into new component TIcsAppMonSrv
                 in OverbyteIcsAppMonSrv.
               Added web and websocket servers to provide status information
                 from a continuously refreshed web page using websockets,
                 and also as Json for websocket client applications.



Pending - support for INSTALL command to install applications, really needs
  downloading of new software as well for complete updating solution.
Pending - server and/or client send USERINFO to each other.


Warning - this sample is only tested on recent Unicode compilers, it will
not build on older compilers without changes.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit IcsAppMonMain;

{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }

{$WARN UNSAFE_TYPE off}
{$WARN UNSAFE_CAST off}
{$WARN UNSAFE_CODE off}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_LIBRARY OFF}
{$WARN SYMBOL_DEPRECATED OFF}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, WinApi.WinSvc, System.IniFiles,
  OverbyteIcsUtils,
  OverbyteIcsTicks64,
  OverbyteIcsIpStreamLog,
  OverbyteIcsSslHttpOAuth,
  OverbyteIcsMailQueue,
  OverbyteIcsTypes,
  OverbyteIcsSslUtils,
  OverbyteIcsSslBase,
  OverbyteIcsWndControl,
  OverbyteIcsWSocketS,
  OverbyteIcsHttpSrv,
  OverbyteIcsHttpAppServer,
  OverbyteIcsSslX509Certs,
  OverbyteIcsWebSocketcli,  // V9.4
  OverbyteIcsWebSocketSrv,  // V9.4
  OverbyteIcsAppMonCli,     // only for types and constants common to client and server
  OverbyteIcsWinUtils,      // V9.4
  OverbyteIcsAppMonSrv;     // V9.4

const
    ProgTitle = IcsAppMonName;
    ProgVersion = ProgTitle + ' - Release 1.1 - 11th February 2025' ;
    AppTitle = IcsAppMonName;
    SimpLogName = '"' + IcsAppMonName + '"yyyymmdd".log"' ;
    NameLogsPath = 'logs\' ;
    NameIniConfig = IcsAppMonName + '.config' ;
    EncodeKeyFileSrv = 61234 ;
    WM_FM_STARTUP = WM_USER + 804 ;
    RestartDelaySecs = 30;   // seconds before attempting internal service restart
    FailedAttemptsMax = 5;   // max restart attempts before stopping this service so it restarts

type
  TMyHttpConnection = class(THttpWSSrvConn)    { client connection, where we add special stuff }
  public
    CStartTick        : Int64;  // used for logging requests
    CLastRead         : Int64;
    CLastWrite        : Int64;
    CPeerHostName     : string;
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure BgExceptionEvent (Sender : TObject; E : Exception; var CanClose : Boolean);
  end ;

  TUrlHandlerWSServerStatus = class(TUrlHandler)    { WebSocket page handler }
  public
      procedure Execute; override;
  end;

type
  TMonitorMain = class(TForm)
    Panel1: TPanel;
    LogWin: TMemo;
    IcsMailQueue: TIcsMailQueue;
    IcsRestEmail: TIcsRestEmail;
    doClose: TButton;
    SslHttpAppSrv: TSslHttpAppSrv;
    IcsAppMonSrv: TIcsAppMonSrv;
    TimerMain: TTimer;
    SslX509Certs: TSslX509Certs;
    procedure IcsMailQueueOATokenEvent(ServNr: Integer; var Token, TokAccount: string; var TokExpireDT: TDateTime);
    procedure IcsMailQueueLogEvent(LogLevel: TMailLogLevel; const Info: string);
    procedure FormCreate(Sender: TObject);
    procedure IcsRestEmailEmailNewToken(Sender: TObject);
    procedure IcsRestEmailEmailProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
    procedure doCloseClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure IcsAppMonSrvEmailEvent(Sender: TObject; const Subject, Body: string; Fatal: Boolean);
    procedure IcsAppMonSrvLogEvent(Sender: TObject; const Line: string);
    procedure TimerMainTimer(Sender: TObject);
    procedure SslHttpAppSrvAuthGetPassword(Sender, Client: TObject; var Password: string);
    procedure SslHttpAppSrvAuthGetType(Sender, Client: TObject);
    procedure SslHttpAppSrvAuthResult(Sender, Client: TObject; Success: Boolean);
    procedure SslHttpAppSrvBeforeProcessRequest(Sender, Client: TObject);
    procedure SslHttpAppSrvBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure SslHttpAppSrvClientConnect(Sender, Client: TObject; Error: Word);
    procedure SslHttpAppSrvDisplay(Sender: TObject; const Msg: string);
    procedure SslHttpAppSrvProcDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
    procedure SslHttpAppSrvHttpRequestDone(Sender, Client: TObject);
    procedure IcsAppMonSrvChangeEvent(Sender: TObject);
    procedure SslX509CertsCertProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
    procedure SslHttpAppSrvSslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
  public
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure AddLogLine (S1: string);
    procedure CloseLogFile;
    procedure FlushLogFile(OldFName: Boolean = False);
    procedure OpenLogFiles;
    procedure GetGenSettings ;
    procedure StartQueueMail;
    procedure StopQueueMail;
    procedure WMFMSTARTUP (var Msg : TMessage); message WM_FM_STARTUP;
    procedure MonitorStop;
    function GetMonStatusWeb: String;
    function GetMonStatusJson: String;
    procedure ClientBgException(Sender: TObject; E: Exception; var CanClose : Boolean);
    procedure ClientWSLogEvent(Sender: TObject; const Msg: string);                                       { websockets }
    procedure ClientWSHandshakeEvent(Client: THttpWSSrvConn; var OK: Boolean; var WelcomeMsg: String);    { websockets }
    procedure ClientWSFrameRcvdEvent(Client: THttpWSSrvConn; const APacket: String; var AFrame: TWebSocketReceivedFrame);  { websockets }
    procedure ClientWSFrameSentEvent(Client: THttpWSSrvConn; var AFrame: TWebSocketOutgoingFrame);         { websockets }
    procedure ClientWSDisconnectedEvent(Sender: TObject);                                                  { websockets }
    procedure ClientWSReadyEvent(Client: THttpWSSrvConn);                                                  { websockets }
    procedure ClientWSPingTimerEvent(Client: THttpWSSrvConn);                                              { websockets }
    procedure ReportHosts;
  end;

var
    MonitorMain: TMonitorMain;
    GStartedByScm: Boolean = False;
    ServiceStartedByScm: string ;
    DiagLogBuffer: TIcsBuffLogStream ;     // temporary buffer for log
    FileIniFile: TMemIniFile;
    FileIniConfig: String;
    DirApplication: String;
    ClosingFlag: Boolean;
    curDT: TDateTime;
    LogDate: TDateTime;
    LogOpenFlag: Boolean;
    OldRefrToken: String;
    SrvActiveTick: Int64;
    SrvRestarts: integer;
    SrvCompName: String;
    ServerInfo: String;
    ServerWSHost: String;

// INI stuff for email and logging, more loaded by IcsLoadTAppMonSrvFromIni and web server versions
    FileLoggingsDir: string ;
    AdminEmailFrom: string ;
    AdminEmailTo: string ;
    AdminEmailStart: boolean ;

implementation

{$R *.dfm}

Uses IcsAppMonSrv;  // DDService

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.FormCreate(Sender: TObject);
begin
{$WARNINGS OFF}
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);     { V8.69 }
{$WARNINGS ON}
    Caption := ProgVersion ;
//    Application.OnMessage := TriggerMessageEvent ;
    Application.OnException := ApplicationEventsException ;
    if NOT IcsCheckUniqueInstance then begin
        IcsSimpleLogging (SimpLogName, 'ICS Application Monitor is Already Running') ;
        Application.Terminate ;
        exit ;
    end ;
    SrvActiveTick := IcsGetTickCount64;
    PostMessage (Handle, WM_FM_STARTUP, 0, 0) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if ClosingFlag then
        Exit ;
    ClosingFlag := true ;
    try
        TimerMain.Enabled := false ;
        if Assigned (DiagLogBuffer) then
            AddLogLine ('FormClose, Stopping Server Monitor') ;
        MonitorStop;
    except
        IcsSimpleLogging (SimpLogName, 'Exception FormClose') ;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.FormDestroy(Sender: TObject);
begin
    try
        if NOT ClosingFlag then begin
            ClosingFlag := true ;
            AddLogLine ('FormDestroy, Stopping Server Monitor') ;
            MonitorStop;
        end;
        FreeAndNil (FileIniFile) ;
        CloseLogFile ;
        FreeAndNil (DiagLogBuffer) ;
    except
        IcsSimpleLogging (SimpLogName, 'Exception FormDestroy');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.AddLogLine (S1: string);
var
    res: integer ;
begin
    curDT := Now ;
//    if ClosingFlag then
//        Exit;
    try
        S1 := TimeToStr (curDT) + IcsSpace + S1 ;
        if NOT GStartedByScm then
           LogWin.Lines.Add(S1);

   // file logging
        res := 0;
        try
            if Assigned (DiagLogBuffer) then
                res := DiagLogBuffer.WriteLine (S1) ;
        except
            res := -1 ;
        end ;
        if (res = -1) and Assigned (DiagLogBuffer) then
            DiagLogBuffer.WriteLine (S1) ;  // one retry before exception
    except
        IcsSimpleLogging (SimpLogName, 'Error Writing Log - ' + IcsGetExceptMess (ExceptObject)) ;
   end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.FlushLogFile(OldFName: Boolean = False) ;
begin
    try
        if Assigned (DiagLogBuffer) then
        begin
        //    if NOT FileInUse (DiagLogBuffer.FullName) then
                DiagLogBuffer.FlushFile(OldFName)
          //  else
         //       AddLogLine ('Skipped Flushing Diag Log, In-Use') ;
        end ;
    except
        IcsSimpleLogging (SimpLogName, 'Error Flushing Logs - ' + IcsGetExceptMess (ExceptObject)) ;
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.CloseLogFile ;
begin
    try
        if Assigned (DiagLogBuffer) then
        begin
            DiagLogBuffer.FlushFile ;
            DiagLogBuffer.Free ;
            DiagLogBuffer := Nil ;
        end ;
    except
        IcsSimpleLogging (SimpLogName, 'Aborting - Error Closing Diag Log') ;
    end ;
    LogOpenFlag := false ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.doCloseClick(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.OpenLogFiles ;
var
    diagfname, S: string;
begin
    curDT := Now ;
    CloseLogFile ;

    diagfname := IcsDQUOTE + FileLoggingsDir + '\' + IcsAppMonName + '-"yyyymmdd".log"' ;
    S := '"' + ProgVersion + IcsCRLF +
         'Log File Created: "dd mmm yyyy hh:mm:ss"' + IcsCRLF +
         'Computer: ' + IcsGetCompName + '"' + IcsCRLF ;
    try
        DiagLogBuffer := TIcsBuffLogStream.Create (Self, diagfname, S) ;
        DiagLogBuffer.FlushFile ;
        LogOpenFlag := true ;
    except
        IcsSimpleLogging (SimpLogName, 'Aborting - Error Opening Log - ' + IcsGetExceptMess (ExceptObject)) ;
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.GetGenSettings ;
var
    section: string ;
begin
    if NOT FileExists (FileIniConfig) then exit ;
    FreeAndNil (FileIniFile) ;
    try
        FileIniFile := TMemIniFile.Create (FileIniConfig) ;
        section := 'General' ;
        with FileIniFile do
        begin
            FileLoggingsDir := ReadString (section, 'FileLoggingsDir', 'c:\weblogs\' + IcsAppMonName + '\') ;
            AdminEmailTo := ReadString (section, 'AdminEmailTo', '') ;
            AdminEmailFrom := ReadString (section, 'AdminEmailFrom', '') ;
            AdminEmailStart := IcsCheckTrueFalse (ReadString (section, 'AdminEmailStart', 'False')) ;
        end ;
    except
        IcsSimpleLogging (SimpLogName, 'Aborting - Failed to open INI File - ' + IcsGetExceptMess (ExceptObject)) ;
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.IcsAppMonSrvEmailEvent(Sender: TObject; const Subject, Body: string; Fatal: Boolean);
var
    sTempFrom: string ;
    id: integer ;
begin
    if Fatal then begin
        if NOT IcsMailQueue.Active then exit;   // July 2024 only once
        AddLogLine ('Stopping Mail Queue After Fatal Error') ;
        IcsMailQueue.Active := False;
        exit;
    end
    else begin
        if NOT IcsMailQueue.Active then
            StartQueueMail ;  // restart if not running
        if NOT IcsMailQueue.Active then
        begin
             AddLogLine ('Error Starting Mail Queue') ;
        end ;
    end;
    try
        with IcsMailQueue.QuHtmlSmtp do
        begin
            sTempFrom := AppTitle + '_' + IcsGetCompName ;
            sTempFrom := '"' + sTempFrom + '" <' + sTempFrom + AdminEmailFrom + '>' ;
            EmailFiles.Clear ;
            RcptName.clear;
            Allow8bitChars := true ;
            ContentType := smtpPlainText ;
            WrapMessageText := false ;
            WrapMsgMaxLineLen := 76 ;
            PlainText.Clear ;
            PlainText.Text := IcsAppMonTitle + ' on ' + IcsGetCompName + IcsCRLF + IcsCRLF + Body ;
            AddLogLine ('Queuing Email Form to ' + AdminEmailTo + ' from ' + sTempFrom) ;
            RcptName.Clear ;
            RcptName.Add (AdminEmailTo) ;
            FromName := sTempFrom ;
            HdrCc := '' ;
            HdrTo := AdminEmailTo ;
            HdrFrom := sTempFrom ;
            HdrReplyTo := sTempFrom ;
            if Subject = '' then
                HdrSubject := IcsAppMonTitle
            else
                HdrSubject := IcsAppMonTitle + ' - ' + Subject;
            XMailer := '' ;
            id := IcsMailQueue.QueueMail ;
            if id > 0 then
            begin
       // done OK
                AddLogLine ('Email Form Queued OK') ;
            end
            else
            begin
                AddLogLine ('Failed to Queue Email Form: ' + ErrorMessage) ;
            end ;
        end ;
    except
        AddLogLine ('Failed to Queue Email Form: ' + IcsGetExceptMess (ExceptObject)) ;
    end;
    FlushLogFile;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.IcsAppMonSrvLogEvent(Sender: TObject; const Line: string);
begin
    AddLogLine(Line);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.IcsRestEmailEmailNewToken(Sender: TObject);
var
    info: String;
begin
// we should save refresh token since only get it after a login window
    if (IcsRestEmail.RefrToken <> '') and (IcsRestEmail.RefrToken <> OldRefrToken) then begin
        info := 'New OAuth2 Refresh Token for Account: ' + IcsRestEmail.NewAccEmail +
                ', Should be Saved in Config File' + IcsCRLF +
                'RefrToken=' + IcsRestEmail.RefrToken;
        AddLogLine(info);
        if (AdminEmailTo <> '') then
            IcsAppMonSrvEmailEvent (Self, 'Refresh Token', 'IcsMailQueue New OAuth2 Refresh Token.' + IcsCRLF + IcsCRLF + info, False);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.IcsRestEmailEmailProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
begin
    AddLogLine (Msg) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.StartQueueMail;
var
    S: string ;
    I: integer ;
begin
    try
        if NOT IcsMailQueue.Active then
        begin
            IcsLoadMailQuFromIni(FileIniFile, IcsMailQueue, 'MailQueue');
            if (IcsMailQueue.MailQuDir = '') or (NOT ForceDirectories (IcsMailQueue.MailQuDir)) then
            begin
                AddLogLine ('!! Failed to Start Mail Queue, No Directory') ;
                exit ;
            end;
            IcsLoadRestEmailFromIni(FileIniFile, IcsRestEmail, 'RestEmail');  // June 2022
            OldRefrToken := IcsRestEmail.RefrToken;                           // June 2022
        end;
        if IcsMailQueue.MailServers.Count = 0 then
        begin
            AddLogLine ('!! Failed to Start Mail Queue, No Mail Servers') ;
            exit ;
        end;
        S := '';
        for I := 0 to IcsMailQueue.MailServers.Count - 1 do
        begin
            IcsMailQueue.MailServers[I].Password := IcsStrBXDecryptEx (IcsMailQueue.MailServers[I].Password, EncodeKeyFileSrv) ;
            S := S + IcsMailQueue.MailServers[I].Host + ', ';
        end;
        IcsMailQueue.Debug := False;
        IcsMailQueue.QuStartDelay := 0; // no delay sending email
        IcsMailQueue.Active := true ;
        if IcsMailQueue.Active then
        begin
             AddLogLine ('Started Mail Queue OK, Servers: ' + S) ;
        end;
    except
         AddLogLine ('!! Failed to Start Mail Queue - ' +  IcsGetExceptMess (ExceptObject)) ;
    end;
 end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.StopQueueMail;
begin
    if IcsMailQueue.Active then
    begin
        if (IcsMailQueue.MailImmItems > 0) then
            AddLogLine ('Waiting up to 10 seconds to send Queued Mail Items') ;
        IcsMailQueue.WaitSendandStop (10) ;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.IcsMailQueueLogEvent(LogLevel: TMailLogLevel; const Info: string);
begin
    if LogLevel <= MLogLevelInfo then
        AddLogLine (Info) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.IcsMailQueueOATokenEvent(ServNr: Integer; var Token, TokAccount: string; var TokExpireDT: TDateTime);
begin
    AddLogLine('Starting to get OAuth2 Bearer Token');
    if NOT IcsRestEmail.GetNewToken(False) then   // no interaction, waits for browser window to be completed
        AddLogLine('Failed to get OAuth2 Bearer Token')
    else begin
        Token := IcsRestEmail.AccToken;
        TokExpireDT := IcsRestEmail.AccExpireDT;
        TokAccount := IcsRestEmail.NewAccEmail;
      // note, AccToken has a short life, few hours, no need to save it in INI file, it can be refreshed
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.ApplicationEventsException(Sender: TObject; E: Exception);
var
    S: string ;
begin
    S := '!!! Application Exception Event - ' + IcsGetExceptMess (E) ;
    if LogOpenFlag then
    begin
        AddLogLine (S) ;
        FlushLogFile;
    end
    else
        IcsSimpleLogging (SimpLogName, S) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.ReportHosts;
var
    S: string ;
    J: integer ;
    autoorder: string ;
begin
    autoorder := 'Disabled';
    AddLogLine (IcsCRLF + 'Total source server hosts ' + IntToStr(SslHttpAppSrv.IcsHosts.Count)) ;
    for J := 0 to SslHttpAppSrv.IcsHosts.Count - 1 do begin
        with SslHttpAppSrv.IcsHosts[J] do begin
            if CertValRes > chainOK then begin
                AddLogLine ('Server: ' + DisplayName + IcsCRLF + 'SSL Certificate chain: ' +
                                         IcsCRLF + CertInfo + IcsCRLF + CertErrs) ;
            end;
            if CertValRes = chainFail then begin
                AddLogLine ('Server SSL Certificate Errors') ;
            end;
            S := 'Source #' + IntToStr(J) + ' - ' + HostTag + ': ' + Descr + IcsCRLF +
                 'Host: ' + HostNames [0] + ', Bindings: ' + BindInfo + IcsCRLF;
            if BindSslPort <> 0 then begin
                if SslHttpAppSrv.SslCertAutoOrder then begin
                    autoorder := SupplierProtoLits[CertSupplierProto];
                    if CertSupplierProto > SuppProtoNone then
                        autoorder := autoorder + ',  Challenge: ' + ChallengeTypeLits[CertChallenge] +
                        ', Database: ' + CertDirWork;
                end;
                S := S + 'SSL Certificate Auto Order: ' + autoorder + IcsCRLF;
                S := S + 'SSL Security Level ' + SslSrvSecurityNames[SslSrvSecurity] + IcsCRLF;
                S := S + CertErrs + icsCRLF + CertInfo + icsCRLF;
            end
            else
                S := S + 'No SSL' + IcsCRLF;
            if WellKnownPath <> '' then S := S + 'Well-Known path root: ' + WellKnownPath + IcsCRLF;
            AddLogLine (S) ;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.WMFMSTARTUP (var Msg : TMessage);
var
    S: String;
    I: Integer;
begin
    try
        DirApplication := ExtractFileDir (Lowercase (ParamStr(0)));
        FileIniConfig := DirApplication + '\' + NameIniConfig ;   // main config file
        IcsSimpleLogging (SimpLogName, 'Config File: ' + FileIniConfig) ;
        GetGenSettings ;
        if FileLoggingsDir = '' then begin
            IcsSimpleLogging (SimpLogName, 'Fatal Error, No Config File') ;
            Application.Terminate;
            Exit;
        end;
        IcsSimpleLogging (SimpLogName, 'Logs Path: ' + FileLoggingsDir) ;
        IcsLoadAppMonSrvFromIni(FileIniFile, IcsAppMonSrv, 'IcsAppMonSrv');
        IcsAppMonSrv.WinService := GStartedByScm;
        if IcsAppMonSrv.MonSrvIP =  '' then begin
            IcsSimpleLogging (SimpLogName, 'Fatal Error, No Monitor Server IP') ;
            Application.Terminate;
            Exit;
        end;
        SrvCompName := IcsGetCompName;
        IcsSslRootCAStore.Initialise;

    // server information for web page and websocket
        ServerInfo := ProgVersion  + sTagBR +
        'Server Started Time: ' + IcsDateTimeToAStr (Now) + sTagBR +
        'Windows Boot Time: ' + IcsDateTimeToAStr (IcsLastBootDT) + sTagBR +
        'Server Computer: ' + SrvCompName + sTagBR +
        'Server OS: ' + TOSVersion.ToString + sTagBR +
        'Compiler: ' + IcsBuiltWithEx + sTagBR +
        'OpenSSL Version: ' + IcsReportOpenSSLVer(False) + sTagBR;
        IcsAppMonSrv.SrvInfoJson.Clear;
        IcsAppMonSrv.SrvInfoJson.AddItem('progver', ProgVersion);
        IcsAppMonSrv.SrvInfoJson.AddItem('servername', SrvCompName);
        IcsAppMonSrv.SrvInfoJson.AddItem('starttime', IcsDateTimeToAStr (Now));
        IcsAppMonSrv.SrvInfoJson.AddItem('boottime', IcsDateTimeToAStr (IcsLastBootDT));
        IcsAppMonSrv.SrvInfoJson.AddItem('serveros', TOSVersion.ToString);
        IcsAppMonSrv.SrvInfoJson.AddItem('compiler', IcsBuiltWithEx);
        IcsAppMonSrv.SrvInfoJson.AddItem('openssl', IcsReportOpenSSLVer(False));

    // logging and OpenSSL for mail
        OpenLogFiles;
        AddLogLine ('');
        AddLogLine (ProgVersion);
        AddLogLine ('OS: ' + TOSVersion.ToString);
        AddLogLine ('Compiler: ' + IcsBuiltWithEx) ;
        AddLogLine('SSL/TLS ' + IcsReportOpenSSLVer(True));
        AddLogLine('Config File: ' + FileIniConfig) ;
        AddLogLine('Logs Path: ' + FileLoggingsDir) ;
        S := 'Initialising';
        AddLogLine(S) ;
 //       IcsAppMonSrvEmailEvent(Self, S, IcsAppMonTitle + ' ' + S, False);
 //       AddLogLine('LogPackets=' + BoolToStr(IcsAppMonSrv.LogPackets, True));   // TEMP !!!!

    // start application montor
        SrvActiveTick := IcsGetTickCount64;
        if NOT IcsAppMonSrv.MonStartup then begin
            S := 'Failed to Start, Will Try Again in 30 Seconds';
            AddLogLine(S) ;
            IcsAppMonSrvEmailEvent(Self, 'Failed to Start', IcsAppMonTitle + ' ' + S, False);
            IcsAppMonSrv.MonStop;
            FlushLogFile(True);
            Exit;
        end
        else begin
            if IcsAppMonSrv.WinService then
                S := 'Running as Windows Service'
            else
                S := 'Running as Windows GUI, Restricted Capabilities';
            AddLogLine(S);
            ServerInfo := ServerInfo + S + sTagBR + sTagBR +
                IcsAppMonName + 'Server: ' + sTagBR +
                StringReplace(IcsAppMonSrv.ListenStates, IcsCRLF, sTagBR, [rfReplaceAll]) + sTagBR ;
            IcsAppMonSrv.SrvInfoJson.AddItem('monlisten', IcsAppMonSrv.ListenStates);
        end;
        FlushLogFile(True);
    except
        S :='Failed to Start Server Monitor: ' + IcsGetExceptMess (ExceptObject) ;
        IcsSimpleLogging (SimpLogName, S) ;
        AddLogLine(S) ;
        IcsAppMonSrvEmailEvent(Self, 'Failed to Start', IcsAppMonTitle + ' ' + S, False);
        Exit;
    end;

  // start web server, if configured
    try
        IcsLoadTHttpAppSrvFromIni(FileIniFile, SslHttpAppSrv, 'WebAppServer');
        IcsLoadIcsHostsFromIni(FileIniFile, SslHttpAppSrv.IcsHosts, 'Host');
        if SslHttpAppSrv.IcsHosts.Count = 0 then begin
            AddLogLine('No Web Server Hosts Configured, Web Server Disabled');
        end
        else begin
            AddLogLine('Number of Hosts Configured: ' + IntToStr(SslHttpAppSrv.IcsHosts.Count));

       // validate hosts and keep site certificate information
            try
                S := SslHttpAppSrv.ValidateHosts(False, True); // don't stop on first error, no exceptions
                if S <> '' then begin
                    AddLogLine('Server Validation Errors:' + icsCRLF + S);
                end;
                ReportHosts;
                AddLogLine('Required Listen Bindings:' + icsCRLF + SslHttpAppSrv.ListenStates);
            except
                on E:Exception do begin
                    AddLogLine('Host Validation Failed, Server Stopped - ' + E.Message);
                    Exit;
                end;
            end;

        // setup some web server defauls, most were done in IcsLoadTHttpAppSrvFromIni
            SslHttpAppSrv.TemplateDir := DirApplication;
            SslHttpAppSrv.DocDir := DirApplication;
            for I := 0 to SslHttpAppSrv.IcsHosts.Count -1  do begin
                SslHttpAppSrv.IcsHosts [I].WebTemplDir := DirApplication;
                SslHttpAppSrv.IcsHosts [I].WebDocDir := DirApplication;
            end;
            SslHttpAppSrv.DefaultDoc := WebServStatusPage;   // only one page supported
            SslHttpAppSrv.ServerHeader := DefServerHeader;  // get latest version
            SslHttpAppSrv.ClientClass := TMyHttpConnection;  // only needed if we have our own class or Websockets
            SslHttpAppSrv.SocketErrs := wsErrFriendly ;
            SslHttpAppSrv.ExclusiveAddr := true ;
            if SslHttpAppSrv.SessionTimeout < 30 then
                SslHttpAppSrv.SessionTimeout := 300;  // sanity check

          // note that AllowedPaths and Handlers must match a HostTag for each Host in INI file

          // application web server, only allow access to folders where static documents are.
            SslHttpAppSrv.AddGetAllowedPath('/', afBeginBy, 'APPMON');

          // Add dynamic webpage application page handlers, create pages from a template and dynamic content
            SslHttpAppSrv.AddGetHandler(WebServStatusPage, TUrlHandlerWSServerStatus, hgWillSendMySelf, 'APPMON');

          // start as many hosts as possible
            S := SslHttpAppSrv.Start (True) ;
            if S <> '' then
                AddLogLine('Status Web Server Error - ' + S);
            if NOT SslHttpAppSrv.ListenAllOK then
                S := 'Status Web Server Failed to Start, '
            else
               S := 'Status Web Server Started OK, ';
            S := S + 'Listen Bindings:' + IcsCRLF + SslHttpAppSrv.ListenStates;
            AddLogLine(S);
            ServerInfo := ServerInfo + 'WebServer Listen Bindings: ' + sTagBR +
                          StringReplace(SslHttpAppSrv.ListenStates, IcsCRLF, sTagBR, [rfReplaceAll]) + sTagBR ;
            IcsAppMonSrv.SrvInfoJson.AddItem('weblisten', SslHttpAppSrv.ListenStates);
        end;
    except
        S :='Failed to Start Status Web Server: ' + IcsGetExceptMess (ExceptObject) ;
        IcsSimpleLogging (SimpLogName, S) ;
        AddLogLine(S) ;
        IcsAppMonSrvEmailEvent(Self, 'Failed to Start', IcsAppMonTitle + ' ' + S, False);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.MonitorStop;
var
    S: String;
    I: Integer;

    procedure Waiting(secs: integer);
    var
        Trg: Int64;
    begin
        Trg := IcsGetTrgSecs64 (secs) ;
        while True do begin
            Application.ProcessMessages ;
            if Application.Terminated then
                break ;
            if IcsTestTrgTick64 (Trg) then
                break ;
            if IcsAppMonSrv.CurClients = 0 then  // all monitoring stopped
                break;
        end;
    end;

begin
    try
        if IcsAppMonSrv.MonActive then begin
            S := 'Stopping Client Monitoring';
            IcsSimpleLogging (SimpLogName, S);
            AddLogLine (S);
            IcsAppMonSrvEmailEvent(Self, S, IcsAppMonTitle + ' ' + S, False);
            FlushLogFile;
            IcsAppMonSrv.MonStop;
            FlushLogFile;
            if IcsAppMonSrv.CurClients > 0 then begin
                AddLogLine ('Waiting Five Seconds for Monitoring to Stop');
                FlushLogFile;
                Waiting(5);
            end;

          // close websocket clients
            if SslHttpAppSrv.ClientCount > 0 then begin
                AddLogLine ('Closing Websockets');
                for I := SslHttpAppSrv.ClientCount - 1 downto 0 do begin
                    with (SslHttpAppSrv.Client[I] as THttpWSSrvConn) do begin
                        if (State = wsConnected) and WSClient and (NOT WSClosing) then
                            WSClose(wscrNormalClosure, 'Server Closing');
                    end;
                end;
            end;
            AddLogLine ('Stopping Web Server');
            FlushLogFile;
            SslHttpAppSrv.Stop;

            AddLogLine ('Stopping Mail Queue');
            FlushLogFile;
            StopQueueMail;

            S :='Stopped Servers and Mail Queue OK';
            IcsSimpleLogging (SimpLogName, S) ;
            AddLogLine(S) ;
            FlushLogFile;
        end;
    except
        S :='Exception Stopping Monitoring: ' + IcsGetExceptMess (ExceptObject) ;
        IcsSimpleLogging (SimpLogName, S) ;
        AddLogLine(S) ;
        FlushLogFile;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.TimerMainTimer(Sender: TObject);
var
    DeadSecs: Integer;
    S: string;
begin
    TimerMain.Enabled := False;
    try
        try
            curDT := Now ;
            if NOT IcsAppMonSrv.MonActive then begin
                DeadSecs := IcsElapsedSecs64 (SrvActiveTick);
                if DeadSecs > RestartDelaySecs then begin
                    SrvRestarts := SrvRestarts + 1;
                    if IcsAppMonSrv.WinService and (SrvRestarts > FailedAttemptsMax) then begin
                        S := 'Too Many Failed Internal Restart Attempts, Restarting Windows Service';
                        AddLogLine(S) ;
                        IcsAppMonSrvEmailEvent(Self, 'Restarting Windows Service', IcsAppMonTitle + ' ' + S, False);
                        IcsAppMonControl.StopService;
                    end
                    else begin
                        AddLogLine('Restarting After 30 Secs Not Detected Running') ;
                        PostMessage (Handle, WM_FM_STARTUP, 0, 0) ;
                    end;
                end;
            end
            else
                SrvActiveTick := IcsGetTickCount64;

        // midnight log rotation
            if Trunc(CurDT) <> LogDate then begin
                LogDate := Trunc(CurDT);
                FlushLogFile(True);  // use new date name
              // should we log something?
            end;
        except
            // not logging errors, too often
        end;
    finally
        TimerMain.Enabled := True;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMonitorMain.GetMonStatusWeb: String;
begin
    Result := sTagPBolds + 'Reported at: ' + IcsDateTimeToAStr (Now) + sTagPe + sTagBR;
    if IcsAppMonSrv.MonActive then
        Result := Result + IcsAppMonSrv.StatusWeb
    else
        Result := Result + sTagPBolds + IcsAppMonTitle + ' Not Running' + sTagPe + IcsCRLF;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMonitorMain.GetMonStatusJson: String;
begin
    Result := IcsAppMonSrv.StatusJson;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMyHttpConnection.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    OnBgException := BgExceptionEvent ;
    CPeerHostName := '' ;
    OnWSLogEvent := MonitorMain.ClientWSLogEvent;
    OnWSHandshake := MonitorMain.ClientWSHandshakeEvent;
    OnWSFrameRcvd := MonitorMain.ClientWSFrameRcvdEvent;
    OnWSFrameSent := MonitorMain.ClientWSFrameSentEvent;
    OnWSDisconnected := MonitorMain.ClientWSDisconnectedEvent;
    OnWSReady := MonitorMain.ClientWSReadyEvent;
    OnWSPingTimer := MonitorMain.ClientWSPingTimerEvent;
    // DebugBody decodes all frames, DebugHdr only main info , DebugConn only connections oe DebugNone
    DebugLevel := DebugConn;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMyHttpConnection.BgExceptionEvent(Sender : TObject; E : Exception; var CanClose : Boolean);
begin
     MonitorMain.AddLogLine('Client Error - ' + IcsGetExceptMess (E)) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We need to override parent class destructor  }
destructor TMyHttpConnection.Destroy;
begin
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// called before returning pages that require authenication, so we can check AuthUserName and find it's password }
procedure TMonitorMain.SslHttpAppSrvAuthGetPassword(Sender, Client: TObject; var Password: string);
var
    ClientCnx  : TMyHttpConnection;
begin
    ClientCnx := TMyHttpConnection(Client);
    AddLogLine('AuthGetPassword for "' + ClientCnx.Path + '" AuthType is "' + AuthTypesToString(ClientCnx.AuthTypes) + '"' +
                                                                                    ' for Username: ' + ClientCnx.AuthUserName);

 { this is where your code should look up user names and get password to check in a database }

    if (ClientCnx.AuthUserName = 'test') then   // hardcoded for testing
        Password := 'password';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// called before any pages, so they can be protected by authenication
procedure TMonitorMain.SslHttpAppSrvAuthGetType(Sender, Client: TObject);
var
    ClientCnx  : TMyHttpConnection;
begin
    ClientCnx := TMyHttpConnection(Client);
    ClientCnx.AuthTypes  := [];    { V9.2 }
    if Pos('/private/', ClientCnx.Path) = 1 then begin  // all files in /private will need authentication
        ClientCnx.AuthTypes  := [atBasic, atDigest, atDigestSha2];
        ClientCnx.AuthRealm := 'Enter Password';
    end
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// called autentication finished, so we can log the result, if we care
procedure TMonitorMain.SslHttpAppSrvAuthResult(Sender, Client: TObject; Success: Boolean);
var
    ClientCnx  : TMyHttpConnection;
const
    SuccessStr : array [Boolean] of String = ('failed', 'OK');
begin
    ClientCnx := TMyHttpConnection(Client);
    AddLogLine('Authentication result ' + SuccessStr[Success] + ' with type ' + HttpAuthTypeNames[ClientCnx.AuthGetMethod] +
                                                          ', Username: ' + ClientCnx.AuthUserName + ' for ' + ClientCnx.Path);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// called before authentication and specific methods are processed }
procedure TMonitorMain.SslHttpAppSrvBeforeProcessRequest(Sender, Client: TObject);
var
    ClientCnx: TMyHttpConnection;
    I: integer ;
    Protocol, S: String;
begin
    ClientCnx := TMyHttpConnection(Client) ;
    Protocol := ClientCnx.RequestProtocol + '://';
    ClientCnx.CStartTick := IcsGetTickCount64;
    ClientCnx.CLastWrite := ClientCnx.WriteCount ;

{ log request and heders }
    S := '[' + FormatDateTime('HH:NN:SS', Now) + ' ' + IntToStr(ClientCnx.CliId) + IcsSpace + ClientCnx.PeerAddr + '] ' +
                                                   ClientCnx.HostTag + ' ' +  ClientCnx.Method + ' ' + ClientCnx.Path;
    if ClientCnx.Params <> '' then
        S := S + '?' + ClientCnx.Params;
    AddLogLine(S);

  // check for absolute URL, strip off protocol and host
    if Pos (Protocol, ClientCnx.Path) = 1 then begin
        for I := (Length (Protocol) + 1) to Length (ClientCnx.Path) do begin
            if ClientCnx.Path [I] = '/' then begin
                AddLogLine('Found and Trimmed Absolute URL: ' + ClientCnx.Path);
                ClientCnx.Path := Copy (ClientCnx.Path, I, 999) ;
                Break ;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// something horrible has happened, it may be terminal and have crashed the server
procedure TMonitorMain.SslHttpAppSrvBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
begin
    AddLogLine('Exception processing page - ' +  E.ClassName + ': ' + E.Message);
    CanClose := FALSE;  // Let the server continue to accept connections
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// something horrible has happened, it may be terminal and have crashed the server
procedure TMonitorMain.ClientBgException(Sender: TObject; E: Exception; var CanClose : Boolean);
begin
    AddLogLine('Exception processing page - ' + E.ClassName + ': ' + E.Message);
    CanClose := TRUE;  // Shutdown client
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.SslHttpAppSrvClientConnect(Sender, Client: TObject; Error: Word);
var
    ClientCnx : TMyHttpConnection;
begin
    ClientCnx := Client as TMyHttpConnection;
    ClientCnx.WSessionCookie := 'OverbyteIcsWebAppServer' + SslHttpAppSrv.Port;
    ClientCnx.OnBgException := ClientBgException;

 { log something at start }
    AddLogLine('New Web Server Remote Client: ' + ClientCnx.CPeerAddr);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.SslHttpAppSrvDisplay(Sender: TObject; const Msg: string);
begin
    AddLogLine(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.SslHttpAppSrvHttpRequestDone(Sender, Client: TObject);
var
    ClientCnx  : TMyHttpConnection;
begin
    ClientCnx := TMyHttpConnection(Client);
    AddLogLine('Web Server Request Completed, ' + IntToStr(ClientCnx.CliId) + IcsSpace +
                                                        ClientCnx.GetPeerAddr + ', Status ' + IntToStr(ClientCnx.AnswerStatus));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// This event handler is shared by Get/Post/Put/Head to handle all requests, you can have separate handlers
procedure TMonitorMain.SslHttpAppSrvProcDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
var
    ClientCnx : TMyHttpConnection;
    FileExt: String;
begin
    ClientCnx := Client as TMyHttpConnection;
    FileExt := Lowercase(ExtractFileExt(ClientCnx.Path));

 // ignore most page requests and simply return our status page
    if (Length(ClientCnx.Path) <= 1) or (FileExt = '.htm') or (FileExt = '.html') or
                                                                (FileExt = '.exe')  or (FileExt = '.config') then begin
        ClientCnx.Path := WebServStatusPage;   // ignore page requested and return our status page only
        Exit;
    end;
   // warning will look for other URLs like images in the application directory

 // except our Json status page
   if (CompareText(ClientCnx.Path, WebServStatusJson) = 0) then begin
        Flags := hgWillSendMySelf;
        ClientCnx.AnswerString(Flags, '', 'application/json', '', GetMonStatusJson);
        exit ;
   end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.SslHttpAppSrvSslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
var
    ClientCnx : TMyHttpConnection;
begin
    ClientCnx := Sender as TMyHttpConnection;
    if ErrCode = 0 then
        AddLogLine('SSL Handshake Done: ' + ClientCnx.GetPeerAddr + IcsSpace + ClientCnx.SslHandshakeRespMsg)
    else
        AddLogLine('SSL Handshake Error: ' + ClientCnx.GetPeerAddr + ' - ' + ClientCnx.SslHandshakeRespMsg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.SslX509CertsCertProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
begin
   AddLogLine(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.ClientWSLogEvent(Sender: TObject; const Msg: string);
begin
    AddLogLine(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.ClientWSHandshakeEvent(Client: THttpWSSrvConn; var OK: Boolean; var WelcomeMsg: String);
var
    S: String;
begin
    OK := False;
    if (CompareText(Client.WSReqPage, WebSocStatusWeb) = 0) or (CompareText(Client.WSReqPage, WebSocStatusJson) = 0) then begin
        OK := True;
        Client.WSPingSecs := 30;     // used for server push
        Client.WSPingEnabled := True;
    end;
     if OK then
        S := ' Accepted'
    else
        S := ' Rejected';
    AddLogLine('WebSocket ' + IntToStr(Client.CliId) + IcsSpace + Client.CPeerAddr + ': Request for ' + Client.WSReqPage + S);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
 // called once when the websocket is ready for traffic
procedure TMonitorMain.ClientWSReadyEvent(Client: THttpWSSrvConn);
begin
    AddLogLine('Websocket ' + IntToStr(Client.CliId) + IcsSpace + Client.PeerAddr + ': Ready for ' + Client.WSReqPage);
    if (CompareText(Client.WSReqPage, WebSocStatusWeb) = 0) then begin
        Client.WSSendText(Nil, '==1' + ServerInfo + '<BR><BR>' + IcsCRLF);
        Client.WSSendText(Nil, '==2' + GetMonStatusWeb);
    end;
    if (CompareText(Client.WSReqPage, WebSocStatusJson) = 0) then begin
        Client.WSSendText(Nil, GetMonStatusJson);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// called evry 30 seconds to sending topical content
procedure TMonitorMain.ClientWSPingTimerEvent(Client: THttpWSSrvConn);
begin
    if (CompareText(Client.WSReqPage, WebSocStatusWeb) = 0) then begin
        Client.WSSendText(Nil, '==2' + GetMonStatusWeb);
    end;
    if (CompareText(Client.WSReqPage, WebSocStatusJson) = 0) then begin
        Client.WSSendText(Nil, GetMonStatusJson);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// for any appmonsrv changes, resend any websocket content immediately
procedure TMonitorMain.IcsAppMonSrvChangeEvent(Sender: TObject);
var
    I: Integer;
begin
    if SslHttpAppSrv.ClientCount = 0 then
        Exit;
  // loop around all remote clients
    for I := 0 to SslHttpAppSrv.ClientCount - 1 do begin
        with (SslHttpAppSrv.Client[I] as THttpWSSrvConn) do begin
            if (State = wsConnected) and WSClient then
                ClientWSPingTimerEvent((SslHttpAppSrv.Client[I] as THttpWSSrvConn));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.ClientWSFrameRcvdEvent(Client: THttpWSSrvConn; const APacket: String; var AFrame: TWebSocketReceivedFrame);
begin
    if AFrame = Nil then
        Exit;
    if AFrame.Kind = wsfkPong then
    Exit;
    AddLogLine('Websocket ' + IntToStr(Client.CliId) + IcsSpace + Client.PeerAddr + ': ' + Client.WSReqPage + ' ' +
                                         GetWSFrameKind(AFrame.Kind) + ' received, length: ' + IntToStr(AFrame.DataBytes));
    case AFrame.Kind of
        wsfkClose: begin
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.ClientWSFrameSentEvent(Client: THttpWSSrvConn; var AFrame: TWebSocketOutgoingFrame);
begin
    if AFrame = Nil then
        Exit;
    if AFrame.Kind = wsfkPing then
        Exit;
    AddLogLine('Websocket ' + IntToStr(Client.CliId) + IcsSpace + Client.PeerAddr + ': ' +
                                            Client.WSReqPage + ' ' + GetWSFrameKind(AFrame.Kind) + ' sent');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMonitorMain.ClientWSDisconnectedEvent(Sender: TObject);
var
    Client: THttpWSSrvConn;
begin
    Client := Sender as THttpWSSrvConn;
    AddLogLine('Websocket ' + IntToStr(Client.CliId) + IcsSpace + Client.PeerAddr +
                            ': ' + Client.WSReqPage + ' Closing, Total Frames ' + IntToStr(Client.WSFrameCounter));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// create the ServerStatus.htm web page from a template adding ws:// url and titles
procedure TUrlHandlerWSServerStatus.Execute;
begin
    if Client.SslEnable then                                                                   // host name
        ServerWSHost := 'wss://' + Client.SslServerName + ':' +  Client.CServerPort
    else
        ServerWSHost := 'ws://' + IcsFmtIpv6AddrPort(Client.CServerAddr, Client.CServerPort);  // numeric IP
    ServerWSHost := ServerWSHost + WebSocStatusWeb;
    AnswerPage('', NO_CACHE, WebServStatusPage, nil,
             ['PageFile', WebSocStatusWeb, 'PageTitle', IcsAppMonTitle + ' - ' + SrvCompName, 'WSHost', ServerWSHost ]);
    Finish;
end;

end.
