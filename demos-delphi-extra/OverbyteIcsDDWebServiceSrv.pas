{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  SSL web application server Windows service sample, but may
              also be run interactively for debugging, no real GUI.  It
              requires DDService to be installed. It supports multiple SSL
              hosts with multiple listeners, each with it's own logging file,
              can order it's own SSL certificates, includes hacking protection,
              and will email status information and errors to an administrator.
              It also includes a REST server with simple lookup responses
              from a DISQLite3 SQL database.
              This sample is really a commercial web server.
Creation:     June 2021
Updated:      Feb 2025
Version:      V9.4
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2003-2025 by Fran�ois PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany.

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

Note this web server sample combines the functionality of these two existing samples,
OverbyteIcsWebServ1 for OverbyteIcsHttpSrv, and OverbyteIcsWebAppServerMain for
OverbyteIcsAppHttpSrv.  The main difference is OverbyteIcsAppHttpSrv contains
session support to ease sharing data between pages, and handling of received data
for POST, PUT, PATCH etc so the application does not need it.   Note this sample
shared many units with the other samples.

This sample is non-interactive, web servers are normally run as windows background
servers.  All the server settings come from an INI file which will need to be
edited before the sample will successfully run.  A bare sample INI file is included
which will be copied into the ICS shared INI directory on first run, with the
actual file name shown when you start the application, and that is the file
to edit.

Unlike the other web server samples, this one uses IcsHosts to support multiple
addresses and ports and SSL certificates, but all of these must exist and not being
used by other applications, otherwise the server will not start.  To use SSL, an
SSL certificate should exist for the host name used, IP addresses don't eeally work
with SSL, the OverbyteIcsPemtool sample allows self signed SSL certificates to be
created for testing.  Up to 100 IcsHosts can be specified, you can edit the Windows
HOSTS file if necessary to create alternate host names for your PC, if you don't
have a local DNS server to do it.  Documentation for IcsHosts may be found in
OverbyteIcsWSocketS.pas and at http://wiki.overbyte.eu/wiki/index.php/FAQ_Using_IcsHosts.

This web server will automatically order and install SSL certificates if so required,
from various suppliers, including free certificates from Let's Encrypt, and commercial
certificates for Digicert, Comodo, Thawte and GeoTrust from CertCentre AG.  For
automated ordering, Domain Validation is used which means the web server must be
accessible from the public internet by all the host names for which an SSL
certificate is being ordered. See OverbyteIcsSslX509Certs.pas fore more info.




History:
22 June 2021  V8.67 baseline
23 July 2021 - REST API page now created as TUrlHandlerRestApi instead of as
                 a virtual page, little simpler.
20 May 2022 -  V8.69 - Added OverbyteIcsUrl so it builds again.
               Added authentication using POST requests.
               Added 'Password protected page (POST)' button on demo menu
                 to test authentication using POST.
               Fixed web logging to log correct multiple listener.
               Added OCSP Stapling and certification revoke support, needs
                 OcspSrvStapling=True adding to INI file.  Caches OCSP responses
                 in file specified in INI OcspCacheFile, defaulting to
                  ocspservercache.recs in application directory.
               Added server options oAllowDelete, hoAllowTrace, hoAllowPatch to
                 default INI file to support those verbs.
               Builds on unicode compilers.
               Added Digest SHA-256 authentication page DemoDigest2Auth.html
                 and DemoDigestsAll.html that does both digests.  Note Mozilla
                 Firefox supports SHA-256 but Chrome and Edge do not.
               DemoAuthAll.html no longer does NTLM, use the separate page.
               Need to define YuDISQL to link DISQLite3, pending support for
                 Delphi databases.
               Display server response headers if box ticked, only displayed
                 request headers before.
               File Upload Form and Email Form pages now work correctly.
05 Jul 2022 V8.70 - Added TIcsRestEmail to support OAuth2 authentication for GMail when
                 sending admin emails.  Needs new [RestEmail] section in INI file
                 with OAuth2 account client id and secret and refresh token (see
                 top of OverbyteIcsSslHttpOAuth.pas unit).  The refresh token can
                 be obtained using the OverbyteMailQuTst sample with the same
                 client account.
               hackfilterlist.txt now allows useragent= to filter on the header,
                    allows blocking masscan scanner.
               Better error handling if weblog not opened correctly.
15 Aug 2023 V8.71 Support WebSocket protocol by using TMyHttpConnection connection class
                    THttpWSSrvConn instead of THttpAppSrvConnection (or THttpConnection).
                    In Create, new WS events are set, which are triggered when a WebSocket
                    is requested and when messages are received, there are new client
                    properties like WSReqPage that may be used to determine the WebSocket
                    response. This sample should respond to these WebSocket URLs:
                       wss://localhost/WebSocket/Echo     (echoes messages received)
                       wss://localhost/WebSocket/EchoPing (echo and send keep alive pings)
                       wss://localhost/WebSocket/Chat?MyName (multi user chat server)
               There is a new websocketclient.html page listed on the main demo.html
                  page that allows testing these WebSocket servers.
               Using Int64 ticks.
               Report client SSL certificate it sends one, we could kill connection
                  if not trusted by name or something. Needs CliCertMethod to be set
                  in IcsHosts to sslCliCertRequire or sslCliCertOptional so a client
                  certificate is requested, and it should be tested in the
                  OnSrvSslHandshakeDone event.
               When reporting Hosts, add CliCertMethod.
               Supports using SSL/TLS certificates from the Windows Certificate Store,
                  if specified in INI file.
               Added host list with bindings and certificates to demo.html page.
               Using IcsDomainNameCache component to reverse DNS lookup the remote host
                 name for web logs.
               Replaced hacker delayed 60 second response with immediate redirect to
                 their own address, also check blocked IPs in ClientConnect to avoid
                 SSL negotiation.
               Added TestDottedIp function to treat URLs with IP addresses instead of
                 host names as hackers for SSL only since testing often uses IPs.
Aug 08, 2023 V9.0  Updated version to major release 9.
Feb 08, 2023 V9.1  Added OverbyteIcsCharsetUtils for TextToHtmlText.
                   INI file has new MaxUploadMB and MaxStreamMB settings, each defaults to
                     100 MBbyte to restroct content upload for POST and PUT request, the
                     latter is the largest memory stream before a temporary file is used
                     in UploadDir, so sample now supports large uploads, tested with a
                     6 GBbyte file.  Avoid using Form-Data for large files since the file
                     has to be copied perhaps taking a minute or more before the request
                     response is sent.
                   INI file has new NoSSL setting to prevent HTTPS with SSL/TLS. Various
                     changes to sample so NoSSL does not use OpenSSL functions or offer
                     HTTPS URLs.
                   Added new postinfo.html page that decodes and displays any parameters passed.
                   Replaced LoadSsl with IcsSslRootCAStore.Initialise and LibeayLoadProviders
                     since this sample needs the OpenSSL Legacy provider.
                   Supporting new demo file structure with complex paths.
May 09, 2024 V9.2  DemoAuthAll.html document is now created using a template, to test
                     authentication for templates.
                   Added several buttons to demo.html page to test various GET/POST
                     authentications.
                   Main request logging now in BeforeProcessRequest rather then in
                     each separate request method event, also log RequestDone.
                   Replaced locally generated redirects with AnswerRedirect method.
                   Added Options hoAddMissPath to INI file so missing path is added
                     to directories for template pages.
                   Support favicon.ico request using new AnswerBodyTB method.
Sep 23, 2024 V9.3  Synchronised V9.2 changes from OverbyteIcsSslMultiWebServ.
                   Using OverbyteIcsTypes for consolidated types and constants, allowing
                     other import units to be removed.
                   Added ICS Application Monitor TIcsAppMonCli client component which is
                     designed to report to an ICS Application Monitor server sample
                     IcsAppMon.exe, which will ensure this application remains running,
                     restarting it if it stops or becomes non-responsive, or on demand.
                   When installing or uninstalling as a Windows service, show an error
                     dialog that the program needs administrator rights to install as
                     a service, rather than silently failing.
                   Corrected WebDocDir and WebAppServerData paths in INI file for new
                     directory structure.  If you've built this sample before, the paths
                     will need fixing manually.
                   Using localhost as main host and 127.0.0.1 for main IcsHost, more
                     likely to start than 0.0.0.0
                   More error handling creating web directories.
                   New index.html template default page, TUrlHandlerIndexHtml.
                   Fixed typos for Options in INI file which meant some were ignored.
Feb 07, 2025 V9.4  Close Websocket connections cleanly before stopping server.



Note there is a new sample OverbyteIcsBasicWebServer which is a simplified version of this
sample with only a couple of pages, but much easier to understand for new developers.


DD Service Application Framework Requirement
DDService is an enhanced Windows NT service application framework for Delphi and C++
Builder based on the original VCL service framework. In addition to it it also encapsulates
new Windows NT service APIs introduced since Windows 2000. Original author was the late
Arno Garrels of Duodata in Berlin. Now being maintained by Magenta Systems Ltd.

https://www.magsys.co.uk/delphi/ddservice.asp

DDService must be installed before opening this sample.  Once the sample is built, it
may be installed as a Windows service from an elevated command line prompt with the
command OverbyteIcsDDWebService.exe /install. But this sample is written so it may
also be run as a GUI for debugging.

DISQLite3 SQL Database Support
The REST server sample page restapi.html is only available if DATABASE is defined in
OverbyteIcsDefs.inc to avoid bringing in database units that are not available on all
Delphi editions, and YuDISQL in this unit.  This requires DISQLite3 to be installed
from http://www.yunqa.de, and for the file World.db3 to be copied from the
DISQLite3\Demos into the SslInternet folder.  Note the DISQLite3 demo displays a
dialog box on startup outside the IDE so can not be run as a service, you'll need
to register the component to bypasss the dialog or disable database support.

Insalling note:
All web server configuration is in OverbyteIcsDDWebService.ini file, a default file
is included in the \ics\Samples\Delphi\SslInternet directory, and is copied into the
ICS all users working directory, usually c:\ProgramData\ICS. Note this directory is
different to other ICS samples because it is common to all users instead of one user,
which is necessary because Windows services often run on the system account.  This
INI file will need to be edited for IP addresses, SSL certificates, etc, although
some defaults may work.  Specifically Host4 has certificate ordering settings but
is disabled and will need new valid hosts, IP addresss and files names before it
will do anything useful.

Once this demo has been run once, any changes to the default INI file from new versions
(ie auto certificate ordering) will need to copied manually into the working file.

See OverbyteIcsWSocketS.pas for documentation on TSslWSocketServer whose
properties are exposed by TSslHttpAppSrv, and for IcsHosts which allows the web
server to support multiple Hosts on multiple IP addresses and ports with SSL
support, including automatic SSL certificate ordering.

Also http://wiki.overbyte.eu/wiki/index.php/FAQ_Using_IcsHosts which links to pages
with all the TSslWSocketServer and IcsHosts properties and methods.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
 unit OverbyteIcsDDWebServiceSrv;
{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}
{$I ..\Source\Include\OverbyteIcsDefs.inc}
{.DEFINE YuDISQL}     { V8.69 should DISQLite from http://www.yunqa.de be linked }
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
  Dialogs, StdCtrls, ExtCtrls, TypInfo,
{$IFDEF DATABASE}
    {$IFDEF RTL_NAMESPACES}Data.DB{$ELSE}DB{$ENDIF},
{$IFDEF YuDISQL}
    DISQLite3Api,
    DISQLite3Database,
    DISQLite3DataSet,
{$ENDIF}
{$ENDIF}
  OverbyteIcsWSocket,
  OverbyteIcsWSocketS,
  OverbyteIcsWndControl,
  OverbyteIcsUtils,
  OverbyteIcsIniFiles,
//OverbyteIcsSSLEAY,
//OverbyteIcsLIBEAY,
//OverbyteIcsLogger,
  OverbyteIcsHttpSrv,
  OverbyteIcsHttpAppServer,
  OverbyteIcsSslX509Certs,
//OverbyteIcsSslHttpRest,
//OverbyteIcsSslX509Utils,
  OverbyteIcsWebSession,
  OverbyteIcsFormDataDecoder,
  OverbyteIcsMimeUtils,
//OverbyteIcsSmtpProt,
  OverbyteIcsTicks64,
  OverbyteIcsBlacklist,
  OverbyteIcsMailQueue,
  OverbyteIcsUrl,                     { V8.69 }
  OverbyteIcsSslMultiWebDataModule,
  OverbyteIcsSslMultiWebSessionData,
  OverbyteIcsSslMultiWebConfig,
  OverbyteIcsSslMultiWebUrlDefs,
  OverbyteIcsSslMultiWebHomePage,
  OverbyteIcsSslMultiWebHelloWorld,
  OverbyteIcsSslMultiWebCounter,
  OverbyteIcsSslMultiWebLogin,
  OverbyteIcsSslMultiWebCounterView,
  OverbyteIcsSslMultiWebHead,
  OverbyteIcsSslMultiWebUploads,
  OverbyteIcsSslMultiWebMailer,
  OverbyteIcsSslHttpOAuth,  { V8.70 }
  OverbyteIcsWebSocketcli,  { V8.71 }
  OverbyteIcsWebSocketSrv,  { V8.71 }
  OverbyteIcsDnsQuery,      { V8.71 }
  OverbyteIcsHtmlUtils,     { V9.1 }
  OverbyteIcsSslUtils,      { V9.1 TOcspHttp }
  OverbyteIcsSslBase,       { V9.1 TSslContext, TX509Bas, TX509List }
  OverbyteIcsTypes,         { V9.3 consolidated types and constants }
  OverbyteIcsAppMonCli;     { V9.3 TIcsAppMonCli }

const
    SrvCopyRight : String = ' OverbyteIcsDDWebService (c) 2025 Francois Piette V9.4 ';
    AppTitle     : String = 'OverbyteIcsDDWebService';  { V9.3 }
    MaxWinChars = 800000;
    WM_STARTUP = WM_USER + 722 ;
    LogNameMask = '"ddwebapp-"yyyymmdd".log"' ;
    SimpLogName = '"ddwebapp-"yyyymmdd".log"' ;
    WebLogFields = '#Fields: date time s-sitename s-computername s-ip cs-method cs-uri-stem cs-uri-query ' +
                   's-port cs-username c-ip cs-version cs(User-Agent) cs(Referer) cs-host sc-status ' +
                   'sc-bytes cs-bytes time-taken' ;
    WebLogHdrMask = '"#Date: "' + ISODateMask + #32 + ISOTimeMask + '"';
    XmitBufSize = 65536 ;
    RecvBufSize = 65536;
//  MAX_UPLOAD_SIZE    = 1024 * 1024 * 60; // Accept max 60MB file V9.1 configured in server config file

type
  TMyHttpConnection = class(THttpWSSrvConn)   { V8.71 support WebSocket protocol, was THttpAppSrvConnection }
  public
    CStartTick        : Int64;      { V8.71 }
    CPeerHostName     : string;     { V8.71 }
    CLastRead         : Int64;
    CLastWrite        : Int64;
 {   FRespTimer        : TTimer;    gone V8.71  send a delayed response }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure BgExceptionEvent (Sender : TObject; E : Exception; var CanClose : Boolean);
//    procedure TimerRespTimer(Sender: TObject);
  end ;

Type
  TUrlHandlerWSClientEcho = class(TUrlHandler)       { V8.71  WebSocket page }
  public
      procedure Execute; override;
  end;

  TUrlHandlerIndexHtml = class(TUrlHandler)      { V9.3 index.html page handler }
  public
      procedure Execute; override;
  end;

type
  TDDWebServiceSrv = class(TForm)
    Panel1: TPanel;
    StopButton: TButton;
    StartButton: TButton;
    doClear: TButton;
    RecheckCertsButton: TButton;
    DisplayMemo: TMemo;
    IcsMailQueue: TIcsMailQueue;
    IcsSslX509Certs: TSslX509Certs;
    Timer1: TTimer;
    SslHttpAppSrv1: TSslHttpAppSrv;
    DisplayHeaderCheckBox: TCheckBox;
    DisplaySslInfo: TCheckBox;
    IcsRestEmail: TIcsRestEmail;
    IcsDomainNameCache1: TIcsDomainNameCache;
    IcsAppMonCli: TIcsAppMonCli;
    doRestartApp: TButton;
    procedure WMCMSTARTUP (var Msg : TMessage); message WM_STARTUP ;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure IcsMailQueueLogEvent(LogLevel: TMailLogLevel; const Info: string);
    procedure IcsSslX509CertsCertProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
    procedure Timer1Timer(Sender: TObject);
    procedure SslHttpAppSrv1ClientConnect(Sender, Client: TObject; Error: Word);
    procedure SslHttpAppSrv1AuthGetPassword(Sender, Client: TObject; var Password: string);
    procedure SslHttpAppSrv1AuthGetType(Sender, Client: TObject);
    procedure SslHttpAppSrv1BeforeProcessRequest(Sender, Client: TObject);
    procedure SslHttpAppSrv1BgException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure SslHttpAppSrv1Display(Sender: TObject; const Msg: string);
    procedure SslHttpAppSrv1GetDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1ServerStarted(Sender: TObject);
    procedure SslHttpAppSrv1ServerStopped(Sender: TObject);
    procedure SslHttpAppSrv1SslAlpnSelect(Sender: TObject; ProtoList: TStrings; var SelProto: string; var ErrCode: TTlsExtError);
    procedure SslHttpAppSrv1SslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
    procedure SslHttpAppSrv1SslServerName(Sender: TObject; var Ctx: TSslContext; var ErrCode: TTlsExtError);
    procedure SslHttpAppSrv1VirtualException(Sender: TObject; E: Exception; Method: THttpMethod; const Path: string);
    procedure SslHttpAppSrv1WellKnownDir(Sender, Client: TObject; const Path: string; var BodyStr: string);
    procedure StartQueueMail;
    procedure StopQueueMail;
    procedure SendAdminEmail (const EmailTo, Subject, Body: string) ;
    procedure LoadHackLists;
    function TestFilters (const Argtype, Value: String): Boolean ;
    function TestIpWhiteList (const Value: String): Boolean ;
    procedure onBlackLogEvent (const info: string);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure doClearClick(Sender: TObject);
    procedure RecheckCertsButtonClick(Sender: TObject);
    procedure SslHttpAppSrv1AuthNtlmBeforeValidate(Sender, Client: TObject; var Allow: Boolean);
    procedure SslHttpAppSrv1AuthResult(Sender, Client: TObject; Success: Boolean);
    procedure SslHttpAppSrv1ConnectDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1HttpMimeContentType(Sender, Client: TObject; const FileName: string; var ContentType: string);
    procedure SslHttpAppSrv1OptionsDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1PatchDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1PutDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1TraceDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1HeadDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1PostDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
    procedure IcsSslX509CertsChallengeDNS(Sender: TObject; ChallengeItem: TChallengeItem; var ChlgOK: Boolean);
    procedure IcsSslX509CertsNewCert(Sender: TObject);
    procedure IcsSslX509CertsOAuthAuthUrl(Sender: TObject; const URL: string);
    procedure HttpAppSrvClientBgException(Sender: TObject; E: Exception; var CanClose : Boolean);
    procedure SslHttpAppSrv1DeleteSession(Sender: TObject; Session: TWebSession);
    procedure SslHttpAppSrv1DeleteDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
    procedure SslHttpAppSrv1AfterAnswer(Sender, Client: TObject);
    procedure SslHttpAppSrv1HttpRespHdr(Sender, Client: TObject; const Header: string);
    procedure IcsRestEmailEmailNewToken(Sender: TObject);
    procedure IcsRestEmailEmailProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
    procedure IcsRestEmailOAAuthUrl(Sender: TObject; const URL: string);
    procedure IcsMailQueueOATokenEvent(ServNr: Integer; var Token, TokAccount: string; var TokExpireDT: TDateTime);
    procedure ClientWSLogEvent(Sender: TObject; const Msg: string);                                       { V8.71 websockets }
    procedure ClientWSHandshakeEvent(Client: THttpWSSrvConn; var OK: Boolean; var WelcomeMsg: String);    { V8.71 websockets }
    procedure ClientWSFrameRcvdEvent(Client: THttpWSSrvConn; const APacket: String; var AFrame: TWebSocketReceivedFrame);  { V8.71 websockets }
    procedure ClientWSFrameSentEvent(Client: THttpWSSrvConn; var AFrame: TWebSocketOutgoingFrame);         { V8.71 websockets }
    procedure ClientWSDisconnectedEvent(Sender: TObject);                                                  { V8.71 websockets }
    procedure ClientWSReadyEvent(Client: THttpWSSrvConn);                                                  { V8.71 websockets }
    procedure ClientWSPingTimerEvent(Client: THttpWSSrvConn);                                              { V8.71 websockets }
    procedure WSSendChatResp(SendCli: THttpWSSrvConn; const APacket: String; ToMe: Boolean);               { V8.71 websockets }
    function  WSGetChatUsers(const ChatPage: String; var UList: String): Integer;
    procedure IcsDomainNameCache1DNUpdateEvent(Sender: TObject; ItemNr: Integer);                          { V8.71 websockets }
    function  TestDottedIp (Value: String): Boolean ;
    procedure IcsAppMonCliLogEvent(Sender: TObject; const Line: string);
    procedure doRestartAppClick(Sender: TObject);
    procedure SslHttpAppSrv1HttpRequestDone(Sender, Client: TObject);
    procedure SslHttpAppSrv1PostedData(Sender, Client: TObject; Error: Word);                                                      { V8.71 }
  private
    FIniFileName : String;
    FFinalized   : Boolean;
    FDataDir     : String;
    FSessionFile : String;
//    FCountRequests : Integer;          V9.2 not really used
    FUploadsDir  : String;
    FServerInfo  : String;   { V9.3 }
    procedure CreateVirtualDocument_Demo(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
{    procedure CreateVirtualDocument_DemoAuthAll(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);     { V9.2 now a template }
    procedure CreateVirtualDocument_DemoNtlmAuth(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_DemoDigestAuth(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_DemoBasicAuth(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_Time(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_Bruno(Sender    : TObject;
                                         ClientCnx : TMyHttpConnection;
                                         var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_Redir(Sender    : TObject;
                                             ClientCnx : TMyHttpConnection;
                                             var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_MyIP(Sender    : TObject;
                                             ClientCnx : TMyHttpConnection;
                                             var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_HeaderBug(Sender    : TObject;
                                              ClientCnx : TMyHttpConnection;
                                              var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_Template(Sender    : TObject;
                                             ClientCnx : TMyHttpConnection;
                                             var Flags : THttpGetFlag);
    procedure CreateVirtualDocument_DemoDigest2Auth(Sender  : TObject;
                                             ClientCnx : TMyHttpConnection;
                                             var Flags : THttpGetFlag);               { V8.69 }
    procedure CreateVirtualDocument_DemoDigestsAll(Sender  : TObject;
                                             ClientCnx : TMyHttpConnection;
                                             var Flags : THttpGetFlag);               { V8.69 }
    procedure DisplayHeader(ClientCnx : TMyHttpConnection);

  public
    procedure Display(Msg : String);
    procedure ReportHosts;
    property IniFileName : String read FIniFileName write FIniFileName;
    property DataDir : String read FDataDir write FDataDir;
    property UploadsDir : String read FUploadsDir write FUploadsDir;
  end;

  TUrlHandlerRestApi = class(TUrlHandler)
  public
      procedure Execute; override;
  end;


var
    DDWebServiceSrv: TDDWebServiceSrv;
    WinLinesBuff: string ;
    WinDispCur: Integer;
    ProgDirectory: String;
    LogDate: Integer;
    SrvCompName: string;
    StatSrvSslCert: String;
    StatSrvSslCertWeb: String;
    HouseKeepingTrg: int64;
    CertCheckTrigger: int64 ;
    LockFileAccess: TRtlCriticalSection;
    DiagLogBuffer: TIcsBuffLogStream;    { server diag log file }
    TotWebLogs: Integer;                 { how many different web log files }
    WebLogBuffers: array of TIcsBuffLogStream;  { web log files }
    AttemptsBlackList: TIcsBlackList ; { restrict access by IP addresses }
    HackBlackList: TIcsBlackList ;     { detected hackers }
    HackFilterList: TStringList ;      { filters to detect hackers }
    WhiteIpList: TStringList ;         { people we don't want to block }
    AdminEmailTo: String;              { email address for admin alerts }
    LastErrorEmail: String;            { avoid sending to many error emails }
    OldRefrToken: String;              { V8.70 see if it changes }
{$IFDEF DATABASE}
{$IFDEF YuDISQL}
    DISQLite3Database: TDISQLite3Database;
    DISQLite3Query: TDISQLite3UniDirQuery;
{$ENDIF}
    DbOpened: Boolean;
{$ENDIF}

implementation

{$R *.dfm}
{$R ICSMedia.res}    { V9.2 }

uses
    OverbyteIcsDDWebServiceCtl;

const
    SectionData        = 'Data';
    SectionGeneraL     = 'General';
    KeyServLogDir      = 'ServLogDir';
    KeyAdminEmailTo    = 'AdminEmailTo';

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMyHttpConnection.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
  { keep alive means connection may be used for multiple requests so we must track how much
    data is sent before and after each request }
    OnBgException := BgExceptionEvent ;
    CLastRead := 0 ;
    CLastWrite := 0 ;
    CPeerHostName := '' ;                                            { V8.71 }
    OnWSLogEvent := DDWebServiceSrv.ClientWSLogEvent;                { V8.71 websockets }
    OnWSHandshake := DDWebServiceSrv.ClientWSHandshakeEvent;         { V8.71 websockets }
    OnWSFrameRcvd := DDWebServiceSrv.ClientWSFrameRcvdEvent;         { V8.71 websockets }
    OnWSFrameSent := DDWebServiceSrv.ClientWSFrameSentEvent;         { V8.71 websockets }
    OnWSDisconnected := DDWebServiceSrv.ClientWSDisconnectedEvent;   { V8.71 websockets }
    OnWSReady := DDWebServiceSrv.ClientWSReadyEvent;                 { V8.71 websockets }
    OnWSPingTimer := DDWebServiceSrv.ClientWSPingTimerEvent;         { V8.71 websockets }
    // DebugBody decodes all frames, DebugHdr only main info , DebugConn only connections oe DebugNone
    DebugLevel := DebugConn;                                        { V8.71 websockets }
    if DDWebServiceSrv.DisplayHeaderCheckBox.Checked then
        DebugLevel := DebugHdr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMyHttpConnection.BgExceptionEvent(Sender : TObject;
                                  E : Exception; var CanClose : Boolean);
begin
     DDWebServiceSrv.Display('Client Error - ' + IcsGetExceptMess (E)) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We need to override parent class destructor  }
destructor TMyHttpConnection.Destroy;
begin
 //   FreeAndNil (FRespTimer);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ we update DisplayMemo once a second in the timer }
procedure TDDWebServiceSrv.Display(Msg : String);
begin
    if NOT GStartedByScm then  // no visible log window if running as service
        WinLinesBuff := WinLinesBuff + Msg + icsCRLF ;
    if Assigned (DiagLogBuffer) then begin
        try
            DiagLogBuffer.WriteLine (Msg);
        except
        end ;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.doClearClick(Sender: TObject);
begin
    WinLinesBuff := '';
    DisplayMemo.Lines.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// V9.3 is ICS Application Monitor is running in GUI, ask it to restart our program
procedure TDDWebServiceSrv.doRestartAppClick(Sender: TObject);
begin
    if IcsAppMonCli.CliState = CliStateOK then begin
        Display ('Requesting Applicaton Restart by ' + IcsAppMonTitle) ;
        IcsAppMonCli.RestartNow('Manually Requested Restart by User');
        StopButtonClick(Self);
    end
    else
        Display ('Manual Restart Unavailable, ' + IcsAppMonTitle + ' Not Running') ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.Timer1Timer(Sender: TObject);
var
    displen, removelen, newstart, K: integer ;
    S1: String ;
    NewFlag: Boolean;
begin
 // V9.3  Application Monitor component, only if server is running, should do more checks
    if SslHttpAppSrv1.ListenAny then
        IcsAppMonCli.Awake('', '');

  // check SSL certificates every two hours, may order expired certs
    if IcsTestTrgTick64 (CertCheckTrigger) then begin
        CertCheckTrigger := IcsGetTrgMins64 (120) ;
        try
          // don't stop on first error, no exceptions
            if NOT SslHttpAppSrv1.NoSSL then begin  { V9.1 SSL is optional }
                Display('Regular Server Recheck for New SSL Certificates Starting');
                DiagLogBuffer.FlushFile(True);
                NewFlag := SslHttpAppSrv1.RecheckSslCerts(S1, False, True);
                if NewFlag or (S1 <> '') then  begin
                    if NewFlag then Display('Server Recheck Loaded New SSL Certificate(s)');
                    Display('Server Recheck SSL Certificate Errors:' + icsCRLF + S1);
                    if (S1 <> '') and (LastErrorEmail <> S1) then { v8.60 tell someone }
                        SendAdminEmail (AdminEmailTo, 'ICS DD Web Service Recheck SSL Certificate Errors', S1);
                    LastErrorEmail := S1 ;
                    ReportHosts;    // report everything again
                    Display('Listen Bindings:' + icsCRLF + SslHttpAppSrv1.ListenStates);
                end
                else
                    Display('Server Recheck SSL Certificate Nothing New');
            end;
            LoadHackLists;  //update filters
        except
            on E:Exception do begin
               Display('Server Recheck SSL Certificate Failed - ' + E.Message);
            end;
        end;
    end;

  // rotate logs at midnight, recheck SSL certificates, log configuration again
    if LogDate <> Trunc(Date) then begin
        LogDate := Trunc(Date);

      { V8.60 flush all logs with old file name, new stuff will be new date }
        try
            if Assigned (DiagLogBuffer) then
                DiagLogBuffer.FlushFile(True);
            if TotWebLogs > 0 then begin
                for K := 0 to TotWebLogs - 1 do begin
                    if Assigned (WebLogBuffers[K]) then
                        WebLogBuffers[K].FlushFile(True);
                end;
            end;
        except
        end ;
        Display('Nightly Server Recheck Starting');
        ReportHosts;    // report everything again
        Display('Listen Bindings:' + icsCRLF + SslHttpAppSrv1.ListenStates);
        CertCheckTrigger := Trigger64Immediate;
        DiagLogBuffer.FlushFile(True);
    end;

  // see if updating the log window with multiple lines
    displen := Length (WinLinesBuff) ;
    if displen > 0 then begin
        try
            if WinDispCur + displen > MaxWinChars then begin
                S1 := DisplayMemo.Lines.Text ;
                removelen := MaxWinChars - displen - 20000 ;
                if removelen > 20000 then begin
                    S1 := copy (S1, removelen, 9999999) ;
                    newstart := Pos(#13, S1) ; // find start of next line
                    DisplayMemo.Text := copy (S1, newstart, 9999999) + WinLinesBuff ;
                    WinDispCur := Length (S1) - newstart + displen ;
                end
                else begin
                    DisplayMemo.Lines.Text := WinLinesBuff ;
                    WinDispCur := displen ;
                end;
            end
            else begin
                SetLength (WinLinesBuff, displen - 2) ;  // remove CRLF
                DisplayMemo.Lines.Add (WinLinesBuff) ;
                WinDispCur := WinDispCur + displen ;
                SendMessage (DisplayMemo.Handle, WM_VSCROLL, SB_BOTTOM, 0);
            end;
        except
        end ;
        WinLinesBuff := '' ;
    end;

 // house keeping every five minutes
    if IcsTestTrgTick64(HouseKeepingTrg) then begin
        try
            HouseKeepingTrg := IcsGetTrgMSecs64 (300);
            if SslMultiWebDataModule.DataDir <> '' then
                CleanupTimeStampedDir(SslMultiWebDataModule.DataDir);
        except
            HouseKeepingTrg := IcsGetTrgMSecs64 (300);
        end ;
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.LoadHackLists;

    procedure LoadList (fname: string; MyList: TStringList);
    var
        I, J: Integer;
        Line, Fullname: String;
    begin
        Fullname := IncludeTrailingPathDelimiter(ProgDirectory) + fname;
        if NOT FileExists (Fullname) then begin
            Display('List File Not Found: ' + Fullname) ;
            exit ;
        end;
        try
            MyList.LoadFromFile (Fullname) ;

         //  clean up filter lines, removed comments from end  ***'
            if MyList.Count > 0 then begin
                for I := (MyList.Count - 1) downto 0 do begin
                    Line := IcsLowerCase(Trim(MyList[I]));
                    J := Pos(' ***', Line);                 // *** comment end
                    if J > 4 then Line := Trim(Copy(Line, 1, J));
                    if Pos('*', Line) = 1 then Line := '';  // * comment start
                    if Length(Line) > 0 then
                        MyList[I] := Line
                    else begin
                        MyList.Delete(I);  // remove line
                        if MyList.Count = 0 then break; // sanity check
                    end;
                end;
            end;
            if MyList.Count = 0 then
                 Display('List is empty: ' + Fullname)
            else
                 Display('Loaded List OK: ' + Fullname +
                                 ', total ' + IntToStr(MyList.Count)) ;
            Display(MyList.Text);  // !!! TEMP DIAG
        except
            Display('Failed to Load List: ' + Fullname);
        end;
    end;

begin
    LoadList ('hackfilterlist.txt', HackFilterList) ;  // Feb 2019 - hackers bad paths and IP addresses
    LoadList ('whiteiplist.txt', WhiteIpList) ;  // Feb 2019 - address we don't want to block
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDDWebServiceSrv.TestFilters (const Argtype, Value: String): Boolean ;  // Feb 2019
var
    I: Integer;
    Line, Filter: String;
begin
    Result := False;
    if HackFilterList.Count = 0 then Exit;
    for I := 0 to HackFilterList.Count - 1 do begin
        Line := HackFilterList [I];
        if Pos (Argtype + '=', Line) = 1 then begin
            Filter := Copy (Line, Length(Argtype) + 2, 999);
            if Pos (Filter, Value) > 0 then begin
                Result := True;
                Display('Filter List matched: ' + Line + ' - ' + Value);
                Exit;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDDWebServiceSrv.TestIpWhiteList (const Value: String): Boolean ;
var
    I: Integer;
    Line: String;
begin
    Result := False;
    if WhiteIpList.Count = 0 then Exit;
    for I := 0 to WhiteIpList.Count - 1 do begin
        Line := WhiteIpList [I];
        if Pos (Line, Value) = 1 then begin
            Result := True;
            Display('While IP Address List matched: ' + Line + ' - ' + Value);
            Exit;
        end;
    end;

 // pending - handle blocks ie 178.255.82.64/27 is 178.255.82.64 through 178.255.82.95

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDDWebServiceSrv.TestDottedIp (Value: String): Boolean ;   { V8.71 }
var
    I: Integer;
    ASocketFamily: TSocketFamily;
begin
    if Pos('[', Value) = 1  then  //   http://[2a00:1940:1:2::115:84]:88/
    begin
         I := Pos(']', Value);
         Value := Copy(Value, 2, I - 2);
    end
    else
    begin
        if (Pos('.', Value) > 1) then  // IPv4 only
        begin
            I := Pos(':', Value); // look for port on end
            if I > 1 then SetLength(Value, I - 1);
        end;
    end;
    Result := WSocketIsIP (Value, ASocketFamily);
    if Result then
         Display('Found Dotted Host: ' + Value);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.ApplicationEventsException(Sender: TObject;
  E: Exception);
begin
    Display('!!! Application Exception Event - ' + IcsGetExceptMess (E));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.FormCreate(Sender: TObject);
var
    List: TStringList;
    FIniFile: TIcsIniFile;
    FName, FHdr: String;
begin
    FIniFileName := GetIcsIniFileName(False);  // use all users/programdata since service runs without an account
  // ensure SSL DLLs come from program directory, and exist, else die
    GSSLEAY_DLL_IgnoreOld := true;
    Application.OnException := ApplicationEventsException ;
    ProgDirectory := ExtractFileDir(IcsLowercase (ParamStr(0)));
    GSSL_DLL_DIR := ProgDirectory + '\';
    GSSL_SignTest_Check := True;
    GSSL_SignTest_Certificate := True;
//    OverbyteIcsWSocket.LoadSsl;     { V9.1 not until server settings loaded }
    LogDate := Trunc(Date);
    HouseKeepingTrg := IcsGetTrgSecs64 (300);
    CertCheckTrigger := Trigger64Disabled;
    Timer1.Enabled := True;
    SrvCompName := IcsGetCompName ;

   { try and open INI file in c:\ProgramData\ICS, if missing copy default from sample directory  }
   { note this is a different INI directory to other ICS samples, perhaps easier to find than Users }
    IcsSimpleLogging (SimpLogName, 'DD Web App INI File: ' + FIniFileName);
    try
        if NOT FileExists(FIniFileName) then begin
            FName := IcsAbsolutisePath(ExtractFilePath(ParamStr(0)) + '..\..\..\' + ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini'));  { V9.1 lower directory }
            if FileExists(FName) then begin
                List := TStringList.Create;
                try
                    List.LoadFromFile(FName);
                    List.SaveToFile(FIniFileName);
                    Display('Copied default INI file to: ' + FIniFileName + ', where it should be edited');
                    IcsSimpleLogging (SimpLogName, 'Copied default INI File, it should be edited');
                except
                    List.Free;
                end;
            end;
        end;
    except
    end;
    FIniFile := TIcsIniFile.Create(FIniFileName);
    FName := FIniFile.ReadString(SectionGeneral, KeyServLogDir,  '');
    AdminEmailTo := FIniFile.ReadString(SectionGeneral, KeyAdminEmailTo,  '');
    IcsLoadMailQuFromIni(FIniFile, IcsMailQueue, 'MailQueue');
    IcsLoadRestEmailFromIni(FIniFile, IcsRestEmail, 'RestEmail');  { V8.70 load OAuth2 secrets and token }
    OldRefrToken := IcsRestEmail.RefrToken;                        { V8.70 see if it changes }
    FIniFile.Free;
    HackFilterList := TStringList.Create  ;
    WhiteIpList := TStringList.Create  ;
    if FName <> '' then begin
        ForceDirectories(ExtractFileDir(FName));
        FName := '"' + IncludeTrailingPathDelimiter(FName) + '"' + LogNameMask;  // file name is a mask to add date
        FHdr :=  '"' + SrvCopyRight + IcsCRLF +
         'Log File Created: "dd mmm yyyy hh:mm:ss"' + IcsCRLF +
         'Computer: ' + SrvCompName + '"' + IcsCRLF ;
        DiagLogBuffer := TIcsBuffLogStream.Create (Self, FName, FHdr, FileCPUtf8);
        Display('Log File: ' + DiagLogBuffer.FullName);
        IcsSimpleLogging (SimpLogName, 'Log File: ' + DiagLogBuffer.FullName);
    end
    else begin
        DisplayMemo.Lines.Add ('Web server can not open configuration file or illegal, stopping: ' + FIniFileName);
        if GStartedByScm then begin
         // pending need message file
         //   DDWebServiceCtl.LogMessage (FIniFileName, EVENTLOG_ERROR_TYPE, 0, DM_MSG_NOINI);
            PostMessage (Handle, WM_CLOSE, 0, 0) ;
        end;
        Exit;
    end;
    Display('INI file: ' + FIniFileName);
    DiagLogBuffer.FlushFile(True);
    PostMessage (Handle, WM_STARTUP, 0, 0) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.RecheckCertsButtonClick(Sender: TObject);
begin
    CertCheckTrigger := Trigger64Immediate;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.FormClose(
    Sender     : TObject;
    var Action : TCloseAction);
begin
    if FFinalized then Exit;   { V8.64 only once }

 // V9.3 Application Monitor component, nothing happens if never started or stopped already
    IcsAppMonCli.Stop(AppTitle + 'Monitoring Stopping') ;

    FFinalized := True;
    Timer1.Enabled := False;
    CertCheckTrigger := Trigger64Disabled;
    HouseKeepingTrg := Trigger64Disabled;
    StopButtonClick(Self);
    StopQueueMail;
    FreeAndNil(DiagLogBuffer);
    FreeAndNil(HackFilterList);
    FreeAndNil(WhiteIpList);
    FreeAndNil(HackBlackList);
    FreeAndNil(AttemptsBlackList);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.WMCMSTARTUP (var Msg : TMessage);
begin
    StartButtonClick(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.ReportHosts;
var
    I: Integer;
begin
    StatSrvSslCert := '';
    Display('Total server hosts ' + IntToStr(SslHttpAppSrv1.IcsHosts.Count)) ;
    for I := 0 to SslHttpAppSrv1.IcsHosts.Count - 1 do begin
        with SslHttpAppSrv1.IcsHosts[I] do begin
            if CertValRes = chainFail then begin
                Display('Server SSL Certificate Errors - ' + DisplayName);
              { might want to stop here and warn user }
           //     Exit;
            end;

        // build site info for log and status page
            StatSrvSslCert := StatSrvSslCert + 'Site Host ' + IntToStr (I) +
                ' [' + DisplayName + '] ' + HostNames.CommaText + icsCRLF +
                'Bindings: ' + BindInfo + icsCRLF +
                'Web Pages Root: ' + WebDocDir + icsCRLF +
                'Templates Root: ' + WebTemplDir + icsCRLF;
            if WebRedirectStat <> 0 then
                StatSrvSslCert := StatSrvSslCert + 'Redirection to: ' + WebRedirectURL + icsCRLF;
            if WellKnownPath <> '' then
                StatSrvSslCert := StatSrvSslCert + 'Well-Known Root: ' + WellKnownPath + icsCRLF;
            if NOT SslHttpAppSrv1.NoSSL then begin           { V9.1 }
                StatSrvSslCert := StatSrvSslCert + 'SSL Security Level: ' +
                    GetEnumName(TypeInfo(TSslSrvSecurity), Ord(SslSrvSecurity)) + IcsCRLF;
                if CliCertMethod > sslCliCertNone then StatSrvSslCert := StatSrvSslCert +
                    'SSL Client Certificate: ' + GetEnumName(TypeInfo(TSslCliCertMethod), Ord(CliCertMethod)) + IcsCRLF;  { V8.71 }
                StatSrvSslCert := StatSrvSslCert + 'SSL Certificate: ' + CertErrs + IcsCRLF +
                    CertInfo + IcsCRLF;
            end;
            StatSrvSslCert := StatSrvSslCert + IcsCRLF;
        end;
    end;
    StatSrvSslCertWeb := StringReplace (StatSrvSslCert, icsCRLF, '<BR>' + icsCRLF, [rfReplaceAll]);
    Display(StatSrvSslCert);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.StartQueueMail;
var
    S: string ;
    I: integer ;
begin
    try
        if NOT IcsMailQueue.Active then begin
            if (IcsMailQueue.MailQuDir = '') or
                         (NOT ForceDirectories (IcsMailQueue.MailQuDir)) then begin
                Display('!! Failed to Start Mail Queue, No Directory');
                exit ;
            end;

        end;
        if IcsMailQueue.MailServers.Count = 0 then begin
            Display('!! Failed to Start Mail Queue, No Mail Servers');
            exit ;
        end;
        S := '';
        for I := 0 to IcsMailQueue.MailServers.Count - 1 do begin
          { the email account password should be encrypted, here we decrypt it
            IcsMailQueue.MailServers[I].Password := Decrypt(
                                IcsMailQueue.MailServers[I].Password) ;  }
            S := S + IcsMailQueue.MailServers[I].Host + ', ';
        end;
        IcsMailQueue.Active := true ;
        if IcsMailQueue.Active then begin
             Display('Started Mail Queue OK, Servers: ' + S);
        end;
    except
         Display('!! Failed to Start Mail Queue - ' +  IcsGetExceptMess(ExceptObject)) ;
    end;
 end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.StopQueueMail;
begin
    if IcsMailQueue.Active then begin
        if (IcsMailQueue.MailImmItems > 0) then begin
            Display('Waiting up to 10 seconds to send Queued Mail Items');
            IcsMailQueue.WaitSendandStop (10);
        end;
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SendAdminEmail(const EmailTo, Subject, Body: string) ;  { V8.60 }
var
    sTempFrom: string ;
    id: integer ;
begin
    if EmailTo = '' then Exit;
    if NOT IcsMailQueue.Active then StartQueueMail ;  // restart if not running
    if NOT IcsMailQueue.Active then begin
         Display('Error Starting Mail Queue') ;
    end ;
    try
        with IcsMailQueue.QuHtmlSmtp do begin
            sTempFrom := '"' + SrvCompName + '" <' + SrvCompName + '@magsys.co.uk>' ;
            EmailFiles.Clear ;
            RcptName.clear;
            Allow8bitChars := true ;
            ContentType := smtpPlainText ;
            WrapMessageText := false ;
            WrapMsgMaxLineLen := 76 ;
            PlainText.Clear ;
            PlainText.Text := SrvCopyRight  + IcsCRLF + IcsCRLF + Body ;
            Display('Queuing Email to ' + EmailTo + ' from ' + sTempFrom) ;
            RcptName.Clear ;
            RcptName.Add (EmailTo) ;
            FromName := sTempFrom ;
            HdrCc := '' ;  // Aug 2014
            HdrTo := EmailTo ;
            HdrFrom := sTempFrom ;
            HdrReplyTo := sTempFrom ;
            HdrSubject := Subject ;
            XMailer := '' ;
            id := IcsMailQueue.QueueMail ;
            if id > 0 then
            begin
       // done OK
                Display('Email Form Queued OK') ;
            end
            else
            begin
                Display('Failed to Queue Email: ' + ErrorMessage) ;
            end ;
        end ;
    except
        Display('Failed to Queue Email: ' + IcsGetExceptMess (ExceptObject)) ;
    end;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.DisplayHeader(ClientCnx : TMyHttpConnection);
var
    I : Integer;
    S: String;
begin
    if not DisplayHeaderCheckBox.Checked then
        Exit;
    S := ClientCnx.Method + ' ' + ClientCnx.Path;
    if ClientCnx.Params <> '' then S := S + '?' + ClientCnx.Params;
    S := S + ' ' + ClientCnx.Version;
    Display('HDR0) ' + S);
    for I := 0 to ClientCnx.RequestHeader.Count - 1 do
        Display('HDR' + IntToStr(I + 1) + ') ' +
                ClientCnx.RequestHeader.Strings[I]);
end;




{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.StartButtonClick(Sender: TObject);
var
    J, K: Integer;
    Errs, S, BaseDir: String;
    FIniFile: TIcsIniFile;

    function BuildDemoURIs: String;
    var
        I: integer;
    begin
        Result := '' ;
        for I:= 0 to SslHttpAppSrv1.IcsHosts.Count - 1 do begin
            with SslHttpAppSrv1.IcsHosts [I] do begin
                if NOT HostEnabled then continue;
                if BindNonPort <> 0 then begin
                    Result := Result + 'http://' + SslHttpAppSrv1.IcsHosts [I].HostNames[0];
                    if BindNonPort <> 80 then Result := Result + ':' + IntToStr(BindNonPort);
                    Result := Result + '/' + WebDefDoc + icsCRLF;
                end;
                if (BindSslPort <> 0) and (NOT SslHttpAppSrv1.NoSSL) then begin       { V9.1 }
                    Result := Result + 'https://' + HostNames[0];
                    if BindSslPort <> 443 then Result := Result + ':' + IntToStr(BindSslPort);
                    Result := Result + '/' + WebDefDoc + icsCRLF;
                end;
            end;
        end;
    end;

begin
    BaseDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)));
    BaseDir := IcsAbsolutisePath(BaseDir + '..\..\..\..\demos-data\');    { V9.1 new data path }
    FDataDir := BaseDir + 'WebAppServerData\Data';
    FUploadsDir := BaseDir + 'WebAppServerData\Uploads';
    FSessionFile := FDataDir + '\Sessions.dat';

    try

      // main web server settings from INI file, built in CA if no file found
        FIniFile := TIcsIniFile.Create(FIniFileName);
        IcsLoadTHttpAppSrvFromIni(FIniFile, SslHttpAppSrv1, 'WebAppServer');
        FServerInfo := SrvCopyRight;
        if NOT SslHttpAppSrv1.NoSSL then begin  { V9.1 SSL is optional }
         //   OverbyteIcsWSocket.LoadSsl;
            IcsSslRootCAStore.Initialise;       { V9.1 if OpenSSL and internal not loaded, do it }
            IcsSslLoadProviders(True, False);   { V9.3 need legacy provider }
      // tell them who we are
            Display(SrvCopyRight + icsCRLF + 'SSL/TLS: ' + IcsReportOpenSSLVer(True));    { V9.1 simplify }
            FServerInfo := FServerInfo + '<br>SSL/TLS ' + IcsReportOpenSSLVer(False);
        //    if (SslHttpAppSrv1.RootCA = '') or (NOT FileExists(SslHttpAppSrv1.RootCA)) then
       //         SslHttpAppSrv1.RootCA := sslRootCACertsBundle;

          // V8.65 ordering X509 certificates may need a proxy server
            IcsSslX509Certs.ProxyURL := FIniFile.ReadString ('WebAppServer', 'X509ProxyURL', '') ;
            SslHttpAppSrv1.OcspSrvHttp.CacheFName := FIniFile.ReadString ('WebAppServer', 'OcspCacheFile', 'ocspddservercache.recs') ;  { V8.69 }
            SslHttpAppSrv1.OcspSrvHttp.OcspHttpProxy := IcsSslX509Certs.ProxyURL;                                   { V8.69 }
        end
        else begin
            SslHttpAppSrv1.OcspSrvStapling := False;
        end;
        FServerInfo := FServerInfo + '<br>Running on: ' + SrvCompName;

    // V9.3 Application Monitor component
        IcsAppMonCli.AppTitle := AppTitle;
        IcsAppMonCli.AppGeneral := SrvCopyRight + ', ' + IcsBuiltWithEx + ', ' + IcsReportOpenSSLVer(False);
        if GStartedByScm then
            IcsAppMonCli.ServiceName := DDWebServiceCtl.ServiceName
        else
            IcsAppMonCli.ServiceName := ServiceNone;
        IcsAppMonCli.LANWide := False;  // only listen for IcsAppMon on this computer
        IcsAppMonCli.AppHandle := Self.Handle;
        IcsAppMonCli.AppMonMode := AMModeNonStop;
        IcsAppMonCli.LogPackets := True;
        IcsAppMonCli.Start (AppTitle + ' Monitoring Starting');

      // read the server hosts from INI file and check SSL files exist
        IcsLoadIcsHostsFromIni(FIniFile, SslHttpAppSrv1.IcsHosts, 'Host');
        if SslHttpAppSrv1.IcsHosts.Count <= 0 then begin
            Display('Can Not Start Server - No Source Server Hosts Configured') ;
            exit ;
        end;
        Display('Number of Hosts Configured: ' + IntToStr(SslHttpAppSrv1.IcsHosts.Count));  { V8.64 }
        FIniFile.Free;

    { V8.60  each host may have a different log file, or share a log with other hosts }
        TotWebLogs := 0;
        SetLength(WebLogBuffers, SslHttpAppSrv1.IcsHosts.Count);

        for J := 0 to SslHttpAppSrv1.IcsHosts.Count - 1 do begin
            with SslHttpAppSrv1.IcsHosts [J] do begin
                if NOT HostEnabled then continue;

            // special case, add ICS sample directory path to directories without drive letters
                if Pos ('WebAppServerData\', WebDocDir) = 1 then WebDocDir := BaseDir + WebDocDir;
                if Pos ('WebAppServerData\', WebTemplDir) = 1 then WebTemplDir := BaseDir + WebTemplDir;
                if Pos ('WebServData\', WebDocDir) = 1 then WebDocDir := BaseDir + WebDocDir;           { V9.3 }
                if Pos ('WebServData\', WebTemplDir) = 1 then WebTemplDir := BaseDir + WebTemplDir;     { V9.3 }

            // create web logging file name, use INI directory if nothing better specified
                if WebLogDir = '' then WebLogDir := IncludeTrailingPathDelimiter(ExtractFileDir(FIniFileName)) + HostTag;
                try
                    ForceDirectories(ExtractFileDir(WebLogDir));
                except
                    Display('Failed to Create Directory: ' + WebLogDir);  { V9.3 }
                end;
                WebLogDir := '"' + IncludeTrailingPathDelimiter(WebLogDir) + '"' + LogNameMask;  // file name is a mask to add date
            end;
        end;

    // validate hosts and keep site certificiate information
        try
            Errs := SslHttpAppSrv1.ValidateHosts(False, True); // don't stop on first error, no exceptions
            if Errs <> '' then begin
                Display('Server Validation Errors:' + icsCRLF + Errs);
                if AdminEmailTo <> '' then
                    SendAdminEmail (AdminEmailTo, 'ICS DD Web Service Validation Errors', Errs);
            end;
            ReportHosts;
            Display('Required Listen Bindings:' + icsCRLF + SslHttpAppSrv1.ListenStates);
        except
            on E:Exception do begin
                Display('Host Validation Failed, Server Stopped - ' + E.Message);
                Exit;
            end;
        end;

    // setup some web server defauls, most were done in IcsLoadTHttpAppSrvFromIni
        SslHttpAppSrv1.TemplateDir :=  SslHttpAppSrv1.IcsHosts [0].WebTemplDir;
        SslHttpAppSrv1.DocDir :=  SslHttpAppSrv1.IcsHosts [0].WebDocDir;
        SslHttpAppSrv1.DefaultDoc :=  SslHttpAppSrv1.IcsHosts [0].WebDefDoc;
        SslHttpAppSrv1.ServerHeader := DefServerHeader;  // get latest version
        SslHttpAppSrv1.ClientClass := TMyHttpConnection;
        SslHttpAppSrv1.Options := SslHttpAppSrv1.Options + [hoContentEncoding];  // compress replies
        SslHttpAppSrv1.MaxBlkSize := XmitBufSize;
        SslHttpAppSrv1.SocketErrs := wsErrFriendly ;
        SslHttpAppSrv1.ExclusiveAddr := true ;
        if SslHttpAppSrv1.SessionTimeout < 30 then SslHttpAppSrv1.SessionTimeout := 300;  // sanity check

     // Force directory creation
        try
            ForceDirectories(FDataDir);
            ForceDirectories(FUploadsDir);
            if SslHttpAppSrv1.TemplateDir <> '' then
                ForceDirectories(SslHttpAppSrv1.TemplateDir);
            ForceDirectories(SslHttpAppSrv1.DocDir);
            ForceDirectories(SslHttpAppSrv1.DocDir + '\Js');
            ForceDirectories(SslHttpAppSrv1.DocDir + '\Styles');
            ForceDirectories(SslHttpAppSrv1.DocDir + '\Images');
        except
            Display('Failed to Create Directory: ' + SslHttpAppSrv1.DocDir);  { V9.3 }
        end;

        SslMultiWebDataModule.IniFileName := FIniFileName;
        SslMultiWebDataModule.OnDisplay   := SslHttpAppSrv1Display;
        SslMultiWebDataModule.DataDir     := FDataDir;
        SslMultiWebDataModule.ImagesDir   := SslHttpAppSrv1.DocDir + '\Images';
        SslMultiWebDataModule.LoadConfig;

    { V8.69 sharing variables with UrlHandlers indirectly }
        SslMultiWebDataModule.UploadDir   := FUploadsDir;
        SslHttpAppSrv1.UploadDir := FUploadsDir;           { V9.1 }
        SslMultiWebDataModule.IcsMailQueue := IcsMailQueue;
        if (IcsMailQueue.MailQuDir <> '') then
            ForceDirectories (IcsMailQueue.MailQuDir);

      // note that AllowedPaths and Handlers must match a HostTag for each Host in INI file

      // simple web server, no handlers, allow access to all static files
        SslHttpAppSrv1.AddGetAllowedPath('/', afBeginBy, 'HTTP-WEB');

      // application web server, only allow access to folders where static documents are.
        SslHttpAppSrv1.AddGetAllowedPath('/',        afBeginBy, 'WEB-APP');
        SslHttpAppSrv1.AddGetAllowedPath('/js/',     afBeginBy, 'WEB-APP');
        SslHttpAppSrv1.AddGetAllowedPath('/styles/', afBeginBy, 'WEB-APP');
        SslHttpAppSrv1.AddGetAllowedPath('/images/', afBeginBy, 'WEB-APP');

      // Add all dynamic webpage handlers
//        SslHttpAppSrv1.AddGetHandler('/', TUrlHandlerDefaultDoc, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler('/', TUrlHandlerIndexHtml, hgWillSendMySelf, 'WEB-APP');   { V9.3 }
        SslHttpAppSrv1.AddGetHandler('/index.html', TUrlHandlerIndexHtml, hgWillSendMySelf, 'WEB-APP');  { V9.3 }
        SslHttpAppSrv1.AddGetHandler('/appindex.html', TUrlHandlerDefaultDoc, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlLogin, TUrlHandlerLoginFormHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlDoLoginSecure, TUrlHandlerDoLoginSecureHtml, hgWillSendMySelf, 'WEB-APP');    // !! handler causes crash on exit?
        SslHttpAppSrv1.AddGetHandler(UrlCounter, TUrlHandlerCounterJpg, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlHomePage, TUrlHandlerHomePageHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlConfigForm, TUrlHandlerConfigFormHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlConfigLogoPng, TUrlHandlerConfigLogoPng, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlDoConfigConfirmSaveHtml, TUrlHandlerDoConfigConfirmSaveHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddPostHandler(UrlDoConfigHtml, TUrlHandlerDoConfigHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlCounterViewHtml, TUrlHandlerCounterViewHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlAjaxFetchCounter, TUrlHandlerAjaxFetchCounter, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlJavascriptErrorHtml, TUrlHandlerJavascriptErrorHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler(UrlHeadForm, TUrlHandlerHead, hgWillSendMySelf, 'WEB-APP');

      // Just for demoing the simplest handler, let's add an "Helloworld" one
        SslHttpAppSrv1.AddGetHandler('/HelloWorld.html', TUrlHandlerHelloWorld, hgWillSendMySelf, 'WEB-APP');

      // these handlers replace the POST code in OverbyteIcsWebServ1.pas that does not work for HttpAppSrv
        SslHttpAppSrv1.AddPostHandler('/cgi-bin/FormHandler', TUrlHandlerUploadData, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler('/mailer.html', TUrlHandlerMailer, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddPostHandler('/mailer.html', TUrlHandlerMailer, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler('/uploadfile.html', TUrlHandlerUploadFile, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddPostHandler('/uploadfile.html', TUrlHandlerUploadFile, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddGetHandler('/websocketclient.html', TUrlHandlerWSClientEcho, hgWillSendMySelf, 'WEB-APP');  { V8.71 }
        SslHttpAppSrv1.AddGetHandler('/postinfo.html', TUrlHandlerPostInfo, hgWillSendMySelf, 'WEB-APP');             { V9.1 }
        SslHttpAppSrv1.AddPostHandler('/postinfo.html', TUrlHandlerPostInfo, hgWillSendMySelf, 'WEB-APP');            { V9.1 }
        SslHttpAppSrv1.AddGetHandler('/DemoAuthAll.html', TUrlHandlerDemoAuthAll, hgWillSendMySelf, 'WEB-APP');       { V9.2 }
        SslHttpAppSrv1.AddPostHandler('/DemoAuthAll.html', TUrlHandlerDemoAuthAll, hgWillSendMySelf, 'WEB-APP');      { V9.2 }

     // REST API - note currently only GET verb supported
        SslHttpAppSrv1.AddGetHandler('/restapi.html', TUrlHandlerRestApi, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddPostHandler('/restapi.html', TUrlHandlerRestApi, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddDeleteHandler('/restapi.html', TUrlHandlerRestApi, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv1.AddPutHandler('/restapi.html', TUrlHandlerRestApi, hgWillSendMySelf, 'WEB-APP');

      // look for old sessions
        if FileExists(FSessionFile) then begin
            try
                SslHttpAppSrv1.WSessions.LoadFromFile(FSessionFile);
                Display(IntToStr(SslHttpAppSrv1.SessionsCount) + ' sessions loaded');
            except
                // Ignore any exception, but clear anything partially loaded
                SslHttpAppSrv1.WSessions.Clear;
                // and delete existing (corrupted) file
                DeleteFile(FSessionFile);
                Display('Unable to load existing sessions');
            end;
        end;

       // V8.60 load hackers filters and blacklists
        LoadHackLists;
        if NOT Assigned (AttemptsBlacklist) then
            AttemptsBlacklist := TIcsBlackList.Create(self);
        AttemptsBlacklist.SaveAscii := true ;
        AttemptsBlacklist.ListName := 'Attempts' ;
        AttemptsBlacklist.BlackLogEvent := OnBlackLogEvent;
        AttemptsBlacklist.BlockAfterMins := 1440;
        AttemptsBlacklist.BlockAttempts := 1000;
        AttemptsBlacklist.BlockForMins := 720;
        AttemptsBlacklist.BlackFile := FDataDir + '\AttemptsBlacklist.lst' ;
        AttemptsBlacklist.WhiteFile := FDataDir + '\Attemptswhitelist.lst' ;
        if NOT Assigned (HackBlackList) then
            HackBlackList := TIcsBlackList.Create(self);
        HackBlackList.SaveAscii := true ;  // needed to save IPv6 addresses
        HackBlackList.ListName := 'Hackers' ;
        HackBlackList.BlackLogEvent := OnBlackLogEvent;
        HackBlackList.BlockAfterMins := 1;
        HackBlackList.BlockAttempts := 1;
        HackBlackList.BlockForMins := 1440;
        HackBlackList.BlackFile := FDataDir + '\HackBlacklist.lst' ;
        HackBlackList.WhiteFile := FDataDir + '\HackWhitelist.lst' ;

      // Cleanup temporary files left from a previous run
//        CleanupTimeStampedDir(SslMultiWebDataModule.DataDir);

      // start logging file for each host
        for J := 0 to SslHttpAppSrv1.IcsHosts.Count - 1 do begin
            with SslHttpAppSrv1.IcsHosts [J] do begin
                WebLogIdx := -1;
                if NOT HostEnabled then continue;
                Display('W3C logging for host: ' + HostTag + ' to ' + FormatDateTime(WebLogDir, Date)) ;
              { V8.60 create log file buffer, may be shared between hosts }
                if TotWebLogs > 0 then begin
                    for K := 0 to TotWebLogs - 1 do begin
                        if J = K then Continue;
                        if WebLogDir <> SslHttpAppSrv1.IcsHosts [K].WebLogDir then Continue;
                        WebLogIdx := K;
                        Break;
                    end;
                end;
                if (WebLogIdx < 0) then begin
                    TotWebLogs := TotWebLogs + 1;
                    WebLogIdx := TotWebLogs;
                    WebLogBuffers [WebLogIdx] :=
                        TIcsBuffLogStream.Create(Self, WebLogDir, WebLogHdrMask, FileCPUtf8);
                    WebLogBuffers [WebLogIdx].WriteLine(WebLogFields);
                end;
            end;
        end;

      // start as many hosts as possible
        Errs := SslHttpAppSrv1.Start (True) ;
        if Errs <> '' then Display('Start Web Server Error - ' + Errs) ;
        if NOT SslHttpAppSrv1.ListenAllOK then
            S := 'Failed to Start, '
        else
           S := 'Started OK, ';
        S := S + 'Listen Bindings:' + IcsCRLF + SslHttpAppSrv1.ListenStates;
        Display(S);
        CertCheckTrigger := IcsGetTrgSecs64 (15) ;  { V8.57 first check is early to order new certificates }
        StartButton.Enabled := false;
        StopButton.Enabled := true;
        if (AdminEmailTo <> '') then { v8.60 tell someone }
            SendAdminEmail (AdminEmailTo, 'ICS DD Web Service Started',
              'ICS Multi Web Server' + IcsCRLF + IcsCRLF + S + IcsCRLF + Errs);

     // try and show URLSs that will access the first host
        Display('Now browse to one of these URLs:' + icsCRLF + BuildDemoURIs);
        DiagLogBuffer.FlushFile(True);
    except
        on E:Exception do begin
           Display('Failed to start web server - ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.StopButtonClick(Sender: TObject);
var
    K: Integer;
    S: string;
begin
 // V9.3 Application Monitor component, nothing happens if never started or stopped already
    IcsAppMonCli.Stop(AppTitle + 'Monitoring Stopping') ;

    if NOT StopButton.Enabled then Exit;
    IcsSslX509Certs.CloseAccount;
    CertCheckTrigger := Trigger64Disabled;
    StartButton.Enabled := true;
    StopButton.Enabled := false;
    DiagLogBuffer.FlushFile(True);

  // close websocket clients
    if SslHttpAppSrv1.ClientCount > 0 then begin
        Display ('Closing Websockets');
        for K := SslHttpAppSrv1.ClientCount - 1 downto 0 do begin
            with (SslHttpAppSrv1.Client[K] as THttpWSSrvConn) do begin
                if (State = wsConnected) and WSClient and (NOT WSClosing) then
                    WSClose(wscrNormalClosure, 'Server Closing');
            end;
        end;
    end;

    SslHttpAppSrv1.Stop;

{$IFDEF DATABASE}
   if DbOpened then begin
        try
{$IFDEF YuDISQL}
            DISQLite3Database.Close;
            sqlite3_shutdown;
{$ENDIF}
        except
        end;
        DbOpened := false;
    end;
{$ENDIF}

  { report Blacklisted remotes }
    S := '' ;
    if Assigned (AttemptsBlackList) then
        S := S + AttemptsBlackList.ListName + ' Report:' + IcsCRLF +
                         AttemptsBlackList.ReportBlackList (true) + IcsCRLF ;
    if Assigned (HackBlackList) then
        S := S + HackBlackList.ListName + ' Report:' + IcsCRLF +
                         HackBlackList.ReportBlackList (true) + IcsCRLF ;
    Display(S);

  { save blacklists to files }
    if Assigned (HackBlackList) then HackBlackList.FlushToFiles;
    if Assigned (AttemptsBlackList) then AttemptsBlackList.FlushToFiles;

  { close web log files }
    if TotWebLogs > 0 then begin
        for K := 0 to TotWebLogs - 1 do begin
            FreeAndNil(WebLogBuffers[K]);
        end;
    end;
    if (AdminEmailTo <> '') then begin { tell someone }
        IcsMailQueue.QuStartDelay := 0; // V9.3 no delay sending last email
        SendAdminEmail (AdminEmailTo, 'ICS DD Web Service Stopped', 'ICS DD Web Service Stopped' + IcsCRLF);
    end;
    Display('Server stopped');
    DiagLogBuffer.FlushFile(True);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.OnBlackLogEvent (const info: string);
begin
    Display(info);
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ write W3C log, similar to Microsoft IIS, log line looks something like this:
12:34:48 2009-07-03 12:34:48 WebAppDemo PC19 0.0.0.0 GET /login/loginform.html - 20105 - 192.168.1.119 HTTP/1.1 Mozilla/5.0+(Windows;+U;+Windows+NT+5.1;+en-GB;+rv:1.9.0.11)+Gecko/2009060215+Firefox/3.0.11+(.NET+CLR+3.5.30729) http://pc19:20105/ pc19:20105 200 2101 1138 31 }
procedure TDDWebServiceSrv.SslHttpAppSrv1AfterAnswer(Sender, Client: TObject);
var
    RemoteClient: TMyHttpConnection;
    newparams, newuser, info, host: string ;
    duration: longword ;
    curread, curwrite: int64 ;

    function SpaceToPlus (const S: String): String ;
    begin
        result := StringReplace (S, #32, '+', [rfReplaceAll]) ;
    end;

begin
    RemoteClient := TMyHttpConnection(Client) ;
    if RemoteClient.WebLogIdx < 0 then Exit; // no logging
    info := FormatDateTime (ISODateMask + #32 + ISOTimeMask, Now) ;
    with RemoteClient do
    begin
        try
            host := CPeerHostName ;     { V8.71 use remote host name instead of IP if we have it }
            if host = '' then
            newparams := Params ;
            if newparams = '' then newparams := '-' ;
            newuser := SpaceToPlus (AuthUserName)  ;
            if newuser = '' then newuser := '-' ;
            curread := ReadCount - CLastRead ;
            curwrite := WriteCount - CLastWrite ;
            duration := IcsElapsedMSecs64 (CStartTick) ;      { V8.71 }
// #Fields: date time s-sitename s-computername s-ip cs-method
            info := info + #32 + HostTag + #32 + SrvCompName + #32 + CServerAddr + #32 + Method + #32 +     { V8.69 was Server.Addr }
// #Fields: cs-uri-stem cs-uri-query s-port
                    SpaceToPlus (Path) + #32 + SpaceToPlus (newparams) + #32 + CServerPort + #32 +          { V8.69 was Server.Addr }
//  #Fields: cs-username c-ip cs-version
                    newuser + #32 + host + #32 + Version + #32 +
// #Fields: cs(User-Agent) cs(Referer)
                    SpaceToPlus (RequestUserAgent) + #32 + SpaceToPlus (RequestReferer) + #32 +
// #Fields: cs-host sc-status
                    RequestHost + #32 + IntToStr (AnswerStatus) + #32 +         { V8.69 was FAnswerStatus }
// #Fields: sc-bytes cs-bytes time-taken
                    IntToStr (curwrite) + #32 + IntToStr (curread) + #32 + IntToStr (duration);
            if Assigned(WebLogBuffers[RemoteClient.WebLogIdx]) then                          { V8.70 }
                WebLogBuffers[RemoteClient.WebLogIdx].WriteLine(info) // note separate file name for each host
            else
                Display ('No Web Log Assigned') ;                                              { V8.71 better wording }
        except
            Display ('AfterAnswer Error - ' + info + ' - ' + IcsGetExceptMess(ExceptObject)) ;  { V8.70 }
        end;
        Display ('Answer Log - ' + info) ;
    end;
    RemoteClient.CLastRead := RemoteClient.ReadCount ;   // reset read ready for next request
    DiagLogBuffer.FlushFile(True);  // !!! TEMP
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1AuthGetPassword(Sender, Client: TObject;
  var Password: string);
var
    ClientCnx  : TMyHttpConnection;
begin
    { It's easyer to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);
    Display('AuthGetPassword for "' + ClientCnx.Path + '" AuthType is "' +
            AuthTypesToString(ClientCnx.AuthTypes) + '"');
    if (ClientCnx.AuthTypes = [atNtlm]) then Exit;
    if (ClientCnx.AuthUserName = 'test') then
        Password := 'password';  { V8.69 password is independent of authentication }

(*    if (ClientCnx.AuthTypes = [atDigest]) and (ClientCnx.AuthUserName = 'testmd5') then
        Password := 'digestmd5'
    else if (ClientCnx.AuthTypes = [atDigestSha2]) and (ClientCnx.AuthUserName = 'testsha2') then   { V8.69 }
        Password := 'digestsha2'
    else if (ClientCnx.AuthTypes = [atBasic]) and (ClientCnx.AuthUserName = 'testbasic') then
        Password := 'basic'
    else if (ClientCnx.AuthTypes = [atNtlm]) then ;
        //  nothing to do windows will validate credentials
 *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1AuthGetType(Sender, Client: TObject);
var
    ClientCnx  : TMyHttpConnection;
begin
    { It's easyer to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);
    ClientCnx.AuthTypes  := [];    { V9.2 }
    if CompareText(ClientCnx.Path, '/DemoBasicAuth.html') = 0 then begin
        ClientCnx.AuthTypes  := [atBasic];
        ClientCnx.AuthRealm := 'DemoBasicAuth';
    end
    else if CompareText(ClientCnx.Path, '/DemoDigestAuth.html') = 0 then begin
        ClientCnx.AuthTypes  := [atDigest];
        ClientCnx.AuthRealm := 'DemoDigestAuth';
    end
    else if CompareText(ClientCnx.Path, '/DemoDigest2Auth.html') = 0 then begin       { V8.69 }
        ClientCnx.AuthTypes  := [atDigestSha2];
        ClientCnx.AuthRealm := 'DemoDigest2Auth';
    end
    else if CompareText(ClientCnx.Path, '/DemoDigestsAll.html') = 0 then begin
        ClientCnx.AuthTypes  := [atDigest, atDigestSha2];                             { V8.69 }
        ClientCnx.AuthRealm := 'DemoDigestsAll';
    end
    else if CompareText(ClientCnx.Path, '/DemoNtlmAuth.html') = 0 then begin
        ClientCnx.AuthTypes  := [atNtlm];
        ClientCnx.AuthRealm := 'DemoNtlmAuth';
    end
    else if CompareText(ClientCnx.Path, '/DemoAuthAll.html') = 0 then begin
        ClientCnx.AuthTypes  := [atBasic, atDigest, atDigestSha2];               { V8.69 no NTLM, added Sha2 }
        ClientCnx.AuthRealm := 'DemoAuthAll';
    end;
    if ClientCnx.AuthTypes <> [] then                                                        { V8.69 tell user }
        Display('Request requires authentication "' + ClientCnx.Path + '" AuthType is "' +
            AuthTypesToString(ClientCnx.AuthTypes) + '"' + ', ReqAuth: ' + ClientCnx.RequestAuth);  { V9.2 added Auth header }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1AuthNtlmBeforeValidate(Sender, Client: TObject; var Allow: Boolean);
var
    ClientCnx  : TMyHttpConnection;
begin
    ClientCnx := TMyHttpConnection(Client);
    Allow := (ClientCnx.AuthNtlmSession.Username <> '') and
             (ClientCnx.AuthNtlmSession.Domain <> 'SomeDomain');

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1AuthResult(Sender, Client: TObject; Success: Boolean);
var
    ClientCnx  : TMyHttpConnection;
const
    SuccessStr : array [Boolean] of String = ('failed', 'OK');
begin
    { It's easier to do the cast one time. Could use with clause...         }
    ClientCnx := TMyHttpConnection(Client);

    { If we always want to pop up client browser's login dialog with digest }
    { authentication when the nonce is stale we may set FAuthDigestStale    }
    { back to FALSE.  Note: Do not set this value to TRUE.                  }
    { A nonce is considered stale after AuthDigestNonceLifeTimeMin expired. }
    { Uncomment next three lines to see what changes.                       }
    {if (not Success) and (ClientCnx.AuthTypes = [atDigest]) and
       ClientCnx.FAuthDigestStale then
        ClientCnx.FAuthDigestStale := FALSE;}

    Display('Authentication result ' + SuccessStr[Success] + ' with type ' + HttpAuthTypeNames[ClientCnx.AuthGetMethod] +
                                                          ', Username: ' + ClientCnx.AuthUserName + ' for ' + ClientCnx.Path); { V9.2 cleaner }

    if (not Success) and (ClientCnx.AuthTypes = [atNtlm]) and
       (ClientCnx.AuthNtlmSession <> nil) then
        Display(ClientCnx.AuthNtlmSession.AuthErrorDesc);  // just for debugging!

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1BeforeProcessRequest(Sender,
  Client: TObject);
var
    RemoteClient: TMyHttpConnection;
    I: integer ;
    CProtocol, S: String;
begin
    RemoteClient := TMyHttpConnection(Client) ;
    CProtocol := RemoteClient.RequestProtocol + '://';
    RemoteClient.CStartTick := IcsGetTickCount64;    { V8.71 }
    RemoteClient.CLastWrite := RemoteClient.WriteCount ;

{ V9.2 log request and heders before authentication and specific methods are processed }
    S := '[' + FormatDateTime('HH:NN:SS', Now) + ' ' + RemoteClient.PeerAddr + '] ' + RemoteClient.HostTag + ' ' +
                                                                              RemoteClient.Method + ' ' + RemoteClient.Path;
    if RemoteClient.Params <> '' then
        S := S + '?' + RemoteClient.Params;
    Display(S);
    DisplayHeader(RemoteClient);

  // check for absolute URL, strip off protocol and host
    if Pos (CProtocol, RemoteClient.Path) = 1 then begin
        for I := (Length (CProtocol) + 1) to Length (RemoteClient.Path) do begin
            if RemoteClient.Path [I] = '/' then begin
                Display('Found and Trimmed Absolute URL: ' + RemoteClient.Path);
                RemoteClient.Path := Copy (RemoteClient.Path, I, 999) ;
                Break ;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1BgException(Sender: TObject; E: Exception; var CanClose: Boolean);
begin
    Display('Exception processing page - ' +  E.ClassName + ': ' + E.Message);
    CanClose := TRUE;

 { V9.3 if Application Monitor is running, ask it to restart this program after error }
    if IcsAppMonCli.CliState = CliStateOK then begin
        Display('Requesting Application Monitor Restarts This Program');
        IcsAppMonCli.RestartNow('Exception processing page - ' +  E.ClassName + ': ' + E.Message);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1ClientConnect(Sender, Client: TObject; Error: Word);
var
    ClientCnx : TMyHttpConnection;
begin
    ClientCnx := Client as TMyHttpConnection;
    ClientCnx.WSessionCookie := 'OverbyteIcsWebAppServer' + SslHttpAppSrv1.Port;
    ClientCnx.OnBgException  := HttpAppSrvClientBgException;
    ClientCnx.OnClientAlpnChallg := IcsSslX509Certs.WebSrvAlpn; { V8.64 }

 { V8.71 reverse DNS for remote host using new DNS cache component }
 { may find CPeerAddr in cache, if not returns CPeerAddr and starts a DNS lookup calling event when done }
    ClientCnx.CPeerHostName := IcsDomainNameCache1.LookupIPOne(ClientCnx.CPeerAddr, 1, sfAny, IcsDomainNameCache1DNUpdateEvent);

 { V8.64 log something at start }
    Display(FormatDateTime('HH:NN:SS', Now) + ' New Remote Client: ' + ClientCnx.CPeerAddr);  { V8.71 added time }

{ V8.71 check blacklist before starting SSL negotiation }
    if TestFilters ('remaddr', ClientCnx.CPeerAddr) or
           TestFilters ('remhost', ClientCnx.CPeerHostName) or
              HackBlackList.CheckBlackList (ClientCnx.CPeerAddr) then begin
        if NOT TestIpWhiteList (ClientCnx.CPeerAddr) then
        begin
            HackBlackList.AddBlackList (ClientCnx.CPeerAddr, OneMask, 9999) ;
            Display('Hacker Aleady Blacklisted, Connection Refused: ' + ClientCnx.CPeerAddr) ;
            TWSocketServer ((Sender as THttpServer).WSocketServer).Disconnect(ClientCnx);
        end;
    end;

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1ConnectDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
begin
//
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1DeleteDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
var
    ClientCnx  : TMyHttpConnection;
    Dummy     : THttpGetFlag;
begin
    if Flags = hg401 then
    { Not authenticated (yet), we might be still in an authentication       }
    { session with ClientCnx.RequestContentLength = 0 which was valid,      }
    { i.e. NTLM uses a challenge/response method, anyway just exit.         }
        Exit;

    { It's easier to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);

    { Count request and display a message }
//    InterlockedIncrement(FCountRequests);
//    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
//            ClientCnx.GetPeerAddr + '] ' + IntToStr(FCountRequests) +
//            ': ' + ClientCnx.Version + ' DELETE ' + ClientCnx.Path);
//    DisplayHeader(ClientCnx);

    { path may specify a file name or just a name, something we want to delete    }

    { tell user }
    Flags := hgWillSendMySelf;
    ClientCnx.AnswerString(Dummy,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS DD Web Service DELETE Demo</TITLE>' +
          '</HEAD>' + icsCRLF +
          '<BODY>' +
            '<H2>Your DELETE request has been noted:</H2>' + icsCRLF +
            '<P>Command: ' + ClientCnx.Path + '</P>' + icsCRLF +
            '<A HREF="/demo.html">Back to demo menu</A><BR>' +
          '</BODY>' +
        '</HTML>');

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1DeleteSession(Sender: TObject; Session: TWebSession);
var
    MySessionData : TAppSrvSessionData;
begin
    MySessionData := Session.SessionData as TAppSrvSessionData;
    Display('Session for user "' + MySessionData.UserCode + '" timed out');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1Display(Sender: TObject; const Msg: string);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1GetDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
var
    ClientCnx : TMyHttpConnection;
    Path: String;
    AbandonFlag: Boolean;
    ImageTB: TBytes;   { V9.2 }
begin
    ClientCnx := Client as TMyHttpConnection;
    Path := Lowercase (ClientCnx.Path) ;

  { V8.60 detect too many attempts to access web site }
    AbandonFlag := False;
    if AttemptsBlackList.FailedBlackList (ClientCnx.CPeerAddr, 9999) then begin
        AbandonFlag := True;
        Display('Maximum Attempts Exceeded, Blacklisted: ' + ClientCnx.CPeerAddr);
    end;

  { V8.60 detect PHP hacking attempts and illegal URLs using URL filters }
    if TestFilters ('path', Path) or TestFilters ('remaddr', ClientCnx.CPeerAddr) or
         TestFilters ('useragent', ClientCnx.RequestUserAgent) or     { V8.70 }
               TestFilters ('remhost', ClientCnx.CPeerHostName) or
                { V8.71 next line stops http://1.2.3.4/ for commercial servers, but might need IPs for testing }
               //  TestDottedIp (ClientCnx.RequestHost) or  { V8.71 check IP address instead of host name }
                   TestDottedIp (ClientCnx.SslServerName) or   { V8.71 check SSL Server Name as dotted IP address }
                      HackBlackList.CheckBlackList (ClientCnx.CPeerAddr) then begin
        AbandonFlag := True;
        Display('Hacker Blacklisted: ' + ClientCnx.CPeerAddr + ' - ' + path) ;
        HackBlackList.AddBlackList(ClientCnx.CPeerAddr, OneMask, 9999) ;
    end;

  { V8.60 check white list so we don't lock ourself out of site, then send
     a delayed reponse after 60 seconds }
    if AbandonFlag and (NOT TestIpWhiteList (ClientCnx.CPeerAddr)) then begin
        ClientCnx.KeepAlive := false ;
        Flags := hgWillSendMySelf;
        ClientCnx.AuthUserName := '';
        ClientCnx.AuthPassword := '' ;
        ClientCnx.PostedDataLen := 0 ;
        ClientCnx.Path := '';

     { V8.71 redirect back themselves instead of delayed response }
        Flags := hgWillSendMySelf;
        path := ClientCnx.CPeerAddr;
        if Pos(':', Path) > 1 then
            path := '[' + path + ']';
        path := 'http://' + path + '/';
        Display ('Hacker Redirected to: ' + path);
        ClientCnx.AnswerRedirect(301, path);  { V9.2 }
        exit ;
    end;

 { if starting authentication, don't process any pages yet }
    if Flags = hg401 then
        Exit;

  { some hosts are purely for redirection }
    if ClientCnx.WebRedirectStat <> 0 then begin
        Flags := hgWillSendMySelf;
        ClientCnx.AnswerRedirect(ClientCnx.WebRedirectStat, ClientCnx.WebRedirectURL);  { V9.2 }
        Exit;
    end;

 { webapp host is the one we handle here, mostly }
    if ClientCnx.HostTag = 'WEB-APP' then begin

      // note many special pages are processed by AddGetHandlers set earlier

        { Instead of the long if/then/else below, we could use a lookup table  }
        if (CompareText(ClientCnx.Path, '/demo.html') = 0 ) then   // V9.3 no longer our default document
            CreateVirtualDocument_Demo(Sender, ClientCnx, Flags)
        else if CompareText(ClientCnx.Path, '/demoBasicAuth.html') = 0 then
            CreateVirtualDocument_DemoBasicAuth(Sender, ClientCnx, Flags)
        else if CompareText(ClientCnx.Path, '/demoDigestAuth.html') = 0 then
            CreateVirtualDocument_DemoDigestAuth(Sender, ClientCnx, Flags)
        else if CompareText(ClientCnx.Path, '/demoDigest2Auth.html') = 0 then      { V8.69 }
            CreateVirtualDocument_DemoDigest2Auth(Sender, ClientCnx, Flags)
        else if CompareText(ClientCnx.Path, '/demoDigestsAll.html') = 0 then       { V8.69 }
            CreateVirtualDocument_DemoDigestsAll(Sender, ClientCnx, Flags)
        else if CompareText(ClientCnx.Path, '/demoNtlmAuth.html') = 0 then
            CreateVirtualDocument_DemoNtlmAuth(Sender, ClientCnx, Flags)
//        else if CompareText(ClientCnx.Path, '/demoAuthAll.html') = 0 then
//            CreateVirtualDocument_DemoAuthAll(Sender, ClientCnx, Flags)        V9.2 now a template
        { Trap '/time.html' path to dynamically generate a dynamic answer. }
        else if CompareText(ClientCnx.Path, '/time.html') = 0 then
            CreateVirtualDocument_Time(Sender, ClientCnx, Flags)
        else if CompareText(ClientCnx.Path, '/bruno.html') = 0 then
            CreateVirtualDocument_Bruno(Sender, ClientCnx, Flags)
        { Trap '/myip.html' path to dynamically generate a dynamic answer. }
        else if CompareText(ClientCnx.Path, '/myip.html') = 0 then
            CreateVirtualDocument_MyIP(Sender, ClientCnx, Flags)
        { Trap '/HeaderBug.html' path to dynamically generate a dynamic answer. }
        else if CompareText(ClientCnx.Path, '/HeaderBug.html') = 0 then
            CreateVirtualDocument_HeaderBug(Sender, ClientCnx, Flags)
        { Trap '/redir.html' to dynamically generate a redirection answer }
        else if CompareText(ClientCnx.Path, '/redir.html') = 0 then
            CreateVirtualDocument_Redir(Sender, ClientCnx, Flags)
        else if CompareText(ClientCnx.Path, '/template.html') = 0 then
            CreateVirtualDocument_template(Sender, ClientCnx, Flags)
        else if CompareText(ClientCnx.Path, '/favicon.ico') = 0 then begin    { V9.2 }
            ImageTB := IcsResourceGetTB('ICSICON16', RT_RCDATA);
            if Length(ImageTB) > 0 then
                ClientCnx.AnswerBodyTB(Flags, '200 OK', 'image/icon', '', ImageTB)
            else
                ClientCnx.Answer404;
        end;
    end;
    DiagLogBuffer.FlushFile(True);  // !!! TEMP
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1HeadDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
var
    ClientCnx : TMyHttpConnection;
    S: String;
begin
    ClientCnx := Client as TMyHttpConnection;
    S := ClientCnx.PeerAddr + ' - ' + ClientCnx.HostTag + ' ' + ClientCnx.Path;
    if ClientCnx.Params = '' then
        Display(S)
    else
        Display(S + '?' + ClientCnx.Params);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1HttpMimeContentType(Sender, Client: TObject; const FileName: string;
  var ContentType: string);
var
    ClientCnx  : TMyHttpConnection;
begin
    ClientCnx := TMyHttpConnection(Client);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            ClientCnx.GetPeerAddr + '] ' +
            ': Document: ' + FileName + ', Content-Type: ' + ContentType);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1HttpRequestDone(Sender, Client: TObject);     { V9.2 }
var
    ClientCnx  : TMyHttpConnection;
begin
    ClientCnx := TMyHttpConnection(Client);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' + ClientCnx.GetPeerAddr + '] Request Completed, Status ' +
                                                                               IntToStr(ClientCnx.AnswerStatus) + IcsCRLF);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1HttpRespHdr(Sender, Client: TObject; const Header: string);      { V8.69 }
var
    ClientCnx  : TMyHttpConnection;
begin
    if not DisplayHeaderCheckBox.Checked then
        Exit;
    ClientCnx := TMyHttpConnection(Client);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
            ClientCnx.GetPeerAddr + '] Response Headers');
    Display(Header);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1OptionsDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
//var
//    ClientCnx  : TMyHttpConnection;
begin
    { It's easier to do the cast one time. Could use with clause... }
//    ClientCnx := TMyHttpConnection(Client);

    { Count request and display a message }
//    InterlockedIncrement(FCountRequests);
//    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
//            ClientCnx.GetPeerAddr + '] ' + IntToStr(FCountRequests) +
//            ': ' + ClientCnx.Version + ' OPTIONS ' + ClientCnx.Path);
//    DisplayHeader(ClientCnx);
    Exit;  { let web server send general response }

    { path may be the resource for which OPTIONS should be checked }

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1PatchDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
var
    ClientCnx  : TMyHttpConnection;
begin
    if Flags = hg401 then
    { Not authenticated (yet), we might be still in an authentication       }
    { session with ClientCnx.RequestContentLength = 0 which was valid,      }
    { i.e. NTLM uses a challenge/response method, anyway just exit.         }
        Exit;

    { It's easier to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);

    { Count request and display a message }
//    InterlockedIncrement(FCountRequests);
//    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
//            ClientCnx.GetPeerAddr + '] ' + IntToStr(FCountRequests) +
//            ': ' + ClientCnx.Version + ' PATCH ' + ClientCnx.Path);
//    DisplayHeader(ClientCnx);

    if (ClientCnx.RequestContentLength > (SslHttpAppSrv1.MaxUploadMB * IcsMBYTE)) or   { V9.1 }
       (ClientCnx.RequestContentLength <= 0) then begin
        if (ClientCnx.RequestContentLength > (SslHttpAppSrv1.MaxUploadMB * IcsMBYTE)) then
            Display('Upload size exceeded limit (' + IntToKByte((SslHttpAppSrv1.MaxUploadMB * IcsMBYTE)) + ')');
        Flags := hg403;
        Exit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is triggered when HTTP server component receive a POST }
{ command from any client.                                                  }
{ We count the request, display a message and trap posted data.             }
{ To check for posted data, you may construct the following HTML document:  }
{ <HTML>                                                                    }
{   <HEAD>                                                                  }
{     <TITLE>Test Form 1</TITLE>                                            }
{   </HEAD>                                                                 }
{   <BODY>                                                                  }
{     <H2>Enter your first and last name</H2>                               }
{     <FORM METHOD="POST" ACTION="/cgi-bin/FormHandler">                    }
{       <TABLE BORDER="0" ALIGN="DEFAULT" WIDTH="100%">                     }
{         <TR>                                                              }
{           <TD>First name</TD>                                             }
{           <TD><INPUT TYPE="TEXT" NAME="FirstName"                         }
{                      MAXLENGTH="25" VALUE="YourFirstName"></TD>           }
{         </TR>                                                             }
{         <TR>                                                              }
{           <TD>Last name</TD>                                              }
{           <TD><INPUT TYPE="TEXT" NAME="LastName"                          }
{                      MAXLENGTH="25" VALUE="YourLastName"></TD>            }
{         </TR>                                                             }
{       </TABLE>                                                            }
{       <P><INPUT TYPE="SUBMIT" NAME="Submit" VALUE="Button"></P>           }
{     </FORM>                                                               }
{   </BODY>                                                                 }
{ </HTML>                                                                   }


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1PostDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
var
    ClientCnx  : TMyHttpConnection;
begin
    if Flags = hg401 then
    { Not authenticated (yet), we might be still in an authentication       }
    { session with ClientCnx.RequestContentLength = 0 which was valid,      }
    { i.e. NTLM uses a challenge/response method, anyway just exit.         }
        Exit;

    { It's easier to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);

//    { Count request and display a message }
//    InterlockedIncrement(FCountRequests);
//    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +  ClientCnx.GetPeerAddr + '] ' + ClientCnx.HostTag + ' POST ' + ClientCnx.Path);
//    DisplayHeader(ClientCnx);

 { V8.69 webapp host is the one we handle here, V9.2 demoAuthAll.html now handled as template }
    if ClientCnx.HostTag = 'WEB-APP' then begin
        if CompareText(ClientCnx.Path, '/DemoBasicAuth.html') = 0 then begin  // handled as virtual document
            CreateVirtualDocument_DemoBasicAuth(Sender, ClientCnx, Flags);
            Exit;
        end;
    end;

  { sanity check upload size }
    if (ClientCnx.RequestContentLength > (SslHttpAppSrv1.MaxUploadMB * IcsMBYTE))  then begin
        Display('Upload size exceeded limit (' + IntToKByte((SslHttpAppSrv1.MaxUploadMB * IcsMBYTE)) + ')');
        Flags := hg403;
        Exit;
    end;
 {  if (ClientCnx.RequestContentLength <= 0) then begin       // V9.2 do we care?
        Display('No POST content');
        Flags := hg404;
        Exit;
    end;   }

{  NOTE - HttpAppSrv overrides POST data processing for which
       a UrlHandler must be provided. V8.69 incorrect, event is called! }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V9.2 this is where we'd accept posted data for virtual pages, but this sample only POSTs to template pages }
procedure TDDWebServiceSrv.SslHttpAppSrv1PostedData(Sender, Client: TObject; Error: Word);
var
    ClientCnx  : TMyHttpConnection;
begin
    ClientCnx := TMyHttpConnection(Client);
    ClientCnx.PostedDataReceived;   // got all the data we expected, ie none
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1PutDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
var
    ClientCnx  : TMyHttpConnection;
begin
    if Flags = hg401 then
    { Not authenticated (yet), we might be still in an authentication       }
    { session with ClientCnx.RequestContentLength = 0 which was valid,      }
    { i.e. NTLM uses a challenge/response method, anyway just exit.         }
        Exit;

    { It's easier to do the cast one time. Could use with clause... }
    ClientCnx := TMyHttpConnection(Client);

    { Count request and display a message }
//    InterlockedIncrement(FCountRequests);
//    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
//            ClientCnx.GetPeerAddr + '] ' + IntToStr(FCountRequests) +
//            ': ' + ClientCnx.Version + ' PUT ' + ClientCnx.Path);
//    DisplayHeader(ClientCnx);

    if (ClientCnx.RequestContentLength > (SslHttpAppSrv1.MaxUploadMB * IcsMBYTE)) or   { V9.1 }
       (ClientCnx.RequestContentLength <= 0) then begin
        if (ClientCnx.RequestContentLength > (SslHttpAppSrv1.MaxUploadMB * IcsMBYTE)) then
            Display('Upload size exceeded limit (' + IntToKByte((SslHttpAppSrv1.MaxUploadMB * IcsMBYTE)) + ')');
        Flags := hg403;
        Exit;
    end;

    { URL may specify a file name or just a name, worry about it later    }
 (*
    { Tell HTTP server that we will accept posted data                 }
    { OnPostedData event will be triggered when data comes in          }
    Flags := hgAcceptData;
    { We wants to receive any data type. So we turn line mode off on   }
    { client connection.                                               }
    ClientCnx.LineMode := FALSE;
    { We need a buffer to hold posted data. We allocate as much as the }
    { size of posted data plus one byte for terminating nul char.      }
    { We should check for ContentLength = 0 and handle that case...    }
    ReallocMem(ClientCnx.FPostedRawData,
               ClientCnx.RequestContentLength + 1);
    { Clear received length                                            }
    ClientCnx.FDataLen := 0;
   *)
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1ServerStarted(Sender: TObject);
begin
    Display('Server has started');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1ServerStopped(Sender: TObject);
begin
    SslHttpAppSrv1.WSessions.SaveToFile(FSessionFile);
    CleanupTimeStampedDir(SslMultiWebDataModule.DataDir);
    Display('Server is now stopped');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1SslHandshakeDone(Sender: TObject;
  ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
var
    ClientCnx : TMyHttpConnection;
    Ciphers: String;
begin
    ClientCnx := Sender as TMyHttpConnection;
  { V8.64 need to log client hello if no SNI, then ciphers }
    if DisplaySslInfo.Checked then begin
        if ClientCnx.SslServerName = '' then begin
            Display(ClientCnx.CPeerAddr + ' - Client Hello: ' +
                              Trim(WSocketGetCliHelloStr(ClientCnx.CliHelloData))) ;
        end;
        Ciphers := Trim(ClientCnx.SslGetSupportedCiphers (True, True));
        if Ciphers <> '' then begin
            Ciphers := StringReplace(Ciphers, #13#10, ', ', [rfReplaceAll]);
            Display('SSL Ciphers from Client: ' + Ciphers);
        end;
    end;
    Display(ClientCnx.PeerAddr + ' - ' + ClientCnx.HostTag + ' ' + ClientCnx.SslHandshakeRespMsg);

    { V8.71 check if client has sent a certificate }
    if Assigned(PeerCert) and PeerCert.IsCertLoaded then begin
        Display(ClientCnx.PeerAddr + ' - ' + ClientCnx.HostTag + ' Client certificate received, should we trust client?' +
                                                                                             IcsCRLF + PeerCert.CertMainInfo);
     // we should now check common name and/or issuer against people we trust
     // Disconnect := True;  // no, kill connection
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1SslServerName(Sender: TObject;     { V8.64 }
  var Ctx: TSslContext; var ErrCode: TTlsExtError);
var
    ClientCnx : TMyHttpConnection;
begin
    if DisplaySslInfo.Checked then begin
        ClientCnx := Sender as TMyHttpConnection;
        Display('Client Hello from ' + ClientCnx.CPeerAddr + ', ' +
                    Trim(WSocketGetCliHelloStr(ClientCnx.CliHelloData))) ;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1SslAlpnSelect(Sender: TObject;
  ProtoList: TStrings; var SelProto: string; var ErrCode: TTlsExtError);    { V8.64 }
var
//    ClientCnx : TMyHttpConnection;
    I: Integer;
begin
   if ProtoList.Count = 0 then Exit;
//  ClientCnx := Sender as TMyHttpConnection;
//  { ALPN already logged from CliHelloData }

  // optionally select a protocol we want to use
    for I := 0 to ProtoList.Count - 1 do begin
        if ProtoList[I] = ALPN_ID_HTTP11 then begin
            SelProto := ALPN_ID_HTTP11;
        //    SelProto := ALPN_ID_HTTP2; // TEMP confuse them
            ErrCode := teeOk;
            Exit;
        end;
   //     if ProtoList[I] = ALPN_ID_HTTP2 then begin  don't support HTTP/2 yet

    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1TraceDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
//var
//    ClientCnx  : TMyHttpConnection;
begin
    { It's easier to do the cast one time. Could use with clause... }
//    ClientCnx := TMyHttpConnection(Client);

    { Count request and display a message }
//    InterlockedIncrement(FCountRequests);
//    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' +
//            ClientCnx.GetPeerAddr + '] ' + IntToStr(FCountRequests) +
//            ': ' + ClientCnx.Version + ' TRACE ' + ClientCnx.Path);
//    DisplayHeader(ClientCnx);

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1VirtualException(Sender: TObject;
  E: Exception; Method: THttpMethod; const Path: string);
begin
    Display('Exception creating virtual page: ' + Path + ' - ' + E.Message);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.SslHttpAppSrv1WellKnownDir(Sender,
  Client: TObject; const Path: string; var BodyStr: string);
var
    ClientCnx : TMyHttpConnection;
begin
    ClientCnx := Client as TMyHttpConnection;
    Display(ClientCnx.PeerAddr + ' - ' + ClientCnx.HostTag +
                                        ' Well-Known File Requested: ' + Path);
    if (Pos('/acme-challenge/', Path) > 1) or  (Pos('/ics-validation/', Path) > 1) then begin  { V8.64 }
     // check challenge token received Let's Encrypt and return key authorization
        IcsSslX509Certs.WebSrvHttp(Self, ClientCnx.RequestHost, Path, BodyStr);
        if BodyStr <> '' then
           Display(ClientCnx.PeerAddr + ' - ' + ClientCnx.HostTag +
                                            ' acme-challenge response: ' + BodyStr);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.71 event called by IcsDomainNameCache component when it completes the lookup of
  of a remote host name, find the correct client and update it, for web logs }
procedure TDDWebServiceSrv.IcsAppMonCliLogEvent(Sender: TObject; const Line: string);    { V9.3 }
begin
    Display(IcsAppMonName + ': ' + Line);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.IcsDomainNameCache1DNUpdateEvent(Sender: TObject; ItemNr: Integer);
var
    ClientCnx: TMyHttpConnection;
    I: Integer;
begin
    if ItemNr >= 0 then begin

    // find client with the IP address we just looked up
        for I := 0 to SslHttpAppSrv1.ClientCount - 1 do begin
            ClientCnx := TMyHttpConnection(SslHttpAppSrv1.Client[I]);
            if ClientCnx.CPeerAddr = IcsDomainNameCache1.GetDNItem(ItemNr).Request then begin
                ClientCnx.CPeerHostName := IcsDomainNameCache1.BuildRespList(ItemNr);
                Display (ClientCnx.CPeerAddr + ' - Client DNS Lookup: ' + ClientCnx.CPeerHostName) ;
                Break;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.IcsMailQueueLogEvent(LogLevel: TMailLogLevel;  { V8.60 }
  const Info: string);
begin
    if LogLevel <= MLogLevelInfo then  { V9.3 less information }
        Display(Info);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.IcsMailQueueOATokenEvent(ServNr: Integer; var Token, TokAccount: string; var TokExpireDT: TDateTime);     { V8.70 }
begin
    Display('Starting to get OAuth2 Bearer Token');
    if NOT IcsRestEmail.GetNewToken(True) then   // allow interaction, waits for browser window to be completed
 // !!! warning, if this is a real service application don't allow interaction since the browser will not work
        Display('Failed to get OAuth2 Bearer Token')
    else begin
        Token := IcsRestEmail.AccToken;
        TokExpireDT := IcsRestEmail.AccExpireDT;
        TokAccount := IcsRestEmail.NewAccEmail;
      // note, AccToken has a short life, few hours, no need to save it in INI file, it can be refreshed
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.IcsRestEmailEmailNewToken(Sender: TObject);                                                                 { V8.70 }
var
    info: String;
begin
// we should save refresh token since only get it after a login window
    if (IcsRestEmail.RefrToken <> '') and (IcsRestEmail.RefrToken <> OldRefrToken) then begin
        info := 'New OAuth2 Refresh Token for Account: ' + IcsRestEmail.NewAccEmail +
                ', Should be Saved in INI File: ' + FIniFileName + IcsCRLF +
                'RefrToken=' + IcsRestEmail.RefrToken;
        Display(info);
        if (AdminEmailTo <> '') then
            SendAdminEmail (AdminEmailTo, 'ICS DD Web Service New OAuth2 Refresh Token',
            'Web server New OAuth2 Refresh Token.' + IcsCRLF + IcsCRLF + info);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.IcsRestEmailEmailProg(Sender: TObject; LogOption: TLogOption; const Msg: string);                            { V8.70 }
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.IcsRestEmailOAAuthUrl(Sender: TObject; const URL: string);                                                  { V8.70 }
begin
//
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.IcsSslX509CertsCertProg(Sender: TObject;
  LogOption: TLogOption; const Msg: string);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.IcsSslX509CertsChallengeDNS(Sender: TObject; ChallengeItem: TChallengeItem; var ChlgOK: Boolean);
begin
    ChlgOK := False;
//   update DNS server with TXT challenge information
//   not sure worth the trouble for a web server
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.IcsSslX509CertsNewCert(Sender: TObject);
begin
    CertCheckTrigger := Trigger64Immediate;
 // force certiificate check to load new ones
    Display ('Trigger Recheck Certificates') ;
    Display('DD Web Service ordered new SSL certificate.' + IcsCRLF +
                                      IcsSslX509Certs.GetOrderResult);
    if (AdminEmailTo <> '') then
        SendAdminEmail (AdminEmailTo, 'ICS DD Web Service SSL Certificate Order Completed',
        'Web server ordered new SSL certificate.' + IcsCRLF + IcsCRLF +
        IcsSslX509Certs.GetOrderResult + IcsCRLF) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.IcsSslX509CertsOAuthAuthUrl(Sender: TObject; const URL: string);
begin
   Display('Web server demo needs OAuth authenfication for new certificate, ' +
                'Browse to this URL: ' + URL +  ', From PC: ' + IcsGetCompName) ;
    if (AdminEmailTo <> '') then
        SendAdminEmail (AdminEmailTo, 'ICS DD Web Service needs OAuth authenfication for new certificate',
            'Browse to this URL: ' + URL + IcsCRLF + IcsCRLF + 'From Browser on PC: ' + SrvCompName + IcsCRLF) ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.HttpAppSrvClientBgException(Sender: TObject; E: Exception; var CanClose : Boolean);
begin
    Display('Exception processing page - ' +
            E.ClassName + ': ' + E.Message);
    CanClose := TRUE;  // Shutdown client
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /demo.html document                      }
procedure TDDWebServiceSrv.CreateVirtualDocument_Demo(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS WebServer Demo - Menu</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS WebAppServer Demo Menu</H2>' +
            '<H3>' + DDWebServiceSrv.Caption + '</H3>' +
            '<p>' + IcsReportOpenSSLVer + '</p>' +        { V9.1 }
            '<p></p><p>' +
            '<A HREF="/time.html">Server time</A><BR>'  +
            '<A HREF="/template.html">Template demo</A><BR>'  +
            '<A HREF="/appindex.html">Application Web Server session demo</A><BR>'  +
            '<A HREF="/form.html">Data entry</A><BR>'   +
            '<A HREF="/uploadfile.html">Upload a file using POST</A><BR>' +
            '<A HREF="/postinfo.html?GetParam=Hello">Parameters Display</A><BR>' +             { V9.1 }
            '<A HREF="/mailer.html">Send Email Form</A><BR>' +   { V8.61 }
            '<A HREF="/websocketclient.html">WebSocket Client Echo Test</A><BR>' +   { V8.71 }
            '<A HREF="/redir.html">Redirection</A><BR>' +
            '<A HREF="/myip.html">Show client IP and Host Name</A><BR>' +            { V8.71 }
            '<A HREF="/DemoBasicAuth.html">Password protected page</A> (Basic method)<BR>' +
            '<A HREF="/DemoDigestAuth.html">Password protected page</A> (Digest MD5 method)<BR>' +
            '<A HREF="/DemoDigest2Auth.html">Password protected page</A> (Digest SHA-256 method)<BR>' +      { V8.69 }
            '<A HREF="/DemoDigestsAll.html">Password protected page</A> ' + '(Digest MD5 or SHA-256 methods))<BR>' +          { V8.69 }
            '<A HREF="/DemoAuthAll.html">Password protected page</A> ' + '(method is selected by the browser)<BR>' +
            '<A HREF="/DemoNtlmAuth.html">Password protected page</A> ' + '(NTLM method, usercode=(Windows) user, password=(Windows) password)<BR>' +
            '<A HREF="/">Default document</A><BR>'  +
            '<A HREF="https://www.overbyte.be">ICS Home page</A><BR>' +
            '<A HREF="https://wiki.overbyte.eu/wiki/index.php/Main_Page">ICS Wiki and Downloads</A><BR>' +
            '</p>' +
            '<P><form action="/postinfo.html" method="POST"><input type="submit" ' +
              'value="Parameters Display (POST)"><input type="hidden" name="PostParam" value="Hello"></form></P>' +  { V9.2 }
            '<P><input type="button" value="Password protected page (GET button)" onclick="window.location.href=''/DemoAuthAll.html'';"></P>' +    { V8.69 }
            '<P><form action="/DemoAuthAll.html" method="GET"><input type="submit" ' +
                                                                'value="Password protected page (GET template)"></form></P>' +   { V8.69 }
            '<P><form action="/DemoAuthAll.html" method="POST"><input type="submit" ' +
              'value="Password protected page (POST template)"><input type="hidden" name="PostParam" value="Hello"></form></P>' + { V8.69 }
            '<P><form action="/DemoBasicAuth.html" method="POST"><input type="submit" ' +
               'value="Password protected page (POST page)"><input type="hidden" name="PostParam" value="Hello"></form></P>' +   { V9.2 }
            '<P><form action="/DemoAuthAll.html" method="PUT"><input type="submit" ' +
               'value="Password protected page (PUT template)"><input type="hidden" name="PutParam" value="Hello"></form></P>' +   { V9.2 }
            '<p></p>' +
            '<p>Login for protected pages (except NTLM) is "test" with password of "password"</p>' +                    { V8.69 }
            '<p></p>' +
            '<p>' + StatSrvSslCertWeb + '</p>' +                    { V8.71 }
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /DemoBasicAuth.html document                      }
procedure TDDWebServiceSrv.CreateVirtualDocument_DemoBasicAuth(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
var
    sPageUrl: String;
begin
    sPageUrl := ClientCnx.Method + ' ' + ClientCnx.RequestProtocol + '://' + ClientCnx.RequestHost + ClientCnx.Path;  { V9.2 }
    if ClientCnx.PostedDataLen > 0 then
        sPageUrl := sPageUrl + ', PostParams: ' + ClientCnx.PostedDataStr;
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>' +
                'ICS DD Web Service Demo - Password Protected Page - Basic' +
            '</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS DD Web Service Password Protected Page</H2>' +
            'This page is protected by a username and password.<BR>' +
            'The authentication is done using Basic mode.<BR>' +
            'Request Type: ' + sPageUrl + '<BR>' +  { V9.2 }
            '<A HREF="/Demo.html">Demo menu</A><BR>'  +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /DemoBasicAuth.html document            }
procedure TDDWebServiceSrv.CreateVirtualDocument_DemoDigestAuth(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>' +
                'ICS DD Web Service Demo - Password Protected Page - Digest MD5' +
            '</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS DD Web Service Password Protected Page</H2>' +
            '<p>This page is protected by a username and password.</p>' +
            '<p>The authentication is done using Digest MD5 mode.</p>' +
            '<p><A HREF="/Demo.html">Demo menu</A></p>'  +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /DemoDigestSha2Auth.html document            }
procedure TDDWebServiceSrv.CreateVirtualDocument_DemoDigest2Auth(                   { V8.69 }
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>' +
                'ICS WebServer Demo - Password Protected Page - Digest SHA-256' +
            '</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS WebServer Password Protected Page</H2>' +
            'This page is protected by a username and password.<BR>' +
            'The authentication is done using Digest mode with SHA-256 Algorithm.<BR>' +
            '<A HREF="/Demo.html">Demo menu</A><BR>'  +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ V8.69 This procedure is use to generate /DemoDigestsAll.html document             }
procedure TDDWebServiceSrv.CreateVirtualDocument_DemoDigestsAll(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>' +
                'ICS DD Web Service Demo - Password Protected Page - Multiple Digests' +
            '</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS DD Web Service Password Protected Page</H2>' +
            '<p>This page is protected by a username and password.</p>' +
            '<p>Your browser selected the <B>' +
            HttpAuthTypeNames[ClientCnx.AuthType] + '</B> authentication mode.</p>' +
            '<p><A HREF="/Demo.html">Demo menu</A></p>'  +
          '</BODY>' +
        '</HTML>');
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /DemoNtlmAuth.html document             }
procedure TDDWebServiceSrv.CreateVirtualDocument_DemoNtlmAuth(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>' +
                'ICS DD Web Service Demo - Password Protected Page - NTLM' +
            '</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS DD Web Service Password Protected Page</H2>' +
            '<p>This page is protected by a username and password.</p>' +
            '<p>The authentication is done using NTLM mode.</p>' +
            '<p><A HREF="/Demo.html">Demo menu</A></p>'  +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /DemoAuthAll.html document             }
(* V9.2 this page is now created using a template, to test authentication for templates }
procedure TDDWebServiceSrv.CreateVirtualDocument_DemoAuthAll(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        '',           { Default header                  }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>' +
                'ICS DD Web Service Demo - Password Protected Page - Multiple Types' +
            '</TITLE>' +
          '</HEAD>' +
          '<BODY>' +
            '<H2>ICS DD Web Service Password Protected Page</H2>' +
            '<p>This page is protected by a username and password.</p>' +
            '<p>Your browser selected the <B>' +
            HttpAuthTypeNames[ClientCnx.AuthType] + '</B> authentication mode.</p>' +
            '<p><A HREF="/Demo.html">Demo menu</A></p>'  +
          '</BODY>' +
        '</HTML>');
end; *)


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.CreateVirtualDocument_Template(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerPage(
        Flags,
        '',
        NO_CACHE,
        'TemplateDemo.html',
        nil,
        ['TIME',    DateTimeToStr(Now),
         'PROGVER', SrvCopyRight,
         'SOURCE',  TextToHtmlText(ClientCnx.TemplateDir + 'TemplateDemo.html')]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /redir.html document                     }
procedure TDDWebServiceSrv.CreateVirtualDocument_Redir(
    Sender    : TObject;            { HTTP server component                 }
    ClientCnx : TMyHttpConnection;  { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    Location : String;
begin
    Location := ClientCnx.Params;
 //   if Location = '' then
 //       Location := Trim(RedirUrlEdit.text);

    ClientCnx.AnswerString(Flags,
        '302 Moved',                    { Tell the browser about relocation }
        '',                             { Default Content-Type: text/html   }
        'Location: ' + Location + icsCRLF,            { Specify new location }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS DD Web Service Demo - Redir</TITLE>' +
          '</HEAD>' + icsCRLF +
          '<BODY>' +
            'You should be redirected automatically !<BR>' + icsCRLF +
            '<A HREF="' + Location + '">Click Here</A><BR>' + icsCRLF +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /time.html document                     }
procedure TDDWebServiceSrv.CreateVirtualDocument_Time(
    Sender    : TObject;            { HTTP server component                 }
    ClientCnx : TMyHttpConnection;  { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
begin
    ClientCnx.AnswerString(Flags,
        '',                            { Default Status '200 OK'            }
        '',                            { Default Content-Type: text/html    }
        'Pragma: no-cache' + icsCRLF +  { No client caching please           }
        'Expires: -1'      + icsCRLF,   { I said: no caching !               }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS DD Web Service Demo</TITLE>' +
          '</HEAD>' + icsCRLF +
          '<BODY>' +
            '<H2>Time at server side:</H2>' + icsCRLF +
            '<P>' + DateTimeToStr(Now) +'</P>' + icsCRLF +
            '<A HREF="/demo.html">Demo menu</A>' + icsCRLF +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.CreateVirtualDocument_Bruno(
    Sender    : TObject;            { HTTP server component                 }
    ClientCnx : TMyHttpConnection;  { Client connection issuing command     }
    var Flags : THttpGetFlag);      { Tells what HTTP server has to do next }
var
    Body      : String;
    Header : String;
    I      : Integer;
    Buf    : String;
    Count  : Integer;
begin
    Buf   := ClientCnx.Params;
    Count := StrToIntDef(Buf, 5000);
    if Count > 20000 then
        Count := 20000;
    Body := '<HTML>' + '<HEAD>' + '<TITLE>Test Page from Bruno :-)</TITLE>' +
            '</HEAD>' + icsCRLF +  '<BODY>';
    for I := 1 to Count do
        Body := Body + '<br>This is line number ' + IntToStr(I);
    Body := Body + '</BODY></HTML>';
    Header := 'Pragma: no-cache' + icsCRLF +  { No client caching please     }
              'Expires: -1'      + icsCRLF +
              'Set-Cookie: Usuario=a; path=/'+ icsCRLF +
              'Set-Cookie: Senha=a; path=/'+ icsCRLF;
//    ClientCnx.OnRequestDone := RequestDone;
    ClientCnx.AnswerString(Flags, '', '', Header, Body);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Produce a reply with a huge header line. Used to check client behaviour.  }
procedure TDDWebServiceSrv.CreateVirtualDocument_HeaderBug(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',                            { Default Status '200 OK'            }
        '',                            { Default Content-Type: text/html    }
        'Pragma: no-cache' + icsCRLF +  { No client caching please           }
        'Expires: -1'      + icsCRLF,   { I said: no caching !               }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS DD Web Servicer Demo</TITLE>' +
          '</HEAD>' + icsCRLF +
          '<BODY>' +
            'Congratulations !' + icsCRLF +
          '</BODY>' +
        '</HTML>');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.CreateVirtualDocument_MyIP(
    Sender    : TObject;
    ClientCnx : TMyHttpConnection;
    var Flags : THttpGetFlag);
begin
    ClientCnx.AnswerString(Flags,
        '',           { Default Status '200 OK'         }
        '',           { Default Content-Type: text/html }
        'Pragma: no-cache' + icsCRLF +  { No client caching please           }
        'Expires: -1'      + icsCRLF,   { I said: no caching !               }
        '<HTML>' +
          '<HEAD>' +
            '<TITLE>ICS DD Web Service Demo</TITLE>' +
          '</HEAD>' + icsCRLF +
          '<BODY>' +
            '<P>Your IP is: ' + ClientCnx.PeerAddr + '</P>' + icsCRLF +
            '<P>Your Host Name is: ' + ClientCnx.CPeerHostName + '</P>' + icsCRLF +    { V8.71 }
          '</BODY>' +
        '</HTML>');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ RESTAPI using a URL Handler }
procedure TUrlHandlerRestApi.Execute;
var
    JsonResp: AnsiString;        { V8.69 was string }
{$IFDEF DATABASE}
    RestParamsSrv: TRestParamsSrv;
    sCountry: String;
{$IFDEF YuDISQL}
    info: String;
{$ENDIF}
{$ELSE}
    JsonResult: TRestParams;
{$ENDIF}
begin
   JsonResp := '';

{$IFDEF DATABASE}
   sCountry := '';
    RestParamsSrv := TRestParamsSrv.Create;
    if Client.Method <> 'GET' then begin
     // we could handle DELETE, POST and PUT here
        JsonResp := RestParamsSrv.JsonErr(RestErrUnsupported);
    end
    else begin
        ExtractURLEncodedValue (Client.Params, 'Country', sCountry) ;
        if sCountry = '' then
            JsonResp := RestParamsSrv.JsonErr(RestErrUnsupported);
    end;

 // open DISQLite database, if linked
{$IFDEF YuDISQL}
    if (sCountry <> '') and (NOT DbOpened) then begin
        info := ProgDirectory + '\World.db3';
        try
            if FileExists(info) then begin
                sqlite3_initialize;
                DISQLite3Database := TDISQLite3Database.Create(Self);
                DISQLite3Query := TDISQLite3UniDirQuery.Create(Self);
                DISQLite3Query.Database := DISQLite3Database;
                DISQLite3Database.DatabaseName := info;
                DISQLite3Database.Open;
                DbOpened := DISQLite3Database.Connected;
                if NOT DbOpened then
                    JsonResp := RestParamsSrv.JsonErr(RestErrGeneral, 'Failed to Open Database');
            end
            else
                JsonResp := RestParamsSrv.JsonErr(RestErrGeneral, 'Database File Not found: ' + info);
        except
            on E: ESQLite3 do begin
                info := E.ErrorMessage;
                Display(info);
                JsonResp := RestParamsSrv.JsonErr(RestErrGeneral, info);
            end;
        end
    end;

    if (sCountry <> '') and DbOpened then begin
        try
            DISQLite3Query.Close;
            DISQLite3Query.SelectSQL := 'SELECT ID,Name,Area,Population,"Birth Rate","Life Expectancy",Background FROM Countries WHERE Name = "' + sCountry + '"';
            DISQLite3Query.Open;
         // DISQLite3Query does not set RecordCount !!!!
            DISQLite3Query.First;
            if DISQLite3Query.Eof then
                JsonResp := RestParamsSrv.JsonErr(RestErrNoRecords, 'Can not find country: ' + sCountry) ;
        except
            on E: ESQLite3 do begin
                info := E.ErrorMessage;
                Display(info);
                JsonResp := RestParamsSrv.JsonErr(RestErrGeneral, info);
            end;
        end;
        if DbOpened and (JsonResp  = '') then begin  // no error yet
            try
                if NOT RestParamsSrv.ResultSet2Json(DISQLite3Query, JsonResp) then   // converts DB resultset to Json
                     JsonResp := RestParamsSrv.JsonErr(RestErrConvert);
            except
                info := 'ApiCountry - ' + IcsGetExceptMess (ExceptObject) ;
                Display(info);
                JsonResp := RestParamsSrv.JsonErr(RestErrGeneral, info) ;
            end ;
        end;
    end;
{$ELSE}  // no DISQLite, simple Json response
   JsonResp := RestParamsSrv.JsonErr(RestErrGeneral, 'DISQLite is not linked');

{$ENDIF}
    RestParamsSrv.Free;

{$ELSE}  // no datanase, simple Json response
    JsonResult := TRestParams.Create(Nil);
    JsonResult.PContent := PContJson;
    JsonResult.AddItem('success', false);
    JsonResult.AddItem('reccount', 0);
    JsonResult.AddItem('errno', 8);
    JsonResult.AddItem('errdesc', 'Database is currently unavailable');
    JsonResp := JsonResult.GetParameters;
    JsonResult.Free;
{$ENDIF}

    Display(String(JsonResp));
    Flags := hgWillSendMySelf ;
    AnswerString(
        '',           { Default Status '200 OK'         }
        'application/json',           { Default Content-Type: text/html }
        'Pragma: no-cache' + icsCRLF +  { No client caching please           }
        'Expires: -1'      + icsCRLF,   { I said: no caching !               }
        String(JsonResp),               { the real response }
{$IFDEF COMPILER12_UP}
        CP_UTF8,                        { V8.69 }
{$ENDIF}
        Now);                           { when we sent the response }
end;

 {* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.ClientWSLogEvent(Sender: TObject; const Msg: string);                                       { V8.71 websockets }
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.ClientWSHandshakeEvent(Client: THttpWSSrvConn; var OK: Boolean; var WelcomeMsg: String);    { V8.71 websockets }
var
    S: String;
begin
    OK := False;
    if CompareText(Client.WSReqPage, '/WebSocket/Echo') = 0 then begin
        OK := True;
        WelcomeMsg := 'Welcome to ICS WebSocket echo server, all messages will be echoed.';
    end
    else if CompareText(Client.WSReqPage, '/WebSocket/EchoPing') = 0 then begin
        OK := True;
        WelcomeMsg := 'Welcome to ICS WebSocket echo/ping server, all messages will be echoed.';
        Client.WSPingEnabled := True;
    end
    else if CompareText(Client.WSReqPage, '/WebSocket/Chat') = 0 then begin
        OK := True;
        if Client.WSReqParams = '' then    // ideally they passed a chat name
            Client.WSReqParams := Client.PeerAddr;
    end
    else if (CompareText(Client.WSReqPage, '/WebSocket/Serverinfo') = 0) then begin
        OK := True;
        Client.WSPingSecs := 10;     // used for server push
        Client.WSPingEnabled := True;
    end;
    if OK then
        S := ' Accepted'
    else
        S := ' Rejected';
    Display('WebSocket ' + Client.CPeerAddr + ': Request for ' + Client.WSReqPage + S);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.ClientWSReadyEvent(Client: THttpWSSrvConn);                                                  { V8.71 websockets }
var
    S: String;
begin
 // called once when the websocket is ready for traffic
    Display('Websocket ' + Client.PeerAddr + ': Ready for ' + Client.WSReqPage);
    if (CompareText(Client.WSReqPage, '/WebSocket/Chat') = 0) then begin
        WSGetChatUsers(Client.WSReqPage, S);
        Client.WSSendText(Nil, 'Welcome to ICS WebSocket chat server, Users: '  + S);
        WSSendChatResp(Client, '!! Has Joined Chat', True);

    end
    else if (CompareText(Client.WSReqPage, '/WebSocket/Serverinfo') = 0) then begin
      //  Client.WSSendText(Nil, '==1' + GetCurServInfo(TMagHttpConnection(Client)));
      //  Client.WSSendText(Nil, '==2' + 'Server Sites and SSL Certificates: <BR><BR>' + IcsCRLF +
      //                                  StatSrvSslCertWeb + '<BR><BR>' + IcsCRLF);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.ClientWSPingTimerEvent(Client: THttpWSSrvConn); { V8.71 websockets }
begin
 // if server pings are being sent, called just before a ping is sent, allowing the web page
 //    to be updated with topical content
    if (CompareText(Client.WSReqPage, '/WebSocket/Serverinfo') = 0) then begin
  //      Client.WSSendText(Nil, '==1' + GetCurServInfo(TMagHttpConnection(Client)));
    end;
end;
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.WSSendChatResp(SendCli: THttpWSSrvConn; const APacket: String; ToMe: Boolean);  { V8.71 websockets }
var
    I, J: Integer;
    RecvCli: THttpWSSrvConn;
    Msg: String;
begin
    Msg := '[' + SendCli.WSReqParams + '] ' + APacket;    // Edge and Firefox ignore text in angle brackets <>, probably thinks it is HTML
    J := 0;
    try
        for I := SslHttpAppSrv1.ClientCount - 1 downto 0 do begin
            RecvCli := SslHttpAppSrv1.Client[I] as THttpWSSrvConn;
            if SendCli.WSReqPage = RecvCli.WSReqPage then begin
                if ToMe OR (SendCli.Handle <> RecvCli.Handle) then begin
                    RecvCli.WSSendText(Nil, Msg);
                    J := J + 1 ;
                end;
            end;
        end;
        Display('Send Chat message to ' + IntToStr(J) + ' users: ' + Msg);
    except
        Display('Exception sending chat message');
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TDDWebServiceSrv.WSGetChatUsers(const ChatPage: String; var UList: String): Integer;  { V8.71 websockets }
var
    I: Integer;
    RecvCli: THttpWSSrvConn;
begin
    UList := '';
    Result := 0;
    try
        for I := SslHttpAppSrv1.ClientCount - 1 downto 0 do begin
            RecvCli := SslHttpAppSrv1.Client[I] as THttpWSSrvConn;
            if RecvCli.WSReqPage = ChatPage then begin
                if UList <> '' then
                    UList := UList + ', ';
                UList := UList + RecvCli.WSReqParams;
                Result := Result + 1;
            end;
        end;
        Display('Chat  users line, total: ' + IntToStr(Result) + ' - ' + UList);
    except
        Display('Exception getting chat users');
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.ClientWSFrameRcvdEvent(Client: THttpWSSrvConn; const APacket: String; var AFrame: TWebSocketReceivedFrame);  { V8.71 websockets }
begin
    if AFrame = Nil then Exit;
    if AFrame.Kind = wsfkPong then Exit;
    Display('Websocket ' + Client.PeerAddr + ': ' + Client.WSReqPage + ' ' +
                                             GetWSFrameKind(AFrame.Kind) + ' received, length: ' + IntToStr(AFrame.DataBytes));
    case AFrame.Kind of
        wsfkBin: begin
        //    if Length(APacket) < 200 then
        //        Display(APacket);
            if (CompareText(Client.WSReqPage, '/WebSocket/Echo') = 0) or
                  (CompareText(Client.WSReqPage, '/WebSocket/EchoPing') = 0) then begin
                AFrame.Data.Position := 0;
                Client.WSSendBinaryStream(Nil, AFrame.Data);
            end;
        end;
        wsfkText: begin
       //     if Length(APacket) < 200 then
       //         Display(APacket);
            if (CompareText(Client.WSReqPage, '/WebSocket/Echo') = 0) or
                  (CompareText(Client.WSReqPage, '/WebSocket/EchoPing') = 0) then begin
                Client.WSSendText(Nil, APacket);
            end;
            if CompareText(Client.WSReqPage, '/WebSocket/Chat') = 0 then begin
                WSSendChatResp(Client, APacket, True);
            end;
        end;
        wsfkClose: begin
            if CompareText(Client.WSReqPage, '/WebSocket/Chat') = 0 then begin
                WSSendChatResp(Client, '!! Left Chat', True);
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.ClientWSFrameSentEvent(Client: THttpWSSrvConn; var AFrame: TWebSocketOutgoingFrame);         { V8.71 websockets }
begin
    if AFrame = Nil then Exit;
    if AFrame.Kind = wsfkPing then Exit;
    Display('Websocket ' + Client.PeerAddr + ': ' + Client.WSReqPage + ' ' + GetWSFrameKind(AFrame.Kind) + ' sent');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TDDWebServiceSrv.ClientWSDisconnectedEvent(Sender: TObject);                                                  { V8.71 websockets }
var
    Client: THttpWSSrvConn;
begin
    Client := Sender as THttpWSSrvConn;
    Display('Websocket ' + Client.PeerAddr + ': ' + Client.WSReqPage + ' Closing, Total Frames ' + IntToStr(Client.WSFrameCounter));
    SslHttpAppSrv1AfterAnswer(Self, Client);  // main web log
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TUrlHandlerWSClientEcho.Execute;                                                                             { V8.71 websockets }

    function BuildWSURIs: String;
    var
        I: integer;
        Sel: String;
    begin
        Result := '' ;
        Sel := ' selected';
        for I:= 0 to DDWebServiceSrv.SslHttpAppSrv1.IcsHosts.Count - 1 do begin
            with DDWebServiceSrv.SslHttpAppSrv1.IcsHosts [I] do begin
                if NOT HostEnabled then continue;
                if HostTag <> 'WEB-APP' then continue;    { V9.3 }
                if BindNonPort <> 0 then begin
                    Result := Result + '<option' + Sel + '>ws://' + HostNames[0];
                    Sel := '';
                    if BindNonPort <> 80 then Result := Result + ':' + IntToStr(BindNonPort);
                    Result := Result + '/WebSocket/Echo</option>' + icsCRLF;
                end;
                if BindSslPort <> 0 then begin
                    Result := Result + '<option' + Sel + '>wss://' + HostNames[0];
                    Sel := '';
                    if BindSslPort <> 443 then Result := Result + ':' + IntToStr(BindSslPort);
                    Result := Result + '/WebSocket/Echo</option>' + icsCRLF;
                    Result := Result + '<option>wss://' + HostNames[0];
                    if BindSslPort <> 443 then Result := Result + ':' + IntToStr(BindSslPort);
                    Result := Result + '/WebSocket/EchoPing</option>' + icsCRLF;
                    Result := Result + '<option>wss://' + HostNames[0];
                    if BindSslPort <> 443 then Result := Result + ':' + IntToStr(BindSslPort);
                    Result := Result + '/WebSocket/Chat?MyName</option>' + icsCRLF;
                end;
            end;
        end;
    end;

begin
    AnswerPage('', NO_CACHE, 'websocketclient.html', nil,
             ['WSSURL', BuildWSURIs ]);
    Finish;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TUrlHandlerIndexHtml.Execute;                                    { V9.3 }
begin
    AnswerPage('', NO_CACHE, 'Index.html', nil, ['serverinfo',  DDWebServiceSrv.FServerInfo]);
    Finish;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
