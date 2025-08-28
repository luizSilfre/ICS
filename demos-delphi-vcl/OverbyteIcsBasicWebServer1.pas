{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Basic web application server sample, with Websocket server,
              no real GUI, all settings in code.
Creation:     Sept 2024
Updated:      Feb 2025
Version:      V9.4
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2025 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.

              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany

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
Sept 2024  - V9.3 baseline
Simplified version of OverbyteIcsSslMultiWebServ ignoring configuration INI
files, security features, session data, most demo pages and most logging,
and settings for localhost set in code, search for IcsHosts to change IP
addresses, etc.
Feb 07, 2025 V9.4 - Cleanup.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsBasicWebServer1;

{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  OverbyteIcsTypes,
  OverbyteIcsUtils,
  OverbyteIcsTicks64,
  OverbyteIcsSslBase,
  OverbyteIcsWndControl,
  OverbyteIcsHttpSrv,
  OverbyteIcsHttpAppServer,
  OverbyteIcsWebSocketcli,
  OverbyteIcsWebSocketSrv;

const
    SrvCopyRight : String = ' OverbyteIcsBasicWebServer (c) 2024 Francois Piette V9.3 ';
    WebLogFields = '#Fields: date time s-sitename s-computername s-ip cs-method cs-uri-stem cs-uri-query ' +
                   's-port cs-username c-ip cs-version cs(User-Agent) cs(Referer) cs-host sc-status ' +
                   'sc-bytes cs-bytes time-taken' ;
    WebLogHdrMask = '"#Date: "' + ISODateMask + #32 + ISOTimeMask + '"';

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

  TUrlHandlerWSClientEcho = class(TUrlHandler)    { WebSocket page handler }
  public
      procedure Execute; override;
  end;

  TUrlHandlerIndexHtml = class(TUrlHandler)      { index.html page handler }
  public
      procedure Execute; override;
  end;

type
  TWebServerForm = class(TForm)
    ToolsPanel: TPanel;
    StartButton: TButton;
    StopButton: TButton;
    DisplayHeaderCheckBox: TCheckBox;
    DisplayMemo: TMemo;
    SslHttpAppSrv: TSslHttpAppSrv;
    procedure SslHttpAppSrvAfterAnswer(Sender, Client: TObject);
    procedure SslHttpAppSrvAuthGetPassword(Sender, Client: TObject; var Password: string);
    procedure SslHttpAppSrvAuthGetType(Sender, Client: TObject);
    procedure SslHttpAppSrvAuthResult(Sender, Client: TObject; Success: Boolean);
    procedure SslHttpAppSrvBeforeAnswer(Sender, Client: TObject);
    procedure SslHttpAppSrvBeforeProcessRequest(Sender, Client: TObject);
    procedure SslHttpAppSrvBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure SslHttpAppSrvClientConnect(Sender, Client: TObject; Error: Word);
    procedure SslHttpAppSrvDisplay(Sender: TObject; const Msg: string);
    procedure SslHttpAppSrvProcDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
    procedure SslHttpAppSrvHttpRequestDone(Sender, Client: TObject);
    procedure SslHttpAppSrvHttpRespHdr(Sender, Client: TObject; const Header: string);
    procedure SslHttpAppSrvSslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
    procedure SslHttpAppSrvSslServerName(Sender: TObject; var Ctx: TSslContext; var ErrCode: TTlsExtError);
    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure Display(Msg : String);
    procedure ClientBgException(Sender: TObject; E: Exception; var CanClose : Boolean);
    procedure CreateVirtualDocument_Demo(Sender: TObject; ClientCnx : TMyHttpConnection; var Flags : THttpGetFlag);
    procedure ClientWSLogEvent(Sender: TObject; const Msg: string);                                       { websockets }
    procedure ClientWSHandshakeEvent(Client: THttpWSSrvConn; var OK: Boolean; var WelcomeMsg: String);    { websockets }
    procedure ClientWSFrameRcvdEvent(Client: THttpWSSrvConn; const APacket: String; var AFrame: TWebSocketReceivedFrame);  { websockets }
    procedure ClientWSFrameSentEvent(Client: THttpWSSrvConn; var AFrame: TWebSocketOutgoingFrame);         { websockets }
    procedure ClientWSDisconnectedEvent(Sender: TObject);                                                  { websockets }
    procedure ClientWSReadyEvent(Client: THttpWSSrvConn);                                                  { websockets }
    procedure ClientWSPingTimerEvent(Client: THttpWSSrvConn);                                              { websockets }
  public
    { Public declarations }
    FFinalized   : Boolean;
    FDataDir     : String;
//    FSessionFile : String;
//    FUploadsDir  : String;
    FSrvCompName: String;
    FServerInfo: String;
  end;

var
  WebServerForm: TWebServerForm;

implementation

{$R *.dfm}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMyHttpConnection.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    OnBgException := BgExceptionEvent ;
    CPeerHostName := '' ;
    OnWSLogEvent := WebServerForm.ClientWSLogEvent;
    OnWSHandshake := WebServerForm.ClientWSHandshakeEvent;
    OnWSFrameRcvd := WebServerForm.ClientWSFrameRcvdEvent;
    OnWSFrameSent := WebServerForm.ClientWSFrameSentEvent;
    OnWSDisconnected := WebServerForm.ClientWSDisconnectedEvent;
    OnWSReady := WebServerForm.ClientWSReadyEvent;
    OnWSPingTimer := WebServerForm.ClientWSPingTimerEvent;
    // DebugBody decodes all frames, DebugHdr only main info , DebugConn only connections oe DebugNone
    DebugLevel := DebugConn;
    if WebServerForm.DisplayHeaderCheckBox.Checked then
        DebugLevel := DebugHdr;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMyHttpConnection.BgExceptionEvent(Sender : TObject; E : Exception; var CanClose : Boolean);
begin
     WebServerForm.Display('Client Error - ' + IcsGetExceptMess (E)) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ We need to override parent class destructor  }
destructor TMyHttpConnection.Destroy;
begin
 //   FreeAndNil (FRespTimer);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServerForm.Display(Msg : String);
begin
    DisplayMemo.Lines.Add (Msg) ;
end;

procedure TWebServerForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if FFinalized then Exit;   { V8.64 only once }
    FFinalized := True;
    StopButtonClick(Self);
end;

procedure TWebServerForm.FormCreate(Sender: TObject);
begin
// don't start server here, use a message
end;

procedure TWebServerForm.StartButtonClick(Sender: TObject);
var
    Errs, S, BaseDir: String;

    function BuildDemoURIs: String;
    var
        I: integer;
    begin
        Result := '' ;
        for I:= 0 to SslHttpAppSrv.IcsHosts.Count - 1 do begin
            with SslHttpAppSrv.IcsHosts [I] do begin
                if NOT HostEnabled then continue;
                if BindNonPort <> 0 then begin
                    Result := Result + 'http://' + HostNames[0];
                    if BindNonPort <> 80 then Result := Result + ':' + IntToStr(BindNonPort);
                    Result := Result + '/' + WebDefDoc + icsCRLF;
                end;
                if (BindSslPort <> 0) and (NOT SslHttpAppSrv.NoSSL) then begin
                    Result := Result + 'https://' + HostNames[0];
                    if BindSslPort <> 443 then Result := Result + ':' + IntToStr(BindSslPort);
                    Result := Result + '/' + WebDefDoc + icsCRLF;
                end;
            end;
        end;
    end;

begin
    BaseDir := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
    BaseDir := IcsAbsolutisePath(BaseDir + '..\..\..\..\demos-data\');
    Display('Demo Pages Base Directory: ' + BaseDir);
    FDataDir := BaseDir + 'WebAppServerData\Data';
//    FUploadsDir := BaseDir + 'WebAppServerData\Uploads';
//    FSessionFile := FDataDir + '\Sessions.dat';

    try

      // main web server settings from INI file, built in CA if no file found
    //    FIniFile := TIcsIniFile.Create(FIniFileName);
    //    IcsLoadTHttpAppSrvFromIni(FIniFile, SslHttpAppSrv1, 'WebAppServer');
    // manually set-up some server setting
        with SslHttpAppSrv do begin
           //  multiple server Options: hoAllowDirList, hoAllowOutsideRoot, hoContentEncoding, hoAllowOptions,
           //  hoAllowPut, hoAllowDelete, hoAllowTrace, hoAllowPatch, hoAllowConnect, hoSendServerHdr, hoIgnoreIfModSince, hoAddMissPath]
            Options := [hoContentEncoding,hoSendServerHdr, hoAllowPut, hoAllowDelete, hoAllowTrace, hoAllowPatch,
                                                                                        hoAddMissPath, hoContentEncoding];
           // should browser send certificate: sslCliCertNone, sslCliCertOptional, sslCliCertRequire
            SslCliCertMethod := sslCliCertNone;
            NoSSL := False;
        end;

        FServerInfo := SrvCopyRight;
        Display(SrvCopyRight);
        if NOT SslHttpAppSrv.NoSSL then begin
            IcsSslRootCAStore.Initialise;       {if OpenSSL and internal not loaded, do it }
          //  IcsSslLoadProviders(True, False);   { need legacy provider }
      // tell them who we are
            FServerInfo := FServerInfo + '<br>SSL/TLS ' + IcsReportOpenSSLVer(False);
            Display('SSL/TLS ' + IcsReportOpenSSLVer(True));
        end;
        FSrvCompName := IcsGetCompName;
        FServerInfo := FServerInfo + '<br>Running on: ' + FSrvCompName;

      // read the server hosts from INI file and check SSL files exist
    //    IcsLoadIcsHostsFromIni(FIniFile, SslHttpAppSrv.IcsHosts, 'Host');
    // manually set-up one IcsHosts so more obvious the minimum that is needed
        SslHttpAppSrv.IcsHosts.Clear;
        SslHttpAppSrv.IcsHosts.Add;
        with SslHttpAppSrv.IcsHosts[0] do begin
            HostEnabled := True;
            HostNames.Text := 'localhost';  // may be more than one host name, but must match domains in SslCert below
            HostTag := 'WEB-APP';        // only needed if more than one host
            Descr := 'WebApp Server';
            BindIpAddr := '127.0.0.1';  // main listening, avoid 0.0.0.0 which will not work if any other web servers are open
            BindIpAddr2 := '::1';       // optional, usually an IPv6 address
            BindNonPort := 80;
            BindSslPort := 443;         // optional 0 for no SSL
         // note domains in SslCert must match HostNames above
            SslCert := GSSL_CERTS_DIR + GSSL_LOCALHOST_NAME;  // C:\ProgramData\ICS-OpenSSL\ICS-Certs\localhost-bundle.pem
            SslPassword := 'password';
            SslSrvSecurity := sslSrvSecHigh;  // or sslSrvSecTls12Less or sslSrvSecTls13Only
            WebDefDoc := 'index.html';              // document returned if blank path passed
            WebDocDir := BaseDir + 'WebAppServerData\wwwRoot';     // where our HTM pages are accessed
            WebTemplDir := BaseDir + 'WebAppServerData\Templates'; // template pages specified below, they include masks
        end;
        if SslHttpAppSrv.IcsHosts.Count <= 0 then begin
            Display('Can Not Start Server - No Source Server Hosts Configured') ;
            exit ;
        end;
        Display('Number of Hosts Configured: ' + IntToStr(SslHttpAppSrv.IcsHosts.Count));  { V8.64 }
//        FIniFile.Free;

   // validate hosts and keep site certificiate information
        try
            Errs := SslHttpAppSrv.ValidateHosts(False, True); // don't stop on first error, no exceptions
            if Errs <> '' then begin
                Display('Server Validation Errors:' + icsCRLF + Errs);
            end;
         //   ReportHosts;
            Display('Required Listen Bindings:' + icsCRLF + SslHttpAppSrv.ListenStates);
        except
            on E:Exception do begin
                Display('Host Validation Failed, Server Stopped - ' + E.Message);
                Exit;
            end;
        end;

    // setup some web server defauls, most were done in IcsLoadTHttpAppSrvFromIni
        SslHttpAppSrv.TemplateDir :=  SslHttpAppSrv.IcsHosts [0].WebTemplDir;
        SslHttpAppSrv.DocDir :=  SslHttpAppSrv.IcsHosts [0].WebDocDir;
        SslHttpAppSrv.DefaultDoc :=  'index.html';
        SslHttpAppSrv.ServerHeader := DefServerHeader;  // get latest version
        SslHttpAppSrv.ClientClass := TMyHttpConnection;  // only needed if we have our own class or Websockets
        SslHttpAppSrv.SocketErrs := wsErrFriendly ;
        SslHttpAppSrv.ExclusiveAddr := true ;
        if SslHttpAppSrv.SessionTimeout < 30 then
            SslHttpAppSrv.SessionTimeout := 300;  // sanity check

      // note that AllowedPaths and Handlers must match a HostTag for each Host in INI file

      // application web server, only allow access to folders where static documents are.
        SslHttpAppSrv.AddGetAllowedPath('/', afBeginBy, 'WEB-APP');

      // Add dynamic webpage application page handlers, create pages from a template and dynamic content
        SslHttpAppSrv.AddGetHandler('/', TUrlHandlerIndexHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv.AddGetHandler('/index.html', TUrlHandlerIndexHtml, hgWillSendMySelf, 'WEB-APP');
        SslHttpAppSrv.AddGetHandler('/websocketclient.html', TUrlHandlerWSClientEcho, hgWillSendMySelf, 'WEB-APP');
     // need separate AddPostHandler for any pages used with POST requests
     // demo.html is created with code in the SslHttpAppSrvProcDocument event

      // start as many hosts as possible
        Errs := SslHttpAppSrv.Start (True) ;
        if Errs <> '' then
            Display('Start Web Server Error - ' + Errs) ;
        if NOT SslHttpAppSrv.ListenAllOK then
            S := 'Failed to Start, '
        else
           S := 'Started OK, ';
        S := S + 'Listen Bindings:' + IcsCRLF + SslHttpAppSrv.ListenStates;
        Display(S);
        StartButton.Enabled := false;
        StopButton.Enabled := true;

     // try and show URLs that will access the first host
        Display('Now browse to one of these URLs:' + icsCRLF + BuildDemoURIs);
    except
        on E:Exception do begin
           Display('Failed to start web server - ' + E.Message);
        end;
    end;end;

procedure TWebServerForm.StopButtonClick(Sender: TObject);
begin
    StartButton.Enabled := true;
    StopButton.Enabled := false;
    SslHttpAppSrv.Stop;
    Display('Server stopped');
end;

{ write W3C log, similar to Microsoft IIS, log line looks something like this:
12:34:48 2009-07-03 12:34:48 WebAppDemo PC19 0.0.0.0 GET /login/loginform.html - 20105 - 192.168.1.119 HTTP/1.1 Mozilla/5.0+(Windows;+U;+Windows+NT+5.1;+en-GB;+rv:1.9.0.11)+Gecko/2009060215+Firefox/3.0.11+(.NET+CLR+3.5.30729) http://pc19:20105/ pc19:20105 200 2101 1138 31 }
procedure TWebServerForm.SslHttpAppSrvAfterAnswer(Sender, Client: TObject);
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
                host := CPeerAddr ;
            newparams := Params ;
            if newparams = '' then newparams := '-' ;
            newuser := SpaceToPlus (AuthUserName)  ;
            if newuser = '' then newuser := '-' ;
            curread := ReadCount - CLastRead ;
            curwrite := WriteCount - CLastWrite ;
            duration := IcsElapsedMSecs64 (CStartTick) ;     { V8.71 }
// #Fields: date time s-sitename s-computername s-ip cs-method
            info := info + #32 + HostTag + #32 + FSrvCompName + #32 + CServerAddr + #32 + Method + #32 +     { V8.69 was Server.Addr }
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
           // should write log file here !!
        except
            Display ('AfterAnswer Error - ' + info + ' - ' + IcsGetExceptMess(ExceptObject)) ;  { V8.70 }
        end;
        Display ('Answer Log - ' + info) ;
    end;
    RemoteClient.CLastRead := RemoteClient.ReadCount ;   // reset read ready for next request
end;

// called before returning pages that require authenication, so we can check AuthUserName and find it's password }
procedure TWebServerForm.SslHttpAppSrvAuthGetPassword(Sender, Client: TObject; var Password: string);
var
    ClientCnx  : TMyHttpConnection;
begin
    ClientCnx := TMyHttpConnection(Client);
    Display('AuthGetPassword for "' + ClientCnx.Path + '" AuthType is "' + AuthTypesToString(ClientCnx.AuthTypes) + '"' +
                                                                                    ' for Username: ' + ClientCnx.AuthUserName);

 { this is where your code should look up user names and get password to check in a database }

    if (ClientCnx.AuthUserName = 'test') then   // hardcoded for testing
        Password := 'password';
end;


// called before any pages, so they can be protected by authenication
procedure TWebServerForm.SslHttpAppSrvAuthGetType(Sender, Client: TObject);
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

// called autentication finished, so we can log the result, if we care
procedure TWebServerForm.SslHttpAppSrvAuthResult(Sender, Client: TObject; Success: Boolean);
var
    ClientCnx  : TMyHttpConnection;
const
    SuccessStr : array [Boolean] of String = ('failed', 'OK');
begin
    ClientCnx := TMyHttpConnection(Client);
    Display('Authentication result ' + SuccessStr[Success] + ' with type ' + HttpAuthTypeNames[ClientCnx.AuthGetMethod] +
                                                          ', Username: ' + ClientCnx.AuthUserName + ' for ' + ClientCnx.Path);
end;

procedure TWebServerForm.SslHttpAppSrvBeforeAnswer(Sender, Client: TObject);
begin

end;

// called before authentication and specific methods are processed }
procedure TWebServerForm.SslHttpAppSrvBeforeProcessRequest(Sender, Client: TObject);
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
    S := '[' + FormatDateTime('HH:NN:SS', Now) + ' ' + ClientCnx.PeerAddr + '] ' + ClientCnx.HostTag + ' ' +
                                                                              ClientCnx.Method + ' ' + ClientCnx.Path;
    if ClientCnx.Params <> '' then
        S := S + '?' + ClientCnx.Params;
    Display(S);

    if DisplayHeaderCheckBox.Checked then begin
        S := ClientCnx.Method + ' ' + ClientCnx.Path;
        if ClientCnx.Params <> '' then
            S := S + '?' + ClientCnx.Params;
        S := S + ' ' + ClientCnx.Version;
        Display('HDR0) ' + S);
        for I := 0 to ClientCnx.RequestHeader.Count - 1 do
            Display('HDR' + IntToStr(I + 1) + ') ' + ClientCnx.RequestHeader.Strings[I]);
    end;

  // check for absolute URL, strip off protocol and host
    if Pos (Protocol, ClientCnx.Path) = 1 then begin
        for I := (Length (Protocol) + 1) to Length (ClientCnx.Path) do begin
            if ClientCnx.Path [I] = '/' then begin
                Display('Found and Trimmed Absolute URL: ' + ClientCnx.Path);
                ClientCnx.Path := Copy (ClientCnx.Path, I, 999) ;
                Break ;
            end;
        end;
    end;
end;


// something horrible has happened, it may be terminal and have crashed the server
procedure TWebServerForm.SslHttpAppSrvBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
begin
    Display('Exception processing page - ' +  E.ClassName + ': ' + E.Message);
    CanClose := FALSE;  // Let the server continue to accept connections
end;

// something horrible has happened, it may be terminal and have crashed the server
procedure TWebServerForm.ClientBgException(Sender: TObject; E: Exception; var CanClose : Boolean);
begin
    Display('Exception processing page - ' + E.ClassName + ': ' + E.Message);
    CanClose := TRUE;  // Shutdown client
end;


procedure TWebServerForm.SslHttpAppSrvClientConnect(Sender, Client: TObject; Error: Word);
var
    ClientCnx : TMyHttpConnection;
begin
    ClientCnx := Client as TMyHttpConnection;
    ClientCnx.WSessionCookie := 'OverbyteIcsWebAppServer' + SslHttpAppSrv.Port;
    ClientCnx.OnBgException := ClientBgException;

 { reverse DNS for remote host using new DNS cache component }
 { may find CPeerAddr in cache, if not returns CPeerAddr and starts a DNS lookup calling event when done }
//    ClientCnx.CPeerHostName := IcsDomainNameCache1.LookupIPOne(ClientCnx.CPeerAddr, 1, sfAny, IcsDomainNameCache1DNUpdateEvent);

 { log something at start }
    Display(FormatDateTime('HH:NN:SS', Now) + ' New Remote Client: ' + ClientCnx.CPeerAddr);  { V8.71 added time }

{ public web servers get thousands of hacking attempts daily, generally a good idea to stop them }
{ for instance any access by public IP address is usually hacking }
{ check blacklist before starting SSL negotiation }
{    if TestFilters ('remaddr', ClientCnx.CPeerAddr) or
           TestFilters ('remhost', ClientCnx.CPeerHostName) or
              HackBlackList.CheckBlackList (ClientCnx.CPeerAddr) then begin
        if NOT TestIpWhiteList (ClientCnx.CPeerAddr) then
        begin
            HackBlackList.AddBlackList (ClientCnx.CPeerAddr, OneMask, 9999) ;
            Display('Hacker Aleady Blacklisted, Connection Refused: ' + ClientCnx.CPeerAddr) ;
            TWSocketServer ((Sender as THttpServer).WSocketServer).Disconnect(ClientCnx);
        end;
    end;  }
end;

procedure TWebServerForm.SslHttpAppSrvDisplay(Sender: TObject; const Msg: string);
begin
    Display(Msg);
end;


procedure TWebServerForm.SslHttpAppSrvHttpRequestDone(Sender, Client: TObject);
var
    ClientCnx  : TMyHttpConnection;
begin
    ClientCnx := TMyHttpConnection(Client);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' + ClientCnx.GetPeerAddr + '] Request Completed, Status ' +
                                                                               IntToStr(ClientCnx.AnswerStatus) + IcsCRLF);
end;


procedure TWebServerForm.SslHttpAppSrvHttpRespHdr(Sender, Client: TObject; const Header: string);
var
    ClientCnx  : TMyHttpConnection;
begin
    if not DisplayHeaderCheckBox.Checked then
        Exit;
    ClientCnx := TMyHttpConnection(Client);
    Display('[' + FormatDateTime('HH:NN:SS', Now) + ' ' + ClientCnx.GetPeerAddr + '] Response Headers');
    Display(Header);
end;


// This event handler is shared by Get/Post/Put/Head to handle all requests, you can have separate handlers
// call once a authentication has finished OK o failed 401, the Flags argument controls what happens after
// this event returns, the default hgSendDoc is for the server to find a disk file (htm, jpg, zip, etc)
// matching the path to open and send.  But we can create our own pages here with an Answer method
// We may also want to abandon malicious requests, such as any for cgi if we don't support that.
procedure TWebServerForm.SslHttpAppSrvProcDocument(Sender, Client: TObject; var Flags: THttpGetFlag);
var
    ClientCnx : TMyHttpConnection;
    Path: String;
    ImageTB: TBytes;
begin
    ClientCnx := Client as TMyHttpConnection;
    Path := Lowercase (ClientCnx.Path) ;

 { if starting authentication, don't process any pages yet }
    if Flags = hg401 then
        Exit;

  { some IcsHosts are purely for redirection }
    if ClientCnx.WebRedirectStat <> 0 then begin
        Flags := hgWillSendMySelf;
        ClientCnx.AnswerRedirect(ClientCnx.WebRedirectStat, ClientCnx.WebRedirectURL);
        Exit;
    end;

 { webapp host is the one we handle here, mostly }
    if (ClientCnx.RequestMethod = httpMethodGet) and (ClientCnx.HostTag = 'WEB-APP') then begin

      // note many special pages are processed by AddGetHandlers using template files set earlier

      // the following pages are created virtually using code, not from files
        if (CompareText(Path, '/demo.html') = 0 ) then
            CreateVirtualDocument_Demo(Sender, ClientCnx, Flags)
        else if CompareText(Path, '/favicon.ico') = 0 then begin
            ImageTB := IcsResourceGetTB('ICSICON16', RT_RCDATA);
            if Length(ImageTB) > 0 then
                ClientCnx.AnswerBodyTB(Flags, '200 OK', 'image/icon', '', ImageTB)
            else
                ClientCnx.Answer404;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This procedure is use to generate /demo.html document                      }
{ !!! most of these links don't work in this basic sample }
procedure TWebServerForm.CreateVirtualDocument_Demo(Sender: TObject; ClientCnx : TMyHttpConnection; var Flags : THttpGetFlag);
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
            '<H3>' + WebServerForm.Caption + '</H3>' +
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
          '</BODY>' +
        '</HTML>');
end;

// called for SSL connections, will report an error if we are refusing the connection
procedure TWebServerForm.SslHttpAppSrvSslHandshakeDone(Sender: TObject; ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
var
    ClientCnx : TMyHttpConnection;
begin
    ClientCnx := Sender as TMyHttpConnection;
    Display(ClientCnx.PeerAddr + ' - ' + ClientCnx.HostTag + ' ' + ClientCnx.SslHandshakeRespMsg);

{ check if client has sent a certificate }
    if Assigned(PeerCert) and PeerCert.IsCertLoaded then begin
        Display(ClientCnx.PeerAddr + ' - ' + ClientCnx.HostTag + ' Client certificate received, should we trust client?' +
                                                                                             IcsCRLF + PeerCert.CertMainInfo);
     // we should now check common name and/or issuer against people we trust
     // Disconnect := True;  // no, kill connection
    end;
end;


procedure TWebServerForm.SslHttpAppSrvSslServerName(Sender: TObject; var Ctx: TSslContext; var ErrCode: TTlsExtError);
begin

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServerForm.ClientWSLogEvent(Sender: TObject; const Msg: string);
begin
    Display(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServerForm.ClientWSHandshakeEvent(Client: THttpWSSrvConn; var OK: Boolean; var WelcomeMsg: String);
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
    end ;
    if OK then
        S := ' Accepted'
    else
        S := ' Rejected';
    Display('WebSocket ' + Client.CPeerAddr + ': Request for ' + Client.WSReqPage + S);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServerForm.ClientWSReadyEvent(Client: THttpWSSrvConn);
begin
 // called once when the websocket is ready for traffic
    Display('Websocket ' + Client.PeerAddr + ': Ready for ' + Client.WSReqPage);
//    if (CompareText(Client.WSReqPage, '/WebSocket/Serverinfo') = 0) then begin
//    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServerForm.ClientWSPingTimerEvent(Client: THttpWSSrvConn); { V8.71 websockets }
begin
 // if server pings are being sent, called just before a ping is sent, allowing the web page
 //    to be updated with topical content
    if (CompareText(Client.WSReqPage, '/WebSocket/Serverinfo') = 0) then begin
  //      Client.WSSendText(Nil, '==1' + GetCurServInfo(TMagHttpConnection(Client)));
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServerForm.ClientWSFrameRcvdEvent(Client: THttpWSSrvConn; const APacket: String; var AFrame: TWebSocketReceivedFrame);
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
        end;
        wsfkClose: begin
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServerForm.ClientWSFrameSentEvent(Client: THttpWSSrvConn; var AFrame: TWebSocketOutgoingFrame);
begin
    if AFrame = Nil then Exit;
    if AFrame.Kind = wsfkPing then Exit;
    Display('Websocket ' + Client.PeerAddr + ': ' + Client.WSReqPage + ' ' + GetWSFrameKind(AFrame.Kind) + ' sent');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TWebServerForm.ClientWSDisconnectedEvent(Sender: TObject);
var
    Client: THttpWSSrvConn;
begin
    Client := Sender as THttpWSSrvConn;
    Display('Websocket ' + Client.PeerAddr + ': ' + Client.WSReqPage + ' Closing, Total Frames ' + IntToStr(Client.WSFrameCounter));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TUrlHandlerIndexHtml.Execute;
begin
    AnswerPage('', NO_CACHE, 'Index.html', nil, ['serverinfo',  WebServerForm.FServerInfo]);
    Finish;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TUrlHandlerWSClientEcho.Execute;

    function BuildWSURIs: String;
    var
        I: integer;
        Sel: String;
    begin
        Result := '' ;
        Sel := ' selected';
        for I:= 0 to WebServerForm.SslHttpAppSrv.IcsHosts.Count - 1 do begin
            with WebServerForm.SslHttpAppSrv.IcsHosts [I] do begin
                if NOT HostEnabled then continue;
                if HostTag <> 'WEB-APP' then
                    continue;
                if BindNonPort <> 0 then begin
                    Result := Result + '<option' + Sel + '>ws://' + HostNames[0];
                    Sel := '';
                    if BindNonPort <> 80 then
                        Result := Result + ':' + IntToStr(BindNonPort);
                    Result := Result + '/WebSocket/Echo</option>' + icsCRLF;
                end;
                if BindSslPort <> 0 then begin
                    Result := Result + '<option' + Sel + '>wss://' + HostNames[0];
                    Sel := '';
                    if BindSslPort <> 443 then
                        Result := Result + ':' + IntToStr(BindSslPort);
                    Result := Result + '/WebSocket/Echo</option>' + icsCRLF;
                    Result := Result + '<option>wss://' + HostNames[0];
                    if BindSslPort <> 443 then
                        Result := Result + ':' + IntToStr(BindSslPort);
                    Result := Result + '/WebSocket/EchoPing</option>' + icsCRLF;
                    Result := Result + '<option>wss://' + HostNames[0];
                end;
            end;
        end;
    end;

begin
    AnswerPage('', NO_CACHE, 'websocketclient.html', nil, ['WSSURL', BuildWSURIs ]);
    Finish;
end;


end.
