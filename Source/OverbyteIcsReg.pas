{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsReg;
  {$DEFINE ICS_COMMON}
{$ENDIF}

{
Feb 15, 2012 Angus - added OverbyteIcsMimeUtils
May 2012 - V8.00 - Arno added FireMonkey cross platform support with POSIX/MacOS
                   also IPv6 support, include files now in sub-directory
Jun 2012 - V8.00 - Angus added SysLog and SNMP components VCL only for now
Jul 2012   V8.02   Angus added TSslHttpAppSrv
Sep 2013   V8.03 - Angus added TSmtpSrv and TSslSmtpSrv
May 2017   V8.45 - Angus added TIcsProxy, TIcsHttpProxy
Apr 2018   V8.54 - Angus added TSslHttpRest, TSimpleWebSrv and TRestOAuth
May 2018   V8.54 - Angus added TSslX509Certs
Oct 2018   V8.58 - New components now installed for FMX and VCL
                   Added subversion to sIcsLongProductName for splash screen
Nov 2019   V8.59 - Version only
Mar 2019   V8.60 - Angus added TIcsMailQueue, TIcsIpStrmLog, TIcsWhoisCli,
                     TIcsTimeServer, TIcsTimeClient, TIcsBlacklist,
                     TIcsFileCopy, TIcsFtpMulti, TIcsHttpMulti.
                   For Delphi 2007 only, added TFtpClientW, TFtpServerW,
                     TIcsFileCopyW, TIcsFtpMultiW and TIcsHttpMultiW.
                   Added Forum and Wiki URLs to About Box.
Apr 2019  V8.61  - Added TDnsQueryHttps, TIcsSms
May 2019  V8.62  - Version only
Oct 2019  V8.63  - Version only
Nov 2019  V8.64  - Version only
Sep 2020  V8.65  - Added TIcsTwitter and TIcsRestEmail
Mar 2021  V8.66 -  Added TIcsInetAlive, OverbyteIcsSslThrdLock gone.
May 2021  V8.67 -  Version only
Oct 2021  V8.68 -  Version only
Mar 2022  V8.69 -  Added TOcspHttp, OverbyteIcsSslHttpOAuth.
Jun 2022  V8.70 -  Version only
Jul 2023  V8.71 -  Added TOAuthBrowser and TSslWebSocketCli
                   Added TIcsMonSocket and TIcsMonPcap
                   Added TIcsMQTTServer and TIcsMQTTClient
                   Added TIcsDomainNameCache and TIcsDomNameCacheHttps
                   Added TIcsNeighbDevices and TIcsIpChanges
Aug 08, 2023 V9.0  Updated version to major release 9.
Nov 19, 2023 V9.1  Version and SVN URL only.
                   Added OverbyteIcsDnsHttps, OverbyteIcsSslUtils and OverbyteIcsSslBase.
                  Tryng to add TX509Base and TX509List, may not be useful.
Apr 08, 2024 V9.2 Version only.
Sep 25, 2024 V9.3 Using RegisterSelectionEditor and TSelectionEditor to ensure that
                    OerbyteIcsTypes and OverbyteIcsSslBase get added to the Uses of
                    applications that drop components onto forms, XE2 and later only.
                  Added TIcsAppMonCli.
Jan 24, 2025 V9.4 Added TIcsAppMonSrv, D10.4 and later currently


}


{$I Include\OverbyteIcsDefs.inc}
(*
{$IFDEF BCB}
  { So far no FMX support for C++ Builder, to be removed later }
  {$DEFINE VCL}
  {$IFDEF FMX}
    {$UNDEF FMX}
  {$ENDIF}
{$ENDIF}
*)
{$IFNDEF COMPILER16_UP}
  {$DEFINE VCL}
  {$IFDEF FMX}
    {$UNDEF FMX}
  {$ENDIF}
{$ENDIF}

{$IFDEF VCL}
  {$DEFINE VCL_OR_FMX}
{$ELSE}
  {$IFDEF FMX}
    {$DEFINE VCL_OR_FMX}
  {$ENDIF}
{$ENDIF}

interface

uses
  {$IFDEF FMX}
    FMX.Types,
    Ics.Fmx.OverbyteIcsWndControl,
    Ics.Fmx.OverbyteIcsWSocket,
    Ics.Fmx.OverbyteIcsDnsQuery,
    Ics.Fmx.OverbyteIcsFtpCli,
    Ics.Fmx.OverbyteIcsFtpSrv,
    Ics.Fmx.OverbyteIcsMultipartFtpDownloader,
    Ics.Fmx.OverbyteIcsHttpProt,
    Ics.Fmx.OverbyteIcsHttpSrv,
    Ics.Fmx.OverbyteIcsMultipartHttpDownloader,
    Ics.Fmx.OverbyteIcsHttpAppServer,
    Ics.Fmx.OverbyteIcsCharsetComboBox,
    Ics.Fmx.OverbyteIcsPop3Prot,
    Ics.Fmx.OverbyteIcsSmtpProt,
    Ics.Fmx.OverbyteIcsNntpCli,
    Ics.Fmx.OverbyteIcsFingCli,
    Ics.Fmx.OverbyteIcsPing,
    {$IFDEF USE_SSL}
      Ics.Fmx.OverbyteIcsSslSessionCache,
      Ics.Fmx.OverbyteIcsProxy,
      Ics.Fmx.OverbyteIcsSslHttpRest,
      Ics.Fmx.OverbyteIcsSslX509Certs,
      Ics.Fmx.OverbyteIcsIpStreamLog,
      Ics.Fmx.OverbyteIcsMailQueue,
      Ics.Fmx.OverbyteIcsFtpMulti,
      Ics.Fmx.OverbyteIcsHttpMulti,
      Ics.Fmx.OverbyteIcsSslHttpOAuth,  { V8.69 }
      OverbyteIcsOAuthFormFmx,          { V8.71 }
      Ics.Fmx.OverbyteIcsWebSocketCli,  { V8.71 }
      Ics.Fmx.OverbyteIcsDnsHttps,      { V9.1 }
      Ics.Fmx.OverbyteIcsSslBase,        { V9.1 TX509Base, TSslBaseComponent }
      Ics.Fmx.OverbyteIcsSslUtils,      { V9.1 }
    {$ENDIF}
    Ics.Fmx.OverbyteIcsWSocketE,
    Ics.Fmx.OverbyteIcsWSocketS,
    Ics.Fmx.OverbyteIcsWhoisCli,
    Ics.Fmx.OverbyteIcsSntp,
    Ics.Fmx.OverbyteIcsBlacklist,
    Ics.Fmx.OverbyteIcsFileCopy,
  {$ENDIF FMX}
  {$IFDEF VCL}
    Controls,
    OverbyteIcsWndControl,
    OverbyteIcsWSocket,
    OverbyteIcsDnsQuery,
    OverbyteIcsFtpCli,
    OverbyteIcsFtpSrv,
    OverbyteIcsMultipartFtpDownloader,
    OverbyteIcsHttpProt,
    OverbyteIcsHttpSrv,
    OverbyteIcsMultipartHttpDownloader,
    OverbyteIcsHttpAppServer,
    OverbyteIcsCharsetComboBox,
    OverbyteIcsPop3Prot,
    OverbyteIcsSmtpProt,
    OverbyteIcsNntpCli,
    OverbyteIcsFingCli,
    OverbyteIcsPing,
    {$IFDEF USE_SSL}
      OverbyteIcsSslSessionCache,
      OverbyteIcsProxy,
      OverbyteIcsSslHttpRest,
      OverbyteIcsSslX509Certs,
      OverbyteIcsIpStreamLog,
      OverbyteIcsMailQueue,
      OverbyteIcsFtpMulti,
      OverbyteIcsHttpMulti,
      OverbyteIcsSslHttpOAuth,  { V8.69 }
      {$IFDEF COMPILER11_UP}
        OverbyteIcsOAuthFormVcl,  { V8.71 }
      {$ENDIF}
      OverbyteIcsWebSocketCli,  { V8.71 }
      OverbyteIcsMQTT,          { V8.71 }
      OverbyteIcsDnsHttps,      { V9.1 }
      OverbyteIcsSslBase,       { V9.1 TX509Base, TSslBaseComponent }
      OverbyteIcsSslUtils,      { V9.1 }
      {$IFDEF COMPILER27_UP}              // !! TEMP D10.4 and later
      OverbyteIcsAppMonCli,     { V9.3 }
      OverbyteIcsAppMonSrv,     { V9.4}
     {$ENDIF}
    {$ENDIF USE_SSL}
    OverbyteIcsWSocketE,
    OverbyteIcsWSocketS,
    OverbyteIcsSysLogClient,
    OverbyteIcsSysLogServer,
    OverbyteIcsSnmpCli,
    OverbyteIcsSmtpSrv,
    OverbyteIcsWhoisCli,
    OverbyteIcsSntp,
    OverbyteIcsBlacklist,
    OverbyteIcsFileCopy,
    OverbyteIcsMonCommon,     { V8.71 }
    OverbyteIcsMonSock,       { V8.71 }
    OverbyteIcsMonPcap,       { V8.71 }
    OverbyteIcsMonNdis,       { V8.71 }
    OverbyteIcsIpHlpApi,      { V8.71 }
   {$IFDEF DELPHI11}
      OverbyteIcsFtpCliW,
      OverbyteIcsFtpSrvW,
      OverbyteIcsFileCopyW,
      OverbyteIcsFtpMultiW,
      OverbyteIcsHttpMultiW,
   {$ENDIF DELPHI11}
    // VCL only
    OverbyteIcsMultiProgressBar,
    OverbyteIcsEmulVT,
    OverbyteIcsTnCnx,
    OverbyteIcsTnEmulVT,
    OverbyteIcsTnScript,
    {$IFNDEF BCB}
      OverbyteIcsWSocketTS,
    {$ENDIF}
  {$ENDIF VCL}
  {$IFDEF ICS_COMMON}
    OverbyteIcsMimeDec,
    OverbyteIcsMimeUtils,
    OverbyteIcsTimeList,
    OverbyteIcsLogger,
    {$IFNDEF BCB}
      OverbyteIcsCookies,
    {$ENDIF !BCB}
  {$ENDIF ICS_COMMON}
  {$IFDEF RTL_NAMESPACES}System.SysUtils{$ELSE}SysUtils{$ENDIF},
  {$IFDEF RTL_NAMESPACES}System.Classes{$ELSE}Classes{$ENDIF},
    DesignIntf, DesignEditors;    { V9.3 }

{ V9.3 used to add required units to projects }
{$IFDEF COMPILER16_UP}
type
  TIcsTypesSelEdt = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  TIcsSslTypesSelEdt = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;
{$ENDIF}

procedure Register;

implementation

{$IFDEF COMPILER10_UP}
    {$IFDEF MSWINDOWS}
uses
        {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
        ToolsApi;
        {$IFDEF COMPILER6_UP}
//          DesignIntf, DesignEditors;
        {$ELSE}
//          DsgnIntf;
        {$ENDIF}
    {$ENDIF}
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
{$IFDEF COMPILER16_UP}
{$IFDEF VCL_OR_FMX}
var
    LClassGroup: TPersistentClass;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF COMPILER16_UP}
  {$IFDEF VCL_OR_FMX}
    {$IFDEF FMX}
      LClassGroup := TFmxObject;
    {$ELSE}
      LClassGroup := TControl;
    {$ENDIF}
    GroupDescendentsWith(TIcsWndControl, LClassGroup);
    GroupDescendentsWith(TDnsQuery, LClassGroup);
    GroupDescendentsWith(TFingerCli, LClassGroup);
  {$ENDIF VCL_OR_FMX}
{$ENDIF COMPILER16_UP}

{$IFDEF VCL_OR_FMX}
    RegisterComponents('Overbyte ICS', [
      TWSocket, TWSocketServer,
      THttpCli, THttpServer, THttpAppSrv, TMultipartHttpDownloader,
      TFtpClient, TFtpServer, TMultipartFtpDownloader,
      TSmtpCli, TSyncSmtpCli, THtmlSmtpCli,
      TPop3Cli, TSyncPop3Cli,
      TNntpCli, THtmlNntpCli,
      TDnsQuery, TFingerCli, TPing,
      TIcsCharsetComboBox,
      {$IFDEF DELPHI11}
        TFtpClientW,    { V8.60 }
        TFtpServerW,    { V8.60 }
        TIcsFileCopyW,  { V8.60 }
      {$ENDIF}
      TIcsBlacklist,     { V8.60 }
      TIcsFileCopy,      { V8.60 }
      TIcsDomainNameCache  { V8.71 }
    ]);

{ V9.3 used to add required units to projects }
{$IFDEF COMPILER16_UP}
   RegisterSelectionEditor(TWSocket, TIcsTypesSelEdt);
   RegisterSelectionEditor(TWSocketServer, TIcsTypesSelEdt);
   RegisterSelectionEditor(THttpCli, TIcsTypesSelEdt);
   RegisterSelectionEditor(THttpServer, TIcsTypesSelEdt);
   RegisterSelectionEditor(THttpAppSrv, TIcsTypesSelEdt);
   RegisterSelectionEditor(TMultipartHttpDownloader, TIcsTypesSelEdt);
   RegisterSelectionEditor(TFtpClient, TIcsTypesSelEdt);
   RegisterSelectionEditor(TFtpServer, TIcsTypesSelEdt);
   RegisterSelectionEditor(TMultipartFtpDownloader, TIcsTypesSelEdt);
   RegisterSelectionEditor(TSmtpCli, TIcsTypesSelEdt);
   RegisterSelectionEditor(TSyncSmtpCli, TIcsTypesSelEdt);
   RegisterSelectionEditor(THtmlSmtpCli, TIcsTypesSelEdt);
   RegisterSelectionEditor(TPop3Cli, TIcsTypesSelEdt);
   RegisterSelectionEditor(TSyncPop3Cli, TIcsTypesSelEdt);
   RegisterSelectionEditor(TNntpCli, TIcsTypesSelEdt);
   RegisterSelectionEditor(THtmlNntpCli, TIcsTypesSelEdt);
   RegisterSelectionEditor(TDnsQuery, TIcsTypesSelEdt);
   RegisterSelectionEditor(TFingerCli, TIcsTypesSelEdt);
   RegisterSelectionEditor(TPing, TIcsTypesSelEdt);
   RegisterSelectionEditor(TIcsFileCopy, TIcsTypesSelEdt);
   RegisterSelectionEditor(TIcsBlacklist, TIcsTypesSelEdt);
   RegisterSelectionEditor(TIcsDomainNameCache, TIcsTypesSelEdt);
{$ENDIF}

{$ENDIF}
{$IFDEF VCL}
    RegisterComponents('Overbyte ICS', [
      { Not yet ported to FMX }
      TEmulVT, TTnCnx, TTnEmulVT, TTnScript,
      {$IFNDEF BCB}
        TWSocketThrdServer,
      {$ENDIF}
      TMultiProgressBar,
      TSysLogClient,
      TSysLogServer,
      TSnmpCli,
      TSmtpServer,
      TIcsWhoisCli,      { V8.60 }
      TIcsTimeServer,    { V8.60 }
      TIcsTimeClient,    { V8.60 }
      TIcsMonSocket,     { V8.71 }
      TIcsMonPcap,       { V8.71 }
      TIcsIpChanges,     { V8.71 }
      TIcsNeighbDevices  { V8.71 }
    ]);

{ V9.3 used to add required units to projects }
{$IFDEF COMPILER16_UP}
   RegisterSelectionEditor(TSysLogClient, TIcsTypesSelEdt);
   RegisterSelectionEditor(TSysLogServer, TIcsTypesSelEdt);
   RegisterSelectionEditor(TSnmpCli, TIcsTypesSelEdt);
   RegisterSelectionEditor(TSmtpServer, TIcsTypesSelEdt);
   RegisterSelectionEditor(TSmtpServer, TIcsTypesSelEdt);
   RegisterSelectionEditor(TIcsTimeServer, TIcsTypesSelEdt);
   RegisterSelectionEditor(TIcsTimeClient, TIcsTypesSelEdt);
   RegisterSelectionEditor(TIcsMonSocket, TIcsTypesSelEdt);
   RegisterSelectionEditor(TIcsIpChanges, TIcsTypesSelEdt);
   RegisterSelectionEditor(TIcsNeighbDevices, TIcsTypesSelEdt);
{$ENDIF}

{$ENDIF VCL}
{$IFDEF ICS_COMMON}
    RegisterComponents('Overbyte ICS', [
      { Components neither depending on the FMX nor on the VCL package }
      TMimeDecode,
      TMimeDecodeEx,
      TMimeDecodeW,
      TMimeTypesList,
   {$IFNDEF BCB}
      TIcsCookies,
   {$ENDIF !BCB}
      TTimeList,
      TIcsLogger
    ]);
{$ENDIF}

{$IFDEF USE_SSL}
  {$IFDEF COMPILER16_UP}
    {$IFDEF VCL_OR_FMX}
      GroupDescendentsWith(TSslBaseComponent, LClassGroup);
    {$ENDIF VCL_OR_FMX}
  {$ENDIF COMPILER16_UP}

  {$IFDEF VCL_OR_FMX}
    RegisterComponents('Overbyte ICS SSL', [
      TSslWSocket, TSslWSocketServer,
      TSslContext,
      TSslFtpClient, TSslFtpServer,
      TSslHttpCli, TSslHttpServer, TSslHttpAppSrv,
      TSslPop3Cli,
      TSslSmtpCli, TSslHtmlSmtpCli,
      TSslNntpCli,
      TSslAvlSessionCache,
      TIcsProxy,
      TIcsHttpProxy,
      TSslHttpRest,   { V8.54 }
      TSimpleWebSrv,  { V8.54 }
      TRestOAuth,     { V8.54 }
      TSslX509Certs,  { V8.54 }
      TIcsMailQueue,  { V8.60 }
      TIcsIpStrmLog,  { V8.60 }
      TIcsFtpMulti,   { V8.60 }
      TIcsHttpMulti,  { V8.60 }
      TDnsQueryHttps, { V8.61 }
      TIcsSms,        { V8.61 }
      TIcsTwitter,    { V8.65 }
      TIcsRestEmail,  { V8.65 }
      TOcspHttp,      { V8.69 }
      {$IFDEF COMPILER11_UP}
        TOAuthBrowser,       { V8.71 not Delphi 7 }
      {$ENDIF}
      TSslWebSocketCli,      { V8.71 }
      TIcsDomNameCacheHttps, { V8.71 }
      TX509Base,             { V9.1 }
      {$IFDEF DELPHI11}
        TSslFtpClientW,  { V8.60 }
        TSslFtpServerW,  { V8.60 }
        TIcsFtpMultiW,   { V8.60 }
        TIcsHttpMultiW,  { V8.60 }
      {$ENDIF}
    {$IFDEF VCL}
      {$IFNDEF BCB}
        TSslWSocketThrdServer,
      {$ENDIF}
        TSslSmtpServer,
        TIcsMQTTServer,      { V8.71 }
        TIcsMQTTClient,      { V8.71 }
      {$IFDEF COMPILER27_UP}              // !! TEMP D10.4 and later
        TIcsAppMonCli,         { V9.3 }
        TIcsAppMonSrv,         { V9.4 }
      {$ENDIF}
    {$ENDIF VCL}
    {$IFNDEF OPENSSL_NO_ENGINE}
    //  TSslEngine,        V9.1 gone
    {$ENDIF}
      TIcsInetAlive   { V8.66 }
    ]);

{ V9.3 used to add required units to projects }
{$IFDEF COMPILER16_UP}
    {$IFDEF VCL_OR_FMX}
       RegisterSelectionEditor(TSslWSocket, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TSslWSocketServer, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TSslHttpCli, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TSslHttpServer, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TSslHttpAppSrv, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TSslFtpClient, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TSslFtpServer, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TSslSmtpCli, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TSslHtmlSmtpCli, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TSslPop3Cli, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TSslNntpCli, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TIcsProxy, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TIcsHttpProxy, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TSslHttpRest, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TSimpleWebSrv, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TRestOAuth, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TSslX509Certs, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TIcsIpStrmLog, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TIcsFtpMulti, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TIcsHttpMulti, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TDnsQueryHttps, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TOAuthBrowser, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TSslWebSocketCli, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TIcsDomNameCacheHttps, TIcsSslTypesSelEdt);
    {$IFDEF VCL}
       RegisterSelectionEditor(TSslSmtpServer, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TIcsMQTTServer, TIcsSslTypesSelEdt);
       RegisterSelectionEditor(TIcsMQTTServer, TIcsSslTypesSelEdt);
    {$ENDIF VCL}
       RegisterSelectionEditor(TIcsInetAlive, TIcsSslTypesSelEdt);

    {$ENDIF VCL_OR_FMX}
{$ENDIF}

  {$ENDIF VCL_OR_FMX}
{$ENDIF USE_SSL}

{$IFDEF VCL_OR_FMX}
    RegisterPropertyEditor(TypeInfo(AnsiString), TWSocket, 'LineEnd', TWSocketLineEndProperty);
{$ENDIF}

{$IFDEF COMPILER10_UP}
  {$IFNDEF COMPILER16_UP}
    {$IFDEF ICS_COMMON}
      ForceDemandLoadState(dlDisable); // Required to show our product icon on splash screen
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

end;

{ V9.3 used to add required units to projects }
{$IFDEF COMPILER16_UP}
procedure TIcsTypesSelEdt.RequiresUnits(Proc: TGetStrProc);
begin
    inherited RequiresUnits(Proc);
    Proc('OverbyteIcsTypes');
end;

procedure TIcsSslTypesSelEdt.RequiresUnits(Proc: TGetStrProc);
begin
    inherited RequiresUnits(Proc);
    Proc('OverbyteIcsTypes');
{$IFDEF USE_SSL}
  {$IFDEF FMX}
    Proc('Ics.Fmx.OverbyteIcsSslBase');
  {$ELSE}
    Proc('OverbyteIcsSslBase');
  {$ENDIF}
{$ENDIF USE_SSL}
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER10_UP}
{$IFDEF VCL}
{$R OverbyteIcsProductIcon.res}
const
{$IFDEF COMPILER14_UP}
    sIcsSplashImg       = 'ICSPRODUCTICONBLACK';
{$ELSE}
    {$IFDEF COMPILER10}
        sIcsSplashImg   = 'ICSPRODUCTICONBLACK';
    {$ELSE}
        sIcsSplashImg   = 'ICSPRODUCTICON';
    {$ENDIF}
{$ENDIF}
    sIcsLongProductName = 'Internet Component Suite V9.4';
    sIcsFreeware        = 'Freeware';
    sIcsDescription     = sIcsLongProductName + #13#10 +
                          //'Copyright (C) 1996-2024 by Fran�ois PIETTE'+ #13#10 +
                          // Actually there's source included with different
                          // copyright, so either all or none should be mentioned
                          // here.
                          'https://www.overbyte.eu/' + #13#10 +
                          'Wiki: https://wiki.overbyte.eu/' + #13#10 +
                          'Support: https://en.delphipraxis.net/forum/37-ics-internet-component-suite/' + #13#10 +
                          'svn://svn.overbyte.be/icsv9' + #13#10 +
                          'https://svn.overbyte.be/svn/icsv9' + #13#10 +
                          'User and password = "ics"';

var
    AboutBoxServices: IOTAAboutBoxServices = nil;
    AboutBoxIndex: Integer = -1;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure PutIcsIconOnSplashScreen;
var
    hImage: HBITMAP;
begin
    if Assigned(SplashScreenServices) then begin
        hImage := LoadBitmap(FindResourceHInstance(HInstance), sIcsSplashImg);
        SplashScreenServices.AddPluginBitmap(sIcsLongProductName, hImage,
                                             FALSE, sIcsFreeware);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RegisterAboutBox;
begin
    if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then begin
        AboutBoxIndex := AboutBoxServices.AddPluginInfo(sIcsLongProductName,
          sIcsDescription, 0, FALSE, sIcsFreeware);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure UnregisterAboutBox;
begin
    if (AboutBoxIndex <> -1) and Assigned(AboutBoxServices) then begin
        AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
        AboutBoxIndex := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

initialization
    PutIcsIconOnSplashScreen;
    RegisterAboutBox;

finalization
    UnregisterAboutBox;
{$ENDIF VCL}
{$ENDIF COMPILER10_UP}
end.

