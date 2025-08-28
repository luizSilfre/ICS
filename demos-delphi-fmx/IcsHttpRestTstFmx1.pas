{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  ICS FMX HTTPS REST functions demo.
Creation:     Dec 2022
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
Dec 19, 2022 - V8.71 baseline, converted from VCL sample.
Aug 08, 2023 V9.0  Updated version to major release 9.
Feb 07, 2024 V9.1  Added OverbyteIcsSslBase which now includes TSslContext,TX509Base and TX509List.
Aug 07, 2024 V9.3  Using OverbyteIcsTypes for consolidated types and constants, allowing
                     other import units to be removed.


Pending - WebSocket page from VCL sample, more features from VCL sample

Note - only tested on Windows.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

unit IcsHttpRestTstFmx1;

{$I Include\OverbyteIcsDefs.inc}
{$DEFINE FMX}

{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }

interface

uses
WinApi.Windows,Messages,System.SysUtils,System.Variants,System.Classes,
FMX.Controls,FMX.Forms,FMX.Dialogs,TypInfo,FMX.Grid,FMX.StdCtrls,System.Rtti,
FMX.Grid.Style,FMX.Memo.Types,FMX.ListView.Types,FMX.ListView.Appearances,
FMX.ListView.Adapters.Base,FMX.Types,FMX.ListView,FMX.Layouts,FMX.ListBox,
FMX.Memo,FMX.Edit,FMX.ScrollBox,FMX.ComboEdit,FMX.TabControl,
FMX.Controls.Presentation,System.Types,System.UITypes,
ActiveX, System.ImageList, FMX.ImgList,
Ics.Fmx.OverbyteIcsWSocket,
Ics.Fmx.OverbyteIcsWndControl,
OverbyteIcsIniFiles,
OverbyteIcsTypes,
OverbyteIcsUtils,
OverbyteIcsMimeUtils,
OverbyteIcsURL,
//OverbyteIcsLogger,{ for TLogOption }
//OverbyteIcsSSLEAY,
//OverbyteIcsLibeay,
Ics.Fmx.OverbyteIcsSslHttpRest,
Ics.Fmx.OverbyteIcsSslHttpOAuth,
Ics.Fmx.OverbyteIcsHttpProt,
OverbyteIcsSuperObject,
OverbyteIcsSslJose,
//OverbyteIcsBlacklist,
OverbyteIcsDnsQuery,
OverbyteIcsSuperXMLParser,
Ics.Fmx.OverbyteIcsSmtpProt,
OverbyteIcsOAuthFormFmx,
Ics.Fmx.OverbyteIcsDnsHttps,
Ics.Fmx.OverbyteIcsSslBase,
Ics.Fmx.OverbyteIcsSslUtils;      { V9.3 }

const
    WM_401REPEAT = WM_USER + 758 ;

type
  THttpRestForm = class(TForm)
 // properties saved
    AuthBearer: TEdit;
    AuthLogin: TEdit;
    AuthPassword: TEdit;
    ExtraHeaders: TMemo;
    OAuthScope: TEdit;
    OAuthOptNoRedir: TCheckBox;
    OAuthAutoRefresh: TCheckBox;
    OAuthRefrMins: TEdit;
    OAuthAccToken: TEdit;
    OAuthAppUrl: TEdit;
    OAuthAuthCode: TEdit;
    OAuthClientId: TEdit;
    OAuthClientSecret: TEdit;
    OAuthExpire: TEdit;
    OAuthRedirectUrl: TEdit;
    OAuthRefToken: TEdit;
    OAuthTokenUrl: TEdit;
    OAuthWebIP: TComboEdit;
    OAuthWebPort: TEdit;
    ProxyURL: TEdit;
    RawParams: TEdit;
    ReportCertChain: TCheckBox;
    RestURL: TComboEdit;
    SslClientCertFile: TEdit;
    SslRootBundleFile: TEdit;
    DirLogs: TEdit;
    DnsHttpsUrl: TComboEdit;
    DnsDomainName: TComboEdit;
    DnsQueryType: TComboEdit;
    DnsDnssec: TCheckBox;
    DnsNoValidation: TCheckBox;
    SmsAccSender: TEdit;
    SmsMsgText: TMemo;
    SmsDestNums: TMemo;
    KapowAccPw: TEdit;
    KapowAccName: TEdit;
    AlpnProtos: TEdit;
    OAuthPrompt: TEdit;
    OAuthAccess: TCheckBox;
    TwitAccSecret: TEdit;
    TwitAccToken: TEdit;
    TwitApiKey: TEdit;
    TwitApiSecret: TEdit;
    TwitForceLogin: TCheckBox;
    TwitIdList: TEdit;
    TwitMsg: TMemo;
    TwitQuery: TEdit;
    TwitScrnId: TEdit;
    TwitScrnName: TEdit;
    OAuthConsoleUrl: TEdit;
    EmailClientId: TEdit;
    EmailAccToken: TEdit;
    EmailRefrToken: TEdit;
    EmailAccount: TEdit;
    EmailForceLogin: TCheckBox;
    EmailTokenAccnt: TEdit;
    EmailAccExpiry: TEdit;
    EmailRestType: TComboEdit;
    EmailClientSecret: TEdit;
    EmailRecipList: TMemo;
    EmailFrom: TEdit;
    EmailCC: TEdit;
    EmailSubject: TEdit;
    EmailMessage: TMemo;
    EmailAccountHint: TEdit;
    ReqAccept: TComboEdit;
    ReqReplFile: TCheckBox;
    RestDownFile: TEdit;
    CertRevoke: TCheckBox;
    HttpUploadFile: TEdit;
    OAuthEdgeCache: TEdit;
    SslSecurity: TListBox;
    AuthType: TListBox;
    CertVerMethod: TListBox;
    OAuthAuthType: TListBox;
    AliveMethod: TListBox;
    AliveNets: TListBox;
    ReqMemStrategy: TListBox;
    HttpUploadStrat: TListBox;
    EmailAuthType: TListBox;
    TwitAuthType: TListBox;
    ReqMode: TListBox;
    ParamContent: TListBox;
    ReqType: TListBox;
    IpSockFamily: TListBox;

 // properties not saved
    LogWin: TMemo;
    Label1: TLabel;
    Label3: TLabel;
    doStartReq: TButton;
    Label5: TLabel;
    HttpRest1: TSslHttpRest;
    doClear: TButton;
    doAbort: TButton;
    PageControl1: TTabControl;
    TabREST: TTabItem;
    TabSettings: TTabItem;
    TabOAuth: TTabItem;
    Label2: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    GroupBox1: TGroupBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    GroupBox2: TGroupBox;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    doOAuthLogin: TButton;
    doOAuthToken: TButton;
    doOAuthRefresh: TButton;
    doTestRedir: TButton;
    RestOAuth1: TRestOAuth;
    doGrantCred: TButton;
    doGrantPassword: TButton;
    Label21: TLabel;
    LabelResult: TLabel;
    TabDNSHTTPS: TTabItem;
    TabTwitter: TTabItem;
    Label22: TLabel;
    OpenDirDiag: TOpenDialog;
    Label23: TLabel;
    Label222: TLabel;
    Label25: TLabel;
    doDNSJson: TButton;
    TabSms: TTabItem;
    DnsQueryHttps1: TDnsQueryHttps;
    doDnsQuery1: TButton;
    doDnsQueryAll: TButton;
    TimerLog: TTimer;
    IcsSMS1: TIcsSMS;
    BoxSmsMsg: TGroupBox;
    Label26: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    GroupBox3: TGroupBox;
    Label24: TLabel;
    Label27: TLabel;
    doKapowSend: TButton;
    doKapowCheck: TButton;
    doKapowCredit: TButton;
    LabelKapowCredit: TLabel;
    BoxSmsWorks: TGroupBox;
    Label30: TLabel;
    doSmsWorksSend: TButton;
    doSmsWorksCheck: TButton;
    doSmsWorksCredit: TButton;
    LabelSmsWorksCredits: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    BoxTritterSettings: TGroupBox;
    Label36: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label40: TLabel;
    Label35: TLabel;
    doTwitLogin: TButton;
    Label41: TLabel;
    doTwitMsg: TButton;
    TwitResultLabel: TLabel;
    Label37: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    doTwitAccSett: TButton;
    doTwitQuery: TButton;
    doTwitGetId: TButton;
    TabEmail: TTabItem;
    TabEmailSet: TTabItem;
    GroupBox4: TGroupBox;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    EmailOAResultLabel: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    LabelSmsWorksResult: TLabel;
    LabelKapowResult: TLabel;
    OAuthAccTitle: TComboEdit;
    Label54: TLabel;
    Label55: TLabel;
    doOAuthConsole: TButton;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    doEmailSend: TButton;
    doEmaiTstRedir: TButton;
    doEmailOALogin: TButton;
    doEmailOARefresh: TButton;
    doEmailOACons: TButton;
    Label59: TLabel;
    Label60: TLabel;
    SslHtmlSmtpCli1: TSslHtmlSmtpCli;
    doEmailList: TButton;
    doEmailRead: TButton;
    Label50: TLabel;
    Label61: TLabel;
    EmailReadMessId: TListBox;
    doEmailHdrs: TButton;
    EmailRawHdrs: TCheckBox;
    LabelEmailResult: TLabel;
    doEmailDelete: TButton;
    IcsTwitter1: TIcsTwitter;
    IcsRestEmail1: TIcsRestEmail;
    doClearParams: TButton;
    EmailHdrFolder: TListBox;
    Label62: TLabel;
    TabInetAlive: TTabItem;
    IcsInetAlive1: TIcsInetAlive;
    GroupBoxAlive: TGroupBox;
    doAliveStart: TButton;
    doAliveStop: TButton;
    LabelInetAlive: TLabel;
    doListCertStore: TButton;
    Label63: TLabel;
    Label64: TLabel;
    LabelProgress: TLabel;
    doExit: TButton;
    Label65: TLabel;
    Label66: TLabel;
    EmailUserAuth: TComboEdit;
    OAuthUserAuth: TComboEdit;
    Label67: TLabel;
    Label68: TLabel;
    Label69: TLabel;
    DebugLogging: TListBox;
    ImageList1: TImageList;
    GridJsonResp: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    GridParams: TStringGrid;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    StringColumn7: TStringColumn;
    StringColumn8: TStringColumn;
    SmsWorksJWT: TEdit;
    OAuthBrowser1: TOAuthBrowser;
    Label70: TLabel;
    OAuthLoginHint: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure HttpRest1HttpRestProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure HttpRest1RestRequestDone(Sender: TObject; RqType: THttpRequest;
      ErrCode: Word);
    procedure doStartReqClick(Sender: TObject);
    procedure doAbortClick(Sender: TObject);
    procedure doClearClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure doTestRedirClick(Sender: TObject);
    procedure RestOAuth1OAuthAuthUrl(Sender: TObject; const URL: string);
    procedure doOAuthLoginClick(Sender: TObject);
    procedure doOAuthTokenClick(Sender: TObject);
    procedure doOAuthRefreshClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure doGrantCredClick(Sender: TObject);
    procedure doGrantPasswordClick(Sender: TObject);
    procedure RestOAuth1OAuthNewCode(Sender: TObject);
    procedure RestOAuth1OAuthNewToken(Sender: TObject);
    procedure SettingsChange(Sender: TObject);
    procedure SelDirLogsClick(Sender: TObject);
    procedure doDNSJsonClick(Sender: TObject);
    procedure DnsQueryHttps1DnsProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure DnsQueryHttps1RequestDone(Sender: TObject; Error: Word);
    procedure doDnsQueryAllClick(Sender: TObject);
    procedure doDnsQuery1Click(Sender: TObject);
    procedure TimerLogTimer(Sender: TObject);
    procedure IcsSMS1SmsDone(Sender: TObject);
    procedure IcsSMS1SmsProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure doKapowSendClick(Sender: TObject);
    procedure doKapowCheckClick(Sender: TObject);
    procedure doKapowCreditClick(Sender: TObject);
    procedure doSmsWorksCreditClick(Sender: TObject);
    procedure doSmsWorksSendClick(Sender: TObject);
    procedure doSmsWorksCheckClick(Sender: TObject);
    procedure RestOAuth1OAuthProg(Sender: TObject; LogOption: TLogOption;
      const Msg: string);
    procedure doTwitLoginClick(Sender: TObject);
    procedure TwitSetup;
    procedure doTwitMsgClick(Sender: TObject);       { V8.65 }
    procedure doTwitAccSettClick(Sender: TObject);
    procedure doTwitQueryClick(Sender: TObject);
    procedure doTwitGetIdClick(Sender: TObject);
    procedure OAuthAccTitleChange(Sender: TObject);
    procedure doOAuthConsoleClick(Sender: TObject);
    procedure doEmailOAConsClick(Sender: TObject);
    procedure doEmaiTstRedirClick(Sender: TObject);
    procedure doEmailOALoginClick(Sender: TObject);
    procedure doEmailOARefreshClick(Sender: TObject);
    procedure doEmailSendClick(Sender: TObject);
    procedure doEmailListClick(Sender: TObject);
    procedure doEmailReadClick(Sender: TObject);         { V8.65 }
    procedure EmailSetup;       { V8.65 }
    procedure doEmailHdrsClick(Sender: TObject);
    procedure SslHtmlSmtpCli1BeforeOutStreamFree(Sender: TObject);
    procedure doEmailDeleteClick(Sender: TObject);
    procedure IcsTwitter1TwitNewToken(Sender: TObject);
    procedure IcsRestEmail1EmailNewToken(Sender: TObject);
    procedure doClearParamsClick(Sender: TObject);
    procedure IcsInetAlive1AliveChange(Sender: TObject);
    procedure doAliveStopClick(Sender: TObject);
    procedure doAliveStartClick(Sender: TObject);
    procedure doListCertStoreClick(Sender: TObject);
    procedure SelDirDownClick(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure WM401REPEAT (var Msg : TMessage); message WM_401REPEAT ;
    procedure SelDirUploadClick(Sender: TObject);
    procedure GridJsonRespCellDblClick(const Column: TColumn; const Row: Integer);
    procedure GridParamsCellClick(const Column: TColumn; const Row: Integer);
    procedure RestOAuth1OAuthEmbed(Sender: TObject; const URL: string; var Success: Boolean);
//    procedure OAuthFormLogEvent(Line: String);                             { V8.71 }
//    procedure OAuthFormRedirEvent(NewUrl: String; var CanClose: Boolean);  { V8.71 }
  private
    { Private declarations }
    FNotifyWindow : HWND;     // FMX
{$IFDEF POSIX}
    { This is only required if we want to post/send custom messages as in Windows }
    FMessagePump  : TIcsMessagePump;
{$ENDIF}
    procedure WndProc(var Msg: TMessage);                              // FMX
  public
    { Public declarations }
    FProgDir: String;
    FIniFileName: String;
    FCookieFileName: String;
    FInitialized: Boolean;
    FIcsBuffLogStream: TIcsBuffLogStream;  { V8.60 }
    procedure AddLog (const S: string) ;
    procedure RestOAuthSetup;
    procedure OpenLogFile;
    procedure CommonRestSettings;
    procedure DisplayJson(RespObj: ISuperObject);

  end;

var
  HttpRestForm: THttpRestForm;
  BuffLogLines: String;  { V8.61 }
  KapowSentId: String;   { V8.61 }
  SmsWorksSentId: String; { V8.62 }
  OAuthUris: TOAuthUris;  { V8.65 }
  SmtpRawMsg: String;             { V8.65 }
  JArrayTot: Integer;              { V8.65 }
  JArrayItems: Array of String;    { V8.65 }

implementation

{$R *.fmx}

Uses IcsHttpRestTstFmx2, IcsLoginFmx;

const
    SectionMainWindow    = 'MainWindow';
    SectionObjWindow     = 'ObjWindow';
    KeyTop               = 'Top';
    KeyLeft              = 'Left';
    KeyWidth             = 'Width';
    KeyHeight            = 'Height';
    SectionData          = 'Data';
    KeyRestParams        = 'RestParams';

procedure THttpRestForm.WndProc(var Msg: TMessage);         // FMX
begin
    try
        { We have to handle all exceptions }
        if Msg.Msg = WM_401REPEAT then
        begin
            WM401REPEAT (Msg);
        end
        else
            Msg.Result := DefWindowProc(FNotifyWindow, Msg.Msg, Msg.WParam, Msg.LParam);
    except
        Application.HandleException(Self);
    end;
end;


procedure THttpRestForm.FormCreate(Sender: TObject);
begin
{$IF CompilerVersion > 17}
{$WARNINGS OFF}
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);     { V8.69 }
{$WARNINGS ON}
{$IFEND}
{$IFDEF POSIX}
    FMessagePump  := TIcsMessagePump.Create;
{$ENDIF}
    FNotifyWindow := AllocateHWND(WndProc);   // FMX

    FProgDir     := ExtractFilePath(ParamStr(0));
    FIniFileName := GetIcsIniFileName;
    FCookieFileName := ChangeFileExt(FIniFileName, '.cookie');
    LogWin.Lines.Add('INI File: ' + FIniFileName) ;

// Avoid dynamical loading and unloading the SSL DLLs plenty of times
//  GSSLEAY_DLL_IgnoreNew := True;     { ignore OpenSSL 3.0 and later }
//  GSSLEAY_DLL_IgnoreOld := True;     { ignore OpenSSL 1.1 }
// note both not allowed true
//    GSSL_DLL_DIR := FProgDir;          { only from our directory }
//    GSSL_SignTest_Check := True;       { check digitally signed }
//    GSSL_SignTest_Certificate := True; { check digital certificate }

    IcsSslRootCAStore.Initialise;   { loads OpenSSL and certificate bundle }
    LogWin.Lines.Add(IcsReportOpenSSLVer);
    LogWin.Lines.Add('Built With ' + IcsBuiltWithEx);                             { V8.70 }
end;

procedure THttpRestForm.FormDestroy(Sender: TObject);
begin
    if FNotifyWindow <> 0 then             // FMX
        DeallocateHWND(FNotifyWindow);
{$IFDEF POSIX}
    FMessagePump.Free;
{$ENDIF}
    Ics.Fmx.OverbyteIcsWSocket.UnLoadSsl;     { V8.69 }
end;

procedure THttpRestForm.FormShow(Sender: TObject);
var
    IniFile: TIcsIniFile;
    SL: TStringList;
    I, J, K, tot: Integer;
    Level: TSslCliSecurity;
    Auth: THttpAuthType;
begin
    if not FInitialized then begin
        FInitialized := TRUE;

    // V8.55 update SSL client security levels
        SslSecurity.Items.Clear;
        for Level := Low(TSslCliSecurity) to High(TSslCliSecurity) do
             SslSecurity.Items.Add (SslCliSecurityNames[Level]);

    // Dns Query drop down
        DnsQueryType.Items.Clear;
        for I := Low(DnsReqTable) to High(DnsReqTable) do
             DnsQueryType.Items.Add (DnsReqTable[I].Asc +
                                    ' [' + DnsReqTable[I].Desc + ']');

    // Dns HTTPS URL drop down
        DnsHttpsUrl.Items.Clear;
        for I := Low(DnsPublicHttpsTable) to High(DnsPublicHttpsTable) do
             DnsHttpsUrl.Items.Add (DnsPublicHttpsTable[I]);

    // V8.69 authentication list
       AuthType.Items.Clear;
       for Auth  := Low(THttpAuthType) to High(THttpAuthType) do
            AuthType.Items.Add(HttpCliAuthNames[Auth]);

    // form positions
        IniFile := TIcsIniFile.Create(FIniFileName);
        Self.Width := IniFile.ReadInteger(SectionMainWindow, KeyWidth,  Width);
        Self.Height := IniFile.ReadInteger(SectionMainWindow, KeyHeight, Height);
        Self.Top := IniFile.ReadInteger(SectionMainWindow, KeyTop, (Trunc(Screen.Height) - Height) div 2);
        Self.Left := IniFile.ReadInteger(SectionMainWindow, KeyLeft, (Trunc(Screen.Width)  - Width) div 2);
        JsonObjectWin.Width := IniFile.ReadInteger(SectionObjWindow, KeyWidth,  JsonObjectWin.Width);  { V8.63 another form }
        JsonObjectWin.Height := IniFile.ReadInteger(SectionObjWindow, KeyHeight, JsonObjectWin.Height);
        JsonObjectWin.Top := IniFile.ReadInteger(SectionObjWindow, KeyTop, (Trunc(Screen.Height) - JsonObjectWin.Height) div 2);
        JsonObjectWin.Left := IniFile.ReadInteger(SectionObjWindow, KeyLeft, (Trunc(Screen.Width)  - JsonObjectWin.Width) div 2);

        SL := TStringList.Create;
        try
             SL.Delimiter := '|';
             SL.DelimitedText := IniFile.ReadString(SectionData, KeyRestParams, '');
             tot := SL.Count;
             GridParams.RowCount := (tot div 3) + 10;
             K := 0;
             for I := 0 to GridParams.RowCount - 1 do begin
                for J := 0 to GridParams.ColumnCount - 1 do begin
                    if K < tot then
                        GridParams.Cells[J, I] := SL[K];
                    K := K + 1;
                end;
             end;
        finally
             SL.Free;
        end;

       with IniFile do begin
          AuthBearer.Text := ReadString (SectionData, 'AuthBearer_Text', AuthBearer.Text) ;
          AuthLogin.Text := ReadString (SectionData, 'AuthLogin_Text', AuthLogin.Text) ;
          AuthPassword.Text := ReadString (SectionData, 'AuthPassword_Text', AuthPassword.Text) ;
          AuthType.ItemIndex := ReadInteger (SectionData, 'AuthType_ItemIndex', AuthType.ItemIndex) ;
          CertVerMethod.ItemIndex := ReadInteger (SectionData, 'CertVerMethod_ItemIndex', CertVerMethod.ItemIndex) ;
          DebugLogging.ItemIndex := ReadInteger (SectionData, 'DebugLogging_ItemIndex', DebugLogging.ItemIndex) ;
          ExtraHeaders.Lines.CommaText := ReadString (SectionData, 'ExtraHeaders_Lines', '') ;
          OAuthScope.Text := ReadString (SectionData, 'OAuthScope_Text', OAuthScope.Text) ;
          if ReadString (SectionData, 'OAuthAutoRefresh_Checked', 'False') = 'True' then OAuthAutoRefresh.IsChecked := true else OAuthAutoRefresh.IsChecked := false ;
          if ReadString (SectionData, 'OAuthOptNoRedir_Checked', 'False') = 'True' then OAuthOptNoRedir.IsChecked := true else OAuthOptNoRedir.IsChecked := false ;
          OAuthRefrMins.Text := ReadString (SectionData, 'OAuthRefrMins_Text', OAuthRefrMins.Text) ;
          OAuthAccToken.Text := ReadString (SectionData, 'OAuthAccToken_Text', OAuthAccToken.Text) ;
          OAuthAppUrl.Text := ReadString (SectionData, 'OAuthAppUrl_Text', OAuthAppUrl.Text) ;
          OAuthAuthType.ItemIndex := ReadInteger (SectionData, 'OAuthAuthType_ItemIndex', OAuthAuthType.ItemIndex) ;
          OAuthClientId.Text := ReadString (SectionData, 'OAuthClientId_Text', OAuthClientId.Text) ;
          OAuthClientSecret.Text := ReadString (SectionData, 'OAuthClientSecret_Text', OAuthClientSecret.Text) ;
          OAuthExpire.Text := ReadString (SectionData, 'OAuthExpire_Text', OAuthExpire.Text) ;
          OAuthRedirectUrl.Text := ReadString (SectionData, 'OAuthRedirectUrl_Text', OAuthRedirectUrl.Text) ;
          OAuthRefToken.Text := ReadString (SectionData, 'OAuthRefToken_Text', OAuthRefToken.Text) ;
          OAuthTokenUrl.Text := ReadString (SectionData, 'OAuthTokenUrl_Text', OAuthTokenUrl.Text) ;
          OAuthWebIP.Text := ReadString (SectionData, 'OAuthWebIP_Text', OAuthWebIP.Text) ;
          OAuthWebPort.Text := ReadString (SectionData, 'OAuthWebPort_Text', OAuthWebPort.Text) ;
          ParamContent.ItemIndex := ReadInteger (SectionData, 'ParamContent_ItemIndex', ParamContent.ItemIndex) ;
          ProxyURL.Text := ReadString (SectionData, 'ProxyURL_Text', ProxyURL.Text) ;
          RawParams.Text := ReadString (SectionData, 'RawParams_Text', RawParams.Text) ;
          if ReadString (SectionData, 'ReportCertChain_Checked', 'False') = 'True' then ReportCertChain.IsChecked := true else ReportCertChain.IsChecked := false ;
          ReqMode.ItemIndex := ReadInteger (SectionData, 'ReqMode_ItemIndex', ReqMode.ItemIndex) ;
          ReqType.ItemIndex := ReadInteger (SectionData, 'ReqType_ItemIndex', ReqType.ItemIndex) ;
          RestURL.Text := ReadString (SectionData, 'RestURL_Text', RestURL.Text) ;
          SslClientCertFile.Text := ReadString (SectionData, 'SslClientCertFile_Text', SslClientCertFile.Text) ;
          SslRootBundleFile.Text := ReadString (SectionData, 'SslRootBundleFile_Text', SslRootBundleFile.Text) ;
          SslSecurity.ItemIndex := ReadInteger (SectionData, 'SslSecurity_ItemIndex', SslSecurity.ItemIndex) ;
          IpSockFamily.ItemIndex := ReadInteger (SectionData, 'IpSockFamily_ItemIndex', IpSockFamily.ItemIndex) ;
          DirLogs.Text := ReadString (SectionData, 'DirLogs_Text', DirLogs.Text) ;
          DnsHttpsUrl.Text := ReadString (SectionData, 'DnsHttpsUrl_Text', DnsHttpsUrl.Text) ;
          DnsDomainName.Text := ReadString (SectionData, 'DnsDomainName_Text', DnsDomainName.Text) ;
          DnsQueryType.ItemIndex := ReadInteger (SectionData, 'DnsQueryType_ItemIndex', DnsQueryType.ItemIndex) ;
          if ReadString (SectionData, 'DnsDnssec_Checked', 'False') = 'True' then DnsDnssec.IsChecked := true else DnsDnssec.IsChecked := false ;
          if ReadString (SectionData, 'DnsNoValidation_Checked', 'False') = 'True' then DnsNoValidation.IsChecked := true else DnsNoValidation.IsChecked := false ;
          KapowAccName.Text := ReadString (SectionData, 'KapowAccName_Text', KapowAccName.Text) ;
          KapowAccPw.Text := ReadString (SectionData, 'KapowAccPw_Text', KapowAccPw.Text) ;
          SmsAccSender.Text := ReadString (SectionData, 'SmsAccSender_Text', SmsAccSender.Text) ;
          SmsMsgText.Lines.CommaText := ReadString (SectionData, 'SmsMsgText_Lines', '') ;
          SmsDestNums.Lines.CommaText := ReadString (SectionData, 'SmsDestNums_Lines', '') ;
          KapowAccPw.Text := ReadString (SectionData, 'KapowAccPw_Text', KapowAccPw.Text) ;
          KapowAccName.Text := ReadString (SectionData, 'KapowAccName_Text', KapowAccName.Text) ;
          AlpnProtos.Text := ReadString (SectionData, 'AlpnProtos_Text', AlpnProtos.Text) ;
          OAuthPrompt.Text := ReadString (SectionData, 'OAuthPrompt_Text', OAuthPrompt.Text) ;
          if ReadString (SectionData, 'OAuthAccess_Checked', 'False') = 'True' then OAuthAccess.IsChecked := true else OAuthAccess.IsChecked := false ;
          TwitAccSecret.Text := ReadString (SectionData, 'TwitAccSecret_Text', TwitAccSecret.Text) ;
          TwitAccToken.Text := ReadString (SectionData, 'TwitAccToken_Text', TwitAccToken.Text) ;
          TwitApiKey.Text := ReadString (SectionData, 'TwitApiKey_Text', TwitApiKey.Text) ;
          TwitApiSecret.Text := ReadString (SectionData, 'TwitApiSecret_Text', TwitApiSecret.Text) ;
          if ReadString (SectionData, 'TwitForceLogin_Checked', 'False') = 'True' then TwitForceLogin.IsChecked := true else TwitForceLogin.IsChecked := false ;
          TwitIdList.Text := ReadString (SectionData, 'TwitIdList_Text', TwitIdList.Text) ;
          TwitMsg.Lines.CommaText := ReadString (SectionData, 'TwitMsg_Lines', '') ;
          TwitQuery.Text := ReadString (SectionData, 'TwitQuery_Text', TwitQuery.Text) ;
          TwitScrnId.Text := ReadString (SectionData, 'TwitScrnId_Text', TwitScrnId.Text) ;
          TwitScrnName.Text := ReadString (SectionData, 'TwitScrnName_Text', TwitScrnName.Text) ;
          OAuthConsoleUrl.Text := ReadString (SectionData, 'OAuthConsoleUrl_Text', OAuthConsoleUrl.Text) ;
          EmailClientId.Text := ReadString (SectionData, 'EmailClientId_Text', EmailClientId.Text) ;
          EmailAccToken.Text := ReadString (SectionData, 'EmailAccToken_Text', EmailAccToken.Text) ;
          EmailRefrToken.Text := ReadString (SectionData, 'EmailRefrToken_Text', EmailRefrToken.Text) ;
          EmailAccount.Text := ReadString (SectionData, 'EmailAccount_Text', EmailAccount.Text) ;
          if ReadString (SectionData, 'EmailForceLogin_Checked', 'False') = 'True' then EmailForceLogin.IsChecked := true else EmailForceLogin.IsChecked := false ;
          EmailTokenAccnt.Text := ReadString (SectionData, 'EmailTokenAccnt_Text', EmailTokenAccnt.Text) ;
          EmailClientSecret.Text := ReadString (SectionData, 'EmailClientSecret_Text', EmailClientSecret.Text) ;
          EmailRecipList.Lines.CommaText := ReadString (SectionData, 'EmailRecipList_Lines', '') ;
          EmailFrom.Text := ReadString (SectionData, 'EmailFrom_Text', EmailFrom.Text) ;
          EmailCC.Text := ReadString (SectionData, 'EmailCC_Text', EmailCC.Text) ;
          EmailSubject.Text := ReadString (SectionData, 'EmailSubject_Text', EmailSubject.Text) ;
          EmailMessage.Lines.CommaText := ReadString (SectionData, 'EmailMessage_Lines', '') ;
          EmailAccExpiry.Text := ReadString (SectionData, 'EmailAccExpiry_Text', EmailAccExpiry.Text) ;
          EmailRestType.ItemIndex := ReadInteger (SectionData, 'EmailRestType_ItemIndex', EmailRestType.ItemIndex) ;
          EmailAccountHint.Text := ReadString (SectionData, 'EmailAccountHint_Text', EmailAccountHint.Text) ;
          ReqAccept.Text := ReadString (SectionData, 'ReqAccept_Text', ReqAccept.Text) ;
          AliveMethod.ItemIndex := ReadInteger (SectionData, 'AliveMethod_ItemIndex', AliveMethod.ItemIndex) ;
          AliveNets.ItemIndex := ReadInteger (SectionData, 'AliveNets_ItemIndex', AliveNets.ItemIndex) ;
          ReqMemStrategy.ItemIndex := ReadInteger (SectionData, 'ReqMemStrategy_ItemIndex', ReqMemStrategy.ItemIndex) ;
          if ReadString (SectionData, 'ReqReplFile_Checked', 'False') = 'True' then ReqReplFile.IsChecked := true else ReqReplFile.IsChecked := false ;
          RestDownFile.Text := ReadString (SectionData, 'RestDownFile_Text', RestDownFile.Text) ;
          if ReadString (SectionData, 'CertRevoke_Checked', 'False') = 'True' then CertRevoke.IsChecked := true else CertRevoke.IsChecked := false ;
          HttpUploadFile.Text := ReadString (SectionData, 'HttpUploadFile_Text', HttpUploadFile.Text) ;
          HttpUploadStrat.ItemIndex := ReadInteger (SectionData, 'HttpUploadStrat_ItemIndex', HttpUploadStrat.ItemIndex) ;
          OAuthUserAuth.Text := ReadString (SectionData, 'OAuthUserAuth_Text', OAuthUserAuth.Text) ;  { V8.70 }
          EmailUserAuth.Text := ReadString (SectionData, 'EmailUserAuth_Text', EmailUserAuth.Text) ;   { V8.70 }
          EmailAuthType.ItemIndex := ReadInteger (SectionData, 'EmailAuthType_ItemIndex', EmailAuthType.ItemIndex) ;     { V8.71 }
          TwitAuthType.ItemIndex := ReadInteger (SectionData, 'TwitAuthType_ItemIndex', TwitAuthType.ItemIndex) ;        { V8.71 }
          OAuthEdgeCache.Text := ReadString (SectionData, 'OAuthEdgeCache_Text', OAuthEdgeCache.Text) ;                  { V8.71 }
          SmsWorksJWT.Text := ReadString (SectionData, 'SmsWorksJWT_Text', SmsWorksJWT.Text) ;                           { V8.71 }
          OAuthLoginHint.Text := ReadString (SectionData, 'OAuthLoginHint_Text', OAuthLoginHint.Text) ;                  { V8.71 }
       end;
        IniFile.Free;
    end;

    if HttpRest1.SslRootFile = '' then
        SslRootBundleFile.Text := HttpRest1.SslRootFile;
    if DnsQueryType.ItemIndex < 0 then DnsQueryType.ItemIndex := 0;
    HttpRest1.RestCookies.LoadFromFile(FCookieFileName);
    OAuthWebIP.Items.Assign(LocalIPList);
    OAuthWebIP.Items.Insert(0, ICS_LOCAL_HOST_NAME);   { V8.65 means both IPv4/6 }
    OAuthWebIP.Items.Insert(1, ICS_LOCAL_HOST_V4);
    OAuthWebIP.Items.Insert(2, ICS_LOCAL_HOST_V6);

 { V8.65 build OAuth2 Account Title list  }
    OAuthAccTitle.Items.Clear;
    SetLength(OAuthUris, 7);
    OAuthUris[0] :=  OAuthUriNone;
    OAuthUris[1] :=  OAuthUriCertCenter;
    OAuthUris[2] :=  OAuthUriGoogle;
    OAuthUris[3] :=  OAuthUriMSRest;
    OAuthUris[4] :=  OAuthUriMSSmtp;
    OAuthUris[5] :=  OAuthUriTwitterOA2;
    OAuthUris[6] :=  OAuthUriSipgate;
    for I := 0 to 6 do
        OAuthAccTitle.Items.Add(OAuthUris[I].CAccName);
 { V8.70 }
    if OAuthUserAuth.Text = '' then
        OAuthUserAuth.Text := OAuthMsUserAuthDef;
    if EmailUserAuth.Text = '' then
        EmailUserAuth.Text := OAuthMsUserAuthDef;
end;

procedure THttpRestForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
    temp: String;
    SL: TStringList;
    I, J: Integer;
begin
    HttpRest1.RestCookies.SaveToFile(FCookieFileName);
    FreeAndNil(FIcsBuffLogStream); // V8.60 write log file }
    IniFile := TIcsIniFile.Create(FIniFileName);
    IniFile.WriteInteger(SectionMainWindow, KeyTop, Top);
    IniFile.WriteInteger(SectionMainWindow, KeyLeft, Left);
    IniFile.WriteInteger(SectionMainWindow, KeyWidth, Width);
    IniFile.WriteInteger(SectionMainWindow, KeyHeight, Height);
    SL := TStringList.Create;
    try
        SL.Delimiter := '|';
        for I := 0 to GridParams.RowCount - 1 do begin
            if (Trim(GridParams.Cells[0,I]) <> '') then begin  // skip if no name
                for J := 0 to GridParams.ColumnCount - 1 do begin
                   SL.Add(GridParams.Cells[J,I]);
                end;
            end;
        end;
        IniFile.WriteString(SectionData, KeyRestParams, SL.DelimitedText);
    finally
        SL.Free;
    end;

    with IniFile do begin
      WriteString (SectionData, 'AuthBearer_Text', AuthBearer.Text) ;
      WriteString (SectionData, 'AuthLogin_Text', AuthLogin.Text) ;
      WriteString (SectionData, 'AuthPassword_Text', AuthPassword.Text) ;
      WriteInteger (SectionData, 'AuthType_ItemIndex', AuthType.ItemIndex) ;
      WriteInteger (SectionData, 'CertVerMethod_ItemIndex', CertVerMethod.ItemIndex) ;
      WriteInteger (SectionData, 'DebugLogging_ItemIndex', DebugLogging.ItemIndex) ;
      WriteString (SectionData, 'ExtraHeaders_Lines', ExtraHeaders.Lines.CommaText) ;
      WriteString (SectionData, 'OAuthScope_Text', OAuthScope.Text) ;
      if OAuthAutoRefresh.IsChecked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'OAuthAutoRefresh_Checked', temp) ;
      if OAuthOptNoRedir.IsChecked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'OAuthOptNoRedir_Checked', temp) ;
      WriteString (SectionData, 'OAuthRefrMins_Text', OAuthRefrMins.Text) ;
      WriteString (SectionData, 'OAuthAccToken_Text', OAuthAccToken.Text) ;
      WriteString (SectionData, 'OAuthAppUrl_Text', OAuthAppUrl.Text) ;
      WriteInteger (SectionData, 'OAuthAuthType_ItemIndex', OAuthAuthType.ItemIndex) ;
      WriteString (SectionData, 'OAuthClientId_Text', OAuthClientId.Text) ;
      WriteString (SectionData, 'OAuthClientSecret_Text', OAuthClientSecret.Text) ;
      WriteString (SectionData, 'OAuthExpire_Text', OAuthExpire.Text) ;
      WriteString (SectionData, 'OAuthRedirectUrl_Text', OAuthRedirectUrl.Text) ;
      WriteString (SectionData, 'OAuthRefToken_Text', OAuthRefToken.Text) ;
      WriteString (SectionData, 'OAuthTokenUrl_Text', OAuthTokenUrl.Text) ;
      WriteString (SectionData, 'OAuthWebIP_Text', OAuthWebIP.Text) ;
      WriteString (SectionData, 'OAuthWebPort_Text', OAuthWebPort.Text) ;
      WriteInteger (SectionData, 'ParamContent_ItemIndex', ParamContent.ItemIndex) ;
      WriteString (SectionData, 'ProxyURL_Text', ProxyURL.Text) ;
      WriteString (SectionData, 'RawParams_Text', RawParams.Text) ;
      if ReportCertChain.IsChecked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ReportCertChain_Checked', temp) ;
      WriteInteger (SectionData, 'ReqMode_ItemIndex', ReqMode.ItemIndex) ;
      WriteInteger (SectionData, 'ReqType_ItemIndex', ReqType.ItemIndex) ;
      WriteString (SectionData, 'RestURL_Text', RestURL.Text) ;
      WriteString (SectionData, 'SslClientCertFile_Text', SslClientCertFile.Text) ;
      WriteString (SectionData, 'SslRootBundleFile_Text', SslRootBundleFile.Text) ;
      WriteInteger (SectionData, 'SslSecurity_ItemIndex', SslSecurity.ItemIndex) ;
      WriteInteger (SectionData, 'IpSockFamily_ItemIndex', IpSockFamily.ItemIndex) ;
      WriteString (SectionData, 'DirLogs_Text', DirLogs.Text) ;
      WriteString (SectionData, 'DnsHttpsUrl_Text', DnsHttpsUrl.Text) ;
      WriteString (SectionData, 'DnsDomainName_Text', DnsDomainName.Text) ;
      WriteInteger (SectionData, 'DnsQueryType_ItemIndex', DnsQueryType.ItemIndex) ;
      if DnsDnssec.IsChecked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'DnsDnssec_Checked', temp) ;
      if DnsNoValidation.IsChecked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'DnsNoValidation_Checked', temp) ;
      WriteString (SectionData, 'KapowAccName_Text', KapowAccName.Text) ;
      WriteString (SectionData, 'KapowAccPw_Text', KapowAccPw.Text) ;
      WriteString (SectionData, 'SmsAccSender_Text', SmsAccSender.Text) ;
      WriteString (SectionData, 'SmsMsgText_Lines', SmsMsgText.Lines.CommaText) ;
      WriteString (SectionData, 'SmsDestNums_Lines', SmsDestNums.Lines.CommaText) ;
      WriteString (SectionData, 'KapowAccPw_Text', KapowAccPw.Text) ;
      WriteString (SectionData, 'KapowAccName_Text', KapowAccName.Text) ;
      WriteString (SectionData, 'AlpnProtos_Text', AlpnProtos.Text) ;
      WriteString (SectionData, 'OAuthPrompt_Text', OAuthPrompt.Text) ;
      if OAuthAccess.IsChecked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'OAuthAccess_Checked', temp) ;
      WriteString (SectionData, 'TwitAccSecret_Text', TwitAccSecret.Text) ;
      WriteString (SectionData, 'TwitAccToken_Text', TwitAccToken.Text) ;
      WriteString (SectionData, 'TwitApiKey_Text', TwitApiKey.Text) ;
      WriteString (SectionData, 'TwitApiSecret_Text', TwitApiSecret.Text) ;
      if TwitForceLogin.IsChecked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'TwitForceLogin_Checked', temp) ;
      WriteString (SectionData, 'TwitIdList_Text', TwitIdList.Text) ;
      WriteString (SectionData, 'TwitMsg_Lines', TwitMsg.Lines.CommaText) ;
      WriteString (SectionData, 'TwitQuery_Text', TwitQuery.Text) ;
      WriteString (SectionData, 'TwitScrnId_Text', TwitScrnId.Text) ;
      WriteString (SectionData, 'TwitScrnName_Text', TwitScrnName.Text) ;
      WriteString (SectionData, 'OAuthConsoleUrl_Text', OAuthConsoleUrl.Text) ;
      WriteString (SectionData, 'EmailClientId_Text', EmailClientId.Text) ;
      WriteString (SectionData, 'EmailAccToken_Text', EmailAccToken.Text) ;
      WriteString (SectionData, 'EmailRefrToken_Text', EmailRefrToken.Text) ;
      WriteString (SectionData, 'EmailAccount_Text', EmailAccount.Text) ;
      if EmailForceLogin.IsChecked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'EmailForceLogin_Checked', temp) ;
      WriteString (SectionData, 'EmailTokenAccnt_Text', EmailTokenAccnt.Text) ;
      WriteString (SectionData, 'EmailClientSecret_Text', EmailClientSecret.Text) ;
      WriteString (SectionData, 'EmailRecipList_Lines', EmailRecipList.Lines.CommaText) ;
      WriteString (SectionData, 'EmailFrom_Text', EmailFrom.Text) ;
      WriteString (SectionData, 'EmailCC_Text', EmailCC.Text) ;
      WriteString (SectionData, 'EmailSubject_Text', EmailSubject.Text) ;
      WriteString (SectionData, 'EmailMessage_Lines', EmailMessage.Lines.CommaText) ;
      WriteString (SectionData, 'EmailAccExpiry_Text', EmailAccExpiry.Text) ;
      WriteInteger (SectionData, 'EmailRestType_ItemIndex', EmailRestType.ItemIndex) ;
      WriteString (SectionData, 'EmailAccountHint_Text', EmailAccountHint.Text) ;
      WriteString (SectionData, 'ReqAccept_Text', ReqAccept.Text) ;
      WriteInteger (SectionData, 'AliveMethod_ItemIndex', AliveMethod.ItemIndex) ;
      WriteInteger (SectionData, 'AliveNets_ItemIndex', AliveNets.ItemIndex) ;
      WriteInteger (SectionData, 'ReqMemStrategy_ItemIndex', ReqMemStrategy.ItemIndex) ;
      if ReqReplFile.IsChecked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'ReqReplFile_Checked', temp) ;
      WriteString (SectionData, 'RestDownFile_Text', RestDownFile.Text) ;
      if CertRevoke.IsChecked then temp := 'True' else temp := 'False' ; WriteString (SectionData, 'CertRevoke_Checked', temp) ;
      WriteString (SectionData, 'HttpUploadFile_Text', HttpUploadFile.Text) ;
      WriteInteger (SectionData, 'HttpUploadStrat_ItemIndex', HttpUploadStrat.ItemIndex) ;
      WriteString (SectionData, 'OAuthUserAuth_Text', OAuthUserAuth.Text) ;        { V8.70 }
      WriteString (SectionData, 'EmailUserAuth_Text', EmailUserAuth.Text) ;        { V8.70 }
      WriteInteger (SectionData, 'EmailAuthType_ItemIndex', EmailAuthType.ItemIndex) ;    { V8.71 }
      WriteInteger (SectionData, 'TwitAuthType_ItemIndex', TwitAuthType.ItemIndex) ;      { V8.71 }
      WriteString (SectionData, 'OAuthEdgeCache_Text', OAuthEdgeCache.Text) ;             { V8.71 }
      WriteString (SectionData, 'SmsWorksJWT_Text', SmsWorksJWT.Text) ;                   { V8.71 }
      WriteString (SectionData, 'OAuthLoginHint_Text', OAuthLoginHint.Text) ;             { V8.71 }
    end;
    IniFile.UpdateFile;
    IniFile.Free;
end;


procedure THttpRestForm.doExitClick(Sender: TObject);
begin
    Close;
end;


procedure THttpRestForm.AddLog (const S: string) ;
begin
    BuffLogLines := BuffLogLines + S + IcsCRLF;   { V8.61 }

  { V8.60 write log file }
    try
        if (DirLogs.Text = '') then Exit ;
        if NOT Assigned(FIcsBuffLogStream) then Exit; // sanity check
        FIcsBuffLogStream.WriteLine(S);
    except
    end;
end;


procedure THttpRestForm.TimerLogTimer(Sender: TObject);
var
    displen: integer ;
begin
    displen := Length(BuffLogLines);
    if displen > 0 then begin
        try
            SetLength(BuffLogLines, displen - 2) ;  // remove CRLF
            LogWin.Lines.Add(BuffLogLines);
            LogWin.GoToTextEnd;
        except
        end ;
        BuffLogLines := '';
    end;
end;


procedure THttpRestForm.HttpRest1HttpRestProg(Sender: TObject;
  LogOption: TLogOption; const Msg: string);
begin
    if LogOption = loProgress then           { V8.68 }
        LabelProgress.Text := Msg
    else
        AddLog(Msg);
end;


{ V8.60 this event is used to open the log file, or change it's name
  if already opened, change only needed for GUI applications where the user
  can change the log path. Note ls written as UTF8 codepage }

procedure THttpRestForm.OpenLogFile;
var
    FName: String;
begin
    if DirLogs.Text = '' then Exit; // no log
    FName := '"' + IncludeTrailingPathDelimiter(DirLogs.Text) +
                                              'ics-httprest-"yyyy-mm-dd".log"';
    if NOT Assigned(FIcsBuffLogStream) then
        FIcsBuffLogStream := TIcsBuffLogStream.Create(self, FName,
                                HttpRestForm.Caption + IcsCRLF, FileCPUtf8)
    else begin
        if FName = FIcsBuffLogStream.NameMask then Exit; // skip no change
        if FIcsBuffLogStream.LogSize > 0 then
            FIcsBuffLogStream.FlushFile(True);  // changing log path, write old log first
        FIcsBuffLogStream.NameMask := FName;
    end;
    AddLog(IcsCRLF + 'Opened log file: ' + FIcsBuffLogStream.FullName);
end;

procedure THttpRestForm.doAbortClick(Sender: TObject);
begin
    if (HttpRest1.State > httpReady) or HttpRest1.Connected then begin
        AddLog ('Aborting operation');
        HttpRest1.Abort;
    end;
  { V8.68 don't free response stream to allow resume to be tested }
    doStartReq.Enabled := True;
end;


procedure THttpRestForm.doClearClick(Sender: TObject);
begin
    LogWin.Lines.Clear;
    GridJsonResp.RowCount := 0;
    HttpRest1.ClearResp;
end;


procedure THttpRestForm.doClearParamsClick(Sender: TObject);  { V8.65 }
begin
    GridParams.RowCount := 0;
end;


procedure THttpRestForm.SelDirDownClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := ExtractFilePath(RestDownFile.Text) ;
    if OpenDirDiag.Execute then
        RestDownFile.Text := OpenDirDiag.FileName;
end;


procedure THttpRestForm.SelDirLogsClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := DirLogs.Text ;
    if OpenDirDiag.Execute then
        DirLogs.Text := ExtractFilePath(OpenDirDiag.FileName);
end;


procedure THttpRestForm.SelDirUploadClick(Sender: TObject);
begin
    OpenDirDiag.InitialDir := ExtractFilePath(HttpUploadFile.Text) ;
    if OpenDirDiag.Execute then
        HttpUploadFile.Text := OpenDirDiag.FileName;
end;


procedure THttpRestForm.SettingsChange(Sender: TObject);
begin
    if HttpRest1.Connected then
        HttpRest1.Abort;  // close socket so new settings used
end;


procedure THttpRestForm.CommonRestSettings;
begin
    HttpRest1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    HttpRest1.CertVerMethod := TCertVerMethod(CertVerMethod.ItemIndex);
    HttpRest1.SslCliSecurity := TSslCliSecurity(SslSecurity.ItemIndex);
    HttpRest1.SslReportChain := ReportCertChain.IsChecked;
    HttpRest1.SslRevocation := CertRevoke.IsChecked;                             { V8.69 }
    if SslClientCertFile.Text <> '' then begin
        if FileExists (SslClientCertFile.Text) then begin
            HttpRest1.SslCliCert.LoadFromFile(SslClientCertFile.Text);
        end
        else
            AddLog ('SSL client certificate ignored, not found: ' +
                                                      SslClientCertFile.Text);
    end;
    HttpRest1.SslRootFile := SslRootBundleFile.Text;
    HttpRest1.ServerAuth := THttpAuthType(AuthType.ItemIndex);
    HttpRest1.Username := AuthLogin.Text;
    HttpRest1.Password := AuthPassword.Text;
    HttpRest1.AuthBearerToken := AuthBearer.Text;
    HttpRest1.ProxyURL := ProxyURL.Text;                                        { V8.62 }
    HttpRest1.AlpnProtocols.CommaText := AlpnProtos.Text;                       { V8.64 }
    HttpRest1.ExtraHeaders := ExtraHeaders.Lines;
    HttpRest1.SocketFamily := TSocketFamily(IpSockFamily.ItemIndex);  { V8.60 IP4 and/or IPV6 }
    HttpRest1.Accept := ReqAccept.Text;                                         { V8.65 }
    HttpRest1.HttpMemStrategy := HttpStratMem;                                  { V8.68 }
    HttpRest1.ShowProgress := True;                                             { V8.68 }
    HttpRest1.OcspHttp.CacheFName := 'OcspRestCache.recs';                      { V8.69 }
    HttpRest1.OcspHttp.CacheStapled := True;                                    { V8.69 }
    HttpRest1.OcspHttp.CacheFlushMins := 2;                                     { V8.69 temp diag }
    HttpRest1.OcspHttp.CacheRefrDays := 3;                                      { V8.69 }
    HttpRest1.OcspHttp.OcspStapleOnly := False;                                 { V8.69 }
    HttpRest1.OcspHttp.OcspHttpProxy := ProxyURL.Text;                          { V8.69 }
    LabelProgress.Text := '';
end;


procedure THttpRestForm.doStartReqClick(Sender: TObject);
const
    ReqList: array[0..5] of THttpRequest =
      (httpGET, httpPOST, httpHEAD, httpPUT, httpDELETE, httpPATCH);  { V8.64 added PATCH }
var
    StatCode, Row: Integer;
    Req: THttpRequest;
    Async: Boolean;

 { V8.65 convert literal ParamType into type }
    function FindPType(const PType: String): TRParamType;
    var
        PT: TRParamType;
    begin
        for PT := Low(TRParamType) to High(TRParamType) do begin
            if RParamTypeLits[PT] = PType then begin
                Result := PT;
                Exit;
            end;
        end;
        Result := RPTypeStr;
    end;


begin
    doStartReq.Enabled := False;
    GridJsonResp.RowCount := 0;
    OpenLogFile;  { V8.60 }

 // optional HTTP parameters, all have defaults so can be ignored if not needed
    CommonRestSettings;
    Req := ReqList[ReqType.ItemIndex];
    Async := (ReqMode.ItemIndex = 1);
    HttpRest1.HttpMemStrategy := THttpMemStrategy(ReqMemStrategy.ItemIndex);  { V8.68 }
    HttpRest1.HttpDownFileName := RestDownFile.Text;                          { V8.68 }
    HttpRest1.HttpDownReplace := ReqReplFile.IsChecked;                         { V8.68 }
    HttpRest1.HttpUploadStrat := THttpUploadStrat(HttpUploadStrat.ItemIndex); { V8.69 }
    HttpRest1.HttpUploadFile := Trim(HttpUploadFile.Text);                    { V8.69 }

  // read grid and build REST parameters
    HttpRest1.RestParams.Clear;
    for Row := 0 to GridParams.RowCount - 1 do begin
        if (Trim(GridParams.Cells[0,Row]) <> '') then begin
            HttpRest1.RestParams.AddItem(Trim(GridParams.Cells[0,Row]),
                Trim(GridParams.Cells[1,Row]), FindPType(GridParams.Cells[2,Row]));   { V8.65 }
        end;
    end;
    HttpRest1.RestParams.PContent := TPContent(ParamContent.ItemIndex);

  // make HTTP request, note RestParams are ignored if RawRarams not blank
    AddLog (DateTimeToStr(Now) + ' Starting REST request for URL: ' + RestURL.Text);   { V8.69 }
    StatCode := HttpRest1.RestRequest(Req, RestURL.Text, Async, RawParams.Text);
    if Async then
        AddLog ('Async REST request started')
    else begin
        AddLog ('Sync REST request completed, Status ' + IntToStr (StatCode));
        AddLog('');
     // for sync we can process the result here instead of HttpRest1RestRequestDone
        doStartReq.Enabled := True;
        if (StatCode = 401) and (HttpRest1.ServerAuth in [httpAuthBasic, httpAuthNtlm, httpAuthDigest, httpAuthDigest2]) then  { V8.71 only if login used }
            PostMessage (FNotifyWindow, WM_401REPEAT, 0, 0) ;
    end;
end;

procedure THttpRestForm.DisplayJson(RespObj: ISuperObject);
var
    JsonItem: TSuperAvlEntry;
    JsonObj: ISuperObject;
    JsonEnum: TSuperAvlIterator;
    I, J, CWid: integer;
    FirstRow: Boolean;
    CVal: String;
begin
  // parse Json or XML response to grid
    GridJsonResp.RowCount := 0;
    for J := 0 to GridJsonResp.ColumnCount - 1 do
        GridJsonResp.Columns[J].Header := '';
    JArrayTot := 0;
    SetLength(JArrayItems, 0);
    if NOT Assigned(RespObj) then exit;
    try
     // note that values containing objects are displayed as raw Json
        if RespObj.DataType = stObject then begin
            GridJsonResp.Columns[0].Header := 'Name';
            GridJsonResp.Columns[0].Width := 100;
            GridJsonResp.Columns[1].Header := 'Type';
            GridJsonResp.Columns[1].Width := 70;
            GridJsonResp.Columns[2].Header := 'Value';
            GridJsonResp.Columns[2].Width := 1000;
            GridJsonResp.Columns[3].Header := '';
            GridJsonResp.Columns[3].Width := 100;
            JsonEnum := RespObj.AsObject.GetEnumerator;
            try
                I := 0;
                while JsonEnum.MoveNext do begin
                    JsonItem := JsonEnum.GetIter;
                    GridJsonResp.RowCount := GridJsonResp.RowCount + 1;
                    GridJsonResp.Cells[0, I] := JsonItem.Name;
                    GridJsonResp.Cells[1, I] := GetEnumName(TypeInfo(TSuperType), Ord(JsonItem.Value.DataType));
                    GridJsonResp.Cells[2, I] := JsonItem.Value.AsString;
                    I := I + 1;
                end;
            finally
                JsonEnum.Free;
            end;
        end;

     // one column per Value, with Name as title
        if RespObj.DataType = stArray then begin
            FirstRow := True;
            JArrayTot := RespObj.AsArray.Length;
            if JArrayTot = 0 then Exit;
            SetLength(JArrayItems, JArrayTot);
            GridJsonResp.RowCount := JArrayTot;
            for I := 0 to JArrayTot - 1 do begin
                JsonObj := RespObj.AsArray[I];
                JArrayItems[I] := RespObj.AsArray[I].AsString;   { keep lines so we display them later, V8.71 fixed }
                JsonEnum := JsonObj.AsObject.GetEnumerator;
                J := 0;
                while JsonEnum.MoveNext do begin
                    JsonItem := JsonEnum.GetIter;
                    CVal := JsonItem.Value.AsString;
                    if FirstRow then begin
                        if (GridJsonResp.ColumnCount <= J)  then   // do we need more blank columns?
                            GridJsonResp.AddObject(GridJsonResp.Columns[0].Clone(Self));
                        CWid := (Length(CVal) * 5) + 30;
                        if CWid > 400 then CWid := 400;
                        GridJsonResp.Columns[J].Header := JsonItem.Name;
                        GridJsonResp.Columns[J].Width := CWid;
                    end;
                    GridJsonResp.Cells[J, I] := CVal;
                    J := J + 1;
                end;
                FirstRow := False;
            end;
        end;
    except
        on E:Exception do
            AddLog('Error parsing Json: ' + E.Message);
    end;
end;


procedure THttpRestForm.GridJsonRespCellDblClick(const Column: TColumn; const Row: Integer);
var
    J: Integer;
begin
    if NOT Assigned(JsonObjectWin) then Exit;
    if Row < 0 then Exit;
    JsonObjectWin.GridSubResp.RowCount := 0;
    if (JArrayTot > 0) and (Length(JArrayItems) = JArrayTot) then begin { V8.65 one array element }
        JsonObjectWin.DispJson(JArrayItems[GridJsonResp.Row]);
    end
    else begin
    //  with GridJsonResp.Items[GridJsonResp.ItemIndex] do begin
        if (GridJsonResp.ColumnCount >= 2) and ((GridJsonResp.Cells[1, Row] = 'stArray') or
                                            (GridJsonResp.Cells[1, Row] = 'stObject')) then
            JsonObjectWin.DispJson(GridJsonResp.Cells[2, Row])

     // V8.64 array may have Json object in any column, search for first, sorry ignore others...
        else begin
            for J := 0 to GridJsonResp.ColumnCount - 1 do begin
                if ((Pos ('{', GridJsonResp.Cells[J, Row]) = 1) or (Pos ('[', GridJsonResp.Cells[J, Row]) = 1)) then begin
                    JsonObjectWin.DispJson(GridJsonResp.Cells[J, Row]);
                    break;
                end;
            end;
        end;
    end;
    JsonObjectWin.BringToFront;
end;


procedure THttpRestForm.GridParamsCellClick(const Column: TColumn; const Row: Integer);
begin
    if Column.OriginalIndex <> 2 then Exit;
    if GridParams.Cells[2, Row] = '' then
        GridParams.Cells[2, Row] := 'RPTypeStr';
end;


procedure THttpRestForm.WM401REPEAT (var Msg : TMessage);     { V8.69 }
var
    Ret, StatCode, I: Integer;
    Async: Boolean;
begin
    Async := (ReqMode.ItemIndex = 1);
    FormLogin := TFormLogin.Create(Self);
    FormLogin.Top := Self.Top + 20;
    FormLogin.Left := Self.Left + 20;

 { default previous login used by request }
 { should really store login for different paths }
    FormLogin.AuthUsername.Text := HttpRest1.Username;
    FormLogin.AuthPassword.Text := HttpRest1.Password;

  { check types of authenication server offered }
    if Length(HttpRest1.WWWAuthInfos) > 0 then begin
        FormLogin.ListMethods.Items.Clear;
        FormLogin.LabelPageURL.Text := 'Login to page: ' + HttpRest1.WWWAuthInfos[0].Uri ;
        for I := 0 to Length(HttpRest1.WWWAuthInfos) - 1 do begin
            with HttpRest1.WWWAuthInfos[I] do
                FormLogin.ListMethods.Items.Add('Type: ' + HttpCliAuthNames[AuthType] + ', Realm: ' + Realm);
        end;
        FormLogin.ListMethods.ItemIndex := FormLogin.ListMethods.Items.Count - 1;  // select last
    end;
    Ret := FormLogin.ShowModal;
    if Ret = mrOK then begin
        HttpRest1.Username := FormLogin.AuthUsername.Text;
        HttpRest1.Password := FormLogin.AuthPassword.Text;
        I := FormLogin.ListMethods.ItemIndex;
        if Length(HttpRest1.WWWAuthInfos) > 0  then
            HttpRest1.ServerAuth := HttpRest1.WWWAuthInfos[I].AuthType
        else
            HttpRest1.ServerAuth := httpAuthBasic;
        AddLog('Authentication Details Entered Manually, Login: ' + HttpRest1.Username);

      // restart HTTP request
        doStartReq.Enabled := False;
        AddLog (DateTimeToStr(Now) + ' Restarting REST request for URL: ' + RestURL.Text);
        StatCode := HttpRest1.RestRequest(HttpRest1.RequestType, RestURL.Text, Async, RawParams.Text);
        if Async then
            AddLog ('Async REST request started')
        else begin
            AddLog ('Sync REST request completed, Status ' + IntToStr (StatCode));
            AddLog('');
         // for sync we can process the result here instead of HttpRest1RestRequestDone
            doStartReq.Enabled := True;
        end;
    end;
    FormLogin.Free;
end;


procedure THttpRestForm.HttpRest1RestRequestDone(Sender: TObject;
  RqType: THttpRequest; ErrCode: Word);
var
    XmlObj: ISuperObject;
    Async: Boolean;
begin
    Async := (ReqMode.ItemIndex = 1);
    doStartReq.Enabled := True;
    if HttpRest1.GetAlpnProtocol <> '' then
        AddLog ('ALPN Requested by Server: ' + HttpRest1.GetAlpnProtocol);
    if ErrCode <> 0 then begin
     // note identical logging has already been done by the TSslHttpRest, but is
     // duplicated here to illustrate what information is available
        AddLog('Request failed: Error: ' + HttpRest1.RequestDoneErrorStr +     { V8.68 error string instead of number }
              ' - ' + IntToStr(HttpRest1.StatusCode) + IcsSpace + HttpRest1.ReasonPhrase);
        Exit;
    end;

    AddLog('Request done, StatusCode ' + IntToStr(HttpRest1.StatusCode));

 { V8.69 need authentication }
    if (HttpRest1.StatusCode = 401) then begin
        AddLog (String(HttpRest1.ResponseRaw));
        if Async and (HttpRest1.ServerAuth in [httpAuthBasic, httpAuthNtlm, httpAuthDigest, httpAuthDigest2]) then  { V8.71 only if login used }
            PostMessage (FNotifyWindow, WM_401REPEAT, 0, 0) ;
        Exit;
    end;

  // V8.68 only display textual content in log
    if (Pos('text/', HttpRest1.ContentType) > 0) or (Pos('xml', HttpRest1.ContentType) > 0) or
         (Pos('json', HttpRest1.ContentType) > 0) or (Pos('java', HttpRest1.ContentType) > 0) then
        AddLog (String(HttpRest1.ResponseRaw))
     else
        AddLog('<Non-textual content received: ' + HttpRest1.ContentType + '>');   { V8.68 }

  // look for Json response }
    if ((Pos('{', HttpRest1.ResponseRaw) > 0) or (Pos('json', HttpRest1.ContentType) > 0)) and
                                                      Assigned(HttpRest1.ResponseJson) then begin
        AddLog ('Json main content type: ' +
               GetEnumName(TypeInfo(TSuperType), Ord(HttpRest1.ResponseJson.DataType)));
        DisplayJson(HttpRest1.ResponseJson);
    end;

  // V8.64 look for XML response }
    if ((Pos('<?xml version=', HttpRest1.ResponseRaw) > 0) or
                       (Pos('xml', HttpRest1.ContentType) > 0)) then begin
        try
            XmlObj := XMLParseStream(HttpRest1.ResponseStream, true);    // pack for easier display
            if Assigned(XmlObj) then begin
                AddLog ('XML content as Json: ' + XmlObj.AsString);  // !!! TEMP Json version of XML
                DisplayJson(XmlObj);
            end;
        except
            on E:Exception do
                AddLog('Error parsing XML: ' + E.Message);
        end;
    end;
    AddLog('');
    TimerLogTimer(Self); // update log window
end;

procedure THttpRestForm.doListCertStoreClick(Sender: TObject);
var
    CertList: TX509List;
    Tot, I: Integer;
    Info: string;
begin
    doListCertStore.Enabled := False;
    OpenLogFile;
    CommonRestSettings;
    CertList := TX509List.Create (self, True);
    try
        HttpRest1.InitSsl;
        Tot := HttpRest1.SslContext.SslGetAllCerts (CertList);
        if Tot > 0 then begin
            CertList.SortChain(xsrtIssuerFirst);
            Info := '! SSL context contains ' + IntToStr (Tot) +
                                            ' certificate in store' + #13#10;
            for I := 1 to Tot do begin
                Info := Info + '#' + IntToStr (I) + ' ';
                if CertList [I-1].SubAltNameDNS <> '' then
                    Info := Info + IcsUnwrapNames(CertList [I-1].SubAltNameDNS)
                else if CertList [I-1].SubjectCName <> '' then  { V8.41 some roots blank }
                    Info := Info + IcsUnwrapNames(CertList [I-1].SubjectCName)
                else
                    Info := Info + IcsUnwrapNames(CertList [I-1].SubjectOName);
                Info := Info + ' (' + IcsUnwrapNames(CertList [I-1].SubjectOName) + ')';
                if CertList [I-1].SubjectOUName <> '' then
                    Info := Info + ' OU: ' + IcsUnwrapNames(CertList [I-1].SubjectOUName);
                Info := Info + #13#10;
            end;
            AddLog(Info);
        end
        else
            AddLog('! SSL context certificate store empty');
    finally
        CertList.Free;
        doListCertStore.Enabled := True;
    end;
end;

// only OAuth2 options

procedure THttpRestForm.RestOAuthSetup;
begin
    LabelResult.Text := 'Result: Please Wait';
    OpenLogFile;                                                       { V8.65 }
    RestOAuth1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    RestOAuth1.ProtoType := OAuthv2; //  TOAuthProto(OAuthProtoType.ItemIndex);   { V8.65 }
    RestOAuth1.AuthCode := OAuthAuthCode.Text;
    RestOAuth1.AuthType := TOAuthType(OAuthAuthType.ItemIndex);
    RestOAuth1.AppUrl := Trim(OAuthAppUrl.Text);
    RestOAuth1.RedirectMsg := 'App: ' + OAuthAppUrl.Text;
    RestOAuth1.ClientId := Trim(OAuthClientId.Text);
    RestOAuth1.ClientSecret := Trim(OAuthClientSecret.Text);
    RestOAuth1.OAOptions := [];
    if OAuthOptNoRedir.IsChecked then
        RestOAuth1.OAOptions := RestOAuth1.OAOptions  + [OAopAuthNoRedir];
    if OAuthPrompt.Text <> '' then begin                   { V8.63 }
        RestOAuth1.LoginPrompt := OAuthPrompt.Text;
        RestOAuth1.OAOptions := RestOAuth1.OAOptions  + [OAopAuthPrompt];
    end;
    if OAuthAccess.IsChecked then begin                   { V8.63 }
        RestOAuth1.RefreshOffline := True;
        RestOAuth1.OAOptions := RestOAuth1.OAOptions  + [OAopAuthAccess];
    end;
    RestOAuth1.RefreshAuto := OAuthAutoRefresh.IsChecked;
    RestOAuth1.RefrMinsPrior := atoi(OAuthRefrMins.Text);
    RestOAuth1.RefreshToken := OAuthRefToken.Text;
    RestOAuth1.Scope := Trim(OAuthScope.Text);
    RestOAuth1.TokenUrl := Trim(OAuthTokenUrl.Text);
    RestOAuth1.StopSrv;                                     { V8.65 }
    RestOAuth1.RedirectUrl := Trim(OAuthRedirectUrl.Text);
    RestOAuth1.WebSrvIP := Trim(OAuthWebIP.Text);
    RestOAuth1.WebSrvPort := Trim(OAuthWebPort.Text);
    RestOAuth1.AccName := Trim(OAuthAccTitle.Text);        { V8.65 }
    RestOAuth1.ConsoleUrl := Trim(OAuthConsoleUrl.Text);   { V8.65 }
    RestOAuth1.MsUserAuthority := Trim(OAuthUserAuth.Text);   { V8.70 }
    RestOAuth1.EdgeCacheDir := OAuthEdgeCache.Text;           { V8.71 }
    RestOAuth1.LoginHint := OAuthLoginHint.Text;              { V8.71 }
end;

procedure THttpRestForm.RestOAuth1OAuthAuthUrl(Sender: TObject; const URL: string);
begin
    if ((Sender as TRestOAuth).AuthType = OAuthTypeMan) then begin
        AddLog('Please copy this URL and browse to it, then enter Auth Code: ' + URL);
    end ;
end;

procedure THttpRestForm.RestOAuth1OAuthEmbed(Sender: TObject; const URL: string; var Success: Boolean);
begin
    Success := OAuthBrowser1.BrowserEmbedded((Sender as TRestOAuth), Self, URL);
end;

procedure THttpRestForm.RestOAuth1OAuthNewCode(Sender: TObject);
begin
    OAuthAuthCode.Text := (Sender as TRestOAuth).AuthCode;
    LabelResult.Text := 'Result: Got New Auth Code OK';
  // NOTE - AuthCode usually expires in 10 minutes or less
end;

procedure THttpRestForm.RestOAuth1OAuthNewToken(Sender: TObject);
begin
    OAuthAccToken.Text := (Sender as TRestOAuth).AccToken;
    OAuthRefToken.Text := (Sender as TRestOAuth).RefreshToken;
    OAuthExpire.Text := DateTimeToStr((Sender as TRestOAuth).ExpireDT);
    AuthBearer.Text := (Sender as TRestOAuth).AccToken;
    LabelResult.Text := 'Result: Got New Token OK';
end;

procedure THttpRestForm.RestOAuth1OAuthProg(Sender: TObject;
  LogOption: TLogOption; const Msg: string);
begin
    AddLog(Msg);   { V8.63 }
end;

procedure THttpRestForm.doTestRedirClick(Sender: TObject);
begin
    RestOAuthSetup;
    RestOAuth1.TestRedirect;
end;

procedure THttpRestForm.doOAuthConsoleClick(Sender: TObject);
begin
    RestOAuthSetup;
    RestOAuth1.LaunchConsole;
end;

procedure THttpRestForm.doOAuthLoginClick(Sender: TObject);
begin
    RestOAuthSetup;
    if RestOAuth1.StartAuthorization then begin
        if (RestOAuth1.AuthType <> OAuthTypeEmbed) then             { V8.71 returns with codes }
            LabelResult.Text := 'Result: Waiting for Auth Code';
    end
    else
        LabelResult.Text := 'Result: Failed - ' + RestOAuth1.LastError;
end;


procedure THttpRestForm.doOAuthTokenClick(Sender: TObject);
begin
    RestOAuthSetup;
    if RestOAuth1.GrantAuthToken then
        LabelResult.Text := 'Result: Got New Token OK'
    else
        LabelResult.Text := 'Result: Failed - ' + RestOAuth1.LastError;
end;

procedure THttpRestForm.doOAuthRefreshClick(Sender: TObject);
begin
    RestOAuthSetup;
    if RestOAuth1.GrantRefresh then
        LabelResult.Text := 'Result: Got New Token OK'
    else
        LabelResult.Text := 'Result: Failed - ' + RestOAuth1.LastError;
end;

procedure THttpRestForm.doGrantCredClick(Sender: TObject);
begin
    RestOAuthSetup;
    if RestOAuth1.GrantAppToken then
        LabelResult.Text := 'Result: Got New Token OK'
    else
        LabelResult.Text := 'Result: Failed - ' + RestOAuth1.LastError;
end;

procedure THttpRestForm.doGrantPasswordClick(Sender: TObject);
begin
    RestOAuthSetup;
    if RestOAuth1.GrantPasswordToken(Trim(AuthLogin.Text), Trim(AuthPassword.Text)) then
        LabelResult.Text := 'Result: Got New Token OK'
    else
        LabelResult.Text := 'Result: Failed - ' + RestOAuth1.LastError;
end;

procedure THttpRestForm.OAuthAccTitleChange(Sender: TObject);
begin
    RestOAuth1.StopSrv;
    if OAuthAccTitle.ItemIndex = 0 then Exit;  // ignore none

 { V8.65 fill OAuth2 details for common accounts }
    with OAuthUris[OAuthAccTitle.ItemIndex] do begin
        OAuthAccTitle.Text := CAccName;
        OAuthConsoleUrl.Text := CConsoleUrl;
        OAuthAppUrl.Text := CAppUrl;
        OAuthRedirectUrl.Text := CRedirectUrl;
        OAuthTokenUrl.Text := CTokenUrl;
        OAuthScope.Text := CScope;
    end;
end;

procedure THttpRestForm.doDNSJsonClick(Sender: TObject);
var
    StatCode, I, qtype, rcode : integer;
    QueryType: String;
    ArrayJson: ISuperObject;
    ResultList: TStringList;
begin
    doDNSJson.Enabled := False;
    ResultList := TStringList.Create;
    try
        GridJsonResp.RowCount := 0;
        OpenLogFile;
        CommonRestSettings;
        HttpRest1.Accept := MimeDnsJson;
        HttpRest1.NoCache := True;
        QueryType := DnsQueryType.Text;
        I := Pos (' [', QueryType);
        if I > 1 then SetLength (QueryType, I - 1);
      // Json DNS parameters
        HttpRest1.RestParams.Clear;
        HttpRest1.RestParams.AddItem('name', IcsIDNAToASCII(Trim(DnsDomainName.Text)), True);  { V8.64 }
        HttpRest1.RestParams.AddItem('type', QueryType, True);
    //    HttpRest1.RestParams.AddItem('ct', MimeDnsJson, True);
        if DnsDnssec.IsChecked then
            HttpRest1.RestParams.AddItem('do', 'true', True);
        if DnsNoValidation.IsChecked then
            HttpRest1.RestParams.AddItem('cd', 'true', True);
        HttpRest1.RestParams.PContent := PContUrlencoded;

      // make HTTP request
        AddLog ('Starting sync DNS request');
        StatCode := HttpRest1.RestRequest(httpGET, DnsHttpsUrl.Text, False);
        if (StatCode = 200) and Assigned(HttpRest1.ResponseJson) then begin
      (*  {"Status": 0,
           "TC": false,
           "RD": true,
           "RA": true,
           "AD": false,
           "CD": false,
           "Question":[
                       {"name": "www.overbyte.eu.",
                        "type": 1}
                      ],
           "Answer":[
                      {"name": "www.overbyte.eu.",
                       "type": 1,
                       "TTL": 1426,
                       "data": "91.183.89.111"}
                     ]
          }

          {"Status": 0,
           "TC": false,
           "RD": true,
           "RA": true,
           "AD": false,
           "CD": false,
           "Question":[
                      {"name": "www.google.com.",
                      "type": 2}
                      ],
           "Authority":[
                      {"name": "google.com.",
                       "type": 6,
                       "TTL": 60,
                       "data": "ns1.google.com. dns-admin.google.com. 242408846 900 900 1800 60"
                       }]
           }
         *)

         // we may have multiple Answers
            rcode := HttpRest1.ResponseJson.I['Status'];
            if rcode = DnsRCodeNoError then begin
                ArrayJson := HttpRest1.ResponseJson.O['Answer'];
                if NOT Assigned(ArrayJson) then
                    ArrayJson := HttpRest1.ResponseJson.O['Authority'];
                if Assigned(ArrayJson) then begin

                  // display on grid, sub headers
                    GridJsonResp.Columns[0].Header := 'Query Name';
                    GridJsonResp.Columns[1].Header := 'Type';
                    GridJsonResp.Columns[2].Header := 'Data';
                    GridJsonResp.Columns[3].Header := 'TTL';
                    GridJsonResp.Columns[0].Width := 150;
                    GridJsonResp.Columns[1].Width := 100;
                    GridJsonResp.Columns[2].Width := 500;
                    GridJsonResp.Columns[3].Width := 50;
                    for I := 0 to ArrayJson.AsArray.Length - 1 do begin
                        qtype := ArrayJson.AsArray[I].I['type'];
                        if qtype in [DnsQueryA, DnsQueryAAAA, DnsQueryMX, DnsQueryNS, DnsQueryCNAME] then
                            ResultList.Add(ArrayJson.AsArray[I].S['data']);

                      // display on grid, answer line
                         GridJsonResp.RowCount := GridJsonResp.RowCount + 1; // blank line
                         GridJsonResp.Cells[0,I] := ArrayJson.AsArray[I].S['name'];
                         GridJsonResp.Cells[1,I] := FindDnsReqTypeName(qtype);
                         GridJsonResp.Cells[2,I] := ArrayJson.AsArray[I].S['data'];
                         GridJsonResp.Cells[3,I] := ArrayJson.AsArray[I].S['TTL'];
                    end;
                end
                else
                    AddLog ('DNS request no answer');
            end
            else
                AddLog ('DNS request failed: ' + DnsRCodeTable[rcode]);
        end
        else
            AddLog ('DNS request failed: HTTP Failed ' + HttpRest1.ResponseRaw);

        if ResultList.Count > 0 then
            AddLog ('DNS result: ' + ResultList.CommaText);
    finally
        ResultList.Free;
        doDNSJson.Enabled := True;
    end;
end;

procedure THttpRestForm.doDnsQuery1Click(Sender: TObject);
var
    qtype: integer;
begin
    GridJsonResp.RowCount := 0;
    OpenLogFile;
    qtype := DnsReqTable[DnsQueryType.ItemIndex].Num;
    DnsQueryHttps1.DnsSrvUrl := DnsHttpsUrl.Text;
    DnsQueryHttps1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    DnsQueryHttps1.HttpRest.CertVerMethod := TCertVerMethod(CertVerMethod.ItemIndex);
    DnsQueryHttps1.HttpRest.SocketFamily := TSocketFamily(IpSockFamily.ItemIndex);
    if DnsQueryHttps1.DOHQueryAny(Trim(DnsDomainName.Text), qtype) then
         AddLog ('Starting async DNS request')
    else
         AddLog ('DNS request failed');
end;

procedure THttpRestForm.doDnsQueryAllClick(Sender: TObject);
begin
    GridJsonResp.RowCount := 0;
    OpenLogFile;
    DnsQueryHttps1.DnsSrvUrl := DnsHttpsUrl.Text;
    DnsQueryHttps1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    DnsQueryHttps1.HttpRest.CertVerMethod := TCertVerMethod(CertVerMethod.ItemIndex);
    DnsQueryHttps1.HttpRest.SocketFamily := TSocketFamily(IpSockFamily.ItemIndex);
    if DnsQueryHttps1.DOHQueryAll(Trim(DnsDomainName.Text)) then
         AddLog ('Starting async DNS request')
    else
         AddLog ('DNS request failed');
end;


procedure THttpRestForm.DnsQueryHttps1DnsProg(Sender: TObject;
  LogOption: TLogOption; const Msg: string);
begin
    AddLog (Msg);
end;

procedure THttpRestForm.DnsQueryHttps1RequestDone(Sender: TObject; Error: Word);
var
    MyDnsQuery: TDnsQueryHttps;
    S: String;
    I, qtype: Integer;
begin
    MyDnsQuery := Sender as TDnsQueryHttps;
    if Error <> 0 then begin
        AddLog('Request failed, error #' + IntToStr(Error) +
              '. Status = ' + IntToStr(MyDnsQuery.HttpRest.StatusCode) +
                               ' - ' + MyDnsQuery.HttpRest.ReasonPhrase);
    end
    else begin
        if MyDnsQuery.ResponseAuthoritative then
            S := ', Authoritative Server'
        else
            S := ', Not Authoritative';
        AddLog ('Reponse Code: ' + DnsRCodeTable[MyDnsQuery.ResponseCode] + S +
             ', Answer Records: ' + IntToStr(MyDnsQuery.ResponseANCount) +
               ', NS Records: ' + IntToStr(MyDnsQuery.ResponseNSCount) +
                 ', Additional Records: ' + IntToStr(MyDnsQuery.ResponseARCount));
        if MyDnsQuery.AnswerTotal > 0 then begin

          // display on grid, sub headers
            GridJsonResp.Columns[0].Header := 'Query Name';
            GridJsonResp.Columns[1].Header := 'Type';
            GridJsonResp.Columns[2].Header := 'Data';
            GridJsonResp.Columns[3].Header := 'TTL';
            GridJsonResp.Columns[0].Width := 150;
            GridJsonResp.Columns[1].Width := 100;
            GridJsonResp.Columns[2].Width := 500;
            GridJsonResp.Columns[3].Width := 50;
            for I := 0 to MyDnsQuery.AnswerTotal - 1 do begin
              // display on grid, answer line
                qtype := MyDnsQuery.AnswerRecord[I].RRType;
                GridJsonResp.RowCount := GridJsonResp.RowCount + 1; // blank line
                GridJsonResp.Cells[0,I] := String(MyDnsQuery.AnswerRecord[I].RRName);
                GridJsonResp.Cells[1,I] := FindDnsReqTypeName(qtype);
                S := String(MyDnsQuery.AnswerRecord[I].RDData);
              // some records return stuff other than a string
                if qtype = DnsQueryMX then
                    S := IntToStr (MyDnsQuery.AnswerRecord[I].MxPref) + ' = ' + S;
                GridJsonResp.Cells[2,I] := S;
                GridJsonResp.Cells[3,I] := IntToStr(MyDnsQuery.AnswerRecord[I].TTL);
            end;
        end;
    end;
end;

procedure THttpRestForm.doKapowSendClick(Sender: TObject);
begin
    OpenLogFile;
    IcsSMS1.SmsProvider := SmsProvKapow;
    IcsSMS1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    IcsSMS1.AccountName := KapowAccName.Text;
    IcsSMS1.AccountPw := KapowAccPw.Text;
    IcsSMS1.MsgSender := SmsAccSender.Text;  // premium feature
    if NOT IcsSMS1.SendSMS (SmsDestNums.Lines.CommaText, SmsMsgText.Lines.Text) then
        AddLog ('Failed to Send SMS: ' + IcsSMS1.LastError)
    else
        AddLog ('SMS Send Started');
end;


procedure THttpRestForm.doKapowCheckClick(Sender: TObject);
begin
    OpenLogFile;
    IcsSMS1.SmsProvider := SmsProvKapow;
    IcsSMS1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    IcsSMS1.AccountName := KapowAccName.Text;
    IcsSMS1.AccountPw := KapowAccPw.Text;
    if NOT IcsSMS1.CheckSMS (KapowSentId) then
        AddLog ('Failed to Delivery Check SMS: ' + IcsSMS1.LastError)
    else
        AddLog ('Delivery Check Started');
end;

procedure THttpRestForm.doKapowCreditClick(Sender: TObject);
begin
    OpenLogFile;
    IcsSMS1.SmsProvider := SmsProvKapow;
    IcsSMS1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    IcsSMS1.AccountName := KapowAccName.Text;
    IcsSMS1.AccountPw := KapowAccPw.Text;
    if NOT IcsSMS1.CheckCredit then
        AddLog ('Failed to Check Credit: ' + IcsSMS1.LastError)
    else
        AddLog ('Credit Check Started');
end;

procedure THttpRestForm.doSmsWorksSendClick(Sender: TObject);
var
    S: String;
begin
    OpenLogFile;
    IcsSMS1.SmsProvider := SmsProvSmsWorks;
    IcsSMS1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    IcsSMS1.AccountJwt := SmsWorksJWT.Text;              { V8.71 }
    IcsSMS1.MsgSender := SmsAccSender.Text;
    if NOT IcsSMS1.SendSMS (SmsDestNums.Lines.CommaText, SmsMsgText.Lines.Text) then
        S := 'Failed to Send SMS: ' + IcsSMS1.LastError
    else
        S := 'SMS Send Started';
    LabelSmsWorksResult.Text := 'Result: ' + S;
    AddLog (S);
end;

procedure THttpRestForm.doSmsWorksCheckClick(Sender: TObject);
begin
    OpenLogFile;
    IcsSMS1.SmsProvider := SmsProvSmsWorks;
    IcsSMS1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    IcsSMS1.AccountJwt := SmsWorksJWT.Text;              { V8.71 }
    if NOT IcsSMS1.CheckSMS(SmsWorksSentId, True, (SmsDestNums.Lines.Count > 1)) then
        AddLog ('Failed to Delivery Check SMS: ' + IcsSMS1.LastError)
    else
        AddLog ('Delivery Check Started');
end;

procedure THttpRestForm.doSmsWorksCreditClick(Sender: TObject);
begin
    OpenLogFile;
    IcsSMS1.SmsProvider := SmsProvSmsWorks;
    IcsSMS1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    IcsSMS1.AccountJwt := SmsWorksJWT.Text;         { V8.71 }
    if NOT IcsSMS1.CheckCredit then
        AddLog ('Failed to Check Credit: ' + IcsSMS1.LastError)
    else
        AddLog ('Credit Check Started');
end;


procedure THttpRestForm.IcsSMS1SmsDone(Sender: TObject);
var
    S: String;
begin
    AddLog ('SMS Response:: ' + IcsSMS1.LastResp);  // !!! TEMP DIAG
    if IcsSMS1.SmsProvider = SmsProvKapow then begin
        if IcsSMS1.LastError = '' then begin
            if IcsSMS1.SentID <> '' then begin
                KapowSentId := IcsSMS1.SentID;
                S := 'SMS Queued for Delivery, Reference: ' + KapowSentId;
                doKapowCheck.Enabled := True;
            end;
            if IcsSMS1.Credits <> '' then begin
                S :=  'Credits: ' + IcsSMS1.Credits;
                LabelKapowCredit.Text := S;
            end;
            if IcsSMS1.Delivery <> '' then begin
                S :=  'SMS Delivery for ' + KapowSentId + ': ' + IcsSMS1.Delivery;
            end;
        end
        else
            S := 'Failed: ' + IcsSMS1.LastError;
        LabelKapowResult.Text := 'Result: ' + S;
    end
    else if IcsSMS1.SmsProvider = SmsProvSmsWorks then begin
        if IcsSMS1.LastError = '' then begin
            if IcsSMS1.SentID <> '' then begin
                SmsWorksSentId := IcsSMS1.SentID;
                S :=  'SMS Queued for Delivery, Reference: ' + SmsWorksSentId;
                doSmsWorksCheck.Enabled := True;
            end;
            if IcsSMS1.Credits <> '' then begin
                S :=  'Credits: ' + IcsSMS1.Credits;
                LabelSmsWorksCredits.Text := S;
            end;
            if IcsSMS1.Delivery <> '' then begin
                S := 'SMS Delivery for ' + SmsWorksSentId + ': ' + IcsSMS1.Delivery;
            end;
        end
        else
            S := 'Failed: ' + IcsSMS1.LastError;
        LabelSmsWorksResult.Text := 'Result: ' + S;
    end;
    AddLog ('SMS Result: ' + S);
    Beep;
end;

procedure THttpRestForm.IcsSMS1SmsProg(Sender: TObject; LogOption: TLogOption;
  const Msg: string);
begin
    AddLog (Msg);
end;


procedure THttpRestForm.TwitSetup;       { V8.65 }
begin
    OpenLogFile;
    TwitResultLabel.Text := 'Result: Pending';
    IcsTwitter1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    IcsTwitter1.ConApiKey := TwitApiKey.Text;
    IcsTwitter1.ConApiSecret := TwitApiSecret.Text;
    IcsTwitter1.AccToken := TwitAccToken.Text;
    IcsTwitter1.AccTokSecret := TwitAccSecret.Text;
    IcsTwitter1.ForceLogin := TwitForceLogin.IsChecked;
    IcsTwitter1.OAAuthType := TOAuthType(TwitAuthType.ItemIndex);   { V8.71 }
    IcsTwitter1.OAEdgeCacheDir := OAuthEdgeCache.Text;              { V8.71 }
end;

procedure THttpRestForm.IcsTwitter1TwitNewToken(Sender: TObject);
begin
    with Sender as TIcsTwitter do begin
        TwitAccToken.Text := AccToken;
        TwitAccSecret.Text := AccTokSecret;
        TwitScrnName.Text := AccScreenName;
        TwitScrnId.Text := AccUserId;
    end;
    TwitResultLabel.Text := 'Result: Got New Twitter Token OK for ' + TwitScrnName.Text;
    AddLog (TwitResultLabel.Text + IcsCRLF);
end;


procedure THttpRestForm.doTwitLoginClick(Sender: TObject);       { V8.65 }
begin
    TwitSetup;
    if IcsTwitter1.StartAuthorization then begin
        TwitResultLabel.Text := 'Result: Started Login Process';
    end
    else begin
        TwitResultLabel.Text := 'Result: ' + IcsTwitter1.LastError;
        DisplayJson(IcsTwitter1.ResponseJson);
    end;
    AddLog (TwitResultLabel.Text);
    TimerLogTimer(Self); // update log window
end;


procedure THttpRestForm.doTwitMsgClick(Sender: TObject);
begin
    TwitSetup;
    if IcsTwitter1.SendTweet(TwitMsg.Lines.Text) then begin
        TwitScrnName.Text := IcsTwitter1.AccScreenName;
        TwitScrnId.Text := IcsTwitter1.AccUserId;
        TwitIdList.Text := IcsTwitter1.LastTweetId;
        TwitResultLabel.Text := 'Result: Sent Tweet OK for ' +
                            TwitScrnName.Text + ' with ID ' + TwitIdList.Text;
    end
    else
        TwitResultLabel.Text := 'Result: ' + IcsTwitter1.LastError;
    AddLog (TwitResultLabel.Text + IcsCRLF);
    DisplayJson(IcsTwitter1.ResponseJson);
    TimerLogTimer(Self); // update log window
end;

procedure THttpRestForm.doTwitAccSettClick(Sender: TObject);
begin
    TwitSetup;
    if IcsTwitter1.GetAccSett then begin
        TwitResultLabel.Text := 'Result: Found Account Settings';
    end
    else
        TwitResultLabel.Text := 'Result: ' + IcsTwitter1.LastError;
    AddLog (TwitResultLabel.Text + IcsCRLF);
    DisplayJson(IcsTwitter1.ResponseJson);
    AddLog (IcsTwitter1.ResponseJson.AsJson(true,false));
    TimerLogTimer(Self); // update log window
end;

procedure THttpRestForm.doTwitQueryClick(Sender: TObject);
var
    StatusesJson, TweetJson: ISuperObject;
    I, tot: Integer;
    AllHdrs: String;
begin
    TwitSetup;
    if IcsTwitter1.SearchTweets(TwitQuery.Text) then begin
        TwitResultLabel.Text := 'Result: Found Some Tweets';
        AddLog (TwitResultLabel.Text + IcsCRLF);
        DisplayJson(IcsTwitter1.ResponseJson);
        AddLog (IcsTwitter1.ResponseJson.AsJson(true,false));
        StatusesJson := IcsTwitter1.ResponseJson.O['statuses'];
        tot := 0;
        if Assigned(StatusesJson) then
            tot := StatusesJson.AsArray.Length;
        if (tot > 0) then begin
            AllHdrs := '';
            for I := 0 to tot - 1 do begin
                TweetJson := StatusesJson.AsArray.O[I];
                AllHdrs := AllHdrs +
                     'TweetId: ' + TweetJson.S['id_str'] +
                     ', Text: ' + TweetJson.S['text'] +
                     ', Created: ' + TweetJson.S['created_at'] +
                     ', Source: ' + TweetJson.S['source'] + IcsCRLF;
            end;
            AddLog (AllHdrs);
        end;
    end
    else begin
        TwitResultLabel.Text := 'Result: ' + IcsTwitter1.LastError;
        AddLog (TwitResultLabel.Text + IcsCRLF);
        DisplayJson(IcsTwitter1.ResponseJson);
    end;


    TimerLogTimer(Self); // update log window
end;

procedure THttpRestForm.doTwitGetIdClick(Sender: TObject);
begin
    TwitSetup;
    if IcsTwitter1.ListTweets(TwitIdList.Text) then begin
        TwitResultLabel.Text := 'Result: Found Some Tweets';
    end
    else
        TwitResultLabel.Text := 'Result: ' + IcsTwitter1.LastError;
    AddLog (TwitResultLabel.Text + IcsCRLF);
    DisplayJson(IcsTwitter1.ResponseJson);
    AddLog (IcsTwitter1.ResponseJson.AsJson(true,false));
    TimerLogTimer(Self); // update log window
end;

procedure THttpRestForm.EmailSetup;       { V8.65 }
begin
    OpenLogFile;
    EmailOAResultLabel.Text := 'Result: Pending';
    IcsRestEmail1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
    IcsRestEmail1.RestEmailType := TRestEmailType(EmailRestType.ItemIndex);
    IcsRestEmail1.ClientId := EmailClientId.Text;
    IcsRestEmail1.ClientSecret := EmailClientSecret.Text;
    IcsRestEmail1.AccToken := EmailAccToken.Text;
    IcsRestEmail1.RefrToken := EmailRefrToken.Text;
    IcsRestEmail1.AccExpireDT := RFC3339_StrToDate(EmailAccExpiry.Text);
    IcsRestEmail1.ForceLogin := EmailForceLogin.IsChecked;
    IcsRestEmail1.AccountHint := EmailAccountHint.Text;
    if (IcsRestEmail1.AccountHint = '') and (EmailAccount.Text <> 'me') then
        IcsRestEmail1.AccountHint := EmailAccount.Text;                { V8.71 }
    IcsRestEmail1.MsUserAuth := EmailUserAuth.Text;    { V8.70 }
    IcsRestEmail1.OAAuthType := TOAuthType(EmailAuthType.ItemIndex);   { V8.71 }
    IcsRestEmail1.OAEdgeCacheDir := OAuthEdgeCache.Text;               { V8.71 }
end;

procedure THttpRestForm.IcsRestEmail1EmailNewToken(Sender: TObject);
var
    S: String;
begin
    with Sender as TIcsRestEmail do begin
        EmailAccToken.Text := AccToken;
        if RefrToken <> '' then begin
            EmailRefrToken.Text := RefrToken;
        end;
        if NewAccName = '' then
            S := NewAccEmail
        else
            S := '"' + NewAccName + '" <' + NewAccEmail + '>';
        EmailTokenAccnt.Text := S;
        EmailFrom.Text := S;
        EmailAccExpiry.Text := RFC3339_DateToStr(AccExpireDT);
        DisplayJson(ResponseJson);    // profile
    end;
    EmailOAResultLabel.Text := 'Result: Got New Email Token OK for ' + EmailTokenAccnt.Text;
    AddLog (EmailOAResultLabel.Text + IcsCRLF);
end;

 procedure THttpRestForm.doEmaiTstRedirClick(Sender: TObject);
begin
    EmailSetup;
    IcsRestEmail1.TestRedirect;
end;

procedure THttpRestForm.doEmailOAConsClick(Sender: TObject);
begin
    EmailSetup;
    IcsRestEmail1.LaunchConsole;
end;

procedure THttpRestForm.doEmailOALoginClick(Sender: TObject);
begin
    EmailSetup;
    IcsRestEmail1.StartAuthorization;
end;

procedure THttpRestForm.doEmailOARefreshClick(Sender: TObject);
begin
    EmailSetup;
    IcsRestEmail1.UpdateToken;
end;

{ this is similar to POP3 get UIDL command getting identifiers for each
  message in the mailbox }
procedure THttpRestForm.doEmailListClick(Sender: TObject);
var
    MsgJson: ISuperObject;
    I: Integer;
    MBIds: String;
begin
    EmailSetup;
    EmailReadMessId.Items.Clear;
    LabelEmailResult.Text := 'Getting Email ID List';
    IcsRestEmail1.GetNewToken(True);  // login to get token
 // mailbox labels include: INBOX,SENT,UNREAD,TRASH,
 // CATEGORY_PERSONAL,IMPORTANT,CATEGORY_UPDATES,CATEGORY_PROMOTIONS
    MBIds := '';  // all labels
    if EmailHdrFolder.ItemIndex >= 0 then
        MBIds := EmailHdrFolder.Items[EmailHdrFolder.ItemIndex];
    if IcsRestEmail1.ListEmails('', MBIds, 100) then begin
        DisplayJson(IcsRestEmail1.ResponseJson);
        MsgJson := IcsRestEmail1.ResponseJson.O['messages'];
        if Assigned(MsgJson) and (MsgJson.AsArray.Length > 0) then begin
            for I := 0 to MsgJson.AsArray.Length - 1 do
                EmailReadMessId.Items.Add(MsgJson.AsArray.O[0].S['id']);
        end;
        LabelEmailResult.Text := 'Finished Getting Email ID List';
    end
    else begin
        DisplayJson(IcsRestEmail1.ResponseJson);
        LabelEmailResult.Text := 'List IDs Failed: ' + IcsRestEmail1.LastError;
        AddLog (LabelEmailResult.Text);
    end;
end;

{ similar to doEmailListClick, but also reads some headers for each message,
  similar to using POP3 TOP command, takes a while for large mailboxes, you
  should save the headers if not deleting email }
procedure THttpRestForm.doEmailHdrsClick(Sender: TObject);
var
    MsgJson, PayloadJson, HdrsJson: ISuperObject;
    I, J, errs, tot: Integer;
    MessId, HdrLine, AllHdrs, HdrName, MBIds: String;

    function GetEmail(HdrJson: ISuperObject): String;
    var
        EmailJson: ISuperObject;
        I, tot: Integer;
    begin
        Result := '';
        if NOT Assigned(HdrJson) then Exit;
        try
            if HdrJson.DataType = stArray then begin
                tot := HdrJson.AsArray.Length;
                if tot = 0 then Exit;
                for I := 0 to tot - 1 do begin
                    if I > 0 then Result := Result + ', ';
                    EmailJson := HdrJson.AsArray.O[I].O['emailAddress'];
                    if Assigned(EmailJson) then
                        Result := '"' + EmailJson.S['name'] + '" <' + EmailJson.S['address'] + '>';
                end;
            end
            else begin
                EmailJson := HdrJson.O['emailAddress'];
                if Assigned(EmailJson) then
                    Result := '"' + EmailJson.S['name'] + '" <' + EmailJson.S['address'] + '>';
            end;
        except
        end;
    end;

begin
    EmailSetup;
    EmailReadMessId.Items.Clear;
    LabelEmailResult.Text := 'Getting Email ID List';
    IcsRestEmail1.GetNewToken(True);  // login to get token
    MBIds := '';
    if EmailHdrFolder.ItemIndex >= 0 then
        MBIds := EmailHdrFolder.Items[EmailHdrFolder.ItemIndex];
    if IcsRestEmail1.RestEmailType = RestEmailGoogle then begin
        IcsRestEmail1.HdrFieldList := 'to,from,subject,date';  // which fields we want
        MBIds := Uppercase(MBIds);
    end
    else if IcsRestEmail1.RestEmailType = RestEmailMSRest then begin
        IcsRestEmail1.HdrFieldList :=
           'toRecipients,from,subject,sentDateTime,bodyPreview';  // which fields we want
        MBIds := Lowercase(MBIds);
    end;
    if IcsRestEmail1.ListEmails('', MBIds, 100) then begin   // only 100 headers
        DisplayJson(IcsRestEmail1.ResponseJson);
        if IcsRestEmail1.RestEmailType = RestEmailGoogle then begin
            MsgJson := IcsRestEmail1.ResponseJson.O['messages'];
            tot := 0;
            if Assigned(MsgJson) then
                tot := MsgJson.AsArray.Length;
            if (tot > 0) then begin
                AddLog ('');
                AllHdrs := '';
                errs := 0;

            { note: Google offers a batch API, that allows a single HTTP request with
               Content-Type: multipart/mixed; boundary=batch_foobarbazto including
               multiple GET/POST request headers in the body, with multiple responses,
               all separated by --boundary. We don't do it yet. }
                AddLog ('Getting Headers for Total Messages: ' + IntToStr(tot));
                for I := 0 to tot - 1 do begin
                    LabelEmailResult.Text := 'Getting Header ' + IntToStr(I + 1) + ' of ' + IntToStr(tot);
                    MessId := MsgJson.AsArray.O[I].S['id'];
                    EmailReadMessId.Items.Add(MessId);
                    if IcsRestEmail1.GetEmail(MessId, EmailFmtHdr) then begin
                        if I = 0 then
                            DisplayJson(IcsRestEmail1.ResponseJson);  // just first
                        if IcsRestEmail1.RestEmailType = RestEmailGoogle then begin
                            PayloadJson := IcsRestEmail1.ResponseJson.O['payload'];
                        // headers are an array of objects with name:value for each header line
                            HdrsJson := PayloadJson.O['headers'];
                            if Assigned(HdrsJson) and (HdrsJson.AsArray.Length > 0) then begin
                                HdrLine := 'MessId: ' + MessId;
                                for J := 0 to HdrsJson.AsArray.Length - 1 do begin
                                    HdrName := HdrsJson.AsArray.O[J].S['name'];
                                    if (HdrName = 'To') or (HdrName = 'From') or
                                         (HdrName = 'Subject') or (HdrName = 'Date')   then
                                         HdrLine := HdrLine + ', ' + HdrName + ': ' + HdrsJson.AsArray.O[J].S['value'];
                                end;
                                HdrLine := HdrLine + ', Size ' + IcsRestEmail1.ResponseJson.S['sizeEstimate'] +
                                                     ', Labelst ' + IcsRestEmail1.ResponseJson.S['labelIds'] +
                                                            ', Snippet ' + IcsRestEmail1.ResponseJson.S['snippet'];
                                AllHdrs := AllHdrs + HdrLine + IcsCRLF;
                            end;
                        end;
                    end
                    else
                        errs := errs + 1;
                    if errs > 4 then break;
                end;
                AddLog (AllHdrs);
            end;
            LabelEmailResult.Text := 'Finished Getting Headers for ' + MBIds +
                                                ', Total Messages: ' + IntToStr(tot);
        end
        else if IcsRestEmail1.RestEmailType = RestEmailMSRest then begin
            MsgJson := IcsRestEmail1.ResponseJson.O['value'];
            tot := 0;
            if Assigned(MsgJson) then
                tot := MsgJson.AsArray.Length;
            if (tot > 0) then begin
                AddLog ('');
                AllHdrs := '';
                for I := 0 to tot - 1 do begin
                    MessId := MsgJson.AsArray.O[I].S['id'];
                    EmailReadMessId.Items.Add(MessId);
                    HdrLine := MsgJson.AsArray.O[I].S['bodyPreview'];
                    HdrLine := StringReplace(HdrLine, IcsCRLF, '\', [rfReplaceAll]);  // remove line feeds
                    HdrLine := 'MessId: ' + MessId +
                        ', To: ' + GetEmail(MsgJson.AsArray.O[I].O['toRecipients']) +
                        ', From: ' + GetEmail(MsgJson.AsArray.O[I].O['from']) +
                        ', Date: ' + MsgJson.AsArray.O[I].S['sentDateTime'] +
                        ', Subject: ' + MsgJson.AsArray.O[I].S['subject'] +
                        ', Snippet ' + HdrLine;
                     AllHdrs := AllHdrs + HdrLine + IcsCRLF;
                end;
                AddLog (AllHdrs);
            end;
            LabelEmailResult.Text := 'Finished Getting Headers, Total Messages: ' + IntToStr(tot);
        end;
    end
    else begin
        DisplayJson(IcsRestEmail1.ResponseJson);
        LabelEmailResult.Text := 'List IDs Failed: ' + IcsRestEmail1.LastError;
    end;
    AddLog (LabelEmailResult.Text);
    AddLog ('');
    TimerLogTimer(Self); // update log window
end;

{ read a single message from the mailbox by ID, Google returns email in
two ways: raw if the entire SMTP message, full has the headers parsed into
separate lines, and MIME messages parsed into separate sectios.  }
procedure THttpRestForm.doEmailReadClick(Sender: TObject);
var
    PayloadJson, HdrsJson, PartsJson: ISuperObject;
    I: Integer;
    EmailFmt: TRestEmailFmt;
    Msg, MessId, Content: String;
begin
    if EmailReadMessId.ItemIndex < 0 then Exit; ;
    MessId := EmailReadMessId.Items[EmailReadMessId.ItemIndex];
    LabelEmailResult.Text := 'Getting Email Content for ID: ' + MessId;
    EmailSetup;
    IcsRestEmail1.GetNewToken(True);  // login to get token
    EmailFmt := EmailFmtFull;
    if EmailRawHdrs.IsChecked then
        EmailFmt := EmailFmtRaw;
    if IcsRestEmail1.RestEmailType = RestEmailMSRest then begin
        IcsRestEmail1.HdrFieldList := 'toRecipients,from,subject,sentDateTime,internetMessageHeaders,body';
    end;
    if IcsRestEmail1.GetEmail(MessId, EmailFmt) then begin
        DisplayJson(IcsRestEmail1.ResponseJson);
        AddLog (IcsRestEmail1.ResponseJson.AsJson(true,false));
        AddLog ('');
        if IcsRestEmail1.RestEmailType = RestEmailGoogle then begin
            if EmailFmt = EmailFmtRaw then begin
                AddLog ('MessId: ' + MessId + ' - Raw Content');
                Msg := IcsRestEmail1.ResponseJson.S['raw'];
                AddLog (IcsBase64UrlDecode(Msg));
            end
            else if EmailFmt = EmailFmtFull then begin
                PayloadJson := IcsRestEmail1.ResponseJson.O['payload'];
            // headers are an array of objects with name:value for each header line
                HdrsJson := PayloadJson.O['headers'];
                if Assigned(HdrsJson) and (HdrsJson.AsArray.Length > 0) then begin
                    AddLog ('MessId: ' + MessId + ' - Headers');
                    for I := 0 to HdrsJson.AsArray.Length - 1 do
                        AddLog (HdrsJson.AsArray.O[I].S['name'] + ': ' +
                                             HdrsJson.AsArray.O[I].S['value']);
                end;
            // may only have a single body
                Msg := PayloadJson.S['body.data'];
            // or several MIME parts, check for text MIME types
                if Msg = '' then begin
                    PartsJson := PayloadJson.O['parts'];
                    if Assigned(PartsJson) and (PartsJson.AsArray.Length > 0) then begin
                        AddLog ('');
                        AddLog ('MessId: ' + MessId + ' - MIME Parts Tota: ' + InttoStr(PartsJson.AsArray.Length));
                        for I := 0 to PartsJson.AsArray.Length - 1 do begin
                            if Pos ('text/', PartsJson.AsArray.O[I].S['mimeType']) = 1 then begin
                                AddLog ('');
                                AddLog ('Content Part ' + IntToStr(I + 1));
                                Msg := PartsJson.AsArray.O[I].S['body.data'];
                                AddLog (IcsBase64UrlDecode(Msg));   // email is base64 encoded
                            end;
                        end;
                    end;
                end
                else begin
                    AddLog ('MessId: ' + MessId + ' - Main Content');
                    AddLog (IcsBase64UrlDecode(Msg));  // email is base64 encoded
                end;
            end;
        end
        else if IcsRestEmail1.RestEmailType = RestEmailMSRest then begin
        // headers are an array of objects with name:value for each header line
            HdrsJson := IcsRestEmail1.ResponseJson.O['internetMessageHeaders'];
            if Assigned(HdrsJson) and (HdrsJson.AsArray.Length > 0) then begin
                AddLog ('MessId: ' + MessId + ' - Headers');
                for I := 0 to HdrsJson.AsArray.Length - 1 do
                    AddLog (HdrsJson.AsArray.O[I].S['name'] + ': ' +
                                         HdrsJson.AsArray.O[I].S['value']);
            end;
            AddLog ('');
            Content := IcsRestEmail1.ResponseJson.S['body.contentType'];
            if (Content = 'text') or (Content = 'html') then
                Msg := IcsRestEmail1.ResponseJson.S['body.content']
            else
                Msg := 'Body Content=' + Content;
            AddLog (Msg);
        end;
        LabelEmailResult.Text := 'Finished Getting Email Content';
    end
    else begin
        DisplayJson(IcsRestEmail1.ResponseJson);
        LabelEmailResult.Text := 'Read Email Failed: ' + IcsRestEmail1.LastError;
        AddLog (LabelEmailResult.Text);
    end;
    TimerLogTimer(Self); // update log window
end;

{ send a single email. Google accepts a complete rfc822 SMTP format message from which
  it uses the To:, CC: and BCC: headers to send the email. Microsoft Outlook builds
  the email from Json.  }
procedure THttpRestForm.doEmailSendClick(Sender: TObject);
var
    I: Integer;
    recip, fname, errresp: String;
    MsgJson, RecipJson: ISuperObject;
begin
    LabelEmailResult.Text := 'Sending Email';
    EmailSetup;
    IcsRestEmail1.GetNewToken(True);  // login to get token

// Google gmail, create the message using the SMTP Client component, but you can
// do so manually with headers and body for simple messages.
    if IcsRestEmail1.RestEmailType = RestEmailGoogle then begin
        SslHtmlSmtpCli1.EmailFiles.Clear;
        SslHtmlSmtpCli1.RcptName.Clear;  // Google does not need this
        SslHtmlSmtpCli1.Allow8bitChars := true;
        SslHtmlSmtpCli1.HdrFrom := EmailFrom.Text;
        SslHtmlSmtpCli1.FromName := ParseEmail(EmailFrom.Text, fname);
        SslHtmlSmtpCli1.HdrCc := EmailCC.Text;
        SslHtmlSmtpCli1.HdrReplyTo := SslHtmlSmtpCli1.FromName ;
        SslHtmlSmtpCli1.HdrSubject := EmailSubject.Text;
        SslHtmlSmtpCli1.ContentType := smtpPlainText;
        SslHtmlSmtpCli1.PlainText.Text := EmailMessage.Text;
        recip := '';
        for I := 0 to EmailRecipList.Lines.Count - 1 do begin
            if Pos('*',  EmailRecipList.Lines [I]) = 1 then continue;
            if Pos('@',  EmailRecipList.Lines [I]) = 0 then continue;
            if I > 0 then recip := recip + ',';
            recip := recip + Trim(EmailRecipList.Lines [I]);
        end;
        SslHtmlSmtpCli1.HdrTo := recip;
     // saves to file stream, which we read in onBeforeOutStreamFree event
        SmtpRawMsg := '';
        SslHtmlSmtpCli1.SendMode := smtpToStream;
        fname := IcsGetTempPath + IntToStr(Int64(GetTickCount)) + '.tmp';
        AddLog ('Preparing Email Raw Message');
        SslHtmlSmtpCli1.SendToFileSync (fname) ;
        errresp := SslHtmlSmtpCli1.LastResponse ;
        DeleteFile(fname); // don't need it
        if SmtpRawMsg = '' then begin
            AddLog ('Failed to Read Email Raw Message');
            exit;
        end;
    end;

// Microsoft Outlook builds Json message block, similar to received messages
    if IcsRestEmail1.RestEmailType = RestEmailMSRest then begin
        RecipJson := SA([]);    // empty array
        for I := 0 to EmailRecipList.Lines.Count - 1 do begin
            if Pos('*',  EmailRecipList.Lines [I]) = 1 then continue;
            if Pos('@',  EmailRecipList.Lines [I]) = 0 then continue;
            RecipJson.O[''] :=  SO(['emailAddress',  // add objects to array
                                    SO(['address', ParseEmail(Trim(EmailRecipList.Lines [I]), fname), 'name', fname]) ]);
        end;
        MsgJson := SO(['subject', EmailSubject.Text,
                           'body', SO(['contentType', 'text', 'content', EmailMessage.Text]),
                           'toRecipients', RecipJson,
                           'from', SO(['emailAddress', SO(['address', ParseEmail(Trim(EmailFrom.Text), fname), 'name', fname]) ])
                       ]);
        if EmailCC.Text <> '' then begin
            MsgJson.O['ccRecipients'] := SA([]);    // empty array
            MsgJson.A['ccRecipients'].Add(SO(['emailAddress',  // add object to array
                                    SO(['address', ParseEmail(Trim(EmailCC.Text), fname), 'name', fname]) ]));
        end;
        SmtpRawMsg := MsgJson.AsJson;
    end;

//    AddLog (SmtpRawMsg);  // TEMP show raw email
    AddLog ('Sending Email');
    if IcsRestEmail1.SendEmail(SmtpRawMsg) then begin
       if Assigned(IcsRestEmail1.ResponseJson) then begin  // Microsoft Outlook has no response
            DisplayJson(IcsRestEmail1.ResponseJson);
            LabelEmailResult.Text := 'Sent Email OK, ID: ' + IcsRestEmail1.LastEmailId;
            AddLog (LabelEmailResult.Text);
            AddLog (IcsRestEmail1.ResponseJson.S['labelIds']);
            EmailReadMessId.Items.Clear;
            EmailReadMessId.Items.Add(IcsRestEmail1.LastEmailId);
       end
       else begin
            LabelEmailResult.Text := 'Sent Email OK';
            AddLog (LabelEmailResult.Text);
       end;
    end
    else begin
        DisplayJson(IcsRestEmail1.ResponseJson);
        LabelEmailResult.Text := 'Sent Email Failed: ' + IcsRestEmail1.LastError;
        AddLog (LabelEmailResult.Text);
    end;
    AddLog ('');
end;

procedure THttpRestForm.SslHtmlSmtpCli1BeforeOutStreamFree(Sender: TObject);
var
    Len: Integer;
    AStr: AnsiString;
begin
 // read file stream into string for REST Json }
    if Assigned(SslHtmlSmtpCli1.OutStream) then begin
        Len := SslHtmlSmtpCli1.OutStream.Size;
        AddLog ('Email Raw Content Size: ' + IntToStr(Len));
        SetLength(AStr, Len);
        SslHtmlSmtpCli1.OutStream.Position := 0;
        SslHtmlSmtpCli1.OutStream.Read(AStr[1], Len);
        SmtpRawMsg := String(AStr);
    end;
end;

procedure THttpRestForm.doEmailDeleteClick(Sender: TObject);
var
    MessId: String;
begin
    if EmailReadMessId.ItemIndex < 0 then Exit; ;
    MessId := EmailReadMessId.Items[EmailReadMessId.ItemIndex];
    LabelEmailResult.Text := 'Deleting Email ID: ' + MessId;
    EmailSetup;
    IcsRestEmail1.GetNewToken(True);  // login to get token
    if IcsRestEmail1.DeleteEmail(MessId) then begin
        DisplayJson(IcsRestEmail1.ResponseJson);
        LabelEmailResult.Text := 'Deleted Email OK';
    end
    else begin
        DisplayJson(IcsRestEmail1.ResponseJson);
        LabelEmailResult.Text := 'Delete Email Failed: ' + IcsRestEmail1.LastError;
    end;
    AddLog (LabelEmailResult.Text);
    AddLog ('');
end;


procedure THttpRestForm.doAliveStartClick(Sender: TObject);
begin
    doAliveStart.Enabled := False;
    doAliveStop.Enabled := True;
    OpenLogFile;
    LabelInetAlive.Text := 'Starting';
    IcsInetAlive1.AliveMethod := TAliveMethod(AliveMethod.ItemIndex);
    IcsInetAlive1.AliveNets := TAliveNetwork(AliveNets.ItemIndex);
    IcsInetAlive1.DebugLevel := THttpDebugLevel(DebugLogging.ItemIndex);
// uncomment to use old servers for Windows 7 to Windows 10 RTM
 {   IcsInetAlive1.HostIPv4 :=  'www.msftncsi.com';
    IcsInetAlive1.HostIPv6 := 'ipv6.msftncsi.com';
    IcsInetAlive1.HttpPage := '/ncsi.txt';
    IcsInetAlive1.HttpText := 'Microsoft NCSI';  }
    IcsInetAlive1.Start;
    IcsInetAlive1.CheckNow;
end;

procedure THttpRestForm.doAliveStopClick(Sender: TObject);
begin
    doAliveStart.Enabled := True;
    doAliveStop.Enabled := False;
    IcsInetAlive1.Stop;
end;

procedure THttpRestForm.IcsInetAlive1AliveChange(Sender: TObject);
var
    S: String;
begin
    S := 'Internet Connectivity State: ' +
        AliveStatusLits[IcsInetAlive1.TestBothOnline] + IcsCRLF;
    S := S + 'IPv4 Connectivity State: ' +
        AliveStatusLits[IcsInetAlive1.AliveIPv4];
    if IcsInetAlive1.LastDTIPv4 > 0 then
            S := S + ', Last Online at ' +
            TimeToStr(IcsInetAlive1.LastDTIPv4);
    S := S + IcsCRLF;
    S := S + 'IPv6 Connectivity State: '+
        AliveStatusLits[IcsInetAlive1.AliveIPv6];
    if IcsInetAlive1.LastDTIPv6 > 0 then
            S := S + ', Last Online at ' +
            TimeToStr(IcsInetAlive1.LastDTIPv6);
    LabelInetAlive.Text := S;
end;




end.

