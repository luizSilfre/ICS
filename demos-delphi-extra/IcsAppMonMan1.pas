{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  The ICS Application Monitor - Remote Manager provides remote
              monitoring of multiple ICS Application Monitor servers using
              Json web and websocket requests. It illustrates use of the
              ICS Websocket Client component to update real time displays,
              in this case TListView components.
Creation:     Feb 2025
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



Feb 2025 - V9.4 - Baseline




This sample communicates with one or more IcsAppMon.exe samples.

Warning - this sample is only tested on recent Unicode compilers, it will
not build on older compilers without changes.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit IcsAppMonMan1;


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
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Mask,
  System.ImageList, Vcl.ImgList, System.IniFiles,
  OverbyteIcsIniFiles,
  OverbyteIcsUtils,
  OverbyteIcsSslUtils,
  OverbyteIcsTicks64,
  OverbyteIcsTypes,
  OverbyteIcsSslBase,
  OverbyteIcsWndControl,
  OverbyteIcsHttpProt,
  OverbyteIcsSslHttpRest,
  OverbyteIcsSuperObject,
  OverbyteIcsWebSocketcli,
  OverbyteIcsAppMonCli;      // only for types and constants common to client and server

const
    ProgName = 'IcsAppMonMan';
    ProgVersion = 'ICS Application Monitor - Remote Manager - V9.4 - 11th February 2025';
    NameIniConfig = 'IcsAppMonMan.config' ;
    WM_FM_STARTUP = WM_USER + 805 ;


type
    TSrvState = (StateNone, StateConnecting, StateConnected, StateWaitReconnect, StateDisconnected);
    TServInfo = record
        SrvState: TSrvState;
        WebsocCli: TSslWebSocketCli;
        LastStatus: String;
        HttpUrl: String;
        WSUrl: String;
        ConnectDT: TDateTime;
        PacketDT: TDateTime;
        RetryTick: Int64;
        ServerName: String;  // set from Json
        ProgVer: String;
        StartTime: String;
        BootTime: String;
        ServerOS: String;
    end;
    TServInfos = array of TServInfo;

const
    SrvStateNames:  array [TSrvState] of PChar = ('None', 'Connecting','Connected','Waiting Reconnect','Disconnected');


type
   TManForm = class(TForm)
// components saved to INI file
    LVMonitor: TListView;   // only save columns, not data
    LVServers: TListView;   // only save caption and checked, not columns
    PrefLogFile: TCheckBox;
    PrefAutoStart: TCheckBox;
    PrefLogDir: TButtonedEdit;
    PrefRetrySecs: TLabeledEdit;
    PrefLogWS: TCheckBox;

// not saved
    Panel1: TPanel;
    PageControl: TPageControl;
    TabMonitor: TTabSheet;
    TabSettings: TTabSheet;
    doExit: TButton;
    doStop: TButton;
    doStart: TButton;
    TimerMain: TTimer;
    BoxEditServer: TGroupBox;
    NewURL: TLabeledEdit;
    NewTest: TButton;
    NewAdd: TButton;
    NewReplace: TButton;
    NewDelete: TButton;
    TabLog: TTabSheet;
    TabDetails: TTabSheet;
    LogWin: TMemo;
    LVAppDetails: TListView;
    GroupBox2: TGroupBox;
    SslContextShared: TSslContext;
    LabelTestUrl: TLabel;
    HttpServerTest: TSslHttpRest;
    OpenDialog: TOpenDialog;
    ImageList: TImageList;
    LabelLastChange: TLabel;
    PrefSockFamily: TRadioGroup;
    procedure LVServersClick(Sender: TObject);
    procedure NewTestClick(Sender: TObject);
    procedure NewAddClick(Sender: TObject);
    procedure NewReplaceClick(Sender: TObject);
    procedure NewDeleteClick(Sender: TObject);
    procedure doStartClick(Sender: TObject);
    procedure doStopClick(Sender: TObject);
    procedure PrefLogDirRightButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure TimerMainTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HttpServerTestHttpRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
    procedure LVMonitorData(Sender: TObject; Item: TListItem);
    procedure LVMonitorClick(Sender: TObject);
    procedure doExitClick(Sender: TObject);
    procedure LVMonitorCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure LVServersCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    { Private declarations }

  public
    { Public declarations }
    procedure AddLog(S1: string);
    procedure CloseLogFile;
    procedure FlushLogFile(OldFName: Boolean = False) ;
    procedure OpenLogFiles;
    procedure GetSettings ;
    procedure SaveSettings ;
    procedure WMFMSTARTUP (var Msg : TMessage); message WM_FM_STARTUP;
    procedure MonStart;
    procedure MonStop;
    function ConntoServer(SrvNr: Integer): Boolean;
    procedure ParseSrvJson(SrvNr: Integer; RespObj: ISuperObject);
    function TestUrl(ItemNr: Integer): Boolean;  private
    procedure WebSocHttpRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
    procedure WebSocWSConnected(Sender: TObject);
    procedure WebSocWSDisconnected(Sender: TObject);
    procedure WebSocWSFrameRcvd(Sender: TSslWebSocketCli; const APacket: string; var AFrame: TWebSocketReceivedFrame);
    procedure WebSocWSFrameSent(Sender: TSslWebSocketCli; var AFrame: TWebSocketOutgoingFrame);
  end;

var
    ManForm: TManForm;
    DiagLogBuffer: TIcsBuffLogStream ;     // temporary buffer for log
    FileIniFile: TMemIniFile;
    FileIniConfig: String;
    DirApplication: String;
    ClosingFlag: Boolean;
    LogOpenFlag: Boolean;
    ServInfos: TServInfos;    // one per server
    MonSuppApps: TIcsAMApps;  // one per monitored app=rows in TListView
    TotServers: Integer;
    MaxApps: Integer;
    TotApps: Integer;
    SrvEditNr: Integer;
    MonActive: Boolean = False;
    LogDate: TDateTime;
    CurDT: TDateTime;

const
    SectionMainWindow    = 'MainWindow';
    SectionSettings      = 'Settings';
    KeyTop               = 'Top';
    KeyLeft              = 'Left';
    KeyWidth             = 'Width';
    KeyHeight            = 'Height';
    KeyServerNames       = 'ServerNames';
    KeyServerChecks      = 'ServerChecks';
    KeyColWidths         = 'ColWidths';

// three SuppAppMon servers at Magenta Systems, to illustrate this sample, remove once you have you own servers running
    DefServerList        = 'https://www.magsys.co.uk:17780|https://www2.magsys.co.uk:17780|https://www3.magsys.co.uk:17780';

implementation

{$R *.dfm}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.FormCreate(Sender: TObject);
begin
{$WARNINGS OFF}
//    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$WARNINGS ON}
    Caption := ProgVersion;
    PageControl.ActivePage := TabMonitor;
    PostMessage (Handle, WM_FM_STARTUP, 0, 0) ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    if ClosingFlag then
        Exit ;
    ClosingFlag := true ;
    try
        SaveSettings;
        TimerMain.Enabled := false ;
        MonStop;
    except
    end ;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.FormDestroy(Sender: TObject);
begin
    try
        SetLength(ServInfos, 0);
        SetLength(MonSuppApps, 0);
        if NOT ClosingFlag then begin
            ClosingFlag := true ;
            MonStop;
        end;
        FreeAndNil (FileIniFile) ;
        CloseLogFile ;
        FreeAndNil (DiagLogBuffer) ;
    except
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.AddLog(S1: string);
var
    res: integer ;
begin
    try
        CurDT := Now;
        S1 := TimeToStr (CurDT) + IcsSpace + S1 ;
        LogWin.Lines.Add(S1);
        SendMessage (LogWin.Handle, EM_LINESCROLL, 0, 999999);  // end of window

   // file logging
        if LogOpenFlag then begin
            res := 0;
            try
                if Assigned (DiagLogBuffer) then
                    res := DiagLogBuffer.WriteLine (S1) ;
            except
                res := -1 ;
            end ;
            if (res = -1) and Assigned (DiagLogBuffer) then
                DiagLogBuffer.WriteLine (S1) ;  // one retry before exception
        end;
    except
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.CloseLogFile ;
begin
    if NOT LogOpenFlag then
        Exit;
    try
        if Assigned (DiagLogBuffer) then
        begin
            DiagLogBuffer.FlushFile ;
            DiagLogBuffer.Free ;
            DiagLogBuffer := Nil ;
        end ;
    except
    end ;
    LogOpenFlag := false ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.OpenLogFiles ;
var
    diagfname, S: string;
begin
    CloseLogFile ;

    diagfname := IcsDQUOTE + IncludeTrailingPathDelimiter(PrefLogDir.Text) + ProgName + '-"yyyymmdd".log"' ;
    S := '"' + ProgVersion + IcsCRLF +
         'Log File Created: "dd mmm yyyy hh:mm:ss"' + IcsCRLF +
         'Computer: ' + IcsGetCompName + '"' + IcsCRLF ;
    try
        DiagLogBuffer := TIcsBuffLogStream.Create (Self, diagfname, S) ;
        DiagLogBuffer.FlushFile ;
        LogOpenFlag := true ;
    except
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.FlushLogFile(OldFName: Boolean = False) ;
begin
    if NOT LogOpenFlag then
        Exit;
    try
        if Assigned (DiagLogBuffer) then begin
            DiagLogBuffer.FlushFile(OldFName)
        end ;
    except
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.GetSettings ;
var
    SL: TStringList;
    I, J: Integer;
    Checks: String;
begin
    if NOT FileExists (FileIniConfig) then  // no file, create it from form defaults
        SaveSettings ;
    FreeAndNil (FileIniFile) ;
    try
        FileIniFile := TMemIniFile.Create (FileIniConfig) ;
        with FileIniFile do
        begin
            Width := ReadInteger(SectionMainWindow, KeyWidth,  Width);
            Height := ReadInteger(SectionMainWindow, KeyHeight, Height);
            Top := ReadInteger(SectionMainWindow, KeyTop, (Screen.Height - Height) div 2);
            Left := ReadInteger(SectionMainWindow, KeyLeft, (Screen.Width  - Width)  div 2);
            PrefLogDir.Text := ReadString(SectionSettings, 'PrefLogDir', 'c:\weblogs\' + ProgName + '\') ;
            PrefRetrySecs.Text := ReadString(SectionSettings, 'PrefRetrySecs', '30') ;
            PrefLogFile.Checked := IcsCheckTrueFalse(ReadString (SectionSettings, 'PrefLogFile', 'False')) ;
            PrefAutoStart.Checked := IcsCheckTrueFalse(ReadString (SectionSettings, 'PrefAutoStart', 'False')) ;
            PrefLogWS.Checked := IcsCheckTrueFalse(ReadString (SectionSettings, 'PrefLogWS', 'False')) ;
            PrefSockFamily.ItemIndex := ReadInteger(SectionSettings, 'PrefSockFamily', 0) ;
            if atoi(PrefRetrySecs.Text) < 30 then   // sanity test
                PrefRetrySecs.Text := '30';

       // read server names and column width, perhaps names if we allow them to be re-ordered or changed
            SL := TStringList.Create;
            try
                SL.Delimiter := '|';
                SL.DelimitedText := ReadString(SectionSettings, KeyServerNames, DefServerList);
                Checks := ReadString(SectionSettings, KeyServerChecks, '');
                if SL.Count <> 0 then begin
                    for I := 0 to SL.Count - 1 do begin
                        if (Length(SL[I]) < 4)  then
                            Continue;
                        with LVServers.Items.Add do begin
                            Caption := SL[I];
                            for J := 0 to 10 do
                                SubItems.Add('');
                            if (I < Length(Checks)) then
                                Checked := (Checks [I+1] = 'Y');
                        end;
                    end;
                end;
                SL.Clear;
                SL.DelimitedText := ReadString(SectionSettings, KeyColWidths, '');
                if SL.Count <> 0 then begin
                    for I := 0 to SL.Count - 1 do begin
                        if I < LVMonitor.Columns.Count then
                            LVMonitor.Columns[I].Width := atoi(SL[I]);
                    end;
                end;
            finally
                 SL.Free;
            end;

        end ;
        FreeAndNil (FileIniFile) ;
    except
    end ;
end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.SaveSettings ;
var
    SL: TStringList;
    I: Integer;
    Checks: String;
begin
    try
        FileIniFile := TMemIniFile.Create (FileIniConfig) ;
        with FileIniFile do
        begin
            WriteInteger(SectionMainWindow, KeyTop, Top);
            WriteInteger(SectionMainWindow, KeyLeft, Left);
            WriteInteger(SectionMainWindow, KeyWidth, Width);
            WriteInteger(SectionMainWindow, KeyHeight, Height);

        // save ListView server list and monitor column widths
            SL := TStringList.Create;
            try
                SL.Delimiter := '|';
                Checks := '';
                if LVServers.Items.Count > 0 then begin
                    SetLength(Checks, LVServers.Items.Count);
                    for I := 0 to LVServers.Items.Count - 1 do begin
                        SL.Add(LVServers.Items[I].Caption);
                        if LVServers.Items[I].Checked then
                            Checks[I+1] := 'Y'
                        else
                            Checks[I+1] := 'N';
                    end;
                end;
                WriteString(SectionSettings, KeyServerNames, SL.DelimitedText);
                WriteString(SectionSettings, KeyServerChecks, Checks);
                SL.Clear;
                for I := 0 to LVMonitor.Columns.Count - 1 do
                    SL.Add(IntToStr(LVMonitor.Columns[I].Width));
                WriteString(SectionSettings, KeyColWidths, SL.DelimitedText);
            finally
                SL.Free;
            end;
            WriteString(SectionSettings, 'PrefLogDir', PrefLogDir.Text) ;
            WriteString(SectionSettings, 'PrefRetrySecs', PrefRetrySecs.Text) ;
            WriteString(SectionSettings, 'PrefLogFile', BooltoStr(PrefLogFile.Checked, True));
            WriteString(SectionSettings, 'PrefAutoStart', BooltoStr(PrefAutoStart.Checked, True));
            WriteString(SectionSettings, 'PrefLogWS', BooltoStr(PrefLogWS.Checked, True));
            WriteInteger(SectionSettings, 'PrefSockFamily', PrefSockFamily.ItemIndex);
            UpdateFile;
        end ;
        FreeAndNil (FileIniFile) ;
    except
    end ;

end ;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.WMFMSTARTUP (var Msg : TMessage);
begin
    DirApplication := ExtractFileDir (Lowercase (ParamStr(0)));
    FileIniConfig := GetIcsIniFileName;   // main config file
    GetSettings ;
    IcsSslRootCAStore.Initialise;
    TotServers := LVServers.Items.Count;
    SetLength(ServInfos, TotServers + 2);  // base 1

    if PrefAutoStart.Checked then
       MonStart;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// connect to remote server
// server returns same Json for a simple GET request, and pushed by Websocket every 30 seconds
// GET server Json once only to build MonSuppApps, then refresh ListView
function TManForm.ConnToServer(SrvNr: Integer): Boolean;
var
    Ret: Integer;
    S: String;
begin
    Result := False;
    with ServInfos[SrvNr] do begin
        try
            if WebsocCli = Nil then
                Exit;
            ConnectDT := 0;
            RetryTick := Trigger64Disabled;
            SrvState := StateConnecting;
            LVServers.Items[SrvNr-1].SubItems[1] := SrvStateNames[SrvState];  // update ListView
            LVServers.UpdateItems (SrvNr - 1, SrvNr - 1);   // repaint single ListView row
            Ret := HttpServerTest.RestRequest(httpGET, HttpUrl + WebServStatusJson, False, '');  // sync request
            if Ret = 200 then begin
                SrvState := StateConnected;  // before Json display
                ConnectDT := Now;
                ParseSrvJson(SrvNr, HttpServerTest.ResponseJson);  // fills ServInfos and MonSuppApps arrays from Json response
                HttpServerTest.Close;
                WebsocCli.URL := WSUrl + WebSocStatusJson;
                WebSocCli.WSPingSecs := 0;  // no keep-alive ping/pongs
                if WebSocCli.WSConnect then begin   // sync request - pending need async request !!!!
                    LastStatus := 'Websocket Connected OK';
                    Result := True;
                end
                else begin
                    SrvState := StateWaitReconnect;   // try again
                    RetryTick := IcsGetTrgSecs64(atoi(PrefRetrySecs.Text)); // when
                    LastStatus := 'Websocket Failed to Connect, Will Retry';
                end;
            end
            else begin
                HttpServerTest.Abort;
                HttpServerTest.Close;
                SrvState := StateWaitReconnect;   // try again
                RetryTick := IcsGetTrgSecs64(atoi(PrefRetrySecs.Text)); // when
                LastStatus := 'HTTP Failed to Connect, Will Retry';
            end;
        except
            S :='HTTP Failed to Connect: ' + IcsGetExceptMess (ExceptObject) ;
            LastStatus := S;
            AddLog(S);
            HttpServerTest.Abort;
            HttpServerTest.Close;
            SrvState := StateWaitReconnect;   // try again
            RetryTick := IcsGetTrgSecs64(atoi(PrefRetrySecs.Text)); // when
        end;
        if NOT MonActive then
            SrvState := StateNone;
        AddLog(ServerName + ': ' + LastStatus);
        LVServers.Items[SrvNr-1].SubItems[1] := SrvStateNames[SrvState];  // update ListView
        LVServers.UpdateItems (SrvNr - 1, SrvNr - 1);   // repaint single ListView row
    end
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.MonStart;
var
    I: Integer;
    AnyOK: Boolean;
begin
    SaveSettings;
    if NOT doStart.Enabled then
        Exit;
    TotServers := LVServers.Items.Count;
    if TotServers = 0 then begin
        AddLog('No Servers Configured');
        Exit;
    end;
    doStart.Enabled := False;
    doStop.Enabled := True;
    BoxEditServer.Visible := False;
    LogDate := Trunc(CurDT);
    if PrefLogFile.Checked then
        OpenLogFiles;
    AddLog('INI File: ' + FileIniConfig);
    AddLog('Starting Monitoring');
    MonActive := True;
    AnyOK := False;
    HttpServerTest.SocketFamily := TSocketFamily(PrefSockFamily.ItemIndex);

// array with one record for each server, with WebsocketCli and status info
    SetLength(ServInfos, TotServers + 2);  // base 1
    MaxApps := TotServers * 10;  // allow 10 apps per server, initially
    SetLength(MonSuppApps, MaxApps + 1);  // base 1
    TotApps := 0;

// build server records and start Websocket connections
    for I := 1 to TotServers do begin
        with ServInfos[I] do begin
            WebsocCli := Nil;
            SrvState := StateNone;
            ConnectDT := 0;
            ServerName := '';
            HttpUrl := LVServers.Items[I-1].Caption;
            if (LVServers.Items[I-1].Checked) then begin
                if HttpServerTest.IsKnownProtocolURL(HttpUrl) then begin
                    ServerName := HttpUrl;  // temp URL, replaced once first packet arrives

                // create WebSocket and it's events for updates for the arrays
                    WSUrl := 'ws' + Copy(HttpUrl, 5, 999);
                    WebsocCli := TSslWebSocketCli.Create(Self);
                    WebsocCli.Tag := I;  // base 1
                    WebsocCli.SharedSslCtx := True;    // use shared context for all websockets
                    WebsocCli.SslContext := SslContextShared;
                    WebsocCli.SocketFamily := TSocketFamily(PrefSockFamily.ItemIndex);
                    WebsocCli.DebugLevel := DebugConn;
                    WebsocCli.ResponseNoException := True;
                    if PrefLogWS.Checked then
                        WebsocCli.DebugLevel := DebugBody;
                    WebsocCli.OnHttpRestProg := WebSocHttpRestProg;
                    WebsocCli.OnWSFrameRcvd := WebSocWSFrameRcvd;
                    WebsocCli.OnWSFrameSent := WebSocWSFrameSent;
                    WebsocCli.OnWSConnected := WebSocWSConnected;
                    WebsocCli.OnWSDisconnected := WebSocWSDisconnected;
                    WebsocCli.URL := WSUrl + WebSocStatusJson;
                    WebSocCli.WSPingSecs := 0;  // no keep-alive ping/pongs

                // connect to HTTP server once, then open Websocket connection
                    if ConnToServer(I) then
                        AnyOK := True;   // if fails, will retry from timer in 30 seconds
              end
                else begin
                    LastStatus := 'Invalid Server URL, Ignored - ' + HttpUrl;
                end;
                AddLog(ServerName + ': ' + LastStatus);
            end;
        end;
    end;
    FlushLogFile;

 // nothing started, stop unless auto start
    if (NOT AnyOK) and (NOT PrefAutoStart.Checked) then
        MonStop
    else
        TimerMain.Enabled := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.MonStop;
var
    I: Integer;
begin
    if NOT doStop.Enabled then
        Exit;
    TimerMain.Enabled := False;
    MonActive := False;
    AddLog('Stopping Monitoring');
    FlushLogFile;
    BoxEditServer.Visible := True;
    doStart.Enabled := True;
    doStop.Enabled := False;
    if TotServers > 0 then begin
        for I := 1 to TotServers do begin
            with ServInfos[I] do begin
                if Assigned(WebsocCli) then begin
                    LastStatus := 'Closing Websocket';
                    if WebsocCli.IsWSConnected then
                        WebsocCli.WSClose(wscrNormalClosure, '');
                     WebsocCli.Destroy;
                     LastStatus := 'Closed Websocket';
                     AddLog(ServerName + ': ' + LastStatus);
                end;
                WebsocCli := Nil;
                RetryTick := Trigger64Disabled;
                SrvState := StateNone;
                LVServers.Items[I-1].SubItems[1] := SrvStateNames[SrvState];  // update ListView
                LVServers.Items[I-1].SubItems[3] := LastStatus;  // update ListView
            end;
        end;
    end;
    CloseLogFile;
    LabelLastChange.Caption := 'Monitoring Stopped at ' + TimeToStr(Now);
    LVMonitor.Refresh;  // repaint all rows
    LVServers.Refresh;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.TimerMainTimer(Sender: TObject);
var
    I: Integer;
begin
    TimerMain.Enabled := False;
    try
        CurDT := Now;

    // check if new connection attempt needed
        for I := 1 to TotServers do begin
            with ServInfos[I] do begin
                if SrvState = StateWaitReconnect then begin
                    if IcsTestTrgTick64(RetryTick) then begin
                        LastStatus := 'Retry Connection to Server';
                        AddLog(ServerName + ': ' + LastStatus);
                        if NOT ConnToServer(I) then begin
                            RetryTick := IcsGetTrgSecs64(atoi(PrefRetrySecs.Text)); // try again soon
                        end;
                        FlushLogFile;
                    end;
                end;
            end;
        end;

    // midnight log rotation
        if Trunc(CurDT) <> LogDate then begin
            LogDate := Trunc(CurDT);
            FlushLogFile(True);  // use new date name
          // should we log something?
        end;

    finally
        TimerMain.Enabled := True;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.doStopClick(Sender: TObject);
begin
    MonStop;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.doExitClick(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.doStartClick(Sender: TObject);
begin
    MonStart;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.LVMonitorClick(Sender: TObject);
begin
//
end;

procedure TManForm.LVMonitorCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
    Appnr: Integer;
    ColorStr: String;
begin
    Appnr := Item.Index + 1;
    ColorStr := MonSuppApps[Appnr].AppColorDisp;
    if Length(ColorStr) < 6 then
        Exit;
    ColorStr := '$00' + Copy(ColorStr, 5, 2) + Copy(ColorStr, 3, 2) + Copy(ColorStr, 1, 2);
    Sender.Canvas.Brush.Color := StrToInt(ColorStr);  // hex to int
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.LVMonitorData(Sender: TObject; Item: TListItem);
var
    Appnr: Integer;
begin
    Appnr := Item.Index + 1;
     with MonSuppApps[Appnr] do begin
        Item.Caption := AppCompName;
        Item.SubItems.Add(AppClientTitle);
        if ServInfos[AppSevrNr].SrvState = StateConnected then
            Item.SubItems.Add(AppStateInfoDisp)
        else begin
            Item.SubItems.Add('Not Monitoring');  // information is stale
            AppColorDisp := sColorPYell;
        end;
        Item.SubItems.Add(AppLastOKDisp);
        Item.SubItems.Add(AppStartStopDisp);
        Item.SubItems.Add(AMModeCmds[AppMonMode]);
        Item.SubItems.Add(AppServiceName);
        Item.SubItems.Add(AppStatus);
        Item.SubItems.Add(ExtractFileName(AppExeFile));
        Item.SubItems.Add(IntToStr(AppClientId));
        Item.SubItems.Add(AppGeneral);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// select server for editing
procedure TManForm.LVServersClick(Sender: TObject);
begin
    if LVServers.ItemIndex >= 0 then begin
        SrvEditNr := LVServers.ItemIndex;
        NewURL.Text := LVServers.Items[SrvEditNr].Caption;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.LVServersCustomDrawItem(Sender: TCustomListView; Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
    SrvNr: Integer;
begin
    SrvNr := Item.Index + 1;
    if SrvNr < Length(ServInfos) then begin
        if ServInfos[SrvNr].SrvState = StateConnected then
            Sender.Canvas.Brush.Color := clLime
         else
            Sender.Canvas.Brush.Color := clWebYellow;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.NewAddClick(Sender: TObject);
var
    J: Integer;
begin
    SrvEditNr := -1;
    if TestUrl(-1) then begin
        with LVServers.Items.Add do begin
            Caption := NewURL.Text;
            SubItems.Add(ServInfos[0].ServerName);
            for J := 0 to 10 do
                SubItems.Add('');
            Checked := True;
        end;
        TotServers := LVServers.Items.Count;
        SetLength(ServInfos, TotServers + 2);  // base 1
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.NewDeleteClick(Sender: TObject);
begin
    if SrvEditNr >= 0 then begin
        LVServers.DeleteSelected;
        SrvEditNr := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.NewReplaceClick(Sender: TObject);
begin
    if SrvEditNr >= 0 then begin
        if TestUrl(SrvEditNr) then begin
            LVServers.Items[SrvEditNr].Caption := NewURL.Text;
            LVServers.Items[SrvEditNr].SubItems[0] := ServInfos[0].ServerName;
            SrvEditNr := -1;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TManForm.TestUrl(ItemNr: Integer): Boolean;
var
    MyUrl: String;
    Ret: Integer;
begin
    Result := False;
    MyUrl := NewURL.Text;
    HttpServerTest.SocketFamily := TSocketFamily(PrefSockFamily.ItemIndex);
    if HttpServerTest.IsKnownProtocolURL(MyUrl) then begin
        if MyUrl[Length(MyUrl)] = '/' then begin       // trim last /, should check for path as well!!!
            SetLength(MyUrl, Length(MyUrl) - 1);
            NewURL.Text := MyUrl;
        end;

    // server returns same Json for a simple GET request, and pushed by Websocket every 30 seconds
    // GET server Json once only to build MonSuppApps, then refresh ListView
        LabelTestUrl.Caption := 'Connecting to URL: ' + MyUrl;
        Ret := HttpServerTest.RestRequest(httpGET, MyUrl + WebServStatusJson, False, '');   // sync request
        if Ret = 200 then begin
            ParseSrvJson(0, HttpServerTest.ResponseJson);  // fills ServInfos[0], but not MonSuppApps arrays
            HttpServerTest.Close;
            LabelTestUrl.Caption := 'Connected to Server OK: ' + ServInfos[0].ServerName;
            Result := True;
        end
        else begin
            HttpServerTest.Close;
            LabelTestUrl.Caption := 'Failed Connection: ' + HttpServerTest.LastResponse;
        end;
    end
    else begin
       LabelTestUrl.Caption := 'Invalid URL';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.NewTestClick(Sender: TObject);
begin
    TestUrl(-1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.PrefLogDirRightButtonClick(Sender: TObject);
begin
    OpenDialog.InitialDir := PrefLogDir.Text ;
    if OpenDialog.Execute then
        PrefLogDir.Text := ExtractFilePath(OpenDialog.FileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(* server Json response:
{"timestamp":"2025-02-03T12:14:38",
"servername":"PC21",
"serverinfo":
  {"progver":"IcsAppMon - Release 1.1 -27th January 2025",
   "servername":"PC21",
   "starttime":"28-Jan-2025 12:04:07",
   "bootime":"14-Jan-2025 19:25:55",
   "serveros":"Windows 11 (Version 24H2, OS Build 26100.2894, 64-bit Edition)",
   "compiler":"Delphi 11.3 Win32",
   "openssl":"Static OpenSSL 3.4.0 22 Oct 2024, Legacy Provider Not Loaded",
   "monlisten":"Socket 1 State: Listening Only IPv4 on 192.168.1.121 port 17778",
   "weblisten":"Socket 1 State: Listening Only IPv4 on 192.168.1.121 port 17779"},
"success":true,
"reccount":5,
"errdesc":"",
"records": [
  {"compname":"PC21",
  "clienttitle":"MagFileServer",
  "stateinfodisp":"OK",
  "suppstateval":1,
  "lastokdisp":"04-Feb-2025 12:23:32",
  "startstopdisp":"Started 04-Feb-2025 11:53:56",
  "monmodedisp":"NONSTOP",
  "monmodeval":1,
  "servicename":"MagFServer",
  "status":"TotSess: 502, Send: 30.4K, Recv: 27.4K",
  "exefile":"c:\\Magdelp\\fileserveru\\magfserver.exe",
  "clientid":2,
  "startedDT":"2025-02-04T11:53:56",
  "stoppedDT":"1899-12-30T00:00:00",
  "restartmode":1,
  "stopstate":0,
  "startstate":0,
  "restartinfo":"",
  "general":"Magenta File Server Release 2.15\/ICS V9.4 - 27th January 2025, xx",
  "colordisp":"00FF00"},
 {xx}
]}  *)
procedure TManForm.ParseSrvJson(SrvNr: Integer; RespObj: ISuperObject);
var
    JsonItems, JsonItem: ISuperObject;
    I, TotItems, AppNr: integer;
    S, CompName, ClientTitle: String;

    procedure FindAMApp;
    var
        J: Integer;
    begin
        Appnr := 0;
        if TotApps = 0 then
            Exit;
        for J:= 1 to TotApps do begin
            if (MonSuppApps[J].AppClientTitle <> ClientTitle) then
                Continue;
            if (MonSuppApps[J].AppCompName <> CompName) then
                Continue;
            Appnr := J;
            Break;
        end;
    end;

begin
    if (SrvNr < 0) or (SrvNr > TotServers) then   // note server 0 allowed for testing
        Exit;
    if NOT Assigned(RespObj) then
        Exit;
    try
        with ServInfos[SrvNr] do begin

        // server specific stuff
            PacketDT := RespObj.DT['timestamp'];
            S := RespObj.S['servername'];
            if (S <> '') and (S <> ServerName) then
                ServerName := S;
            if (RespObj.S['success'] <> 'true') then begin
                LastStatus := RespObj.S['errdesc'];
                Exit;
            end;
            JsonItem := RespObj.O['serverinfo'];
            if Assigned(JsonItem) then begin
                ProgVer := JsonItem.S['progver'];
                StartTime := JsonItem.S['starttime'];
                BootTime := JsonItem.S['boottime'];
                ServerOS := JsonItem.S['serveros'];
            end;
            if SrvNr = 0 then  // only testing server
                Exit;

        // update Servers ListView
            with LVServers.Items[SrvNr - 1] do begin
                SubItems[0] := ServerName;
                SubItems[1] := SrvStateNames[SrvState];
                SubItems[2] := IcsDateTimeToAStr(PacketDT);
                SubItems[3] := LastStatus;
                SubItems[4] := StartTime;
                SubItems[5] := BootTime;
                SubItems[6] := ServerOS;
                SubItems[7] := ProgVer;
            end;
            LVServers.UpdateItems (SrvNr - 1, SrvNr - 1);   // try and repaint single ListView row

        // one record for each monitored application
            TotItems := RespObj.I['reccount'];
            if TotItems > 0 then begin
                JsonItems := RespObj.O['records'];
                for I := 0 to TotItems - 1 do begin
                    JsonItem := JsonItems.AsArray.O[I];
                    CompName := JsonItem.S['compname'];
                    ClientTitle := JsonItem.S['clienttitle'];
                    FindAMApp;
                    if Appnr = 0 then begin  // not found
                        TotApps := TotApps + 1;
                        Appnr := TotApps;
                        if TotApps >= Length(MonSuppApps) then
                            SetLength(MonSuppApps, TotApps * 2);
                        with MonSuppApps[Appnr] do begin   // keep elements that don't change
                            AppSevrNr := SrvNr;
                            AppLastState := AppStateNone;
                            AppClientTitle := ClientTitle;
                            AppCompName := CompName;
                            AppServiceName := JsonItem.S['servicename'];
                            AppMonMode := TAMMode(JsonItem.I['monmodeval']);
                            AppExeFile := JsonItem.S['exefile'];
                            AppGeneral := JsonItem.S['general'];
                        end;
                     // add ListView row
                        while LVMonitor.Items.Count < TotApps do
                            LVMonitor.Items.Add;
                    end;
                    with MonSuppApps[Appnr] do begin   // elements that change continually
                        AppLastOKDisp := JsonItem.S['lastokdisp'];
                        AppStartStopDisp := JsonItem.S['startstopdisp'];
                        AppStateInfoDisp := JsonItem.S['stateinfodisp'];
                        AppColorDisp := JsonItem.S['colordisp'];
                        AppClientID := JsonItem.I['clientid'];
                        AppStatus := JsonItem.S['status'];
                        AppStarted:= JsonItem.DT['startedDT'];
                        AppStopped:= JsonItem.DT['stoppedDT'];
                        AppSuppState := TAMAppState(JsonItem.I['suppstateval']); ;
                        AppRestartMode := TAMRestartMode(JsonItem.I['restartmode']); ;
                        AppStopState := TAMStopState(JsonItem.I['stopstate"']); ;
                        AppStartState := TAMStartState(JsonItem.I['startstate']); ;
                        AppRestartInfo := JsonItem.S['appgeneral'];

                    // report state changes
                        if AppLastState <> AppSuppState then begin
                            S := ServerName + ' - ' + AppClientTitle + ': State Change from ' +
                                            AMAppStateNames[AppLastState] + ' to ' + AMAppStateNames[AppSuppState];
                            AddLog(S);
                            LabelLastChange.Caption := TimeToStr (Now) + IcsSpace + S;
                            AppLastState := AppSuppState;
                        end;
                    end;
                    LVMonitor.UpdateItems (Appnr - 1, Appnr - 1);   // try and repaint single ListView row
                end;
            end;
        end;
    except
        S :='Failed to Parse Json: ' + IcsGetExceptMess (ExceptObject) ;
        AddLog(S) ;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.WebSocWSConnected(Sender: TObject);
var
    MyWSCli: TSslWebSocketCli;
    SrvNr: Integer;
begin
    MyWSCli := (Sender as TSslWebSocketCli);
    SrvNr := MyWSCli.Tag;
    if (SrvNr <= 0) or (SrvNr > TotServers) then
        Exit;
    with ServInfos[SrvNr] do begin
        if MyWSCli.IsWSConnected then begin
            LastStatus := 'Websocket Connected OK to: ' + MyWSCli.URL;
        end
        else begin
            LastStatus := 'Websocket Failed to Connect to: ' + MyWSCli.URL;
            if MonActive then begin
                SrvState := StateWaitReconnect ;
                RetryTick := IcsGetTrgSecs64(atoi(PrefRetrySecs.Text)); // try again soon
                LastStatus := 'Reconnect in ' + PrefRetrySecs.Text + ' seconds';
                AddLog(ServerName + ': ' + LastStatus);
            end
            else
                SrvState := StateNone;
        end;
        AddLog(ServerName + ': ' + LastStatus);
        LVServers.Items[SrvNr-1].SubItems[1] := SrvStateNames[SrvState];  // update ListView
        LVServers.Items[SrvNr-1].SubItems[3] := LastStatus;  // update ListView
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.WebSocWSDisconnected(Sender: TObject);
var
    MyWSCli: TSslWebSocketCli;
    SrvNr: Integer;
begin
    MyWSCli := (Sender as TSslWebSocketCli);
    SrvNr := MyWSCli.Tag;
    if (SrvNr <= 0) or (SrvNr > TotServers) then
        Exit;
    with ServInfos[SrvNr] do begin
        LastStatus := 'Websocket Session Disconnected';
        AddLog(ServerName + ': ' + LastStatus);
        if MonActive then begin
            SrvState := StateWaitReconnect ;
            RetryTick := IcsGetTrgSecs64(atoi(PrefRetrySecs.Text)); // try again soon
            LastStatus := 'Reconnect in ' + PrefRetrySecs.Text + ' seconds';
            AddLog(ServerName + ': ' + LastStatus);
        end
        else
            SrvState := StateNone;
        LVServers.Items[SrvNr-1].SubItems[1] := SrvStateNames[SrvState];  // update ListView
        LVServers.Items[SrvNr-1].SubItems[3] := LastStatus;  // update ListView
    end;
    LVMonitor.Refresh;  // repaint all rows, will show as not being monitored
    LVServers.Refresh;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.HttpServerTestHttpRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
begin
    AddLog(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.WebSocHttpRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
var
    MyWSCli: TSslWebSocketCli;
    SrvNr: Integer;
begin
    MyWSCli := (Sender as TSslWebSocketCli);
    SrvNr := MyWSCli.Tag;
    if (SrvNr <= 0) or (SrvNr > TotServers) then
        Exit;
    with ServInfos[SrvNr] do begin
        LastStatus := Msg;
        AddLog(ServerName + ': ' + LastStatus);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TManForm.WebSocWSFrameRcvd(Sender: TSslWebSocketCli; const APacket: string; var AFrame: TWebSocketReceivedFrame);
var
    S, ErrStr: String;
    MyWSCli: TSslWebSocketCli;
    SrvNr: Integer;
begin
    MyWSCli := (Sender as TSslWebSocketCli);
    SrvNr := MyWSCli.Tag;
    if (SrvNr <= 0) or (SrvNr > TotServers) then
        Exit;
    if AFrame = Nil then
        Exit;
    with ServInfos[SrvNr] do begin
        LastStatus := 'WS Frames in/out: ' + IntToStr(MyWSCli.WSFrameCounter);
        if (MyWSCli.DebugLevel >= DebugBody) then begin
            S := 'Websocket ' + GetWSFrameKind(AFrame.Kind) + ' received';
            if AFrame.DataBytes > 0 then
                S := S + ', data length: ' + IntToStr(AFrame.DataBytes);
            AddLog(ServerName + ': ' + S);
            if AFrame.DataBytes < 1024 then begin
                case AFrame.Kind of
                    wsfkText: begin
                        AddLog(ServerName + ': ' + APacket);
                    end;
                end;
            end;
        end;
        if (AFrame.DataBytes > 10) and (Pos('{', APacket) > 0) and (Pos('}', APacket) > 0) then begin
           try
                ParseSrvJson(SrvNr, TSuperObject.ParseStringEx(PWideChar(APacket), True, ErrStr));
                if ErrStr <> '' then
                    AddLog(ServerName + ': ' + 'Failed to parse Json response: ' + ErrStr);
            except
                on E:Exception do begin
                    AddLog('Failed to parse Json response: ' + E.Message);
                end;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// should not be sending any messages
procedure TManForm.WebSocWSFrameSent(Sender: TSslWebSocketCli; var AFrame: TWebSocketOutgoingFrame);
var
    MyWSCli: TSslWebSocketCli;
    SrvNr: Integer;
begin
    MyWSCli := (Sender as TSslWebSocketCli);
    SrvNr := MyWSCli.Tag;
    if (SrvNr <= 0) or (SrvNr > TotServers) then
        Exit;
    if AFrame = Nil then
        Exit;
    if (MyWSCli.DebugLevel >= DebugBody) then begin
        AddLog(ServInfos[SrvNr].ServerName + ': ' + GetWSFrameKind(AFrame.Kind) + ' sent');
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.
