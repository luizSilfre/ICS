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
Updated:      Sept 2024
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

Sep 11, 2024 V9.3 - Baseline


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

 unit IcsAppMonSrv;

interface

uses
  Windows, Messages, SysUtils, Classes, Dialogs, System.UITypes,
  DDSvcUtils,
  DDWindows,
  DDSvcMgr,
  OverbyteIcsUtils;  // for IcsSimpleLogging

type
  TIcsAppMonControl = class(TDDService)
    procedure DDServiceCreate(Sender: TObject);
    procedure DDServiceDestroy(Sender: TObject);
    procedure DDServiceExecute(Sender: TDDService);
    procedure DDServicePause(Sender: TDDService; var Paused: Boolean);
    procedure DDServiceShutdown(Sender: TDDService);
    procedure DDServiceStart(Sender: TDDService; var Started: Boolean);
    procedure DDServiceContinue(Sender: TDDService; var Continued: Boolean);
    procedure DDServiceAfterInstall(Sender: TDDService);
    procedure DDServiceAfterUninstall(Sender: TDDService);
    procedure DDServiceBeforeInstall(Sender: TDDService);
    procedure DDServiceRunException(Sender: TObject; E: Exception; var LogDefaultErrMsg, CanAbort: Boolean);
    procedure DDServiceStop(Sender: TDDService; var Stopped: Boolean);
  private
    { Private declarations }
  public
    function GetServiceController: TServiceController; override;
    function GetServiceControllerEx: TServiceControllerEx; override;
    function GetConsoleCtrlHandler: TServiceConsoleCtrlHandler; override;
    procedure LogFile (const S: string);
    procedure StopService;
    procedure CrashService;
    procedure GetStatus;
  end;

const
  GServiceIdent = 'HsuYgstEwA';

var
  IcsAppMonControl: TIcsAppMonControl;
  GInstalling: Boolean = False;

implementation

{$R *.DFM}

uses
    IcsAppMonMain;

{///////////////////////////////////////////////////////////////////////////}
procedure ServiceController(CtrlCode: DWORD); stdcall;
begin
    IcsAppMonControl.Controller(CtrlCode);
end;

function TIcsAppMonControl.GetServiceController: TServiceController;
begin
    Result := ServiceController;
end;

function ServiceControllerEx(CtrlCode, EventType: DWORD; EventData, Context: Pointer): DWORD; stdcall;
begin
    Result := IcsAppMonControl.ControllerEx(CtrlCode, EventType, EventData, Context);
end;

function TIcsAppMonControl.GetServiceControllerEx: TServiceControllerEx;
begin
    Result := ServiceControllerEx;
end;

function ServiceConsoleCtrlHandler(Ctrl: DWord): Bool; stdcall;
begin
    Result := IcsAppMonControl.ConsoleCtrlHandler(Ctrl);
end;

procedure TIcsAppMonControl.DDServiceAfterInstall(Sender: TDDService);
var
    I: Integer;
    S: string;
begin
    S := ParamStr(0);
    I := Pos (' /', S);
    if I > 1 then
        S := Copy (S, 1, I - 1);  // strip arguments
    RegisterEventLogSource(Sender.ServiceName, S);
end;

procedure TIcsAppMonControl.DDServiceAfterUninstall(Sender: TDDService);
begin
    UnRegisterEventLogSource(Sender.ServiceName);
end;

procedure TIcsAppMonControl.DDServiceBeforeInstall(Sender: TDDService);
begin
    ImagePath := ImagePath + ' /' + GServiceIdent;
    IcsSimpleLogging (SimpLogName, 'Service Install - ' + ImagePath) ;
end;

procedure TIcsAppMonControl.DDServiceContinue(Sender: TDDService; var Continued: Boolean);
begin
//
end;

procedure TIcsAppMonControl.DDServiceCreate(Sender: TObject);
var
    S: string;
begin
    IcsSimpleLogging (SimpLogName, 'Service Create') ;
    if GInstalling then begin     // windows service
        S := 'Program Needs Administrator Rights to Install as Service';
        IcsSimpleLogging (SimpLogName, S) ;
        if NOT IcsIsProgAdmin then begin
            MessageDlg(S, mtError, [mbAbort], 0);
            ExitProcess(0);
            Exit;
        end;
    end;
end;

procedure TIcsAppMonControl.DDServiceDestroy(Sender: TObject);
begin
   IcsSimpleLogging (SimpLogName, 'Exception DDServiceDestroy');
end;

procedure TIcsAppMonControl.DDServiceExecute(Sender: TDDService);
begin
    IcsSimpleLogging (SimpLogName, 'Service Execute') ;
    LogFile ('Server Monitor Service Execute') ;
    GetStatus;
    while not Terminated do begin
        Sender.ServiceThread.ProcessRequests (true);  // !! must be true else gobbles CPU
    end;
    IcsSimpleLogging (SimpLogName, 'Service Stopped Execute') ;
end;

procedure TIcsAppMonControl.DDServicePause(Sender: TDDService; var Paused: Boolean);
begin
//
end;

procedure TIcsAppMonControl.DDServiceRunException(Sender: TObject; E: Exception; var LogDefaultErrMsg, CanAbort: Boolean);
begin
    LogFile ('Service Run Exception - ' + E.Message) ;
    CanAbort := true ;
end;

procedure TIcsAppMonControl.DDServiceShutdown(Sender: TDDService);
begin
//
end;

procedure TIcsAppMonControl.DDServiceStart(Sender: TDDService; var Started: Boolean);
begin
    IcsSimpleLogging (SimpLogName, 'Service Start') ;
    GetStatus;
    Started := True;
end;

procedure TIcsAppMonControl.DDServiceStop(Sender: TDDService; var Stopped: Boolean);
begin
    IcsSimpleLogging (SimpLogName, 'Service Stop') ;
    try
        GetStatus;
        if MonitorMain <> nil then begin
            LogFile ('Service Stop Event from Windows, Closing Down') ;
            MonitorMain.TimerMain.Enabled := false ;
            if NOT ClosingFlag then begin
                ClosingFlag := true ;
                LogFile('Stopping Server Monitor') ;
                MonitorMain.MonitorStop;
            end;
        // warning, sets Applicaton.Terminated so lots of functions exit
            PostMessage(MonitorMain.Handle, WM_ClOSE, 0, 0);
        end;
    except
    end;
    Stopped := True;
end;

function TIcsAppMonControl.GetConsoleCtrlHandler: TServiceConsoleCtrlHandler;
begin
    Result := ServiceConsoleCtrlHandler;
end;

procedure TIcsAppMonControl.LogFile (const S: string);
begin
    if Assigned (MonitorMain) then
        MonitorMain.AddLogLine (S) ;
end;

procedure TIcsAppMonControl.StopService;
begin
    GetStatus;
    IcsSimpleLogging (SimpLogName, 'Local Service Stop Request') ;
    LogFile ('Local Service Stop Request') ;
    Stop;
    GetStatus;
end;

procedure TIcsAppMonControl.CrashService;
begin
    GetStatus;
    IcsSimpleLogging (SimpLogName, 'Local Service Crash Request') ;
    ServiceThread.Terminate;
    IcsSimpleLogging (SimpLogName, 'Local Service Crash Thread ExitProcess') ;
    ExitProcess(0);
end;

const
  ServStatus: array [Low(TCurrentStatus)..High(TCurrentStatus)] of String =
      ('Stopped', 'Start Pending', 'Stop Pending', 'Running', 'Continue Pending', 'Pause Pending', 'Paused');

procedure TIcsAppMonControl.GetStatus;
begin
    ReportStatus;
    LogFile ('Service Status: ' + ServStatus[Status]) ;
end;


{///////////////////////////////////////////////////////////////////////////}

end.
