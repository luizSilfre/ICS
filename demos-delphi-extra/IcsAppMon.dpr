program IcsAppMon;

uses
  Forms,
  SysUtils,
  DDSvcMgr,
  OverbyteIcsUtils,
  IcsAppMonMain in 'IcsAppMonMain.pas' {MonitorMain: TMonitorMain},
  IcsAppMonSrv in 'IcsAppMonSrv.pas' {IcsAppMonControl: TDDService};

{$R *.RES}

// detect if application is being installed and uninstalled as a service,
// or is being run as a service with argument GServiceIdent = 'HsuYgstEwA'
function MyStartService: Boolean;
begin
    GInstalling := FindCmdLineSwitch('install', ['-','\','/'], True) or
                                            FindCmdLineSwitch('uninstall', ['-','\','/'], True);
    if GInstalling then begin
        Result := True;
        Exit;
    end;
    GStartedByScm := FindCmdLineSwitch(GServiceIdent, ['-','\','/'], False);
    Result := GStartedByScm;
end;

begin
    if MyStartService then begin
 // run as Windows Service
        DDSvcMgr.Application.Initialize;
        Application.CreateForm(TIcsAppMonControl, IcsAppMonControl);
        if NOT GInstalling then      // installing a windows service, don't need main form
            Forms.Application.CreateForm(TMonitorMain, MonitorMain);
        DDSvcMgr.Application.Run;
    end
    else
    begin
// run as Windows GUI
        Forms.Application.Initialize;
        Forms.Application.Title := ProgTitle;
        if NOT GInstalling then     // should not get here, but in case...
            Forms.Application.CreateForm(TMonitorMain, MonitorMain);
        Forms.Application.Run;
    end;

end.
