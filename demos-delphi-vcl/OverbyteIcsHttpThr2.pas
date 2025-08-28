{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE (From a work done by Ed Hochman <ed@mbhsys.com>)
Creation:     Jan 13, 1998
Description:  HttpThrd is a demo program showing how to use TSslHttpRest component
              in a multi-threaded program.
Version:      V9.2
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1998-2024 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.

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

Updates:
Jun 19 2011 V1.01 Arno - Make use of an event object rather than
                  TThread.Suspend/Resume, both are deprecated since D2010.
Jun 20 2011 V1.02 Arno reworked it, was needed.
Jun 04, 2024 V9.2 Updated to use TSslHttpRest and SSL.
Aug 6, 2024  V9.3 Using OverbyteIcsTypes for consolidated types and constants, allowing
                     other import units to be removed.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpThr2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
//OverbyteIcsHttpProt,
//OverbyteIcsLogger,
  OverbyteIcsSslHttpRest,
  OverbyteIcsTypes;  { V9.3 consolidated types and constants }

type
  TThreadState = (tsInvalid, tsReady, tsBusy);
const
  ThreadStateLits: array[0..2] of string = ('tsInvalid', 'tsReady', 'tsBusy');

type
  THTTPThread = class(TThread)
  private
    FEvent        : THandle;
    FState        : TThreadState;
    FURL          : String;
    FProxy        : String;
    FThreadNumber : Integer;
    FSslHttpRest  : TSslHttpRest;     { V9.2 }
    FBodyContent  : String;           { V9.2 }
    FStatusCode   : Integer;          { V9.2 }
    FSuccess      : Boolean;
    FLogList      : TStringList;
    FCritSect     : TRTLCriticalSection;
    procedure Progress(const Msg : String);
    procedure onSslHttpRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
  protected
    procedure Execute; override;
  public
    constructor Create(AThreadNumber: Integer);
    destructor Destroy; override;
    procedure Wakeup;
    function LockLogList: TStringList;
    procedure UnlockLogList;
    property State: TThreadState read FState write FState;

    { None thread-safe properties, they have to be accessed only in State tsReady }
    property URL: String read FURL write FURL;
    property Proxy: String read FProxy write FProxy;
    property ThreadNumber: Integer read FThreadNumber write FThreadNumber;
    property Success: Boolean read FSuccess;
    property BodyContent: String read FBodyContent;
    property StatusCode: Integer read FStatusCode;
  end;

implementation

uses
    OverbyteIcsHttpThr1,
    OverbyteIcsUtils;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor THTTPThread.Create(AThreadNumber: Integer);
begin
    inherited Create(TRUE);
    InitializeCriticalSection(FCritSect);
    FLogList      := TStringList.Create;
    FThreadNumber := AThreadNumber;
    FEvent        := CreateEvent(nil, False, False, nil);
    if FEvent = 0 then
        RaiseLastOSError;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor THTTPThread.Destroy;
begin
    if FEvent <> 0 then begin
        Terminate;
        SetEvent(FEvent); { Wake it up otherwise inherited Destroy won't return }
    end;
    inherited Destroy;
    { The thread is down and THttpCli instance is released now }
    FLogList.Free;
    DeleteCriticalSection(FCritSect);
    if FEvent <> 0 then
        CloseHandle(FEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.Wakeup;
begin
    SetEvent(FEvent);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function THTTPThread.LockLogList;
begin
    EnterCriticalSection(FCritSect);
    Result := FLogList;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.UnlockLogList;
begin
    LeaveCriticalSection(FCritSect);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.Progress(const Msg : String);
begin
    EnterCriticalSection(FCritSect);
    try
        FLogList.Add(Msg);
        if FLogList.Count = 1 then
            PostMessage(HttpThreadForm.Handle, WM_LOG, WParam(Self), 0);
    finally
        LeaveCriticalSection(FCritSect);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.Execute;
begin
    OverbyteIcsUtils.IcsNameThreadForDebugging(AnsiString(ClassName + ' ' + IntToStr(FThreadNumber)));
    FSslHttpRest := TSslHttpRest.Create (Nil);
    try
        FSslHttpRest.OnHttpRestProg := onSslHttpRestProg;
        FSslHttpRest.MultiThreaded := TRUE;
        while not Terminated do begin

        // wait until the main app wakes up the thread for a new request
            WaitForSingleObject(FEvent, INFINITE);
            if Terminated then
                Break;

            Progress('Thread ' + IntToStr(FThreadNumber) + ': Start GET');
            FSslHttpRest.Proxy := FProxy;
            FSuccess := FALSE;
            try
                FSslHttpRest.RestRequest(httpGET, FURL);   // Get page from internet, sync request with internal message pump
                FStatusCode := FSslHttpRest.StatusCode;
                if FStatusCode = 200 then begin
                    if (Pos('text/', FSslHttpRest.ContentType) > 0) then
                        FBodyContent := FSslHttpRest.ResponseRaw
                    else
                        FBodyContent := '<Non-textual content received: ' + FSslHttpRest.ContentType + '>';
                    FSuccess := TRUE;
                end;
            except
                FSuccess := FALSE;
            end;
            FSslHttpRest.Close;

            if not Terminated then
                PostMessage(HttpThreadForm.Handle, WM_THREAD_RESULT, WParam(Self), Ord(Success));
        end;
    finally
        FSslHttpRest.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THTTPThread.onSslHttpRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
begin
   Progress('Thread ' + IntToStr(FThreadNumber) + ': ' + Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

