{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Object:       This demo shows how to use TSslHttpRest component within a console
              mode application. It Connects to an SSL wbeb server and displays
              received document on screen.
Creation:     Apr 20, 2002
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2002-2025 by François PIETTE
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

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

Updates:
Jul 12, 2008 V6.00 A. Garrels - Bumped version number to 6.00 and slightly
             modified to work with ICS v6.
Jul 19, 2008 V6.00 F. Piette made small changes for Unicode
Oct 11, 2010 V6.01 A. Garrels fixed a Unicode bug.
Sep 23, 2024 V9.3  Supporting SSL by replacing THttpCli with TSslHttpRest,
                     no longer needs any events or a message loop for a
                     single sync request, so a less code than without SSL.
                   Now contacts https://wiki.overbyte.eu/wiki.
Feb 07, 2025 V9.4  Corrected insert path so now builds for Win64.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
program OverbyteIcsConHttp;

{$I Include\OverbyteIcsDefs.inc}

{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

{$APPTYPE CONSOLE}
{$IFNDEF NOFORMS}
    Bomb('Please add NOFORMS to your project defines');
{$ENDIF}

uses
  Messages,
  Windows,
  SysUtils,
  Classes,
  OverbyteIcsSslHttpRest,
  OverbyteIcsTypes;  { V9.3 consolidated types and constants }

const
  ConHttpVersion = 903;
  CopyRight      = ' ConHttp (c) 2002-2024 by Francois PIETTE. V9.3';

type
    { We use TConApplication class (actually a component) to encapsulate all }
    { the work to be done. This is easier because THttpCli is event driven   }
    { and need methods (that is procedure of object) to handle events.       }
    TConApplication = class(TComponent)
    protected
        FHttpCli : TSslHttpRest;    { V9.3 }
        procedure HttpRequestDone(Sender    : TObject;
                                  RqType    : THttpRequest;
                                  ErrorCode : Word);
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Execute;
    end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TConApplication.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FHttpCli := TSslHttpRest.Create(Self);
//    FHttpCli.OnRequestDone := HttpRequestDone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TConApplication.Destroy;
begin
    if Assigned(FHttpCli) then begin
        FHttpCli.Destroy;
        FHttpCli := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TConApplication.Execute;
var
    Url : String;
    StatCode: Integer;
begin
    Url := 'https://wiki.overbyte.eu/wiki/index.php/Main_Page';
    WriteLn('Querying ' + Url);

// make sync request, returns when complete
    StatCode := FHttpCli.RestRequest(httpGET, Url, False, '');  // third arg is true for async, fourth is raw params
    WriteLn('Request Done, Status Code: ' + IntToStr(StatCode));
//    if StatCode = 200 then
    if FHttpCli.ResponseOctet <> '' then
        Write(FHttpCli.ResponseOctet);  { Write HTML response to standard output }

   { If we made an async request, need a message loop in order for
      windows message processing to work. }
    { MessageLoop will exit only when WM_QUIT message is posted. We do that
      form the OnRequestDone event handler when the component has finished.   }
//    FHttpCli.MessageLoop;

    { Prompt the user by reading input }
    WriteLn('Hit ENTER key to return quit program...');
    ReadLn;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This event handler is called when THttpCli has finihed his work.          }
{ NOTE not currently used }
procedure TConApplication.HttpRequestDone(
    Sender    : TObject;
    RqType    : THttpRequest;
    ErrorCode : Word);
begin
    { Check status }
    if ErrorCode <> 0 then
        WriteLn('Failed, error #' + IntToStr(ErrorCode))
    else
        WriteLn('Done.');
    { Prompt the user }
    WriteLn('Hit ENTER key to return quit program...');
    ReadLn;
    { Break message loop we called from the execute method }
    FHttpCli.PostQuitMessage;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
var
    ConApp : TConApplication;
begin
    WriteLn(CopyRight);
    WriteLn;
    ConApp := TConApplication.Create(nil);
    try
        ConApp.Execute;
    finally
        ConApp.Destroy;
    end;
end.
