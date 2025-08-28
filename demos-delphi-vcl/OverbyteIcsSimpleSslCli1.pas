{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Creation:     Jan 24, 2003
Description:  A basic SSL client using TSslWSocket.
Version:      V9.0
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1997-2023 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.

              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

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
Aug 08, 2023 V9.0  Updated version to major release 9.





 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

 unit OverbyteIcsSimpleSslCli1;

{$IFNDEF USE_SSL}
  {$MESSAGE FATAL 'Define conditional define "USE_SSL" in the project options'};
{$ENDIF}
{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

interface

uses
  Windows, Messages, SysUtils,
{$IFDEF COMPILER6_UP}
  Variants,
{$ENDIF}
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OverbyteIcsWSocket, OverbyteIcsWndControl, OverbyteIcsSslBase;

type
  TForm1 = class(TForm)
    Sock: TSslWSocket;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    EditHost: TEdit;
    EditPort: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    EditUrl: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    Button3: TButton;
    SslContext1: TSslContext;
    procedure SockSessionConnected(Sender: TObject; ErrCode: Word);
    procedure SockDataAvailable(Sender: TObject; ErrCode: Word);
    procedure SockSslHandshakeDone(Sender: TObject; ErrCode: Word;
                                   PeerCert: TX509Base;
                                   var Disconnect: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure SockSessionClosed(Sender: TObject; ErrCode: Word);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    RecStream : TStream;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
    RecStream := TMemoryStream.Create;
    Memo1.Clear;
    Label4.Caption := IntToStr(Memo1.Lines.Count);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    FreeAndNil(RecStream);
end;

procedure TForm1.SockSessionConnected(Sender: TObject;
    ErrCode: Word);
begin
    if Errcode = 0 then
    begin
        Sock.SslEnable := True;
        Sock.StartSslHandshake;
        Button1.Enabled := FALSE;
        Button2.Enabled := TRUE;
    end;
end;

procedure TForm1.SockSessionClosed(Sender: TObject; ErrCode: Word);
begin
    Button1.Enabled := TRUE;
    Button2.Enabled := FALSE;
    Button3.Enabled := FALSE;
    RecStream.Seek(0, soFromBeginning);
    Memo1.Lines.LoadFromStream(RecStream);
    Label4.Caption := IntToStr(Memo1.Lines.Count);
    Memo1.Lines.Add('');
end;

procedure TForm1.SockDataAvailable(Sender: TObject; ErrCode: Word);
var
    Buf : array [0..1023] of char;
    Len : Integer;
begin
    while TRUE do begin
        Len := Sock.Receive(@Buf, SizeOf(Buf) - 1);
        if Len <= 0 then
            Exit;
        RecStream.Write(Buf[0], Len);
    end;
end;

procedure TForm1.SockSslHandshakeDone(Sender: TObject; ErrCode: Word;
    PeerCert: TX509Base; var Disconnect: Boolean);
begin
    Button3.Enabled := TRUE;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
    Sock.Addr := EditHost.Text;
    Sock.Port := EditPort.Text;
    RecStream.Size := 0;
    Memo1.Clear;
    Label4.Caption := IntToStr(Memo1.Lines.Count);
    Sock.SslEnable := FALSE;
    Sock.Connect; //-->
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
    Sock.Shutdown(1);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
    Button3.Enabled := FALSE;
    Sock.SendStr('GET ' + EditUrl.Text + ' HTTP/1.0'#13#10#13#10);
end;


end.
