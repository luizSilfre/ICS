{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  Demonstration for Client program using TWSocket.
Creation:     8 december 1997
Version:      8.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2012 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>

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
Aug 07, 2024 - V9.3 - Baseline, testing PEM functions on Posix




 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit IcsPemTest1;

interface

{$I Include\OverbyteIcsDefs.inc}

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.IOUtils, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.Layouts, FMX.Memo, FMX.Edit,
  FMX.StdCtrls, System.ImageList, FMX.ImgList,
  FMX.Memo.Types, FMX.Controls.Presentation, FMX.ScrollBox,
  OverbyteIcsUtils,
  OverbyteIcsIniFiles,
  OverbyteIcsTypes,
//  OverbyteIcsSsleay, OverbyteIcsLibeay,
//  Ics.Fmx.OverbyteIcsWndControl,
//  Ics.Fmx.OverbyteIcsWSocket,
  Ics.Fmx.OverbyteIcsSslBase,
  OverbyteIcsSslX509Utils;

type
  TClientForm = class(TForm)
    Log: TMemo;
    Panel1: TPanel;
    CertFName: TEdit;
    doListFile: TButton;
    doClose: TButton;
    Label3: TLabel;
    doListCA: TButton;
    OpenDialog1: TOpenDialog;
    SelCertFile: TButton;
    ImageList1: TImageList;
    Label1: TLabel;
    CertDir: TEdit;
    SelCertDir: TButton;
    doCreateCert: TButton;
    ShowRaw: TCheckBox;
    doEventQueue: TButton;
    procedure doCloseClick(Sender: TObject);
    procedure doListFileClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure SelCertFileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure doListCAClick(Sender: TObject);
    procedure SelCertDirClick(Sender: TObject);
    procedure doCreateCertClick(Sender: TObject);
    procedure doEventQueueClick(Sender: TObject);
  private
    IniFileName  : String;
    procedure Display(Msg : String);
    procedure ShowOneCert(const FileName: String);
  end;

var
  ClientForm: TClientForm;

implementation

{$R *.FMX}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.doCloseClick(Sender: TObject);
begin
    Close;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.FormCreate(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
    ReportMemoryLeaksOnShutdown := (DebugHook <> 0);
{$ENDIF}
end;


procedure TClientForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    IcsSslRootCAStore.Initialise;       { if OpenSSL and internal not loaded, do it }
    IcsSslLoadProviders(True, False);   { need legacy provider }
    IcsNameThreadForDebugging('Main');
    IniFileName     := GetIcsIniFileName;
    IniFile         := TIcsIniFile.Create(IniFileName);

    Top             := IniFile.ReadInteger('Window', 'Top',    Top);
    Left            := IniFile.ReadInteger('Window', 'Left',   Left);
    Width           := IniFile.ReadInteger('Window', 'Width',  Width);
    Height          := IniFile.ReadInteger('Window', 'Height', Height);

    CertFName.Text := IniFile.ReadString('Data', 'CertFName',  GSSL_CERTS_DIR);
    CertDir.Text := IniFile.ReadString('Data', 'CertDir',  GSSL_PUBLIC_DIR );
    ShowRaw.IsChecked := IniFile.ReadBool('Data', 'ShowRaw', False);
    IniFile.Free;

    Log.Lines.Clear;
    Display('SSL/TLS ' + IcsReportOpenSSLVer(True));
    Display('Compiler: ' + IcsBuiltWithEx);
    Display('');
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(IniFileName);
    IniFile.WriteInteger('Window', 'Top',    Top);
    IniFile.WriteInteger('Window', 'Left',   Left);
    IniFile.WriteInteger('Window', 'Width',  Width);
    IniFile.WriteInteger('Window', 'Height', Height);
    IniFile.WriteString('Data', 'CertFName',  CertFName.Text);
    IniFile.WriteString('Data', 'CertDir',    CertDir.Text);
    IniFile.WriteBool('Data', 'ShowRaw', ShowRaw.IsChecked);
    IniFile.UpdateFile;
    IniFile.Free;
end;

procedure TClientForm.SelCertDirClick(Sender: TObject);
begin
    OpenDialog1.InitialDir := ExtractFilePath(CertDir.Text) ;
    if OpenDialog1.Execute then
        CertDir.Text := ExtractFilePath(OpenDialog1.FileName);
end;

procedure TClientForm.SelCertFileClick(Sender: TObject);
begin
    OpenDialog1.InitialDir := ExtractFilePath(CertFName.Text) ;
    if OpenDialog1.Execute then
        CertFName.Text := OpenDialog1.FileName;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Display a message in our display memo. Delete lines to be sure to not     }
{ overflow the memo which may have a limited capacity.                      }
procedure TClientForm.Display(Msg : String);
var
    I : Integer;
begin
    Log.Lines.BeginUpdate;
    try
        if Log.Lines.Count > 2000 then begin
            for I := 1 to 200 do
                Log.Lines.Delete(0);
        end;
        Log.Lines.Add(Msg);
    finally
        Log.Lines.EndUpdate;
        Log.GoToTextEnd;
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientForm.doListCAClick(Sender: TObject);
begin
    Display(IcsSslRootCAStore.AllCertInfo(True));
    Display('');
end;

procedure TClientForm.ShowOneCert(const FileName: String);
var
    Cert: TX509Base;
    Errs: String;
begin
    if (FileName = '') or (not FileExists(FileName)) then begin
        Display('FileName empty or file doesn''t exist');
        Exit;
    end;
    Cert := TX509Base.Create(nil);
    try
        try
            Cert.LoadFromFileEx(Filename, croTry, croTry, 'password', Errs);
            if Errs <> '' then
                Display (Errs);   { V8.65 }
            if NOT Cert.IsCertLoaded then begin
                Display ('No certificate found in file ' + FileName);
            end
            else begin
                Display ('Certificate file ' + FileName);
                Display (Cert.CertInfo);
            //    if CertCheckOCSP.Checked then
            //        AddLog(ListOcspStatus(Cert));
                if ShowRaw.IsChecked then
                    Display ('Raw Cert' + #13#10 + Cert.GetRawText + #13#10);
                if Cert.IsPKeyLoaded then begin
                    if Cert.CheckCertAndPKey then begin
                       Display('!! Private key available for certificate public key: ' + Cert.KeyInfo);
                        if ShowRaw.IsChecked then
                            Display(Cert.GetPKeyRawText(true))
                    end
                    else
                        Display('!! Private key does not match certificate public key: ' + Cert.KeyInfo);
                end;
            end;
            if Cert.IsInterLoaded then begin
                 Display('!! Intermediate certificates: ' + Cert.ListInters + #13#10);
            end;
        except
            on E:Exception do
            begin
                Display (E.Message) ;
            end;
        end;
    finally
        Cert.Free;
        Display('');
    end;
end;


procedure TClientForm.doListFileClick(Sender: TObject);
begin
    ShowOneCert(CertFName.Text);
end;



procedure TClientForm.doCreateCertClick(Sender: TObject);
var
    HostName, NewFName: String;
    AltNames: TStringList;
begin
    doCreateCert.Enabled := false;
    AltNames := TStringList.Create;
    try
        HostName := Lowercase(IcsGetCompName);
        AltNames.Add(HostName);
        NewFName := CertDir.Text + IcsBuildCertFName(HostName) + '.pem';
        Display('Creating ICS signed certificate for: ' + HostName);
        Log.Repaint;
        try
            IcsDeleteFile(NewFName, False);
            CreateSelfSignCertEx(NewFName, HostName, AltNames, PrivKeyRsa2048, 'password', '', 'ICS');   // last argumenmt is interfile, uses ICS Short if not found
            if FileExists(NewFName) then begin
                ShowOneCert(NewFName);
                Display('Created certificate OK: ' + NewFName);
            end
            else
                Display('Failed to create certificate');
         except
            on E:Exception do
                Display(E.Message);
        end;
    finally
        doCreateCert.Enabled := true;
        AltNames.Free;
    end;
end;



procedure TClientForm.doEventQueueClick(Sender: TObject);
{$IFDEF xxSIX}
var
  MyAsyncSocketQueue : TIcsEventQueue;
{$ENDIF POSIX}
begin
{$IFDEF xxSIX}
    Display('Creating TIcsEventQueue');
    try
        MyAsyncSocketQueue := TIcsEventQueue.Create;
        Display('Created TIcsEventQueue');
    finally
        MyAsyncSocketQueue.Free;
        Display('Freed TIcsEventQueue');
    end;
{$ENDIF POSIX}

end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.

