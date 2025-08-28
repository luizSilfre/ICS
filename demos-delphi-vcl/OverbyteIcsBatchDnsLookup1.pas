unit OverbyteIcsBatchDnsLookup1;
{
Program:      NsLookup
Description:  ICS batch async DNS lookup DnsLookup (IPv6 and IPv4)
Author:       François Piette
Creation:      ?
Version:      V9.3
EMail:        http://www.overbyte.be        francois.piette@overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1999-2024 by François PIETTE
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

Mar  7, 2017  V8.43  Added Use Thread tick box so wsockets uses thread for
                    all DNS lookups instead of just IPv4
Apr 15, 2017  V8.44 FPiette removed compiler warnings for D10.2
Mar 10, 2020  V8.64 Added support for International Domain Names for Applications
                     (IDNA), i.e. using accents and unicode characters in domain names.
                    The IDN button encodes the IDN list into Punycode ASCII and
                      then back to Unicode again.  If built on a pre-Unicode compiler,
                      some domains will appear as ???.
                    When doing DNS Lookups, the actual Punycode ASCII domain looked-up
                      is shown.
                    A blank IDN list on startup loads default list.
Jul 22, 2023  V8.71 mâgsÿstést.eu domain gone, UK residents no longer allowed to
                      use eu domains since it's political not geographic.
                    Using IcsDomainNameCache component instead of wsocket DNS lookups.
Aug 08, 2023 V9.0  Updated version to major release 9.
Aug 07, 2024 V9.3  Added OverbyteIcsTypes for consolidated types and constants, allowing
                     other import units to be removed.


}

interface

{$WARN SYMBOL_PLATFORM OFF}
{$I Include\OverbyteIcsDefs.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Contnrs, ExtCtrls,
{$IFDEF DELPHI24_UP}
  UITypes,
{$ENDIF}
  OverbyteIcsUtils,
  OverbyteIcsIniFiles,
//OverbyteIcsWndControl,
//OverbyteIcsWSocket,
  OverbyteIcsDnsQuery,
  OverbyteIcsTypes, OverbyteIcsWndControl;  { V9.3 consolidated types and constants }

type
  TBatchDnsLookupForm = class(TForm)
    StartButton: TButton;
    DnsNamesMemo: TMemo;
    ResultMemo: TMemo;
    Label4: TLabel;
    InstancesEdit: TEdit;
    SocketFamilyComboBox: TComboBox;
    Label5: TLabel;
    IDNMemo: TMemo;
    doIDNEncode: TButton;
    DecodeMemo: TMemo;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    IcsDomainNameCache1: TIcsDomainNameCache;
    DNCacheMethod: TRadioGroup;
    procedure StartButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure doIDNEncodeClick(Sender: TObject);
    procedure DnsNamesMemoDblClick(Sender: TObject);
    procedure IcsDomainNameCache1DNUpdateEvent(Sender: TObject; ItemNr: Integer);
    procedure DNCacheMethodClick(Sender: TObject);
  private
    FInstances : Byte;
    FIniFile : TIcsIniFile;
    FInitialized : Boolean;
  public
    property IniFile: TIcsIniFile read FIniFile;
  end;

var
  BatchDnsLookupForm: TBatchDnsLookupForm;

implementation

{$R *.dfm}
uses
    Math;

const
    SectionWindow      = 'Window';   // Must be unique for each window
    KeyTop             = 'Top';
    KeyLeft            = 'Left';
    SectionSetup       = 'Setup';
    KeyDNCacheMethod   = 'DNCacheMethod';
    KeyInstances       = 'NumberOfInstances';
    SectionDnsNames    = 'DnsNames';
    KeyDnsName         = 'Item';

procedure TBatchDnsLookupForm.FormCreate(Sender: TObject);
begin
{$IF RTLVersion >= 18}
    { Built-in memory leak detection and display since BDS2006 }
    { This is useful for debugging, however a bit slower.      }
    ReportMemoryLeaksOnShutdown := DebugHook <> 0;
{$IFEND}
    IcsNameThreadForDebugging('Main');
    ResultMemo.Clear;
    DnsNamesMemo.WordWrap := FALSE;
    FIniFile := TIcsIniFile.Create(OverbyteIcsIniFiles.GetIcsIniFileName);
    ActiveControl := StartButton;
end;

procedure TBatchDnsLookupForm.FormDestroy(Sender: TObject);
begin
    FIniFile.Free;
end;

procedure TBatchDnsLookupForm.FormShow(Sender: TObject);
begin
    if not FInitialized then begin
        FInitialized := TRUE;
        Top          := IniFile.ReadInteger(SectionWindow, KeyTop,
                                               (Screen.Height - Height) div 2);
        Left         := IniFile.ReadInteger(SectionWindow, KeyLeft,
                                               (Screen.Width  - Width)  div 2);
        DNCacheMethod.ItemIndex := IniFile.ReadInteger(SectionSetup, KeyDNCacheMethod,  0);
        FInstances   := IniFile.ReadInteger(SectionSetup, KeyInstances, 4);
        IniFile.ReadStrings(SectionDnsNames, KeyDnsName, DnsNamesMemo.Lines);

    { beware these names are stored in INI file so only refreshed if the INI lines are removed, or reading them above }
        if DnsNamesMemo.Lines.Count = 0 then  { V8.64 }
        begin
            DnsNamesMemo.Text :=
            'www.overbyte.eu'#13#10 +       { V8.71 was be }
            'svn.overbyte.be'#13#10 +
            'wiki.overbyte.eu'#13#10 +
            'mail.magsys.co.uk'#13#10 +      { V8.71 both v4 and v6 }
            'ipv4.magsys.co.uk'#13#10 +
            'ipv6.magsys.co.uk'#13#10 +
            'www.embarcardero.com'#13#10 +
            'edn.embarcardero.com'#13#10 +
            'www.microsoft.com'#13#10 +
            'ipv6.google.com'#13#10 +
            'www.google.com'#13#10 +           { V8.71 typo }
            'sourceforge.net'#13#10 +
            'msdn.microsoft.com'#13#10 +
            'localhost'#13#10 +
            '127.0.0.1'#13#10 +
            '::1'#13#10 +
            'strøm.no'#13#10 +
            'www.mâgsÿstést.eu'#13#10 +
            'www.háčkyčárky.cz'#13#10 +         { needs Unicode }
            'stránky.háčkyčárky.cz'#13#10 +     { needs Unicode }
            'мособлеирц.рф'#13#10 +             { needs Unicode }
            '例子.测试'#13#10 +                 { needs Unicode, lookup fails }
            'www.bad(name).com'#13#10 +
            'www.-badname.com'#13#10;
        end;
        InstancesEdit.Text := IntToStr(FInstances);
    end;
end;


procedure TBatchDnsLookupForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    try
        IniFile.WriteInteger(SectionWindow,   KeyTop,         Top);
        IniFile.WriteInteger(SectionWindow,   KeyLeft,        Left);
        IniFile.WriteInteger(SectionWindow,   KeyDNCacheMethod, DNCacheMethod.ItemIndex);
        FInstances  := StrToIntDef(InstancesEdit.Text, 3);
        IniFile.WriteInteger(SectionSetup,    KeyInstances,   FInstances);
         IniFile.WriteStrings(SectionDnsNames, KeyDnsName,    DnsNamesMemo.Lines);
       IniFile.UpdateFile;
    except
        on E: Exception do
            MessageDlg(E.ClassName + ' ' + E.Message, mtError, [mbOK], 0);
    end;
end;

// event called by TIcsDomainNameCache when name lookup done
procedure TBatchDnsLookupForm.IcsDomainNameCache1DNUpdateEvent(Sender: TObject; ItemNr: Integer);
var
    Row: Integer;
    MyCache: TIcsDomainNameCache;
begin
    MyCache := (Sender as TIcsDomainNameCache);
    with MyCache.GetDNItem(ItemNr) do begin
        Row := ReqTag;
        ResultMemo.Lines[Row]  := MyCache.BuildRespList(ItemNr);
        ResultMemo.Update;
    end;
end;

procedure TBatchDnsLookupForm.StartButtonClick(Sender: TObject);
var
    I: Integer;
    ErrFlag: Boolean;
begin
    IDNMemo.Clear;
    DecodeMemo.Clear;
    for I := 0 to DnsNamesMemo.Lines.Count - 1 do
    ResultMemo.Clear;
    ResultMemo.Update;
    IDNMemo.Update;
    if StrToIntDef(InstancesEdit.Text, 1) <> FInstances then
        FInstances := StrToIntDef(InstancesEdit.Text, 1);

    IcsDomainNameCache1.MaxLookups := FInstances;
    IcsDomainNameCache1.DNMethod := TDNMethod(DNCacheMethod.ItemIndex);
    for I := 0 to DnsNamesMemo.Lines.Count - 1 do begin
        Application.ProcessMessages;
        if DnsNamesMemo.Lines[I] <> '' then begin
            ResultMemo.Lines.Add('Resolving..');
            IDNMemo.Lines.Add(IcsIDNAToASCII(IcsTrim(DnsNamesMemo.Lines[I]), False, ErrFlag));
            IcsDomainNameCache1.LookupHostAsync(IcsTrim(DnsNamesMemo.Lines[I]), I,
                                        TSocketFamily(SocketFamilyComboBox.ItemIndex), IcsDomainNameCache1DNUpdateEvent);
        end
        else begin
            ResultMemo.Lines.Add('');
            IDNMemo.Lines.Add('');
        end;
    end;
end;

procedure TBatchDnsLookupForm.DNCacheMethodClick(Sender: TObject);
begin
    IcsDomainNameCache1.MaintClearAll;  { clear cache when changing lookup method }
end;

procedure TBatchDnsLookupForm.DnsNamesMemoDblClick(Sender: TObject);
begin
    DnsNamesMemo.Lines.Clear;
end;

procedure TBatchDnsLookupForm.doIDNEncodeClick(Sender: TObject);
var
    I: Integer;
    PunyStr, UniStr: String;
    ErrFlag: Boolean;
begin
    IDNMemo.Clear;
    DecodeMemo.Clear;
    for I := 0 to DnsNamesMemo.Lines.Count -1 do begin
        UniStr := '';
        PunyStr := IcsIDNAToASCII(DnsNamesMemo.Lines[I], True, ErrFlag);
        if ErrFlag then
            PunyStr := 'IDNAToAscii Failed'
        else begin
            UniStr := IcsIDNAToUnicode(PunyStr, ErrFlag);
            if ErrFlag then
                UniStr := 'IDNAToUnicode Failed'
        end;
        IDNMemo.Lines.Add(PunyStr);
        DecodeMemo.Lines.Add(UniStr);
    end;
end;


end.
