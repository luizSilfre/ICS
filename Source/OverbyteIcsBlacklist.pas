{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  TIcsBlackList supports blackisting or block listing of IP addresses
              that attempt repeated failed access to TCP/IP servers. It maintains
              a list of IP addresses or Values that have previously exceeded a
              specific number of failed attempts, against which new attempts may
              be checked.  Unit also contains logging functions.
Creation:     March 2009
Updated:      Oct 2024
Version:      V9.4
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2002-2023 by Angus Robertson, Magenta Systems Ltd,
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



BlockAttempts - maximum attempts allowed within BlockAfterMins period
BlockAfterMins - period in minutes in which BlockAttempts allowed
BlockForMins - minutes for which further attempts blocked after max reached


Baseline - March 2009
2 June 2010  - better attempt to clear old data when removed IPs from blacklist
18 July 2012 - added GetCountAll and GetCountBlock
14 Oct 2016  - added SaveAscii to save strings instead of IP addresses
               added ListName propertry for events
               better check for old saved duplicates
14 Nar 2019  - V8.60 - Adapted for main ICS packages and FMX support.
                       Renamed from TMagBlacklist to TIcsBlacklist
                       Added TIcsStringBuild to efficiently build Ansi or Unicode
                        strings on all versions of Delphi.
                       Added TIcsBuffLogStream buffered log stream designed to
                        write large log files, flushing regularly to disk by
                        opening, writing and closing each time the buffer fills
                        or after a timeout of X seconds. The file name is
                        date/time mask format, typically for one log file per day.
                        Write files in ANSI, UTF-9 or UTF-16, with a BOM.
                       Added IcsSimpleLogging write text to end of old or new
                        file, opening and closing file, ignores any errors,
                        not designed for continual updating!  The file name is
                        date/time mask format, typically for one log file per day.
Dec 09, 2020 - V8.65  Fix for Posix.
                      Renamed Ics.Posix.Messages.pas to Ics.Posix.PXMessages.pas.
Jun 24, 2021 - V8.67  Moved TIcsStringBuild class to OverbyteIcsUtils.
                      Internally use BlockedFlag instead of setting attempts
                        to 9999 so we can keep counting failed attempts.
                      SaveAscii defaulted to true to save IPv6 addresses.
Jun 24, 2022 - V8.70  TIcsBuffLogStream.WriteLine is more tolerant of
                         code pages.
Jan 25, 2023 - V8.71  Using Int64 ticks.
Aug 08, 2023 V9.0  Updated version to major release 9.
Aug 07, 2024 V9.3  Moved TIcsBuffLogStream and
Oct 02, 2024 V9.4  Removed Ics.Posix.PXMessages, not needed here.




Note: TIcsBlacklist IP Mask, and White List not implemented yet

There is a web server test application OverbyteIcsSslMultiWebServ.dpr that
uses TIcsBlacklist to block potential hackers and abusers.

TIcsBuffLogStream buffered log file stream is designed to efficiently
write busy log files ensuring they are safely and reguarly written to disk
in case of application crashes. The log file name is in date/time mask
format, typically for one log file per day, and is updated  before each
write.  The file is updated by being opened. written and closed to ensure
nothing remains in memory only, with repeated attempts to open the file
if another application has it open, and with an inactivity timeout so it
is written regularly, defaulting to every 30 seconds. The file code page
may be FileCPAnsi, FileCPUtf8 or FileCPUtf16 (unicode compilers only)
with a BOM written for unicode.  When writing to the log, CRLF is
optionally added to each line (default on).

IcsSimpleLogging is a non-buffered log file function which writes text
to the end of old or new file, opening and closing file for each line,
ignoring any errors, not designed for continual updating!  The file name
is in date/time mask format, typically for one log file per day.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF ICS_INCLUDE_MODE}
unit OverbyteIcsBlacklist;
{$ENDIF}

{$I Include\OverbyteIcsDefs.inc}

{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$H+}             { Use long strings                    }
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Posix.Time,
    Ics.Posix.WinTypes,
//  Ics.Posix.PXMessages,
{$ENDIF}
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    {$IFDEF Rtl_Namespaces}System.DateUtils{$ELSE}DateUtils{$ENDIF},
{$IFDEF FMX}
    Ics.Fmx.OverbyteIcsWndControl,
{$ELSE}
    OverbyteIcsWndControl,
{$ENDIF FMX}
    OverbyteIcsTypes,
    OverbyteIcsTicks64,
    OverbyteIcsUtils;


const
    CopyRight    : String     = ' TIcsBlacklist  (c) 2024 V9.3 ';

    OneMask = '255.255.255.255' ;
//    MaxAttempts = 9999 ;  // 1 Sept 2009, was 100

type

  TBlackRec = record
    Addr: LongWord ;       // 32-bit IP address
    Mask: LongWord ;
    Value: string ;      // Oct 2016
    Ascii: boolean ;     // Oct 2016
    FirstUtime: int64 ;  // Unix time, seconds since 1970
    LastUtime: int64 ;
    Attempts: integer ;
    ReasonId: integer ;
    BlockedFlag: Boolean;  { V8.67 overrides Attempts so we can keep counting }
  end ;
  PTBlackRec = ^TBlackRec ;

  TWhiteRec = record
    Addr: LongWord ;
    Mask: LongWord ;
    Value: string ;      // Oct 2016
  end ;

  TBlackLogEvent = procedure (const info: string) of object ;

  TIcsBlacklist = class(TIcsWndControl)
  private
    { Private declarations }
  protected
    { Protected declarations }
    fBlackRecs: array of TBlackRec ;
    fBlackIdx: TIcsFindList ;
    fTotBlackRecs: integer ;
    fWhiteRecs: array of TWhiteRec ;
    fWhiteIdx: TIcsFindList ;
    fTotWhiteRecs: integer ;
    fBlackFile: string ;
    fWhiteFile: string ;
    fTimer: TIcsTimer ;
    fChangedFlag: boolean ;
    fFlushMins: integer ;
    fCleanMins: integer ;
    fFlushTrg: int64 ;
    fCleanTrg: int64 ;
    fBlockAfterMins: integer ;
    fBlockAttempts: integer ;
    fBlockForMins: integer ;
    fSaveAscii: boolean ;  // Oct 2016
    fListName: string ;    // Oct 2016
    fBlackLogEvent: TBlackLogEvent ;
    procedure SetFlushMins (Value: integer) ;
    procedure SetCleanMins (Value: integer) ;
    procedure SetBlackFile (Value: string) ;
    procedure OnTimer (Sender: TObject);
    function ResizeArray (NewLen: integer): integer ;
  public
    { Public declarations }
    constructor Create(Aowner:TComponent); override;
    destructor Destroy; override;
    procedure doLogEvent (const info: string) ;
    procedure FlushToFiles ;
    procedure LoadFromFiles ;
    procedure ClearList ;
    procedure CleanList ;
    procedure RebuildList;
    procedure AddBlackList (const IpAddr, IpMask: string; ReasonId: integer) ;
    procedure AddWhiteList (const IpAddr, IpMask: string) ;
    procedure RemBlackList (const IpAddr: string) ;
    procedure RemWhiteList (const IpAddr: string) ;
    function CheckBlackList (const IpAddr: string): boolean ;
    function GetFullBlackList(const IpAddr: string): TBlackRec ;
    function FailedBlackList (const IpAddr: string; ReasonId: integer): boolean ;
    function InternalAddBlack (const IpAddr, IpMask: string;
                                  MoreAttempts: boolean; ReasonId: integer): TBlackRec ;
    function ReportBlackList (All: boolean): string ;
    function GetCountAll: integer ;    // 18 July 2012
    function GetCountBlock: integer ;   // 18 July 2012
  published
    { Published declarations }
    property SaveAscii: boolean         read fSaveAscii      write fSaveAscii;  // Oct 2016
    property ListName: string           read fListName       write fListName;
    property BlackFile: string          read fBlackFile      write SetBlackFile;
    property WhiteFile: string          read fWhiteFile      write fWhiteFile;
    property FlushMins: integer         read fFlushMins      write SetFlushMins;
    property CleanMins: integer         read fCleanMins      write SetCleanMins;
    property BlockAfterMins: integer    read fBlockAfterMins write fBlockAfterMins;
    property BlockAttempts: integer     read fBlockAttempts  write fBlockAttempts;
    property BlockForMins: integer      read fBlockForMins   write fBlockForMins;
    property BlackLogEvent: TBlackLogEvent read fBlackLogEvent write fBlackLogEvent;

  end;


implementation

{ TIcsBlacklist }

// must turn off range checking off
{$R-}
{$Q-}

// convert IP address into ASCII and vice versa


function ConvertIPAddr (Addr: LongWord): string ;
var
   I: integer ;
   num4: longword ;
   num1: array [1..4] of byte absolute num4 ;
begin
    num4 := Addr ;
    result := '' ;
    for I := 1 to 4 do
    begin
        result := result + IntToStr (num1 [I]) ;
        if I = 4 then exit ;
        result := result + '.' ;
    end ;
end ;

function IpToStr (Addr: LongWord): string ;
begin
    result := ConvertIPAddr (Addr) ;
end;

function Str2IP (strIP: string; var IPAddr: LongWord): boolean ;
var
    I, len, value, startpos, dotpos: Integer;
    MyIPAddr: array [1..4] of byte ;
    nonzeroflag: boolean ;
begin
    result := false ;
    IPAddr := 0;
    len := Length (strIP) ;
    if len < 7 then exit ;    // 0.0.0.0 bare IP address

// read each dotted number
    nonzeroflag := false ;
    startpos := 1 ;
    for I := 1 to 4 do
    begin
        if len <= 0 then exit ;
        if I < 4 then
            dotpos := Pos ('.', Copy (strIP, startpos, len))
        else
            dotpos := len + 1 ;
        if dotpos <= 0 then exit ;   // not enough dots
        if dotpos > 1 then
            value := atoi (Copy (strIP, startpos, Pred (dotpos)))
        else
            value := 0 ;  // allow for blank
        if value > 255 then exit ;   // number invalid for conversion
        if value > 0 then nonzeroflag := true ;
        MyIPAddr [I] := value ;
        startpos := startpos + dotpos ;
        len := len - dotpos ;
    end ;

// check valid IP address, only allowed all zeroes
    if (MyIPAddr [1] = 0) and nonzeroflag then exit ;

// found a valid IP address, keep it
    Move (MyIPAddr, IPAddr, SizeOf(LongWord)) ;
    result := true ;
end ;

function StrToIP (strIP: string): LongWord ;
begin
    Str2IP (strIP, result) ;
end ;

function IsIPStr (strIP: string): boolean ;
var
    IPAddr: LongWord ;
begin
   result := Str2IP (strIP, IPAddr) ;
end ;

function IsFmtIPStr (var strIP: string): boolean ;
var
    IPAddr: LongWord ;
begin
    result := Str2IP (strIP, IPAddr) ;
    if result then strIP := ConvertIPAddr (IPAddr) ;  // formats less space, zeros, etc.
end ;


constructor TIcsBlacklist.Create(Aowner: TComponent);
begin
    inherited;
    SetLength (fBlackRecs, 0) ;
    fBlackIdx := TIcsFindList.Create ;
    fBlackIdx.Sorted := true ;
    fTotBlackRecs := 0 ;
    SetLength (fWhiteRecs, 0) ;
    fWhiteIdx := TIcsFindList.Create ;
    fWhiteIdx.Sorted := true ;
    fTotWhiteRecs := 0 ;
    fBlackFile := '' ;
    fWhiteFile := '' ;
    fChangedFlag := false ;
    SetFlushMins (5) ;
    SetCleanMins (2) ;
    fBlockAfterMins := 5 ;
    fBlockAttempts := 5 ;
    fBlockForMins := 120 ;
    fSaveAscii := true ;  { V8.67 now true to save IPv6 addresses }
    fListName := 'Blacklist' ;  // Oct 2016
    AllocateHWnd;
    fTimer := TIcsTimer.Create (self) ;
    fTimer.OnTimer := OnTimer ;
    fTimer.Interval := 30 * TicksPerSecond ;
    fTimer.Enabled := true ;
end;

destructor TIcsBlacklist.Destroy;
begin
    try
        if fChangedFlag then FlushToFiles ;
    except
    end ;
    SetLength (fBlackRecs, 0) ;
    FreeAndNil (fBlackIdx) ;
    SetLength (fWhiteRecs, 0) ;
    FreeAndNil (fWhiteIdx) ;
    FreeAndNil (fTimer) ;
    inherited;
end;

{ swap 32-bit longword endian, useful for IP addresses so they sort }

function EndianLong (L: LongWord): LongWord;
begin
  result := Swap (L shr 16) or (LongWord (Swap (L and $ffff)) shl 16);
end;


// called by TFindList for sort and find comparison
// swap endian of IP addresses so they sort nicely

function CompareBlackRec (Item1, Item2: Pointer): Integer;
// Compare returns < 0 if Item1 is less than Item2, 0 if they are equal
// and > 0 if Item1 is greater than Item2.
begin
    if PTBlackRec (Item1).Ascii then
    begin
        result := CompareText (PTBlackRec (Item1).Value, PTBlackRec (Item2).Value) ;
    end
    else
    begin
        if PTBlackRec (Item1).Addr = PTBlackRec (Item2).Addr then
            result := 0
        else if EndianLong (PTBlackRec (Item1).Addr) < EndianLong (PTBlackRec (Item2).Addr) then
            result := -1
        else
            result := 1 ;
    end;
end ;

// resize array and list for new records

function TIcsBlacklist.ResizeArray (NewLen: integer): integer ;
var
    OldLen, K: integer ;
begin
    OldLen := Length (fBlackRecs) ;
    if NewLen < 16 then NewLen := 16 ;
    SetLength (fBlackRecs, NewLen) ;
    for K := OldLen to Pred (NewLen) do
    begin
        with fBlackRecs [K] do
        begin
            Addr := 0 ;  // clear new addresses
            Value := '' ;
            Ascii := false ;
            Attempts := 0 ;  // 2 June 2010 clear record
            FirstUtime := 0 ;
            LastUtime := 0 ;
            BlockedFlag := False; { V8.67 }
        end ;
    end;
    fBlackIdx.Capacity := NewLen ;
    result := NewLen ;
    RebuildList;  // 20 May 2009 rebuild index
    doLogEvent (fListName + ': Increased Empty List Size to ' + IntToStr (NewLen)) ;
end ;

// internal add IP address to blacklist, incremented attempts if done already
// information is saved in a dynamic array, with a sorted list pointing to the
// each record used to search actual IP address (saved as 32-bits).  If an IP
// is removed, the IP for the record is set to zero, and the record then re-used.
// The array is increased in size if too short.

function TIcsBlacklist.InternalAddBlack (const IpAddr, IpMask: string;
                                   MoreAttempts: boolean; ReasonId: integer): TBlackRec ;
var
    BlackRec: TBlackRec ;
    Index, OldLen, K: integer ;
begin
    result.Addr := 0 ;
    result.Value := '' ;
    result.Attempts := 0 ;  // 2 June 2010
    result.ascii := fSaveAscii ;     // Oct 2016
    BlackRec.ascii := fSaveAscii ;   // Oct 2016
    if fSaveAscii then
    begin
        if IpAddr = '' then exit ;
         BlackRec.Value := IpAddr ;  // Oct 2016
    end
    else
    begin
        // warning, only supports IPv4, IPv6 silently ignored
        if NOT Str2IP (IpAddr, BlackRec.Addr) then exit ;  // sanity check
    end ;
    if fBlackIdx.Find (@BlackRec, CompareBlackRec, Index) then
    begin
     // IP already listed, update record and increment attempts
        PTBlackRec (fBlackIdx [Index]).LastUtime := IcsGetUnixTime ;
        inc (PTBlackRec (fBlackIdx [Index]).Attempts) ;

    // see if exceeded maximum attempts allowed and not yet blocked
        if MoreAttempts then
        begin
            if (PTBlackRec (fBlackIdx [Index]).Attempts >= fBlockAttempts) and
                            (NOT PTBlackRec (fBlackIdx [Index]).BlockedFlag) then
            begin
                fChangedFlag := true ;
                PTBlackRec (fBlackIdx [Index]).BlockedFlag := True;     { V8.67 }
            end ;
        end
        else
        begin
            PTBlackRec (fBlackIdx [Index]).BlockedFlag := True;     { V8.67 }
            fChangedFlag := true ;
        end ;
        result := PTBlackRec (fBlackIdx [Index])^ ;
    end
    else
    begin
    // create new record for this IP
        Str2IP (IpMask, BlackRec.Mask) ;
        BlackRec.ReasonId := ReasonId ;
        BlackRec.FirstUtime := IcsGetUnixTime ;
        BlackRec.LastUtime := BlackRec.FirstUtime ;
        BlackRec.Attempts := 1;
        BlackRec.BlockedFlag := False;       { V8.67 }
        if NOT MoreAttempts then
            BlackRec.BlockedFlag := True;     { V8.67 }
        result := BlackRec ;

       // see if allocating more memory in array
        OldLen := Length (fBlackRecs) ;
        if OldLen <= fTotBlackRecs then OldLen := ResizeArray (OldLen * 2) ;

     // find first free record in array
        Index := -1 ;
        for K := 0 to Pred (OldLen) do
        begin
            if fBlackRecs [K].ascii then
            begin
                if fBlackRecs [K].Value = '' then
                begin
                    Index := K ;
                    break ;
                end ;
            end
            else
            begin
                if fBlackRecs [K].Addr = 0 then
                begin
                    Index := K ;
                    break ;
                end ;
            end;
        end ;
        if Index < 0 then
        begin
            doLogEvent (fListName + ': Internal Error, No Blank Records') ;
            exit ;
        end ;

      // add record to array and sorted list
        fBlackRecs [Index] := BlackRec ;
        fBlackIdx.AddSorted (@fBlackRecs [Index], CompareBlackRec) ;
        inc (fTotBlackRecs) ;
        fChangedFlag := true ;
        doLogEvent (fListName + ': Added New Item: ' + IpAddr +
                                    ', Total Items ' + IntToStr (fTotBlackRecs)) ;
    end ;
end;

// unconditional add IP address to blacklist, no more attempts allowed

procedure TIcsBlacklist.AddBlackList(const IpAddr, IpMask: string;
                                                      ReasonId: integer);
begin
    InternalAddBlack (IpAddr, IpMask, false, ReasonId) ;
end;

// add IP address and mask to white list

procedure TIcsBlacklist.AddWhiteList(const IpAddr, IpMask: string);
begin

end;

// get black list record for specific IP address

function TIcsBlacklist.GetFullBlackList(const IpAddr: string): TBlackRec ;
var
    BlackRec: TBlackRec ;
    Index: integer ;
begin
    result.Addr := 0 ;
    result.Value := '' ;
    result.Attempts := 0 ;  // 2 June 2010
    result.Ascii := fSaveAscii ;   // Oct 2016
    BlackRec.ascii := fSaveAscii ;   // Oct 2016
    if fSaveAscii then
    begin
        if IpAddr = '' then exit ;
         BlackRec.Value := IpAddr ;  // Oct 2016
    end
    else
    begin
        if NOT Str2IP (IpAddr, BlackRec.Addr) then exit ;  // sanity check
    end ;
    if (fTotBlackRecs = 0) then exit ;
    if fBlackIdx.Find (@BlackRec, CompareBlackRec, Index) then
                            result := PTBlackRec (fBlackIdx [Index])^ ;
end;

// check if IP address in black list and exceeded maximum attempts
// return true if blocked

function TIcsBlacklist.CheckBlackList(const IpAddr: string): boolean;
var
    BlackRec: TBlackRec ;
begin
    result := false ;
    BlackRec := GetFullBlackList (IpAddr) ;
    if (BlackRec.Attempts = 0) then exit ;  // not found
    if BlackRec.BlockedFlag then result := true ;  { V8.67 }
    if (BlackRec.Attempts >= fBlockAttempts) then result := true ;
end;

// notify a failed login attempt, add IP address to black list if not already
// done otherwise increment attempts counter, return true if now blocked

function TIcsBlacklist.FailedBlackList(const IpAddr: string;
                                            ReasonId: integer): boolean;
var
    BlackRec: TBlackRec ;
begin
    result := false ;
    BlackRec := InternalAddBlack (IpAddr, OneMask, true, ReasonId) ;
    if (BlackRec.Attempts = 0) then exit ;  // not found
    if BlackRec.BlockedFlag then result := true ;  { V8.67 }
    if (BlackRec.Attempts >= fBlockAttempts) then result := true ;
end;

// remove an IP address from black list

procedure TIcsBlacklist.RemBlackList(const IpAddr: string);
var
    BlackRec: TBlackRec ;
    Index: integer ;
begin
    Blackrec.ascii := fSaveAscii ;   // Oct 2016
    if fSaveAscii then
    begin
        if IpAddr = '' then exit ;
         BlackRec.Value := IpAddr ;  // Oct 2016
    end
    else
    begin
        if NOT Str2IP (IpAddr, BlackRec.Addr) then exit ;  // sanity check
    end ;
    if (fTotBlackRecs = 0) then exit ;
    if fBlackIdx.Find (@BlackRec, CompareBlackRec, Index) then
    begin
        PTBlackRec (fBlackIdx [Index]).Addr := 0 ;
        PTBlackRec (fBlackIdx [Index]).Value := '' ;
        PTBlackRec (fBlackIdx [Index]).Attempts := 0 ;
        PTBlackRec (fBlackIdx [Index]).Ascii := false ;
        PTBlackRec (fBlackIdx [Index]).FirstUtime := 0 ;
        PTBlackRec (fBlackIdx [Index]).LastUtime := 0 ;
        PTBlackRec (fBlackIdx [Index]).BlockedFlag := False;   { V8.67 }
        fBlackIdx.Delete (Index) ;
        dec (fTotBlackRecs) ;
        fChangedFlag := true ;
    end ;
end;

// remove an IP address from white list

procedure TIcsBlacklist.RemWhiteList(const IpAddr: string);
begin
    //
end;

// rebuild black list sorted index to array ignoring blank records
// pending - could also compact list periodically if too many blanks

procedure TIcsBlacklist.RebuildList;
var
    I: integer ;
begin
    fBlackIdx.Clear ;
    if fBlackIdx.Capacity < (Length (fBlackRecs) * 2) then
                        fBlackIdx.Capacity := Length (fBlackRecs) * 2 ;
    for I := 0 to Pred (Length (fBlackRecs)) do
    begin
        if (fBlackRecs [I].Attempts <> 0) then
                fBlackIdx.AddSorted (@fBlackRecs [I], CompareBlackRec) ;
    end ;
    fTotBlackRecs := fBlackIdx.Count ;
    doLogEvent (fListName + ': Rebuilt List, Total Items ' + IntToStr (fTotBlackRecs)) ;
    fChangedFlag := true ;
end ;

// clean black list by removing any IP addresses that have been blocked but
// whose blocked time has now expired, and IPs not blocked because they did not
// reach the maximum within the allowed period

procedure TIcsBlacklist.CleanList;
var
    I: integer ;
    rebuild: boolean ;
    utime: int64 ;
begin
    if (fTotBlackRecs = 0) then exit ;
    rebuild := false ;
    utime := IcsGetUnixTime ;
    for I := 0 to Pred (Length (fBlackRecs)) do
    begin
        with fBlackRecs [I] do
        begin
            if (Attempts > 0) then
            begin
                if (Attempts >= fBlockAttempts) or BlockedFlag then  { V8.67 }
                begin
                    if (((utime - LastUtime) div 60) >= fBlockForMins) or
                                                        (LastUtime > utime) then
                    begin
                        if Ascii then
                            doLogEvent (fListName + ': Removed Blocked Item: ' + Value)
                        else
                            doLogEvent (fListName + ': Removed Blocked Item: ' + IpToStr (Addr)) ;
                        Addr := 0 ;
                        Ascii := false ;
                        Value := '' ;
                        Attempts := 0 ;  // 2 June 2010 clear record
                        FirstUtime := 0 ;
                        LastUtime := 0 ;
                        BlockedFlag := False;   { V8.67 }
                        rebuild := true ;
                    end ;
                end
                else
                begin
                    if (((utime - FirstUtime) div 60) >= fBlockAfterMins) or
                                                        (FirstUtime > utime) then
                    begin
                        if Ascii then
                            doLogEvent (fListName + ': Removed Old Item: ' + Value)
                        else
                            doLogEvent (fListName + ': Removed Old Item: ' + IpToStr (Addr)) ;
                        Addr := 0 ;
                        Value := '' ;
                        Ascii := false ;
                        Attempts := 0 ;  // 2 June 2010 clear record
                        FirstUtime := 0 ;
                        LastUtime := 0 ;
                        BlockedFlag := False;   { V8.67 }
                        rebuild := true ;
                    end ;
                end ;
            end ;
        end ;
    end ;
    if rebuild then RebuildList;
end;

// clear black list of all IP addresses

procedure TIcsBlacklist.ClearList;
begin
    fTotBlackRecs := 0 ;
    SetLength (fBlackRecs, 0) ;
    fBlackIdx.Clear ;
    fChangedFlag := true ;
end;

// log event

procedure TIcsBlacklist.doLogEvent (const info: string) ;
begin
    if Assigned (fBlackLogEvent) then fBlackLogEvent (info) ;
end;

// flush black and white lists to comma separated ASCII files
// this is done automatically every two minutes and when the component
// is destroyed

procedure TIcsBlacklist.FlushToFiles;
var
    FileLines: TStringList ;
    I: integer ;
    S, S1: string ;
begin
    fChangedFlag := false ;
    if fBlackFile = '' then exit ;  // no file name means nothing to save
    if NOT Assigned (fBlackIdx) then exit ;  // sanity check
    FileLines := TStringList.Create ;
    try
        if (fTotBlackRecs > 0) then
        begin
            for I := 0 to Pred (Length (fBlackRecs)) do
            begin
                with fBlackRecs [I] do
                begin
                    if (Attempts = 0) or (FirstUtime = 0) then continue ;
                    if Ascii then
                    begin
                        S := Value ;
                        if S = '' then continue ;
                    end
                    else
                    begin
                        if Addr = 0 then continue ;
                        S := IpToStr (Addr) ;
                    end;
                    if BlockedFlag then
                        S1 := 'Y'
                    else
                        S1 := 'N';
                    FileLines.Add (S + ',' + IpToStr (Mask) + ',' +
                      IntToStr (FirstUtime) + ',' + IntToStr (LastUtime) + ',' +
                        IntToStr (Attempts) + ',' + IntToStr (ReasonId) + ',' + S1 ) ;
                end ;
            end ;
        end ;
        try
            if FileExists (fBlackFile) then
                {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif}.DeleteFile (fBlackFile) ;
            FileLines.SaveToFile (fBlackFile) ;
            doLogEvent (fListName + ': Saved to File: ' + fBlackFile) ;
        except
            doLogEvent (fListName + ': Failed File Save: ' + fBlackFile +
                                         ' - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FileLines.Free ;
        fChangedFlag := false ;
    end ;
end;

// load black and white lists from comma separated ASCII files, then Clean
// any IP addresses that have expired and flush back to disk
// this is done automatically when the BlackFile property is assigned a file name

procedure TIcsBlacklist.LoadFromFiles;
var
    FileLines, FileRecord: TStringList ;
    I, BadNr, Index: integer ;
begin
    fChangedFlag := false ;
    if fBlackFile = '' then exit ;  // no file name means nothing to read
    if NOT Assigned (fBlackIdx) then exit ;  // sanity check
    FileLines := TStringList.Create ;
    FileRecord := TStringList.Create ;
    try
        try
            if NOT FileExists (fBlackFile) then
            begin
                doLogEvent (fListName + ': File Not Found: ' + fBlackFile) ;
                exit ;
            end ;
            FileLines.LoadFromFile (fBlackFile) ;
        except
            doLogEvent (fListName + ': Failed to File Load: ' + fBlackFile +
                                         ' - ' + IcsGetExceptMess (ExceptObject)) ;
            exit ;
        end ;
        ClearList ;
         if FileLines.Count > 0 then
        begin
            BadNr := 0 ;
            ResizeArray (FileLines.Count * 2) ;
            for I := 0 to Pred (FileLines.Count) do
            begin
                FileRecord.CommaText := FileLines [I] ;
                if FileRecord.Count >= 6 then
                begin
                    with fBlackRecs [fTotBlackRecs] do
                    begin
                        Ascii := FSaveAscii ;
                        if FSaveAscii then
                            Value := FileRecord [0]
                        else
                            Str2IP (FileRecord [0], Addr) ;
                        if ((Value <> '') or (Addr <> 0)) and (atoi64 (FileRecord [2]) > 0) and
                                                                    (atoi (FileRecord [4]) > 0) then
                        begin
                            if NOT fBlackIdx.Find (@fBlackRecs [fTotBlackRecs], CompareBlackRec, Index) then
                            begin
                                Str2IP (FileRecord [1], Mask) ;
                                FirstUtime := atoi64 (FileRecord [2]) ;
                                LastUtime := atoi64 (FileRecord [3]) ;
                                Attempts := atoi (FileRecord [4]) ;
                                ReasonId := atoi (FileRecord [5]) ;
                                if (FileRecord.Count >= 7) and (FileRecord [6] = 'Y') then
                                    BlockedFlag := True;                                   { V8.67 }
                                fBlackIdx.AddSorted (@fBlackRecs [fTotBlackRecs],
                                                                     CompareBlackRec) ;
                                inc (fTotBlackRecs) ;
                            end
                            else
                            begin
                                 Addr := 0 ;
                                 Value := '' ;
                                 doLogEvent (fListName + ': Ignored old saved duplicate') ;
                                 inc (BadNr) ;
                            end;
                        end
                        else
                            inc (BadNr) ;
                    end ;
                end
                else
                    inc (BadNr) ;
            end ;
            doLogEvent (fListName + ': Loaded from File: ' + fBlackFile +
                               ', Total Items ' + IntToStr (fTotBlackRecs) +
                                            ', Bad Records ' + IntToStr (BadNr)) ;
            fChangedFlag := false ;
            CleanList;
            if fChangedFlag then FlushToFiles ;
        end
        else
            doLogEvent (fListName + ': File Empty: ' + fBlackFile) ;

    finally
        FileLines.Free ;
        FileRecord.Free ;
        fChangedFlag := false ;
    end ;
end;

// timer called every 30 seconds to Clean and Flush lists

procedure TIcsBlacklist.OnTimer(Sender: TObject);
begin
    fTimer.Enabled := false ;
    try
        if IcsTestTrgTick64 (fCleanTrg) then
        begin
            fCleanTrg := IcsGetTrgMins64 (fCleanMins) ;
            CleanList ;
            if fChangedFlag then FlushToFiles ;
        end ;
        if IcsTestTrgTick64 (fFlushTrg) then
        begin
            fFlushTrg := IcsGetTrgMins64 (fFlushMins) ;
            if fChangedFlag then FlushToFiles ;
        end ;

    finally
        fTimer.Enabled := true ;
    end ;
end;

function TIcsBlacklist.ReportBlackList (All: boolean): string ;
var
    I: integer ;
begin
    result := '' ;
    if (fTotBlackRecs = 0) then exit ;
    for I := 0 to Pred (fBlackIdx.Count) do
    begin
        with PTBlackRec (fBlackIdx [I])^ do
        begin
            if (Attempts >= fBlockAttempts) or All then
            begin
                if Ascii then
                begin
                    if Value = '' then continue ;
                    result := result + Value ;
                end
                else
                begin
                    if Addr = 0 then continue ;
                    result := result + IpToStr (Addr) ;
                end;
                result := result + ' attempts ' + IcsIntToCStr (Attempts) +
                    ', first at ' + TimeToStr (UnixToDateTime (FirstUtime)) +
                          ', last at ' + TimeToStr (UnixToDateTime (LastUtime)) ;
                if BlockedFlag then result := result + ' BLOCKED' ;  { V8.67 }
                result := result + IcsCRLF ;
            end;
        end;
    end;
end;

function TIcsBlacklist.GetCountAll: integer ;    // 18 July 2012
var
    I: integer ;
begin
    result := 0 ;
    if (fTotBlackRecs = 0) then exit ;
    for I := 0 to Pred (fBlackIdx.Count) do
    begin
        with PTBlackRec (fBlackIdx [I])^ do
        begin
           if (Attempts = 0) then continue ;
           inc (result) ;
        end;
    end;
end;

function TIcsBlacklist.GetCountBlock: integer ;   // 18 July 2012
var
    I: integer ;
begin
    result := 0 ;
    if (fTotBlackRecs = 0) then exit ;
    for I := 0 to Pred (fBlackIdx.Count) do
    begin
        with PTBlackRec (fBlackIdx [I])^ do
        begin
            if (Attempts = 0) then continue ;
            if (Attempts >= fBlockAttempts) then inc (result) ;
        end;
    end;
end;

// property setters

procedure TIcsBlacklist.SetCleanMins(Value: integer);
begin
    fCleanMins := Value ;
    if fCleanMins = 0 then fCleanMins := 2 ;
    fCleanTrg := IcsGetTrgMins64 (fCleanMins) ;
end;

procedure TIcsBlacklist.SetFlushMins(Value: integer);
begin
    fFlushMins := Value ;
    if fFlushMins = 0 then fFlushMins := 2 ;
    fFlushTrg := IcsGetTrgMins64 (fFlushMins) ;
end;

procedure TIcsBlacklist.SetBlackFile(Value: string);
begin
    if Value <> fBlackFile then
    begin
        fBlackFile := Value ;
        if fTotBlackRecs = 0 then LoadFromFiles;  // don't kill existing black list
    end ;
end;


end.
