{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  ICS Snippets, small samples of codes for FTP, HTTP, sockets and email.
Creation:     Apr 2023
Updated:      Nov 2024
Version:      V9.4
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

This application contains snippets of code to illustrate many of the newer ICS components,
mostly written self contained within a single method, with most of the parameters also
hard coded in the method, file names, URLs, logins, etc. Most snippets support SSL/TLS
automatically, provided the OpenSSL DLLs are available, by default linked into the
application files.

Most of the snippets access Magenta Systems Ltd public ICS web and FTP servers and ahould
just work without change, except for FTP uploading where you will need to request an
account by emailing delphi@magsys.co.uk

Please change any of the hard coded settings or Temp Dir to your own public or private servers
and logins, but beware installing new versions of ICS may overwrite this application.

For simplicity, the snippets create all the ICS components in code locally and set the
events and properties in code, often quicker to drop them on a form and set events and
maybe some properties that way, although setting them in code is self documenting.

Most snippets have a single event for logging progress and errors to the screen, this
also sometimes updates a progress caption.  Some snippets also have an event for receiving
data which is just logged.  Most ICS high level components can be used in sync (blocking)
or async (non blocking) mode.  Most snippets used sync mode so the code is self contained
within a single function, but a few only work async typically receiving data and so are
followed by a message loop with a five minute timeout to allow them to do their stuff,
clicking the Abort button will mostly stop them.

Hopefully the snippets are easier to follow than the normal samples used to develop ICS
components and which often become very complicated due to all the different functionality
supported.

All the snippets comprise a single button that will cause files to be accessed on public
servers, showing a log window.  Some will download files to a specified local directory
defaulting to c:\temp-ics, please change this in the designer if necessary.  Using the
FTP or HTTP download snippets first will place files in the temp directory which may be
used for the file copy and FTP and HTTP uploading snippets.

Snippets in this unit includes:

Snippet: View Local Directories - print a directory file listing.
Snippet: File Copy One File - copy a single file.
Snippet: File Copy Multiple Files - copy multiple files.
Snippet: FTP View Directories - print a remote directory listing from an FTP site.
Snippet: FTP Download One File - download a single file from an FTP site.
Snippet: FTP Download Multiple Files - downloads multiple files from an FTP site.
Snippet: FTP Upload One File - upload a single file to an FTP site.
Snippet: FTP Upload Multiple Files - upload multiple files to an FTP site.
Snippet: HTTP Download List of Files - downloads a list of files from a web site.
Snippet: HTTP Download Linked Files - downloads multiple files from a web site by parsing HTML pages for links.
Snippet: HTTP REST Json Request - makes an HTTP GET request to a REST server receiving a Json response data.
Snippet: HTTP REST Download - makes an HTTP GET request to download a file, with optional resume of partial download.
Snippet: HTTP Simple Upload File - makes a HTTP POST request to upload a single file to a special upload web page.
Snippet: HTTP Form Upload File - makes a HTTP POST request using form-data parameters and files to a special upload web page.
Snippet: Local Socket Traffic - Send simple text traffic between two sockets on the same PC, using client server concepts.
Snippet: Remote Socket Traffic - Receive simple text traffic from a remote TCP Server.
Snippet: WebSocket Client - Connect to a remote WebSocket server to send and receive data.
Snippet: Send Email using Mail Queue - Runs a mail queue to send multiple emails with extended retries over many hours or days.
Snippet: HTTP REST Get Paramaters - makes an HTTP GET request with URL parameters to a server that echoes back those params.
Snippet: HTTP REST Post Paramaters - makes an HTTP POST request with URL parameters to a server that echoes back those params.


History:
Apr 19, 2023  - V8.71 baseline
Aug 08, 2023 V9.0  Updated version to major release 9.
Nov 07, 2023 V9.1  Improved HTTP uploading snippets for new features in TSslHttpRest.
                   Snippet HTTP POST Upload File is now HTTP Simple Upload File.
                   New snippet HTTP Form Upload File that uses form-data parameters.
Apr 24, 2024 V9.2  https://www.telecom-tariffs.co.uk/testing/ needs authentication, add
                     it where necessary.
Aug 7, 2024  V9.3  Added OverbyteIcsTypes for consolidated types and constants, allowing
                     six other import units to be removed.
                   Corrected form description, OpenSSL usually linked into ICS apps.
Nov 5, 2024  V9.4  Added two new simple REST snippets to Get/Post Parameters that send
                     them to an ICS server that echoes back those params for testing.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSnippets1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ExtCtrls,
//OverbyteIcsWsocket,    { for winsock functions and constants }   { V9.3 use OverbyteIcsTypes instead }
//OverbyteIcsLogger,     { for TLogOption }                        { V9.3 use OverbyteIcsTypes instead }
//OverbyteIcsSSLEAY,     { for debug, ssl and http constants }     { V9.3 use OverbyteIcsTypes instead }
//OverbyteIcsHttpProt,   { TSslHttpCli and HTTP types }            { V9.3 use OverbyteIcsTypes instead }
//OverbyteIcsSmtpProt,   { for mail protocol types }               { V9.3 use OverbyteIcsTypes instead }
//OverbyteIcsFtpcli,     { TSslFtpClient and FTP types }           { V9.3 use OverbyteIcsTypes instead }
  OverbyteIcsURL,        { for REST parameters }
  OverbyteIcsUtils,      { common functions }
  OverbyteIcsTicks64,    { tick timer counters }
  OverbyteIcsMailQueue,  { TIcsMailQueue }
  OverbyteIcsFileCopy,   { TIcsFileCopy, file types and functions }
  OverbyteIcsFtpMulti,   { TIcsFtpMulti }
  OverbyteIcsHttpMulti,  { TIcsHttpMulti }
  OverbyteIcsSslHttpRest,   { TSslHttpRest }
  OverbyteIcsIpStreamLog,   { TIcsIpStrmLog }
  OverbyteIcsWebSocketCli,  { TSslWebsocketCli }
  OverbyteIcsTypes;  { V9.3 consolidated types and constants }

{ note you are unlikely to need all these units for one or two tasks, usually the
  main component and a lower level one with types and constants }

type
  TSnippets = class(TForm)
    Panel1: TPanel;
    LogWin: TMemo;
    doFtpDownOneFile: TButton;
    doFileCopyOneFile: TButton;
    DirTemp: TEdit;
    Label1: TLabel;
    LabelProgress: TLabel;
    ShowDiags: TCheckBox;
    doAbort: TButton;
    doClose: TButton;
    Label2: TLabel;
    doFtpDownMultiFiles: TButton;
    doFileCopyMultiFiles: TButton;
    doFtpUpOneFile: TButton;
    doFtpUpMultiFiles: TButton;
    doFtpViewDirs: TButton;
    doHttpSimpleUpload: TButton;
    doHttpDownList: TButton;
    doHttpDownLinked: TButton;
    doFileDirView: TButton;
    doHttpFormUpload: TButton;
    doHttpRestReq: TButton;
    doEmailMailQu: TButton;
    doSocketLocal: TButton;
    doClearLog: TButton;
    FtpOnlyCheck: TCheckBox;
    doSocketRemote: TButton;
    doWebSocket: TButton;
    doHttpRestDown: TButton;
    doHttpGetParams: TButton;
    doHttpPostParams: TButton;
    procedure doFtpDownOneFileClick(Sender: TObject);
    procedure doFileCopyOneFileClick(Sender: TObject);
    procedure doAbortClick(Sender: TObject);
    procedure doCloseClick(Sender: TObject);
    procedure doFtpDownMultiFilesClick(Sender: TObject);
    procedure doFileCopyMultiFilesClick(Sender: TObject);
    procedure doFtpViewDirsClick(Sender: TObject);
    procedure doFtpUpOneFileClick(Sender: TObject);
    procedure doFtpUpMultiFilesClick(Sender: TObject);
    procedure doFileDirViewClick(Sender: TObject);
    procedure doHttpDownListClick(Sender: TObject);
    procedure doHttpDownLinkedClick(Sender: TObject);
    procedure doHttpRestReqClick(Sender: TObject);
    procedure doHttpSimpleUploadClick(Sender: TObject);
    procedure doHttpFormUploadClick(Sender: TObject);
    procedure doEmailMailQuClick(Sender: TObject);
    procedure doSocketLocalClick(Sender: TObject);
    procedure doClearLogClick(Sender: TObject);
    procedure doWebSocketClick(Sender: TObject);
    procedure doSocketRemoteClick(Sender: TObject);
    procedure doHttpRestDownClick(Sender: TObject);
    procedure doHttpGetParamsClick(Sender: TObject);
    procedure doHttpPostParamsClick(Sender: TObject);
  private
    { Private declarations }
    procedure AddLogText(const Line: String);
    procedure onXferEvent (LogLevel: TIcsCopyLogLevel ; Info: string ; var Cancel: boolean) ; // used for FTP, File Copy and HTTP Multi
    procedure onSslHttpRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);   // used for HTTP REST and WebSockets
    procedure onSslWebSocketCliWSFrameRcvd(Sender: TSslWebSocketCli; const APacket: string; var AFrame: TWebSocketReceivedFrame); // WebSockets
    procedure onSslWebSocketCliWSConnected(Sender: TObject);                                                                      // WebSockets
    procedure onSslWebSocketCliWSDisconnected(Sender: TObject);                                                                   // WebSockets
    procedure onCliLogRecvEvent(Sender: TObject; Socnr: integer; const Line: string);                            // IP Stream Logging client
    procedure onCliLogProgEvent(Sender: TObject; Socnr: integer; LogOption: TLogOption; const Msg: string);      // IP Stream Logging client
    procedure onSrvLogRecvEvent(Sender: TObject; Socnr: integer; const Line: string);                            // IP Stream Logging server
    procedure onSrvLogProgEvent(Sender: TObject; Socnr: integer; LogOption: TLogOption; const Msg: string);      // IP Stream Logging server
    procedure onIcsMailQueueLogEvent(LogLevel: TMailLogLevel; const Info: string);                               // Mail Queue
  public
    { Public declarations }
  end;

var
  Snippets: TSnippets;
  AbortFlag: Boolean;

implementation

{$R *.dfm}

procedure TSnippets.AddLogText(const Line: String);
begin
    LogWin.Lines.Add(Line);
end;


{ onCopyEvent event used by the FTP, HTTP and file copying components for logging and progress
  information, all optional and the components will work without this event.  But it means you
  can see what is happening, with varying levels of logging.  Also allows long running operations
  to be cancelled by clicking the Abort button.
}

procedure TSnippets.onXferEvent (LogLevel: TIcsCopyLogLevel ; Info: string ; var Cancel: boolean) ;
begin
    if (LogLevel = LogLevelInfo) or (LogLevel = LogLevelFile) then
    begin
        AddLogText (Info) ;
        LabelProgress.Caption := Info ;
    end ;
    if (LogLevel = LogLevelProg) then
    begin
        if Info <> '' then
            LabelProgress.Caption := 'Progress: ' + Info
        else
            LabelProgress.Caption := '' ;
    end ;
    if (LogLevel = LogLevelDiag) and ShowDiags.Checked  then
        AddLogText (Info) ;
    if AbortFlag then
        Cancel := true ;
end;


procedure TSnippets.doAbortClick(Sender: TObject);
begin
    AbortFlag := True;
    AddLogText ('Abort Clicked') ;
end;

procedure TSnippets.doClearLogClick(Sender: TObject);
begin
    LogWin.Lines.Clear;
end;

procedure TSnippets.doCloseClick(Sender: TObject);
begin
    Close;
end;


{ Snippet: View Local Directories - print a directory file listing, optionally
  including all sub-directories, with local or UTC time stamps.  This function
  is used by most of the file transfer methods to determine if local files
  already so they can be replaced if older or xfers skipped.  The snippet will
  list the default temporary directory DirTemp.Text.
  The full file transfer sample is \sslinternet\OverbyteIcsXferTst.dpr.
}

procedure TSnippets.doFileDirViewClick(Sender: TObject);
var
    FileCopyClient: TIcsFileCopy ;
    myfilepath, dirlisting: String;
begin

// parameters for the single file copy operation
    myfilepath := IncludeTrailingPathDelimiter(DirTemp.Text);

// create component and events to see progress
    FileCopyClient := TIcsFileCopy.Create (self) ;
    doFileDirView.Enabled := false ;
    try
        try
            AddLogText ('Directory Path: ' + myfilepath) ;
            dirlisting := FileCopyClient.DispLocFiles(myfilepath, '*/*', FCTypeAllDir, True, False, True) ;  // sub-dirs, UTC stamps, dir names
            AddLogText (dirlisting) ;
        except
            AddLogText ('Directory Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (FileCopyClient) ;
        doFileDirView.Enabled := true ;
    end ;
end;

{ Snippet: File Copy One File - copy a single file, optionally replacing an existing
  file if older, Safe Copy means the file is copied with a temporary name and renamed
  upon completion to avoid losing an existing file upon failure.  Copy progress
  information is available in the onCopyEvent.  The CopyOneFile method returns
  TaskResFail if the file or destination are not found with the error in the
  ReqResponse property, TaskResOKNew for successful copy, TaskResOKNone if the destination
  file was the same and copy was skipped, TaskResAbort if the method was aborted.
  The snippet will copy a file downloaded into the default temporary directory DirTemp.Text
  by the FTP Download One File snippet.
  The full file transfer sample is \sslinternet\OverbyteIcsXferTst.dpr.
}

procedure TSnippets.doFileCopyOneFileClick(Sender: TObject);
var
    FileCopyClient: TIcsFileCopy ;
    taskres: TIcsTaskResult ;
    myfilesource, myfiletarget: String;
    myfilereplace: TIcsFileCopyRepl;
    newfsize: Int64;
begin

// parameters for the single file copy operation
    myfilesource := IncludeTrailingPathDelimiter(DirTemp.Text) + 'speed50meg.zip';
    myfiletarget := IncludeTrailingPathDelimiter(DirTemp.Text) + 'copiedfiles\speed50meg.zip';
    myfilereplace := FCReplAlways;     // or FCReplNever, FCReplNewer

// create component and events to see progress
    FileCopyClient := TIcsFileCopy.Create (self) ;
    FileCopyClient.CopyEvent := onXferEvent ;
    doFileCopyOneFile.Enabled := false ;
    AbortFlag := false ;
    LabelProgress.Caption := '' ;
    try
        try
            taskres := FileCopyClient.CopyOneFile (myfilesource, myfiletarget, myfilereplace, True, newfsize);  // SafeCopy=True
            AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
            AddLogText (FileCopyClient.ReqResponse) ;
        except
            AddLogText ('Copying Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (FileCopyClient) ;
        LabelProgress.Caption := 'File Copy Completed' ;
        doFileCopyOneFile.Enabled := true ;
    end ;
end;

{ Snippet: File Copy Multiple Files - copy multiple file, optionally replacing any existing
  files if older, SubDirs means all lower level directories will be copied, EmptyDirs means
  empty directories will be created, CopyType determines how files are selected for copying,
  FCTypeSingle just one, FCTypeMaskDir uses a mask in SrcFName like *.zip, FCTypeArchDir
  selects files by archive flag, FCTypeAllDir copies all files, FCTypeDates by date range
  between CopyLoDT and CopyHiDT.  DelDone means the source file will be deleted after
  being copied, DelOldTar means directories will be synchronised so target files no longer
  in the source directory will be deleted, IgnorePaths is a comma delimited list of paths
  that will skip being copied, Safe Copy means the file is copied with a temporary name and
  renamed upon completion to avoid losing an existing file upon failure.  Copy progress
  information is available in the onCopyEvent.  The SelCopyFiles method returns TaskResFail
  if the source or destination directories are not found with the error in the ReqResponse
  property, TaskResOKNew for successful copy, TaskResOKNone if the destination  files were
  the same and all copy was skipped, TaskResAbort if the method was aborted.  SelCopyFiles
  will copy entire volume loads of file, hundreds of thousands, memory permitting for the
  directories of source and target volumes, provided they are not locked open by the OS.
  The snippet will copy all files downloaded into the default temporary directory DirTemp.Text
  into the sub-directory copiedfile, excluding files in that directory.
  The full file transfer sample is \sslinternet\OverbyteIcsXferTst.dpr.
}

procedure TSnippets.doFileCopyMultiFilesClick(Sender: TObject);
var
    FileCopyClient: TIcsFileCopy ;
    taskres: TIcsTaskResult ;
    mypathsource, mypathtarget, myfilemask: String;
    myfilereplace: TIcsFileCopyRepl;
begin

// parameters for the multiple file copy operation
    mypathsource := IncludeTrailingPathDelimiter(DirTemp.Text);
    mypathtarget := IncludeTrailingPathDelimiter(DirTemp.Text) + 'copiedfiles\';
    myfilemask := '*.*';
    myfilereplace := FCReplAlways;     // or FCReplAlways, FCReplNewer

// create component and events to see progress
    FileCopyClient := TIcsFileCopy.Create (self) ;
    FileCopyClient.CopyEvent := onXferEvent ;
    doFileCopyMultiFiles.Enabled := false ;
    AbortFlag := false ;
    LabelProgress.Caption := '' ;
    try
        try
            FileCopyClient.MultiDir := false ; // true copy multiple specific directories
            FileCopyClient.SrcDir := mypathsource ;
            FileCopyClient.SrcFName := myfilemask ;
            FileCopyClient.TarDir := mypathtarget ;
            FileCopyClient.IgnorePaths := mypathtarget ;   // ignore copying from our target path
        // CopyType: FCTypeSingle, FCTypeMaskDir, FCTypeArchDir, FCTypeAllDir, FCTypeDates
            FileCopyClient.CopyType := FCTypeMaskDir ;
            FileCopyClient.SubDirs := True ;   // true copy all sub directories
            FileCopyClient.EmptyDirs := False ; // true copy empty directories
            FileCopyClient.DelDone := false ;  // true delete source file after copy
            FileCopyClient.DelOldTar := false ; // true delete target files not in source directories
            FileCopyClient.Repl := myfilereplace ;
            FileCopyClient.ReplRO := true ;    // true, replace read only files
            FileCopyClient.Safe := false ;     // true, copy file with TMP extension, rename when done
            FileCopyClient.IgnoreFileExt := 'tmp;ftp' ;
            taskres := FileCopyClient.SelCopyFiles (FtpOnlyCheck.Checked) ;   // main file copy function
            AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
            AddLogText (FileCopyClient.ReqResponse) ;
            if FtpOnlyCheck.Checked then
                AddLogText ('Skipped Download on Request, untick Only check box');
        except
            AddLogText ('Copying Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (FileCopyClient) ;
        LabelProgress.Caption := 'File Copy Completed' ;
        doFileCopyMultiFiles.Enabled := true ;
    end ;
end;


{ Snippet: FTP View Directories - print a remote directory listing from an
  FTP site, optionally including all sub-directories.  This function is used
  by most of the FTP methods to select files to download or uploads, comparing
  with local files so they can be replaced if older or xfers skipped.
  FtpType is FtpTypeNone for no SSl/TLS with port 21, FtpTypeAuthSslBoth to use
  SSL/TLS with the AUTH command with port 21 or FtpTypeConnSslBoth to force
  SSL/TLS with port 990. The DispFtpDir method returns TaskResFail if the FTP
  site can not be accessed with the error in the ReqResponse property, and
  TaskResOKNew for directory listing.
  The snippet will access the FTP site ics.ftptest.org which uses the ICS FTP server
  with anonymous authentication and list all the files on the site.
  The full file transfer sample is \sslinternet\OverbyteIcsXferTst.dpr.
}

procedure TSnippets.doFtpViewDirsClick(Sender: TObject);
var
    FtpMultiClient: TIcsFtpMulti ;
    taskres: TIcsTaskResult ;
    myftppath: String;
    myftphost, myftpusername, myftppassword: String;
    myftptype: TFtpType;
    dirlisting: String;
begin

// parameters for the FTP directory listing operation
    myftppath := '/' ;        // FTP server path for directories
    myftphost := 'ics.ftptest.org' ;   // supports IPv4 and IPv6
    myftpusername := 'anonymous' ;     // no uploads
    myftppassword := 'icssnippets' ;
    myftptype := FtpTypeAuthSslBoth;  // or FtpTypeNone, FtpTypeConnSslBoth  (no SSL or only SSL on port 990)

// create component and events to see progress
    FtpMultiClient := TIcsFtpMulti.Create (self) ;
    FtpMultiClient.CopyEvent := onXferEvent ;
    doFtpViewDirs.Enabled := false ;
    AbortFlag := false ;
    LabelProgress.Caption := '' ;
    try
        try
         // essential FTP parameters
            FtpMultiClient.SocketFamily := sfIPv4;  // or sfIPv6 or sfAny
            FtpMultiClient.HostName1 := myftphost ;
            FtpMultiClient.FtpType := myftptype ;
            FtpMultiClient.UserName := myftpusername ;
            FtpMultiClient.PassWord := myftppassword ;
            FtpMultiClient.MaxAttempts := 2 ;  // logon attempts, may try IPv6 then IPv4
            FtpMultiClient.FailRepeat := 2 ;   // retries for failed xfers
            FtpMultiClient.PassiveX := True ;  // must be after connection type
            FtpMultiClient.FtpSslVerMethod := ftpSslVerBundle;  // or ftpSslVerNone to skip checking certificates
            FtpMultiClient.FtpSslReportChain := False;  // true to list SSL certificates
            FtpMultiClient.SrcDir := myftppath ;
            FtpMultiClient.SubDirs := True ;    // true list all sub-directories
            FtpMultiClient.BulkMode := BulkModeDownload ;   // required

          // connect, login, get features, get directories, log-off, single method
            taskres := FtpMultiClient.DispFtpDir (dirlisting) ;
            AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
            AddLogText (dirlisting) ;
        except
            AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (FtpMultiClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        doFtpViewDirs.Enabled := true ;
    end ;
end;


{ Snippet: FTP Download One File - download a single file from an FTP site, although
  extra single files can easily be added, optionally replacing an existing file if
  older.  The FtpDownOneFile method will change to the correct directory automatically.
  FtpType is FtpTypeNone for no SSl/TLS with port 21, FtpTypeAuthSslBoth to use
  SSL/TLS with the AUTH command with port 21 or FtpTypeConnSslBoth to force
  SSL/TLS with port 990. The FtpLogon and FtpDownOneFile methods returns TaskResFail
  if the FTP site can not be accessed with the error in the ReqResponse property,
  TaskResOKNew for successful download, or TaskResOKNone if the download is the
  same as local and skipped.
  The snippet will access the FTP site ics.ftptest.org which uses the ICS FTP server
  with anonymous authentication and download the speed50meg.zip  file to DirTemp.Text.
  The full file transfer sample is \sslinternet\OverbyteIcsXferTst.dpr.
}

procedure TSnippets.doFtpDownOneFileClick(Sender: TObject);
var
    FtpMultiClient: TIcsFtpMulti ;
    taskres: TIcsTaskResult ;
    myftppath, myftpfile, myfiletarget: String;
    myftphost, myftpusername, myftppassword: String;
    myftptype: TFtpType;
    myfilereplace: TIcsFileCopyRepl;
begin

// parameters for the single FTP download operation
    myftppath := '/testing' ;        // FTP server path for file
    myftpfile := 'speed50meg.zip';   // FTP file to download
    myfiletarget := IncludeTrailingPathDelimiter(DirTemp.Text) + myftpfile;  // where we download to
    myftphost := 'ics.ftptest.org' ;   // supports IPv4 and IPv6
    myftpusername := 'anonymous' ;     // no uploads
    myftppassword := 'icssnippets' ;
    myftptype := FtpTypeAuthSslBoth;  // or FtpTypeNone, FtpTypeConnSslBoth  (no SSL or only SSL on port 990)
    myfilereplace := FCReplAlways;    // or FCReplNever, FCReplNewer

// create component and events to see progress
    FtpMultiClient := TIcsFtpMulti.Create (self) ;
    FtpMultiClient.CopyEvent := onXferEvent ;
    doFtpDownOneFile.Enabled := false ;
    AbortFlag := false ;
    LabelProgress.Caption := '' ;
    try
        try
         // essential FTP parameters
            FtpMultiClient.SocketFamily := sfIPv4;  // or sfIPv6 or sfAny
            FtpMultiClient.HostName1 := myftphost ;
            FtpMultiClient.FtpType := myftptype ;
            FtpMultiClient.UserName := myftpusername ;
            FtpMultiClient.PassWord := myftppassword ;
            FtpMultiClient.MaxAttempts := 2 ;  // logon attempts, may try IPv6 then IPv4
            FtpMultiClient.FailRepeat := 2 ;   // retries for failed xfers
            FtpMultiClient.PassiveX := True ;  // must be after connection type
            FtpMultiClient.FtpSslVerMethod := ftpSslVerBundle;  // or ftpSslVerNone to skip checking certificates
            FtpMultiClient.FtpSslReportChain := False;  // true to list SSL certificates
            FtpMultiClient.SrcDir := '/' ;   // required
            FtpMultiClient.BulkMode := BulkModeDownload ;   // required

          // connect, login, get features
            taskres := FtpMultiClient.FtpLogon ;
            if taskres = TaskResOKNew then
            begin
                taskres := FtpMultiClient.FtpDownOneFile (myftppath, myftpfile, myfiletarget, myfilereplace) ;
                 // could do more single downloads now
            end ;
            AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
            AddLogText (FtpMultiClient.ReqResponse) ;
        except
            AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FtpMultiClient.FtpLogoff ;
        FreeAndNil (FtpMultiClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        doFtpDownOneFile.Enabled := true ;
    end ;
end;


{ Snippet: FTP Download Multiple Files - downloads multiple files from an FTP site,
  optionally including all sub-directories, optionally replacing an existing file if
  older.  The FtpDownload method does everything, logs onto the FTP site, gets remote
  and local directory listings, compares them for files that need to be downloaded,
  then does that, changing directories automatically. It will potentially download
  thousands of files from dozens of directories.. CopyType determines how files are
  selected for download, FCTypeSingle just one, FCTypeMaskDir uses a mask in SrcFName
  like *.zip, FCTypeAllDir downloads all files. FtpType is FtpTypeNone for no SSl/TLS
  with port 21, FtpTypeAuthSslBoth to use SSL/TLS with the AUTH command with port 21
  or FtpTypeConnSslBoth to force SSL/TLS with port 990. The FtpDownload method returns
  TaskResFail if the FTP site can not be accessed with the error in the ReqResponse
  property, TaskResOKNew for successful download, or TaskResOKNone if the dowbload
  is the same as local and skipped.
  The snippet will access the FTP site ics.ftptest.org which uses the ICS FTP server
  with anonymous authentication and download all the files matching the mask *.htm in
  the test directory to DirTemp.Text.
  The full file transfer sample is \sslinternet\OverbyteIcsXferTst.dpr.
}

procedure TSnippets.doFtpDownMultiFilesClick(Sender: TObject);
var
    FtpMultiClient: TIcsFtpMulti ;
    taskres: TIcsTaskResult ;
    myftppath, myftpfile, myfiletarget: String;
    myftphost, myftpusername, myftppassword: String;
    myftptype: TFtpType;
    myfilereplace: TIcsFileCopyRepl;
begin

// parameters for the multiple FTP download operation, selected files in a single directory, could be subdirs or all files
    myftppath := '/test' ;        // FTP server path for file
    myftpfile := '*.htm';         // partial FTP files to download
    myfiletarget := IncludeTrailingPathDelimiter(DirTemp.Text);  // where we download to
    myftphost := 'ics.ftptest.org' ;   // supports IPv4 and IPv6
    myftpusername := 'anonymous' ;     // no uploads
    myftppassword := 'icssnippets' ;
    myftptype := FtpTypeAuthSslBoth;  // or FtpTypeNone, FtpTypeConnSslBoth  (no SSL or only SSL on port 990)
    myfilereplace := FCReplAlways;    // or FCReplNever, FCReplNewer

// create component and events to see progress
    FtpMultiClient := TIcsFtpMulti.Create (self) ;
    FtpMultiClient.CopyEvent := onXferEvent ;
    doFtpDownMultiFiles.Enabled := false ;
    AbortFlag := false ;
    LabelProgress.Caption := '' ;
    try
        try
         // essential FTP parameters
            FtpMultiClient.SocketFamily := sfIPv4;  // or sfIPv6 or sfAny
            FtpMultiClient.HostName1 := myftphost ;
            FtpMultiClient.FtpType := myftptype ;
            FtpMultiClient.UserName := myftpusername ;
            FtpMultiClient.PassWord := myftppassword ;
            FtpMultiClient.MaxAttempts := 2 ;  // logon attempts, may try IPv6 then IPv4
            FtpMultiClient.FailRepeat := 2 ;   // retries for failed xfers
            FtpMultiClient.PassiveX := True ;  // must be after connection type
            FtpMultiClient.FtpSslVerMethod := ftpSslVerBundle;  // or ftpSslVerNone to skip checking certificates
            FtpMultiClient.FtpSslReportChain := False;  // true to list SSL certificates
            FtpMultiClient.SrcDir := myftppath ;
            FtpMultiClient.SrcFName := myftpfile ;
            FtpMultiClient.TarDir := myfiletarget ;
            FtpMultiClient.Repl := myfilereplace ;
        // CopyType: FCTypeSingle, FCTypeMaskDir, FCTypeAllDir
            FtpMultiClient.CopyType := FCTypeMaskDir ;
            FtpMultiClient.SubDirs := False ;    // true look in sub-directories
            FtpMultiClient.DispLDir := True;     // display local file directory
            FtpMultiClient.DispRDir := True;     // display remote FTP directory
            FtpMultiClient.BulkMode := BulkModeDownload ;   // required

          // connect, login, get features, list files, download files, logoff
            taskres := FtpMultiClient.FtpDownload(FtpOnlyCheck.Checked) ;   // main download function
            AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
            AddLogText (FtpMultiClient.ReqResponse) ;
            if FtpOnlyCheck.Checked then
                AddLogText ('Skipped Download on Request, untick Only check box');
        except
            AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (FtpMultiClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        doFtpDownMultiFiles.Enabled := true ;
    end ;
end;


{ Snippet: FTP Upload One File - upload a single file to an FTP site, although extra
  single files can easily be added, optionally replacing an existing file if older.
  The FtpUpOneFile method changes directory automatically. FtpType is FtpTypeNone
  for no SSl/TLS with port 21, FtpTypeAuthSslBoth to use SSL/TLS with the AUTH
  command with port 21 or FtpTypeConnSslBoth to force SSL/TLS with port 990. The
  FtpLogon and FtpUpOneFile methods returns TaskResFail if the FTP site can not
  be accessed with the error in the ReqResponse property, TaskResOKNew for
  successful upload, or TaskResOKNone if the upload is the same as remote and skipped.
  The snippet will access the FTP site ics.ftptest.org which uses the ICS FTP server,
  you must request a login by email from delphi@magsys.co.uk for uploads.  It will
  upload the speed50meg.zip file from DirTemp.Text (previously downloaded with the
  FTP Download One File snippet.
  The full file transfer sample is \sslinternet\OverbyteIcsXferTst.dpr.
}

procedure TSnippets.doFtpUpOneFileClick(Sender: TObject);
var
    FtpMultiClient: TIcsFtpMulti ;
    taskres: TIcsTaskResult ;
    myftppath, myftpfile, myfilesource: String;
    myftphost, myftpusername, myftppassword: String;
    myftptype: TFtpType;
    myfilereplace: TIcsFileCopyRepl;
begin

// parameters for the single FTP upload operation
    myftppath := '/ftptest23' ;        // FTP server path for file
    myftpfile := 'speed50meg.zip';   // FTP file to upload
    myfilesource := IncludeTrailingPathDelimiter(DirTemp.Text) + myftpfile;  // what we upload
    myftphost := 'ics.ftptest.org' ;   // supports IPv4 and IPv6
    myftpusername := 'ftptest23' ;
    myftppassword := 'xxxxx' ;
    myftptype := FtpTypeAuthSslBoth;  // or FtpTypeNone, FtpTypeConnSslBoth  (no SSL or only SSL on port 990)
    myfilereplace := FCReplNever;    // or FCReplNever, FCReplNewer

// create component and events to see progress
    FtpMultiClient := TIcsFtpMulti.Create (self) ;
    FtpMultiClient.CopyEvent := onXferEvent ;
    doFtpUpOneFile.Enabled := false ;
    AbortFlag := false ;
    LabelProgress.Caption := '' ;
    try
        try
         // essential FTP parameters
            FtpMultiClient.SocketFamily := sfIPv4;  // or sfIPv6 or sfAny
            FtpMultiClient.HostName1 := myftphost ;
            FtpMultiClient.FtpType := myftptype ;
            FtpMultiClient.UserName := myftpusername ;
            FtpMultiClient.PassWord := myftppassword ;
            FtpMultiClient.MaxAttempts := 2 ;  // logon attempts, may try IPv6 then IPv4
            FtpMultiClient.FailRepeat := 2 ;   // retries for failed xfers
            FtpMultiClient.PassiveX := True ;  // must be after connection type
            FtpMultiClient.FtpSslVerMethod := ftpSslVerBundle;  // or ftpSslVerNone to skip checking certificates
            FtpMultiClient.FtpSslReportChain := False;  // true to list SSL certificates
            FtpMultiClient.TarDir := '/' ;   // required
            FtpMultiClient.BulkMode := BulkModeUpload ;   // required

          // connect, login, get features
            taskres := FtpMultiClient.FtpLogon ;
            if taskres = TaskResOKNew then
            begin
                taskres := FtpMultiClient.FtpUpOneFile (myfilesource, myftppath, myftpfile, myfilereplace) ;
                 // could do more tasks now
            end ;
            AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
            AddLogText (FtpMultiClient.ReqResponse) ;
        except
            AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FtpMultiClient.FtpLogoff ;
        FreeAndNil (FtpMultiClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        doFtpUpOneFile.Enabled := true ;
    end ;
end;


{ Snippet: FTP Upload Multiple Files - upload multiple files to an FTP site, optionally
  replacing an existing file if older.  The FtpUpload method does everything, logs
  onto the FTP site, gets remote and local directory listings, compares them for files
  that need to be uploaded, then does that, changing directories automatically. It will
  potentially upload thousands of files in dozens of directories. FtpType is FtpTypeNone
  for no SSl/TLS with port 21, FtpTypeAuthSslBoth to use SSL/TLS with the AUTH command
  with port 21 or FtpTypeConnSslBoth to force SSL/TLS with port 990.  The FtpUpload
  method returns TaskResFail if the FTP site can not be accessed with the error in the
  ReqResponse property, TaskResOKNew for successful upload, or TaskResOKNone if the
  upload is the same as remote and skipped.
  The snippet will access the FTP site ics.ftptest.org which uses the ICS FTP server,
  you must request a login by email from delphi@magsys.co.uk for uploads.  It will
  upload all files matching *.htm from DirTemp.Text (previously downloaded with the
  FTP Download Multiple Files snippet.
  The full file transfer sample is \sslinternet\OverbyteIcsXferTst.dpr.
}

procedure TSnippets.doFtpUpMultiFilesClick(Sender: TObject);
var
    FtpMultiClient: TIcsFtpMulti ;
    taskres: TIcsTaskResult ;
    myftppath, myftpfile, myfilesource: String;
    myftphost, myftpusername, myftppassword: String;
    myftptype: TFtpType;
    myfilereplace: TIcsFileCopyRepl;
begin

// parameters for the multiple FTP upload operation
    myftppath := '/ftptest23' ;        // FTP server path for files
    myftpfile := '*.htm';              // File mask to upload
    myfilesource := IncludeTrailingPathDelimiter(DirTemp.Text);  // what we upload
    myftphost := 'ics.ftptest.org' ;   // supports IPv4 and IPv6
    myftpusername := 'ftptest23' ;
    myftppassword := 'miami2Vice' ;
    myftptype := FtpTypeAuthSslBoth;  // or FtpTypeNone, FtpTypeConnSslBoth  (no SSL or only SSL on port 990)
    myfilereplace := FCReplNever;    // or FCReplNever, FCReplNewer

// create component and events to see progress
    FtpMultiClient := TIcsFtpMulti.Create (self) ;
    FtpMultiClient.CopyEvent := onXferEvent ;
    doFtpUpMultiFiles.Enabled := false ;
    AbortFlag := false ;
    LabelProgress.Caption := '' ;
    try
        try
         // essential FTP parameters
            FtpMultiClient.SocketFamily := sfIPv4;  // or sfIPv6 or sfAny
            FtpMultiClient.HostName1 := myftphost ;
            FtpMultiClient.FtpType := myftptype ;
            FtpMultiClient.UserName := myftpusername ;
            FtpMultiClient.PassWord := myftppassword ;
            FtpMultiClient.MaxAttempts := 2 ;  // logon attempts, may try IPv6 then IPv4
            FtpMultiClient.FailRepeat := 2 ;   // retries for failed xfers
            FtpMultiClient.PassiveX := True ;  // must be after connection type
            FtpMultiClient.FtpSslVerMethod := ftpSslVerBundle;  // or ftpSslVerNone to skip checking certificates
            FtpMultiClient.FtpSslReportChain := False;  // true to list SSL certificates
        // CopyType: FCTypeSingle, FCTypeMaskDir, FCTypeArchDir, FCTypeAllDir, FCTypeDates
            FtpMultiClient.CopyType := FCTypeMaskDir ;
            FtpMultiClient.SrcDir := myfilesource ;
            FtpMultiClient.SrcFName := myftpfile ;
            FtpMultiClient.Repl := myfilereplace ;
            FtpMultiClient.SubDirs := False ;    // true look in sub-directories
            FtpMultiClient.DispLDir := True;     // display local file directory
            FtpMultiClient.DispRDir := True;     // display remote FTP directory
            FtpMultiClient.TarDir := myftppath ;
            FtpMultiClient.BulkMode := BulkModeUpload ;   // required

          // connect, login, get features, list files, upload files, log-off
            taskres := FtpMultiClient.FtpUpload(FtpOnlyCheck.Checked) ;   // main upload function
            AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
            AddLogText (FtpMultiClient.ReqResponse) ;
            if FtpOnlyCheck.Checked then
                AddLogText ('Skipped Download on Request, untick Only check box');
        except
            AddLogText ('FTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (FtpMultiClient) ;
        LabelProgress.Caption := 'FTP Completed' ;
        doFtpUpMultiFiles.Enabled := true ;
    end ;
end;


{ Snippet: HTTP Download List of Files - downloads a list of files from a web site,
  optionally checking existing files if newer. Supports http and https.
  The Download method does everything, builds a remote listing of the files, and
  local directory listings, compares them for files that need to be downloaded, then
  does that. The Download method returns TaskResFail if the HTTP site can not be
  accessed  with the error in the ReqResponse property, TaskResOKNew for successful
  download, or TaskResOKNone if the download is the same as local files and skipped.
  The snippet downloads three files from the Magenta Systems HTML Test Pages using
  the ICS web server at: https://www.telecom-tariffs.co.uk/testing/ into DirTemp.Text.
  The full file transfer sample is \sslinternet\OverbyteIcsXferTst.dpr.
}

procedure TSnippets.doHttpDownListClick(Sender: TObject);
var
    HttpMultiClient: TIcsHttpMulti ;
    taskres: TIcsTaskResult ;
    myurllist: TStringList;
    mytardir: String;
    myfilereplace: TIcsFileCopyRepl;
begin

// parameters for the multiple HTTP download operation
    myurllist := TStringList.Create ;
    myurllist.Add('https://www.telecom-tariffs.co.uk/testing/unicodedemo.htm') ;
    myurllist.Add('https://www.telecom-tariffs.co.uk/testing/unicodedemo-mchars.htm') ;
    myurllist.Add('https://www.telecom-tariffs.co.uk/testing/speed20meg.zip');
    myurllist.Add('https://www.telecom-tariffs.co.uk/testing/speed50meg.zip');
    mytardir := IncludeTrailingPathDelimiter(DirTemp.Text);  // where to download
    myfilereplace := FCReplAlways ;  // or FCReplNever, FCReplNewer

// create component and events to see progress
    HttpMultiClient := TIcsHttpMulti.Create (self) ;
    HttpMultiClient.CopyEvent := onXferEvent ;
    doHttpDownList.Enabled := false ;
    AbortFlag := false ;
    LabelProgress.Caption := '' ;
    try
        try
         // essential HTTP parameters
            HttpMultiClient.SocketFamily := sfIPv4;       // or sfIPv6 or sfAny
            HttpMultiClient.URLList := myurllist.Text ;   // one or more source URLs separated by CRLF
            HttpMultiClient.SrcMask := '*.*' ;            // optional source file mask to restrict downloads
            HttpMultiClient.DownDir := mytardir ;         // directory for downloaded files
            HttpMultiClient.ParseHTML := False ;           // if true, parse HTML page for links to files
            HttpMultiClient.ParseLevels := 0 ;            // how many page levels down to parse, 0 only top page
            HttpMultiClient.Repl := myfilereplace ;
            HttpMultiClient.LogFiles := True ;            // log each file downloaded
            HttpMultiClient.LogProt := ShowDiags.Checked ;// log HTTP protocol
            HttpMultiClient.LogLDir := false ;            // log destination directory
            HttpMultiClient.LogRDir := true ;             // log created HTTP directory
            HttpMultiClient.HttpSslVerMethod := httpSslVerBundle;  // or httpSslVerNone to skip checking certificates
            HttpMultiClient.SslReportChain := False;      // true to list SSL certificates

        { V9.2 https://www.telecom-tariffs.co.uk/testing/ needs authentication }
            HttpMultiClient.ServerAuth := httpAuthBasic;    // if we need authentication
            HttpMultiClient.Username := 'test';
            HttpMultiClient.Password := 'password';

          // connect, login, get features, list files, upload files, log-off
            taskres := HttpMultiClient.Download(FtpOnlyCheck.Checked) ;   // main HTTP download function
            AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
            AddLogText (HttpMultiClient.ReqResponse) ;
             if FtpOnlyCheck.Checked then
                AddLogText ('Skipped Download on Request, untick Only check box');
       except
            AddLogText ('HTTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        myurllist.Free;
        FreeAndNil (HttpMultiClient) ;
        LabelProgress.Caption := 'HTTP Completed' ;
        doHttpDownList.Enabled := true ;
    end ;
end;


{ Snippet: HTTP Download Linked Files - downloads multiple files from a web site,
  by parsing HTML pages for links, comparing them against a file mask like *.zip
  and building a list.  If ParseLevel is 1 or higher, pages found will also be
  parsed for links, so the list of files found can increase rapidly.  Once the
  download list is complete, it is optionally checked against existing files to
  see if newer or can be skipped. Supports http and https. Note this parsing
  works very well for files automatically indexed by HTTP servers (if allowed),
  in particular CCTV cameras that index their memory cards for video recordings,
  allowing all the new recordings to be downloaded automatically.
  The Download method does everything, builds a remote listing of the files, and
  local directory listings, compares them for files that need to be downloaded,
  then does that.  The Download method returns TaskResFail if the HTTP site can
  not be accessed with the error in the ReqResponse property, TaskResOKNew for
  successful download, or TaskResOKNone if the download is the same as local files
  and skipped. The snippet parses the Magenta Systems HTML Test Pages using the
  ICS web server at: https://www.telecom-tariffs.co.uk/testing/ and should find
  three zip files that are downloaded into DirTemp.Text.
  The full file transfer sample is \sslinternet\OverbyteIcsXferTst.dpr.
}

procedure TSnippets.doHttpDownLinkedClick(Sender: TObject);
var
    HttpMultiClient: TIcsHttpMulti ;
    taskres: TIcsTaskResult ;
    myurl, mysrcmask, mytardir: String;
    myfilereplace: TIcsFileCopyRepl;
begin

// parameters for the multiple HTTP download operation
    myurl := 'https://test:password@www.telecom-tariffs.co.uk/testing/' ;
    mysrcmask := '*.zip';
    mytardir := IncludeTrailingPathDelimiter(DirTemp.Text);  // where to download
    myfilereplace := FCReplAlways ;  // or FCReplNever, FCReplNewer

// create component and events to see progress
    HttpMultiClient := TIcsHttpMulti.Create (self) ;
    HttpMultiClient.CopyEvent := onXferEvent ;
    doHttpDownLinked.Enabled := false ;
    AbortFlag := false ;
    LabelProgress.Caption := '' ;
    try
        try
         // essential HTTP parameters
            HttpMultiClient.SocketFamily := sfIPv4;      // or sfIPv6 or sfAny
            HttpMultiClient.URLList := myurl ;           // one or more source URLs separated by CRLF
            HttpMultiClient.SrcMask := mysrcmask ;       // optional source file mask to restrict downloads
            HttpMultiClient.DownDir := mytardir ;        // directory for downloaded files
            HttpMultiClient.KeepPath := False ;           // if true, use HTTP path for subdirs
            HttpMultiClient.KeepHost := False ;           // if true, use HTTP host for subdir
            HttpMultiClient.ParseHTML := True ;           // if true, parse HTML page for links to files
            HttpMultiClient.ParseLevels := 1 ;            // how many page levels down to parse, 0 only top page
            HttpMultiClient.Repl := myfilereplace ;
            HttpMultiClient.LogFiles := True ;            // log each file downloaded
            HttpMultiClient.LogProt := ShowDiags.Checked ;// log HTTP protocol
            HttpMultiClient.LogLDir := false ;            // log destination directory
            HttpMultiClient.LogRDir := true ;             // log created HTTP directory
            HttpMultiClient.HttpSslVerMethod := httpSslVerBundle;  // or httpSslVerNone to skip checking certificates
            HttpMultiClient.SslReportChain := False;      // true to list SSL certificates

        { V9.2 https://www.telecom-tariffs.co.uk/testing/ needs authentication }
        { but user name and password are added to the URL followed by @, see above }
        //    HttpMultiClient.ServerAuth := httpAuthBasic;    // if we need authentication
        //    HttpMultiClient.Username := 'test';
        //    HttpMultiClient.Password := 'password';

          // connect, login, get features, list files, upload files, log-off
            taskres := HttpMultiClient.Download(FtpOnlyCheck.Checked) ;   // main HTTP download function
            AddLogText ('Task Result: ' + IcsGetTaskResName (taskres)) ;
            AddLogText (HttpMultiClient.ReqResponse) ;
            if FtpOnlyCheck.Checked then
                AddLogText ('Skipped Download on Request, untick Only check box');
        except
            AddLogText ('HTTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (HttpMultiClient) ;
        LabelProgress.Caption := 'HTTP Completed' ;
        doHttpDownLinked.Enabled := true ;
    end ;
end;

{ progress event for TSslHttpRest component }

procedure TSnippets.onSslHttpRestProg(Sender: TObject; LogOption: TLogOption; const Msg: string);
begin
    if LogOption = loProgress then
        LabelProgress.Caption := Msg
    else
        AddLogText(Msg);
end;

{ Snippet: HTTP REST Json Request - makes an HTTP GET request to a REST server passing a
  UrlEncoded parameter and receiving a Json response which is parsed to extract specific
  data.  Uses the TSslHttpRest component in sync mode, but could be async with the result
  in the event OnRestRequestDone.  Returns an HTTP status code, 200 for OK, 404 page not
  found, etc. RestParams will build Json, XML, UrlEncoded or comma list parameters, from
  strings, integers and TDateTime or you can build them yourself and pass them raw.
  On success, ResponseRaw has a unicode string converted using IcsHtmlToStr according
  to the content, including entities like &pound; and &#9741; to unicode symbols.  If
  Json content is returned, ResponseJson is a SuperObject that may be parsed.  The
  TSslHttpRest descends from THttpCli, and publishes all it's properties and events and
  bundles extra components including SSL configuration and certificate validation with a
  root bundle, SSL session caching, content compression, content code page decoding,
  persistent cookies, Json handling, logging and client SSL certificate. 
  The snippet accesses https://jsonplaceholder.typicode.com which provides several
  datasets on which queries can be made.
  The full HTTP REST sample is \sslinternet\OverbyteIcsHttpRestTst.dpr.
}

procedure TSnippets.doHttpRestReqClick(Sender: TObject);
var
   SslHttpRest: TSslHttpRest;
   StatCode: Integer;
   myurl, myusername: String;
begin

// parameters for the HTTP Rest request
    myurl := 'https://jsonplaceholder.typicode.com/users' ;
    myusername := 'Antonette';
//    myusername := 'Samantha';

// create component and events to see progress
    SslHttpRest := TSslHttpRest.Create (self) ;
    SslHttpRest.OnHttpRestProg := onSslHttpRestProg ;
    doHttpRestReq.Enabled := false ;
    LabelProgress.Caption := '' ;
    try
        try
         // essential and common HTTP parameters
            SslHttpRest.SocketFamily := sfAnyIPv4;  { IP4 and/or IPV6 }
            SslHttpRest.DebugLevel := DebugHdr;  // DebugNone, DebugConn, DebugParams, DebugSSL, DebugHdr, DebugBody, each logs more
            SslHttpRest.CertVerMethod := CertVerBundle;     // or CertVerNone if you don't care
            SslHttpRest.SslCliSecurity := sslCliSecDefault;
//          SslHttpRest.SslReportChain := True;
//          SslHttpRest.SslRevocation := True;
//          SslHttpRest.ServerAuth := httpAuthNone;    // if we need authentication
//          SslHttpRest.Username := '';
//          SslHttpRest.Password := '';
//          SslHttpRest.AuthBearerToken := '';
//          SslHttpRest.HttpUploadStrat := HttpUploadNone;

         // built REST parameters and make request
//          SslHttpRest.ShowProgress := True;
//          SslHttpRest.HttpMemStrategy := HttpStratMem;

        // TRestParams will build Json, XML, UrlEncoded or comma list parameters, from strings, integers and TDateTime.
            SslHttpRest.RestParams.AddItem('username', myusername);   // add string parameter
            SslHttpRest.RestParams.PContent := PContUrlEncoded;       // lots of PContxx options
            StatCode := SslHttpRest.RestRequest(httpGET, myurl, False, '');  // sync request, no extra parameters
            AddLogText ('HTTP Rest Request Response: ' + IntToStr(StatCode)) ;
            if StatCode = 200 then begin
                AddLogText (SslHttpRest.ResponseRaw);                  // anything
             //   AddLogText (SslHttpRest.ResponseJson.AsString);        // returns SuperObject, if Json response
                AddLogText ('Address: ' + SslHttpRest.ResponseJson.AsArray[0].S['address']);    // one element only from array
            end;
        except
            AddLogText ('HTTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (SslHttpRest) ;
        LabelProgress.Caption := 'HTTP Completed' ;
        doHttpRestReq.Enabled := true ;
    end ;
end;


{ Snippet: HTTP REST Download - makes an HTTP GET request to download a file, with
  optional resume of partial download.  Uses the TSslHttpRest component in sync mode
  so could set RestParams if needed, saves the file with a specified path and name.
  Returns an HTTP status code, 200 for OK, 404 file not found, etc. 
  The snippet accesses https://www.telecom-tariffs.co.uk which uses the ICS web server
  and downloads a 300MB file.
  The full HTTP REST sample is \sslinternet\OverbyteIcsHttpRestTst.dpr.
}

procedure TSnippets.doHttpRestDownClick(Sender: TObject);
var
   SslHttpRest: TSslHttpRest;
   StatCode: Integer;
   myfile, myurl, mytarfile: String;
begin

// parameters for the HTTP Rest request
    myfile := 'speed300meg.zip' ;
    myurl := 'https://www.telecom-tariffs.co.uk/testing/' + myfile;
    mytarfile := IncludeTrailingPathDelimiter(DirTemp.Text) + myfile;  // where to download

// create component and events to see progress
    SslHttpRest := TSslHttpRest.Create (self) ;
    SslHttpRest.OnHttpRestProg := onSslHttpRestProg ;
    doHttpRestDown.Enabled := false ;
    LabelProgress.Caption := '' ;
    try
        try
         // essential and common HTTP parameters
            SslHttpRest.SocketFamily := sfAnyIPv4;  { IP4 and/or IPV6 }
            SslHttpRest.DebugLevel := DebugHdr;
            SslHttpRest.CertVerMethod := CertVerBundle;
            SslHttpRest.SslCliSecurity := sslCliSecDefault;
//          SslHttpRest.SslReportChain := True;
//          SslHttpRest.SslRevocation := True;
//          SslHttpRest.HttpUploadStrat := HttpUploadNone;

        { V9.2 https://www.telecom-tariffs.co.uk/testing/ needs authentication }
            SslHttpRest.ServerAuth := httpAuthBasic;    // if we need authentication
            SslHttpRest.Username := 'test';
            SslHttpRest.Password := 'password';
//          SslHttpRest.AuthBearerToken := '';

         // built REST parameters and make request
            SslHttpRest.ShowProgress := True;
            SslHttpRest.HttpMemStrategy := HttpStratFile;     // also HttpStratResume for auto resumed large downloads
            SslHttpRest.HttpDownFileName := mytarfile;
            SslHttpRest.HttpDownReplace := True;
            StatCode := SslHttpRest.RestRequest(httpGET, myurl, False, '');  // sync request, no extra parameters
            AddLogText ('HTTP Rest Request Response: ' + IntToStr(StatCode)) ;
            if StatCode = 200 then begin
                AddLogText ('File Download OK, Size ' + IntToKbyte(IcsGetFileSize(mytarfile)));
            end;
        except
            AddLogText ('HTTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (SslHttpRest) ;
        LabelProgress.Caption := 'HTTP Completed' ;
        doHttpRestDown.Enabled := true ;
    end ;
end;


{ Snippet: HTTP Simple Upload File - makes a HTTP POST request to upload a file
  to a special upload web page. Uses the TSslHttpRest component in sync mode, uses
  HttpUploadStrat of HttpUploadSimple which causes the entire file to be POSTed as
  binary content to the server, with some parameters to tell the server what to
  do with the file.

  This snippet is designed to upload to the ICS web application server samples,
  and a public web site using the same code, the uploadfile.htm  page expects
  FileTitle and FileName parameters in the URL and will reject the upload
  otherwise.  The snippet adds duplicate Json parameters as an example of
  nesting parameters of different types.  Returns an HTTP status code, 200 for OK,
  404 page not found, etc. Apart from 200, there is no indication the upload
  completed OK, the returned web page might say something, uploadfile.htm reports
  the file size received.

  This upload technique will manage very large files, tested to 8 Gbyte, provided
  the server accepts that size, the ICS web server defaults to a maximum of 200MB.

  The full HTTP REST sample is \sslinternet\OverbyteIcsHttpRestTst.dpr.
}

procedure TSnippets.doHttpSimpleUploadClick(Sender: TObject);
var
   SslHttpRest: TSslHttpRest;
   MyJsonParams: TRestParams;
   StatCode: Integer;
   myfile, mytitle, myurl, mysrcfile: String;
begin

// parameters for the HTTP Rest request
    myfile := 'speed50meg.zip' ;
    mytitle := 'A title for the uploaded file';
    myurl := 'https://www.telecom-tariffs.co.uk/testing/uploadfile.htm';
    mysrcfile := IncludeTrailingPathDelimiter(DirTemp.Text) + myfile;  // what to upload
    if NOT FileExists(mysrcfile) then begin
        AddLogText ('Use HTTP Download List of Files first to get upload file - ' + myfile) ;
        Exit;
    end;

// create component and events to see progress
    SslHttpRest := TSslHttpRest.Create (self) ;
    SslHttpRest.OnHttpRestProg := onSslHttpRestProg ;
    MyJsonParams := TRestParams.Create(self);
    doHttpSimpleUpload.Enabled := false ;
    LabelProgress.Caption := '' ;
    try
        try
         // essential and common HTTP parameters
            SslHttpRest.SocketFamily := sfAnyIPv4;  { IP4 and/or IPV6 }
            SslHttpRest.DebugLevel := DebugHdr;
            SslHttpRest.CertVerMethod := CertVerBundle;
            SslHttpRest.SslCliSecurity := sslCliSecDefault;
//          SslHttpRest.SslReportChain := True;
//          SslHttpRest.SslRevocation := True;

        { V9.2 https://www.telecom-tariffs.co.uk/testing/ needs authentication }
            SslHttpRest.ServerAuth := httpAuthBasic;    // if we need authentication
            SslHttpRest.Username := 'test';
            SslHttpRest.Password := 'password';
//          SslHttpRest.AuthBearerToken := '';

        // build some simple Json parameters, that will be nested into the URL parameters
            MyJsonParams.PContent := PContJson;
            MyJsonParams.AddItem('FileTitle', mytitle);
            MyJsonParams.AddItem('FileName', myfile);

        // build main parameters that will be encoded and added after the URL
            SslHttpRest.RestParams.PContent := PContUrlEncoded;
            SslHttpRest.RestParams.AddItem('FileTitle', mytitle);
            SslHttpRest.RestParams.AddItemA('JsonBlock', MyJsonParams.GetParameters, true);  // true means raw data, no escaping
            SslHttpRest.RestParams.AddItem('FileName', myfile);    // this is required by the uploadfile.htm page
         //   SslHttpRest.RestParams.AddItemSO('JsonBlock', ISuperObject);  // alternate if using SuperObject to build Json

         // make simple POST upload request with file name as binary content
            SslHttpRest.ShowProgress := True;
            SslHttpRest.HttpMemStrategy := HttpStratMem;
            SslHttpRest.HttpUploadStrat := HttpUploadSimple;  // POST content is binary file
            SslHttpRest.HttpUploadFile := mysrcfile;
            StatCode := SslHttpRest.RestRequest(httpPOST, myurl, False, '');  // sync request, use RestParams parameters
            AddLogText ('HTTP Rest Request Response: ' + IntToStr(StatCode)) ;
            if StatCode = 200 then begin
                AddLogText ('File Upload OK');
            end;
    //        AddLogText (SslHttpRest.ResponseRaw);                  // web page might say done okay
        except
            AddLogText ('HTTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (MyJsonParams) ;
        FreeAndNil (SslHttpRest) ;
        LabelProgress.Caption := 'HTTP Completed' ;
        doHttpSimpleUpload.Enabled := true ;
    end ;
end;

{ Snippet: HTTP Form Upload File - makes a HTTP POST request to upload a file
  to a special upload web page. Uses the TSslHttpRest component in sync mode, using
  MIME multipart/form-data parameters similar to uploading from a web page form
  using a browser.

  This snippet is designed to upload to the ICS web application server samples,
  and a public web site using the same code, the uploadfile.htm page expects
  FileTitle and FileName parameters in the form-data and will reject the
  upload otherwise.  Specifying binary upload file using AddItemFile with the
  full disk file name and an optional file size which will be checked if left
  as 0.  The snippet adds duplicate Json parameters as an example of
  nesting parameters of different types.  Returns an HTTP status code, 200 for OK,
  404 page not found, etc. Apart from 200, there is no indication the upload
  completed OK, the returned web page might say something, uploadfile.htm reports
  the file size received.  Multiple files may be added, with different names.

  This upload technique will manage very large files, tested to 8 Gbyte, provided
  the server accepts that size, the ICS web server defaults to a maximum of 200MB.
  Beware preparing the Form-Data content involves copying the binary file which
  may take a minute or more for gigabyte files, and likewise the server also
  has to copy the file from the form stream which might delay the upload response
  by a minute or more, causing sync requests to timeout, Simple Upload avoids
  this copying and is better for very large files.

  The full HTTP REST sample is \sslinternet\OverbyteIcsHttpRestTst.dpr.
}

procedure TSnippets.doHttpFormUploadClick(Sender: TObject);
var
   SslHttpRest: TSslHttpRest;
   MyJsonParams: TRestParams;
   StatCode: Integer;
   myfile, mytitle, myurl, mysrcfile: String;
begin

// parameters for the HTTP Rest request
    myfile := 'speed50meg.zip' ;
    mytitle := 'A title for the uploaded file';
    myurl := 'https://www.telecom-tariffs.co.uk/testing/uploadfile.htm';
    mysrcfile := IncludeTrailingPathDelimiter(DirTemp.Text) + myfile;  // what to upload
    if NOT FileExists(mysrcfile) then begin
        AddLogText ('Use HTTP Download List of Files first to get upload file - ' + myfile) ;
        Exit;
    end;

// create component and events to see progress
    SslHttpRest := TSslHttpRest.Create (self) ;
    SslHttpRest.OnHttpRestProg := onSslHttpRestProg ;
    MyJsonParams := TRestParams.Create(self);
    doHttpFormUpload.Enabled := false ;
    LabelProgress.Caption := '' ;
    try
        try
         // essential and common HTTP parameters
            SslHttpRest.SocketFamily := sfAnyIPv4;  { IP4 and/or IPV6 }
            SslHttpRest.DebugLevel := DebugHdr;
            SslHttpRest.CertVerMethod := CertVerBundle;
            SslHttpRest.SslCliSecurity := sslCliSecDefault;
//          SslHttpRest.SslReportChain := True;
//          SslHttpRest.SslRevocation := True;

        { V9.2 https://www.telecom-tariffs.co.uk/testing/ needs authentication }
            SslHttpRest.ServerAuth := httpAuthBasic;    // if we need authentication
            SslHttpRest.Username := 'test';
            SslHttpRest.Password := 'password';
//          SslHttpRest.AuthBearerToken := '';

        // build some simple Json parameters, that will be nested added to the Form-Data parameters
            MyJsonParams.PContent := PContJson;
            MyJsonParams.AddItem('FileTitle', mytitle);
            MyJsonParams.AddItem('FileName', myfile);

        // build main parameters that will be built into MIME multipart/form-data parameters as the POST content
            SslHttpRest.RestParams.PContent := PContFormData;
            SslHttpRest.RestParams.AddItem('FileTitle', mytitle);
            SslHttpRest.RestParams.AddItemA('JsonBlock', MyJsonParams.GetParameters, true);  // true means raw data, no escaping
         //   SslHttpRest.RestParams.AddItemSO('JsonBlock', ISuperObject);  // alternate if using SuperObject to build Json
            SslHttpRest.RestParams.AddItemFile('FileName', mysrcfile, 0);    // the binary file we will upoload, optional file size
        //  SslHttpRest.RestParams.AddItemFile('FileName2', mysrcfile2, 0);  // another file
            SslHttpRest.RestParams.AddItem('Submit', 'SubmitFile');

         // make simple POST request with RestParams containing Form-Data and the binary file
            SslHttpRest.ShowProgress := True;
            SslHttpRest.HttpMemStrategy := HttpStratMem;
            SslHttpRest.HttpUploadStrat := HttpUploadNone;  // file is part of the RestParams
            StatCode := SslHttpRest.RestRequest(httpPOST, myurl, False, '');  // sync request, use RestParams parameters
            AddLogText ('HTTP Rest Request Response: ' + IntToStr(StatCode)) ;
            if StatCode = 200 then begin
                AddLogText ('File Upload OK');
            end;
    //        AddLogText (SslHttpRest.ResponseRaw);                  // web page might say done okay
        except
            AddLogText ('HTTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (MyJsonParams) ;
        FreeAndNil (SslHttpRest) ;
        LabelProgress.Caption := 'HTTP Completed' ;
        doHttpFormUpload.Enabled := true ;
    end ;
end;


{ the next four event handlers are for the TIcsIpStrmLog IP Stream Logging components }

procedure TSnippets.onCliLogRecvEvent(Sender: TObject; Socnr: integer; const Line: string);
begin
    AddLogText('IP Log Client: Received Data - ' + Line);
end;


procedure TSnippets.onCliLogProgEvent(Sender: TObject; Socnr: integer; LogOption: TLogOption; const Msg: string);
begin
    AddLogText('IP Log Client: ' + Msg);
end;


procedure TSnippets.onSrvLogRecvEvent(Sender: TObject; Socnr: integer; const Line: string);
begin
    AddLogText('IP Log Server: Received Data - ' + Line);
end;


procedure TSnippets.onSrvLogProgEvent(Sender: TObject; Socnr: integer; LogOption: TLogOption; const Msg: string);
begin
    AddLogText('IP Log Server: ' + Msg);
end;


{ Snippet: Local Socket Traffic - Send simple text traffic between two sockets
  on the same PC, using client server concepts.  Uses the TIcsIpStrmLog IP
  Streaming Log component which is a high level version of the basic TSslWSocket
  and TSslWSocketServer components combined together and configured for TCP Client,
  TCP Server, UDP Client or UDP Server protocols. The component supports multiple
  client sockets so may be used to send data to two or more different remote
  servers at the same time.  For TCP and UDP clients, the component will optionally
  ping the remote computer first before opening an IP connection to allow faster
  failure retries and some confirmation that UDP may work.  TCP client provides
  repeated connection retry attempts, including re-establishing a lost connection.
  UDP client will optionally keep pinging the remote during a connection to ensure
  it's still there. UDP server sends data to the IP address and port from which
  it last received data. TCP server supports multiple remote clients connecting.
  Received data is parsed for various line endings optionally removing control
  characters and triggering an event for a received line.  For efficiency,
  TIcsIpStrmLog needs to send lines or packets of data with a known line ending
  that triggers the receive event, the default is a LF character for lines of
  text and sending can automatically add CRLF. The line end can be changed to
  an alternate character or can be fixed length binary packet. TIcsIpStrmLog only
  works in async mode, using three main events, RecvEvent for received data,
  ChangeEvent for connection and disconnection and ProgEvent for logging.
  TIcsIpStrmLog supports SSL/TLS if needed, TCP Client is automatic, TCP Server
  is more complicated since it requires an SSL server certificate and private
  key, which it can automatically order from Let's Encrypt for public servers.

  This snippet uses two TIcsIpStrmLog components configured as TCP Client and TCP
  Server, using the same IP address and port, so they can data between themselves,
  usually between two different applications.  Because TIcsIpStrmLoh is async, the
  snippet uses a message loop with a five minute timeout, with triggers causing
  client and server to each send a line of data every two seconds to simulate
  traffic.  Cicking the Abort button will stop it early.
  The full IP Streaming Log sample is \sslinternet\OverbyteIcsIpStmLogTst.dpr.
}

procedure TSnippets.doSocketLocalClick(Sender: TObject);
var
    IpLogClient: TIcsIpStrmLog;
    IpLogServer: TIcsIpStrmLog;
    myIpAddress, myIpPort, myCliData, mySrvData: String;
    TrgTimerEnd, TrgCliData, TrgSrvData: Int64;
begin
    myIpAddress := '127.0.0.1';  // loop back
    myIpPort := '34567';         // random
    myCliData := 'Test data from the IP Streaming Log client component';
    mySrvData := 'Test data from the IP Streaming Log server component';

    doSocketLocal.Enabled := False;
    try
       IpLogServer := TIcsIpStrmLog.Create(Self);
       IpLogClient := TIcsIpStrmLog.Create(Self);
        try
        // create IP Streaming Log local server component and start listening
            IpLogServer.onLogRecvEvent := onSrvLogRecvEvent;
            IpLogServer.onLogProgEvent := onSrvLogProgEvent;
            IpLogServer.LogProtocol := logprotTcpServer ;       // could be UDP
            IpLogServer.ForceSsl := False;
            IpLogServer.SrvTimeoutSecs := 60;
            IpLogServer.SrvIcsHosts.Clear;
            IpLogServer.SrvIcsHosts.Add;  // only need one host
            with IpLogServer.SrvIcsHosts [0] do
            begin
                HostEnabled := True;
                BindIpAddr := myIpAddress;
                BindNonPort := atoi(myIpPort);
                BindSslPort := 0;
                HostTag := 'LocalServer' ;
            end;
            AddLogText('IP Log Server: Starting on ' + myIpAddress + ':' + myIpPort);
            if IpLogServer.StartLogging then
                AddLogText('IP Log Server: Started OK')
            else begin
                AddLogText('IP Log Server: Failed to Start');
                Exit;
            end;

        // create IP Streaming Log client component and open connection to local server we just started
            IpLogClient.MaxSockets := 1;
            IpLogClient.onLogRecvEvent := onCliLogRecvEvent;
            IpLogClient.onLogProgEvent := onCliLogProgEvent;
            IpLogClient.LogProtocol := logprotTcpClient ;   // could be UDP
            IpLogClient.ForceSsl := False;
            IpLogClient.RemoteHost := myIpAddress;  // will also send to multiple hosts/ports, set with SetRemotes method (up to MaxSockets)
            IpLogClient.RemoteIpPort := myIpPort;
            IpLogClient.CheckPing := False;    // TCP connect failure usually waits 30 secs or more, ping can timeout in a few seconds
      // following show defaults so don't need to be set, mostly
      //    IpLogClient.PingWaitSecs := 5;     // how long CheckPing should wait
      //    IpLogClient.AutoReconnect := True; // should we reconnect if connection lost or fails
      //    IpLogClient.RetryAttempts := 0;   // how many connect retries, -1 none, 0 never stop
      //    IpLogClient.RetryWaitSecs := 10;   // how long to wait before retrying
      // following also apply to server mode which can send and receive data identically to the client
      //    IpLogClient.LineEndType := lineendLF;  // when recv event called, or lineendCR, lineendCustom, lineendPacket
      //    IpLogClient.MaxLineLen := 132;         // recv event called after this number without a lineend
      //    IpLogClient.RawData := False;         // are we receiving binary data so no procesing
      //    IpLogClient.UseUtf8 := True;          // should unicode strings to be converted to and from UTF8
      //    IpLogClient.StripControls := true ;   // should control characters like CR and LF be stripped on receive
      //    IpLogClient.AddCRLF := true ;         // should CRLF be added in SendLogLine
            IpLogClient.StartLogging;   // will attempt to connect async, event when done
            AddLogText('IP Log Client: Connecting to Server');

        // the TIcsIpStrmLog component is event driven, data is received in the onLogRecvEvent
        // and sent as needed by the SendLogLine method, but for this snippet we fake a
        // sync method looping for up to five minutes sending data every one or two seconds.

        // client now continues listening until stopped, so we wait and process events
            AddLogText ('Click Abort to stop Local IP Logging snippet');
            AbortFlag := False;
            TrgTimerEnd := IcsGetTrgMins64(5);   // stop after 5 minutes
            TrgCliData := IcsGetTrgSecs64(2);
            TrgSrvData := IcsGetTrgSecs64(1);
            while NOT AbortFlag do begin
                 Application.ProcessMessages;
                 if Application.Terminated then Break;
                 if IcsTestTrgTick64(TrgTimerEnd) then Break;

             // see if client is connected and send data to server
                 if IcsTestTrgTick64(TrgCliData) then begin
                    TrgCliData := IcsGetTrgSecs64(2);
                    if IpLogClient.AnyStateOK and (myCliData <> '') then begin
                        IpLogClient.SendLogLine(TimeToStr(Time) + ' - ' + myCliData);
                    end;
                 end;

             // see if server has any clients connected and send data back to them
                 if IcsTestTrgTick64(TrgSrvData) then begin
                    TrgSrvData := IcsGetTrgSecs64(2);
                    if (IpLogServer.CurSockets > 0) and (mySrvData <> '') then begin
                        IpLogServer.SendLogLine(TimeToStr(Time) + ' - ' + mySrvData);
                    end;
                 end;
            end;

        // stop both
            AddLogText ('Stopping Logging');
            IpLogClient.StopLogging;
            IpLogServer.StopLogging;
        except
            AddLogText ('IP Logging - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        if Assigned(IpLogClient) and IpLogClient.AnyStateOK then
            IpLogClient.StopLogging;
        if Assigned(IpLogServer) and IpLogServer.AnyStateOK then
            IpLogServer.StopLogging;
        FreeAndNil (IpLogServer) ;
        FreeAndNil (IpLogClient) ;
        LabelProgress.Caption := 'IP Logging Completed' ;
        doSocketLocal.Enabled := true ;
    end ;
end;


{ Snippet: Remote Socket Traffic - Receive simple text traffic from a remote TCP
  Server, optionally using SSL/TLS.  Receiving and sending logging streams to and
  from remote network appliances and applications is a common requirement, and
  the reason the component is called TIcsIpStrmLog - IP Stream Logging.
  Uses the TIcsIpStrmLog IP Stream Logging component which is a high level version
  of the basic TSslWSocket and TSslWSocketServer components combined together and
  configured for TCP Client, TCP Server, UDP Client or UDP Server protocols.

  This snippet uses a TIcsIpStrmLog component configured as TCP Client which
  accesses an application called ComGen on a public server, Comgen itself uses
  TIcsIpStrmLog in TCP Server mode to generate lines of text for testing
  purposes, see https://www.magsys.co.uk/comcap/.  The snippet can connect to
  non-SSL or SSL ports.  Because TIcsIpStrmLog is async, the snippet uses a
  message loop with a five minute timeout while traffic is being received.
  Cicking the Abort button will stop it early.
  The full IP Streaming Log sample is \sslinternet\OverbyteIcsIpStmLogTst.dpr.
}

procedure TSnippets.doSocketRemoteClick(Sender: TObject);
var
    IpLogClient: TIcsIpStrmLog;
    myIpAddress, myIpPort: String;
    TrgTimerEnd: Int64;
begin
    myIpAddress := 'comgen.ftptest.co.uk';
    myIpPort := '21501';         // uses SSL
 //   myIpPort := '21502';         // no SSL

    doSocketRemote.Enabled := False;
    try
       IpLogClient := TIcsIpStrmLog.Create(Self);
        try
        // create IP Streaming Log client component and open connection to local server we just started
            IpLogClient.MaxSockets := 1;
            IpLogClient.onLogRecvEvent := onCliLogRecvEvent;
            IpLogClient.onLogProgEvent := onCliLogProgEvent;
            IpLogClient.LogProtocol := logprotTcpClient ;   // could be UDP
            IpLogClient.ForceSsl := True;
            IpLogClient.RemoteHost := myIpAddress;  // will also send to multiple hosts/ports, set with SetRemotes method (up to MaxSockets)
            IpLogClient.RemoteIpPort := myIpPort;
            IpLogClient.CheckPing := False;    // TCP connect failure usually waits 30 secs or more, ping can timeout in a few seconds
            IpLogClient.LogSslVerMethod := logSslVerBundle;

      // following show defaults so don't need to be set, mostly
      //    IpLogClient.LogSslRevocation := False;
      //    IpLogClient.LogSslReportChain := False;
      //    IpLogClient.PingWaitSecs := 5;     // how long CheckPing should wait
      //    IpLogClient.AutoReconnect := True; // should we reconnect if connection lost or fails
      //    IpLogClient.RetryAttempts := 0;   // how many connect retries, -1 none, 0 never stop
      //    IpLogClient.RetryWaitSecs := 10;   // how long to wait before retrying
      //    IpLogClient.LineEndType := lineendLF;  // when recv event called, or lineendCR, lineendCustom, lineendPacket
      //    IpLogClient.MaxLineLen := 132;         // recv event called after this number without a lineend
      //    IpLogClient.RawData := False;         // are we receiving binary data so no procesing
      //    IpLogClient.UseUtf8 := True;          // should unicode strings to be converted to and from UTF8
      //    IpLogClient.StripControls := true ;   // should control characters like CR and LF be stripped on receive
      //    IpLogClient.AddCRLF := true ;         // should CRLF be added in SendLogLine
            IpLogClient.StartLogging;   // will attempt to connect async, event when done
            AddLogText('IP Log Client: Connecting to Server');

        // the TIcsIpStrmLog component is event driven, data is received in the onLogRecvEvent
        // but for this snippet we fake a sync method looping for up to five minutes

        // client now continues listening until stopped, so we wait and process events
            AddLogText ('Click Abort to stop Remote IP Logging snippet');
            AbortFlag := False;
            TrgTimerEnd := IcsGetTrgMins64(5);   // stop after 5 minutes
            while NOT AbortFlag do begin
                 Application.ProcessMessages;
                 if Application.Terminated then Break;
                 if IcsTestTrgTick64(TrgTimerEnd) then Break;
            end;

        // stop
            AddLogText ('Stopping Logging');
            IpLogClient.StopLogging;
        except
            AddLogText ('IP Logging - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        if Assigned(IpLogClient) and IpLogClient.AnyStateOK then
            IpLogClient.StopLogging;
        FreeAndNil (IpLogClient) ;
        LabelProgress.Caption := 'IP Logging Completed' ;
        doSocketRemote.Enabled := true ;
    end ;
end;


{ the next three event handlers are for the TSslWebSocketCli component }

procedure TSnippets.onSslWebSocketCliWSConnected(Sender: TObject);
begin
    with Sender as TSslWebSocketCli do begin
        if IsWSConnected then begin
            LabelProgress.Caption := 'Websocket Connected OK to: ' + URL;
        end
        else begin
            LabelProgress.Caption := 'Websocket Failed to Connect to: ' + URL;
            AbortFlag := True;   // break wait loop
        end;
    end;
end;

procedure TSnippets.onSslWebSocketCliWSDisconnected(Sender: TObject);
begin
    LabelProgress.Caption := 'Websocket Session Disconnected';
    AbortFlag := True;   // break wait loop
end;

procedure TSnippets.onSslWebSocketCliWSFrameRcvd(Sender: TSslWebSocketCli; const APacket: string; var AFrame: TWebSocketReceivedFrame);
begin
    if AFrame = Nil then
        Exit;
    with Sender as TSslWebSocketCli do begin
        LabelProgress.Caption := 'WebSocket Frames in/out: ' + IntToStr(WSFrameCounter);
        case AFrame.Kind of
            wsfkText: begin
                AddLogText (APacket);
            end;
        end;
    end;
end;

{ Snippet: WebSocket Client - Connect to a remote WebSocket server to send and receive
  data, optionally using SSL/TLS.
  WebSocket is a full duplex TCP protocol for web servers to support interactive web pages,
  typically dynamic updating such as chat sessions, spell checkers as you type, search
  hints, etc.  WebSocket extends the HTTP protocol and can be carried through HTTP proxies
  using the same ports as HTTP. The WebSocket protocol includes ping/pong keep alive so
  long lived connections are not dropped.
  The snippet uses the TSslWebSocketCli component to connect to
  wss://www.telecom-tariffs.co.uk/WebSocket/Serverinfo which is a WebSocket server
  supporting the https://www.telecom-tariffs.co.uk/serverinfow.htm page which is
  dynamically updated every 10 seconds, using the ICS WebSocket server component.  HTML
  data received by the WebSocket is normally plugged into the web page, but just displayed
  raw here.  Because TSslWebSocketCli is async, the snippet uses a message loop with a
  five minute timeout while traffic is being received. Cicking the Abort button will
  stop it early.
  The full WebSocket client sample is \sslinternet\OverbyteIcsHttpRestTst.dpr.
}

procedure TSnippets.doWebSocketClick(Sender: TObject);
var
    SslWebSocketCli: TSslWebSocketCli;
    myurl: String;
    TrgTimerEnd: Int64;
begin

// parameters for the WebSocket client request, this will stream HTML information every 10 seconds to update a web page
    myurl := 'wss://www.telecom-tariffs.co.uk/WebSocket/Serverinfo';

// create component and events to see progress
    SslWebSocketCli := TSslWebSocketCli.Create (self);
    SslWebSocketCli.OnHttpRestProg := onSslHttpRestProg;
    SslWebSocketCli.OnWSFrameRcvd := onSslWebSocketCliWSFrameRcvd;
    SslWebSocketCli.OnWSConnected := onSslWebSocketCliWSConnected;
    SslWebSocketCli.OnWSDisconnected := onSslWebSocketCliWSDisconnected;
    doWebSocket.Enabled := false ;
    LabelProgress.Caption := '' ;
    try
        try
         // essential and common HTTP parameters
            SslWebSocketCli.SocketFamily := sfAnyIPv4;  { IP4 and/or IPV6 }
            SslWebSocketCli.DebugLevel := DebugHdr;
            SslWebSocketCli.CertVerMethod := CertVerBundle;
            SslWebSocketCli.SslCliSecurity := sslCliSecDefault;
//          SslWebSocketCli.SslReportChain := True;
//          SslWebSocketCli.SslRevocation := True;
//          SslWebSocketCli.ServerAuth := httpAuthNone;    // if we need authentication
//          SslWebSocketCli.Username := '';
//          SslWebSocketCli.Password := '';
//          SslWebSocketCli.AuthBearerToken := '';

         // built REST parameters and make request
            SslWebSocketCli.URL := myurl;
            SslWebSocketCli.WSPingSecs := 0;  // do we need keep-alive ping/pongs
            SslWebSocketCli.WSConnect;   // sync request, but check event for result

         // client now continues listening until stopped, so we wait and process events
            AddLogText ('Click Abort to stop WebSocket Client snippet');
            TrgTimerEnd := IcsGetTrgMins64(5);   // stop after 5 minutes
            AbortFlag := False;
            while NOT AbortFlag do begin
                 Application.ProcessMessages;
                 if Application.Terminated then Break;
                 if IcsTestTrgTick64(TrgTimerEnd) then Break;
            end;
            SslWebSocketCli.WSClose(wscrNormalClosure, 'Closed by user');
        except
            AddLogText ('WebSocket Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (SslWebSocketCli) ;
        doWebSocket.Enabled := true ;
    end ;
end;


{ the following event is used by the TIcsMailQueue component for logging. }

procedure TSnippets.onIcsMailQueueLogEvent(LogLevel: TMailLogLevel; const Info: string);
begin
    AddLogText (Info);
end;


{ Snippet: Send Email using Mail Queue - Runs a mail queue to send multiple emails
  with extended retries over many hours or days, supports multiple SMTP relay servers
  or looks up MX servers, while alleviating the need for the application to handle
  retries.  Mail is queued to disk, so retries will continue if the application is
  restarted.
  TIcsMailQueue is designed to prepare, queue and send email.  Preparing the email
  is done using the the ICS THtmlSmtpCli component so it may be plain text or HTML
  email with one or more file attachments. Once the mail properties in QuHtmlSmtp
  have been specified, it is queued using the QueueMail method which saves it to
  an EML RFC2822 formatted spool file from where a thread will send it.
  This snippet sends emails to a Magenta Systems junk mailbox that does not require
  authenication, normally you would send your ISP or corporate relay mail server
  with authentication, which can be done by changing properties below.  The snippet
  queues mail in a temporary directory mailqueuework.  When started, the mail queue
  is first started, then a single email is queued and the WaitSendandStop method
  called which will wait up to 10 minutes for the email to be sent, normally it
  happens in a few seconds.  Usually the mail queue would be left running to
  handle new delivery attempts for failed emails and accept new emails for
  delivery. The mail queue itself can be accessed for display and to cancel
  mail before it is delivered.
  The full Mail Queue sample is \sslinternet\OverbyteIcsMailQuTst.dpr.
}

procedure TSnippets.doEmailMailQuClick(Sender: TObject);
var
    IcsMailQueue: TIcsMailQueue;
    mysmptserver, myemailto, myemailfrom, mymailqudir: String;
    FriendlyName: String ;
    Item: Integer;
begin

// parameters for the mail server and email sent
    mysmptserver := 'mail.magsys.co.uk';                // also hosts ftptest.org
    myemailto := '"Junk Testing" <testing@ftptest.org>';   // no authentication needed for SMTP server hosting the account
    myemailfrom := '"ICS Snippets" <icsnippets@ftptest.org>';
    mymailqudir := IncludeTrailingPathDelimiter(DirTemp.Text) + 'mailqueuework';

    if NOT ForceDirectories (mymailqudir) then begin
        AddLogText ('Failed to Create Mail Queue Directory: ' +  mymailqudir);
        exit ;
    end;

// create component and events to see progress
    IcsMailQueue := TIcsMailQueue.Create (self);
    IcsMailQueue.LogEvent := onIcsMailQueueLogEvent;
    doEmailMailQu.Enabled := false ;
    try
        try
            IcsMailQueue.SslVerMethod := MailSslVerBundle;     // or MailSslVerNone
       //   IcsMailQueue.SslRevocation := True;
       //   IcsMailQueue.SslReportChain := True;
            IcsMailQueue.SslRootFile := '';    // blank uses internal bundle
            IcsMailQueue.LogQuSent := true ;  // create log of sent email
            IcsMailQueue.Debug := true ;
       // these are defaults so don't need to be set
       //   IcsMailQueue.RetryList := '5,5,10,10,30,30,60,90,300,300,300,300';  // minutes between send retry attempts
       //   IcsMailQueue.ArchiveSent := true ;  // keep copies of sent mail
       //   IcsMailQueue.DeleteFailed := false ; // delete failed mail
       //   IcsMailQueue.QuStartDelay := 2 ;
            IcsMailQueue.MailQuDir := mymailqudir ;
         // relay is generally your ISPs or corporate email server, specific allows individual servers for each emai
         // Mxlookup will use DNS to find the finally delivery mail server, but might expect reverse DNS matching the SignOn
            IcsMailQueue.SmtpMethod := MailSmtpRelay;  // or MailSmtpSpecific, MailSmtpMxLookup
            IcsMailQueue.MxSrvUseSsl := False;
       //   IcsMailQueue.QuHtmlSmtp.SignOn :=  ;

         // add multiple email servers, not necessary if using specific or MX domain lookup
         // if more than one, sending email will try each one in turn for each attempt
            IcsMailQueue.MailServers.Clear;
            IcsMailQueue.MailServers.Add;
            with IcsMailQueue.MailServers [0] do begin
                Port := '587';     // 25 or 465 (SSL only) or 587
                Host := mysmptserver;
                AuthType := smtpAuthNone;  // smtpAuthAutoSelect, smtpAuthPlain, etc with username and password
                UserName := '';
                Password := '';
                SslType := smtpTlsExplicit;  // explicit means same port as non-SSL, implicit means dedicated SSL port 465
                SignOn := 'ICS Snippet' ;
                SslCliSecurity := sslCliSecDefault;
                RetryWithoutSsl := True;
            end;
            IcsMailQueue.Active := True ;
            if NOT IcsMailQueue.Active then begin
                AddLogText('Failed to Start Mail Queue');
                Exit;
            end;

         // now queue one email, could be hundreds
            IcsMailQueue.QuHtmlSmtp.EmailFiles.Clear ;
            IcsMailQueue.QuHtmlSmtp.Allow8bitChars := false ;     // true prevents MIME encoding of unicode headers
//            IcsMailQueue.QuHtmlSmtp.ConvertToCharset := true ;  // ignored for Unicode compilers, always converts
            IcsMailQueue.QuHtmlSmtp.CharSet := 'utf-8' ;          // so full unicode is supported
            IcsMailQueue.QuHtmlSmtp.FoldHeaders := True;
            IcsMailQueue.QuHtmlSmtp.HdrFrom := myemailfrom;
            IcsMailQueue.QuHtmlSmtp.FromName := IcsParseEmail(myemailfrom, FriendlyName) ;   // V9.3 added Ics, now in Utils
            IcsMailQueue.QuHtmlSmtp.HdrCc := myemailfrom;
            IcsMailQueue.QuHtmlSmtp.HdrReplyTo := IcsMailQueue.QuHtmlSmtp.FromName ;
            IcsMailQueue.QuHtmlSmtp.HdrSubject := 'Testing email from ICS Snippets';
            IcsMailQueue.QuHtmlSmtp.ContentType := smtpPlainText ;
        //  IcsMailQueue.QuHtmlSmtp.ContentType := smtpHtml ;
        //  IcsMailQueue.QuHtmlSmtp.HtmlText.Text :=  ;  // we could build an HTML body, or add file attachments, images, etc
            IcsMailQueue.QuHtmlSmtp.PlainText.Text := 'A very short email message.';

        // can send the same email to hundreds of recepient addresses
            IcsMailQueue.QuHtmlSmtp.RcptName.Clear ;
            IcsMailQueue.QuHtmlSmtp.RcptName.Add(IcsParseEmail (myemailto, FriendlyName));    // V9.3 added Ics, now in Utils
       //   if IcsMailQueue.QuHtmlSmtp.HdrCc <> '' then           // cc won't work since this is a fake from account
       //       IcsMailQueue.QuHtmlSmtp.RcptName.Add(ParseEmail(IcsMailQueue.QuHtmlSmtp.HdrCc, FriendlyName));   // smptprot) ;

        // but they will all have the same recepient in the headers, only sending one here so don't care
        // for mailing lists better to queue each mail separately with it's own HdrTo
            IcsMailQueue.QuHtmlSmtp.HdrTo := myemailto;

        // finally queue this email
            Item := IcsMailQueue.QueueMail ;
            if Item = 0 then
                AddLogText('Failed to Queue Mail - ' + IcsMailQueue.QuHtmlSmtp.ErrorMessage)
            else
                AddLogText('Mail Queued OK as Item ' + IntToStr (Item));

         // mail will now be sent in background from a thread
         // we are not sending any more so just wait until the queue is empty
         // beware Abort does not work here to might need to wait 10 minutes for two retries to send
            IcsMailQueue.WaitSendandStop (630);
        except
            AddLogText ('Mail Queue Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (IcsMailQueue) ;
        doEmailMailQu.Enabled := true ;
    end ;
end;


{ Snippet: HTTP REST Get and Post Paramaters Requests - two snippets, that make HTTP GET and
  POST request to a ICS REST server passing several UrlEncoded parameters and receiving back
  a page displaying those parameters in raw and decoded format, primarily for debugging
  purposes. Uses the TSslHttpRest component in sync mode, but could be async with the result
  in the event OnRestRequestDone.  Returns an HTTP status code, 200 for OK, 404 page not
  found, etc. RestParams will build Json, XML, UrlEncoded or comma list parameters, from
  strings, integers and TDateTime or you can build them yourself and pass them raw.
  On success, ResponseRaw has a unicode string converted using IcsHtmlToStr according
  to the content, including entities like &pound; and &#9741; to unicode symbols.
  This ICS server needs authentication, which is hardcoded here.
  The full HTTP REST sample is \sslinternet\OverbyteIcsHttpRestTst.dpr.
}

procedure TSnippets.doHttpGetParamsClick(Sender: TObject);
var
   SslHttpRest: TSslHttpRest;
   StatCode: Integer;
   myurl: String;
begin

// parameters for the HTTP Rest request
    myurl := 'https://www.telecom-tariffs.co.uk/testing/postinfo.htm' ;

// create component and events to see progress
    SslHttpRest := TSslHttpRest.Create (self) ;
    SslHttpRest.OnHttpRestProg := onSslHttpRestProg ;
    doHttpRestReq.Enabled := false ;
    LabelProgress.Caption := '' ;
    try
        try
         // essential and common HTTP parameters
            SslHttpRest.SocketFamily := sfAny;  { IP4 and/or IPV6 }
            SslHttpRest.DebugLevel := DebugHdr;  // DebugNone, DebugConn, DebugParams, DebugSSL, DebugHdr, DebugBody, each logs more
            SslHttpRest.CertVerMethod := CertVerBundle;     // or CertVerNone if you don't care

        { https://www.telecom-tariffs.co.uk/testing/ needs authentication }
            SslHttpRest.ServerAuth := httpAuthBasic;
            SslHttpRest.Username := 'test';
            SslHttpRest.Password := 'password';

        // TRestParams will build Json, XML, UrlEncoded or comma list parameters, from strings, integers and TDateTime.
            SslHttpRest.RestParams.PContent := PContUrlEncoded;      // lots of PContxx options, Jsson, XML
            SslHttpRest.RestParams.AddItem('myparam1', 'string1');   // add string parameter
            SslHttpRest.RestParams.AddItem('myparam2', 'string2');  // add string parameter
            SslHttpRest.RestParams.AddItem('myparam3', 123456);     // add integer parameter
            SslHttpRest.RestParams.AddItem('myparam4', True);       // add boolean parameter
            SslHttpRest.RestParams.AddItemDT('myparam5', Now);      // add TDateTime parameter
            StatCode := SslHttpRest.RestRequest(httpGET, myurl, False, '');  // sync request, no extra parameters
            AddLogText ('HTTP Rest Request Response: ' + IntToStr(StatCode)) ;
            if StatCode = 200 then begin
                AddLogText (SslHttpRest.ResponseRaw);                  // anything
            end;
        except
            AddLogText ('HTTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (SslHttpRest) ;
        LabelProgress.Caption := 'HTTP Completed' ;
        doHttpRestReq.Enabled := true ;
    end ;
end;

procedure TSnippets.doHttpPostParamsClick(Sender: TObject);
var
   SslHttpRest: TSslHttpRest;
   StatCode: Integer;
   myurl: String;
begin

// parameters for the HTTP Rest request
    myurl := 'https://www.telecom-tariffs.co.uk/testing/postinfo.htm' ;

// create component and events to see progress
    SslHttpRest := TSslHttpRest.Create (self) ;
    SslHttpRest.OnHttpRestProg := onSslHttpRestProg ;
    doHttpRestReq.Enabled := false ;
    LabelProgress.Caption := '' ;
    try
        try
         // essential and common HTTP parameters
            SslHttpRest.SocketFamily := sfAny;  { IP4 and/or IPV6 }
            SslHttpRest.DebugLevel := DebugHdr;  // DebugNone, DebugConn, DebugParams, DebugSSL, DebugHdr, DebugBody, each logs more
            SslHttpRest.CertVerMethod := CertVerBundle;     // or CertVerNone if you don't care

        { https://www.telecom-tariffs.co.uk/testing/ needs authentication }
            SslHttpRest.ServerAuth := httpAuthBasic;
            SslHttpRest.Username := 'test';
            SslHttpRest.Password := 'password';

        // TRestParams will build Json, XML, UrlEncoded or comma list parameters, from strings, integers and TDateTime.
            SslHttpRest.RestParams.PContent := PContUrlEncoded;      // lots of PContxx options, Jsson, XML
            SslHttpRest.RestParams.AddItem('myparam1', 'string1');   // add string parameter
            SslHttpRest.RestParams.AddItem('myparam2', 'string2');  // add string parameter
            SslHttpRest.RestParams.AddItem('myparam3', 123456);     // add integer parameter
            SslHttpRest.RestParams.AddItem('myparam4', True);       // add boolean parameter
            SslHttpRest.RestParams.AddItemDT('myparam5', Now);      // add TDateTime parameter
            StatCode := SslHttpRest.RestRequest(httpPOST, myurl, False, '');  // sync request, no extra parameters
            AddLogText ('HTTP Rest Request Response: ' + IntToStr(StatCode)) ;
            if StatCode = 200 then begin
                AddLogText (SslHttpRest.ResponseRaw);                  // anything
            end;
        except
            AddLogText ('HTTP Error - ' + IcsGetExceptMess (ExceptObject)) ;
        end ;
    finally
        FreeAndNil (SslHttpRest) ;
        LabelProgress.Caption := 'HTTP Completed' ;
        doHttpRestReq.Enabled := true ;
    end;
end;

end.
