{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson
Creation:     July 6, 2017
Description:  This is an posted data demo, taken from the original web server
              sample that had dedicated code to handle POST requests
Version:      9.3
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2003-2024 by François PIETTE
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

History:
July 6, 2017  V8.49 baseline
May 20, 2022 V8.69 - Recognise more MIME types as download files, more logging.
                   Using properties from OverbyteIcsSslMultiWebDataModule so this
                     unit is not dependent upon a single application, and works in
                     DDWebService.
Mar 07, 2024 V9.1  Added OverbyteIcsSslBase which now includes TSslContext,TX509Base and TX509List.
                   Added OverbyteIcsCharsetUtils for TextToHtmlText.
                   Using Client.PostedDataStr instead of Client.PostedData to
                    avoid casting a buffer.
                   TFormDataAnalyser now decodes a form using Client.PostedDataStream
                    instead of creating a new TMemoryStream, now supports uploads
                    larger than memory, tested to 6GB. Should suggest unicode chars.
                   Added new postinfo.html page that decodes and displays any
                    parameters passed.
Apr 12, 2024 V9.2  DemoAuthAll.html is now TUrlHandlerDemoAuthAll template, to test
                     authentication for templates.
Aug 14, 2024 V9.3  Using OverbyteIcsTypes for consolidated types and constants.
                   Fixed a CopyFrom error with older compilers, thanks to Ralf.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslMultiWebUploads;

interface

{$I Include\OverbyteIcsDefs.inc}   { V9.1 }

uses
  {$IFDEF MSWINDOWS}
    Windows,
  {$ENDIF}
    Classes, SysUtils,
    OverbyteIcsUrl,
    OverbyteIcsHttpSrv,
    OverbyteIcsTicks64,
    OverbyteIcsUtils,
//    OverbyteIcsFtpSrvT,
    OverbyteIcsHttpAppServer,
    OverbyteIcsFormDataDecoder,
    OverbyteIcsCharsetUtils,      { V9.1 }
    OverbyteIcsHtmlUtils,         { V9.1 }
    OverbyteIcsSslMultiWebDataModule,
    OverbyteIcsSslUtils,      { V9.3 }
    OverbyteIcsTypes;         { V9.3 consolidated types and constants }

type
    TUploadDisplayEvent = procedure (Sender : TObject; const Msg : String) of object;

    TUrlHandlerUploadData = class(TUrlHandler)
    public
        procedure Execute; override;
    end;

    TUrlHandlerUploadFile = class(TUrlHandler)
    public
        procedure Execute; override;
    end;

    TUrlHandlerPostInfo = class(TUrlHandler)     { V9.1 }
    public
        procedure Execute; override;
    end;

    TUrlHandlerDemoAuthAll = class(TUrlHandler)     { V9.2 }
    public
        procedure Execute; override;
    end;

implementation

procedure TUrlHandlerUploadData.Execute;
var
    Stream    : TStream;
    FileName  : String;
    FirstName : String;
    LastName  : String;
    HostName  : String;
    Buf       : String;
    Bom       : array[0..2] of Byte;
    IsUtf8    : Boolean;
    Len       : Integer;
    Utf8Str   : AnsiString;
begin
    if Client.Method = 'POST' then begin

        { Extract fields from posted data. }
        ExtractURLEncodedValue(Client.PostedDataStr, 'FirstName', FirstName);      { V9.1 }
        ExtractURLEncodedValue(Client.PostedDataStr, 'LastName',  LastName);       { V9.1 }
        { Get client IP address. We could to ReverseDnsLookup to get hostname }
        HostName := Client.PeerAddr;
        { Build the record to write to data file }
        Buf      := FormatDateTime('YYYYMMDD HHNNSS ', Now) +
                    FirstName + '.' + LastName + '@' + HostName + #13#10;

        { Save data to a text file }
        FileName := IncludeTrailingPathDelimiter(SslMultiWebDataModule.UploadDir) + 'FormHandler.txt';
        try
            if FileExists(FileName) then begin
                Stream := TFileStream.Create(FileName, fmOpenReadWrite);
                { Check whether the data file is UTF-8 encoded }
                Len := Stream.Read(Bom[0], SizeOf(Bom));
                IsUtf8 := (Len = 3) and (Bom[0] = $EF) and (Bom[1] = $BB) and (Bom[2] = $BF);
                Stream.Seek(0, soFromEnd);
            end
            else begin
                { We use UTF-8 by default for new data files }
                Stream := TFileStream.Create(FileName, fmCreate);
                IsUtf8 := TRUE;
                Bom[0] := $EF; Bom[1] := $BB; Bom[2] := $BF;
                Stream.Write(Bom[0], SizeOf(Bom));
            end;
            if IsUtf8 then begin
                Utf8Str := StringToUtf8(Buf);
                Stream.Write(PAnsiChar(Utf8Str)^, Length(Utf8Str));
            end
            else
                StreamWriteStrA(Stream, Buf);
            Stream.Destroy;
        except
            on E:Exception do
                SslMultiWebDataModule.Display('Exception Saving Posted Data - ' + E.Message) ;
        end;

        { Here is the place to check for valid input data and produce a HTML }
        { answer according to data validation.                               }
        { Here for simplicity, we don't check data and always produce the    }
        { same HTML answer.                                                  }

        AnswerString(
            '',           { Default Status '200 OK'         }
            '',           { Default Content-Type: text/html }
            '',           { Default header                  }
            '<HTML>' +
              '<HEAD>' +
                '<TITLE>ICS WebServer Form Demo</TITLE>' +
              '</HEAD>' + #13#10 +
              '<BODY>' +
                '<H2>Your data has been recorded:</H2>' + #13#10 +
                '<P>' + TextToHtmlText(FirstName) + '.' +
                        TextToHtmlText(LastName)  + '@' +
                        TextToHtmlText(HostName)  +'</P>' +
                '<P>Filename: ' + TextToHtmlText(FileName)  +'</P>' +
                '<A HREF="/form.html">More data entry</A><BR>' +
                '<A HREF="/FormData.html">View data file</A><BR>' +
                '<A HREF="/demo.html">Back to demo menu</A><BR>' +
              '</BODY>' +
            '</HTML>');
        Finish;
    end;
end;

// upload a file to the server

procedure TUrlHandlerUploadFile.Execute;
//const
//  MAX_UPLOAD_SIZE    = 1024 * 1024 * 60; // Accept max 60MB file
var
    sinfo1, sPageUrl, sContent: string;
    newfilename, sFileName, sfiletitle: string;
    Decoder: TFormDataAnalyser;
    Field: TFormDataItem;
    FileStream: TFileStream;
//    MemoryStream: TMemoryStream;           { V9.1 no longer needed }
    RemoteClient: THttpAppSrvConnection;
    UploadTotTicks, MaxUploadSize: Int64;

    procedure Logit (fsize: Int64);
    var
        Speed: LongWord ;
        Duration: string ;
    begin
        try
            sinfo1 :=  'Saved Uploaded File OK' + #13#10 + 'New File Name: ' + newfilename + #13#10 + 'File Size: ' + IntToKByte(fsize) ;
            if (UploadTotTicks >= 1000) then
                Duration :=  FloatToStrF (UploadTotTicks / 1000, ffFixed, 15, 2) + ' secs'
            else
                Duration := IntToStr (UploadTotTicks) + ' msecs' ;
            speed := 0 ;

        { V9.1 no speed less than five seconds }
            if (UploadTotTicks > 5000) and (UploadTotTicks < 60*60*1000) and (fsize > 1000) then
            begin
                if (UploadTotTicks > 100000) and (fsize > 1000000) then
                begin
                    UploadTotTicks := UploadTotTicks div 1000 ;  // allow for bizarre divide by zero error
                    speed := fsize div UploadTotTicks
                end
                else
                    speed := (fsize * 1000) div UploadTotTicks ;
            end;
            sinfo1 := sinfo1 + IcsCRLF + 'Upload Duration: ' + duration;
            if speed > 0 then
                sinfo1 := sinfo1 +  IcsCRLF + 'Speed: ' + IntToStr (speed) + ' chars/sec' ;
        except
        end;
    end ;

begin
    RemoteClient := THttpAppSrvConnection(Client) ;
    sinfo1 := '' ;
    sFileName := '' ;
    sfiletitle := '' ;
    UploadTotTicks := IcsElapsedTicks64 (RemoteClient.RequestStartTick) + 1 ;
    MaxUploadSize := RemoteClient.AppServer.MaxUploadMB * IcsMBYTE;                        { V9.1 }
    sPageUrl := Client.RequestProtocol + '://' + Client.RequestHost + Client.Path ;

    if RemoteClient.Method = 'GET' then  begin
        if Params <> '' then begin   // not really used !!
            ExtractURLEncodedValue (Params, 'FileName', sFileName) ;
            ExtractURLEncodedValue (Params, 'FileTitle', sfiletitle) ;
        end;
    end ;

// see if page is being POSTed by itself to upload a file
    if (RemoteClient.Method = 'POST') or (RemoteClient.Method = 'PUT')  then begin  { V6.69 added PUT }
        sContent := Lowercase(RemoteClient.RequestContentType);  { V8.69 }
        Display ('Received Post/PUT Data File, Size ' + IntToKbyte (RemoteClient.PostedDataLen) + ', Content Type: ' + sContent) ;
        if RemoteClient.PostTempName <> '' then
            Display('Temporary File Name: ' + RemoteClient.PostTempName);     { V9.1 }

        if (RemoteClient.PostedDataLen > MaxUploadSize) then begin                  { V9.1 configured in server config file  }
             sinfo1 := 'Upload File (' + IntToKbyte (RemoteClient.PostedDataLen) + ') Exceeds Maximum Size' ;
        end
        else begin
         // First we must tell the component that we've got all the data
            RemoteClient.PostedDataReceived;
            try
                // now see how the file was uploaded
                if Pos('multipart/form-data', sContent) > 0 then begin
                //    MemoryStream := TMemoryStream.Create ;    { V9.1 no longer needed }
                //    try
                //        MemoryStream.WriteBuffer (RemoteClient.PostedData^, RemoteClient.PostedDataLen) ;
                //        MemoryStream.Seek(0, 0);
                    Decoder := TFormDataAnalyser.Create(nil);
                    try
                   //    Decoder.OnDisplay := DecoderDisplay;
                    //    Decoder.DecodeStream (MemoryStream) ;
                        Decoder.DecodeStream (RemoteClient.PostedDataStream) ;   { V9.1 now have stream }
                        Display (Decoder.DecodeInfo);   { V9.1 log form-data }

                        // Extract file, do a minimal validity check
                        Field := Decoder.Part ('FileName');
                        if not Assigned (Field) then
                            sinfo1 := 'Upload Form Error, Missing FileName Tag'
                        else  begin
                            sFileName := ExtractFileName(Field.ContentFileName);
                            if sFileName = '' then
                                sinfo1 := 'Upload Form Error, Empty FileName'
                            else if Field.DataLength <= 0 then
                                sinfo1 := 'Upload Form Error, File Empty'
                            else if ((Pos('/', sFileName) > 0) or
                                       (Pos('\', sFileName) > 0) or
                                       (Pos(':', sFileName) > 0)) then
                                sinfo1 := 'Illegal Upload File Name: ' + sFileName
                            else begin
                                try
                            // create a new file name with date and time
                                    newfilename := IncludeTrailingPathDelimiter(SslMultiWebDataModule.UploadDir) +
                                                         FormatDateTime('yyyymmdd"-"hhnnss', Now) + '_' + sFileName;  { V8.69 }
                                    Display ('Saving MIME Upload File as ' + newfilename);   { V8.69 }
                                    Field.SaveToFile (newfilename);
                                    Logit (Field.DataLength) ;
                                except
                                    on E:Exception do
                                        sinfo1 := 'Failed to Save MIME Uploaded File as ' + newfilename + ' - ' + E.Message;
                                 end;
                            end;
                        end;
                   //     Field := Decoder.Part ('FileTitle');
                   //     if Assigned (Field) then sfiletitle := Field.AsString;
 {$IFDEF COMPILER12_UP}
                        sfiletitle := Decoder.PartData ('FileTitle', CP_ACP, True, True);  { V9.1 better method }
{$ELSE}
                        sfiletitle := Decoder.PartData ('FileTitle');
{$ENDIF}
                    finally
                        FreeAndNil(Decoder);
                    end;
                 end
                else if (Pos('application', sContent) > 0) or (Pos('audio', sContent) > 0) or (Pos('image', sContent) > 0) then begin { V8.69 }
                    if Params <> '' then begin
                        ExtractURLEncodedValue (Params, 'FileName', sFileName) ;
                        ExtractURLEncodedValue (Params, 'FileTitle', sfiletitle) ;
                    end;
                    if sFileName = '' then
                        sinfo1 := 'Upload Form Error, Empty FileName'
                    else if ((Pos('/', sFileName) > 0) or
                               (Pos('\', sFileName) > 0) or
                               (Pos(':', sFileName) > 0)) then
                        sinfo1 := 'Illegal Upload File Name: ' + sFileName
                    else begin
                        newfilename := IncludeTrailingPathDelimiter(SslMultiWebDataModule.UploadDir) +     { V8.69 }
                                            FormatDateTime('yyyymmdd"-"hhnnss', Now) + '_' + sFileName;
                        Display('Saving Simple Upload File as ' + newfilename);   { V8.69 }

                     { V9.1 if we have a temporary file, rename it instead of copying it }
                        if (RemoteClient.PostTempName <> '') and FileExists(RemoteClient.PostTempName) then begin
                            FreeAndNil(RemoteClient.PostedDataStream);
                            if (IcsRenameFile(RemoteClient.PostTempName, newfilename, False, False) <> 0) then
                                Display('Failed to rename temporary file - ' + RemoteClient.PostTempName);
                            RemoteClient.PostTempName := '' ;  // stop it being deleted
                        end;
                        if Assigned(RemoteClient.PostedDataStream) then begin
                            try
                                FileStream := TFileStream.Create (newfilename, fmCreate) ;
                                try
{$IFDEF COMPILER12_UP}
                                    FileStream.CopyFrom(RemoteClient.PostedDataStream, 0);   { V9.1 memory stream, V9.3 add count }
{$ELSE}
                                    FileStream.WriteBuffer (RemoteClient.PostedData^, RemoteClient.PostedDataLen);
{$ENDIF}
                               finally
                                    FreeAndNil(FileStream);
                                end;
                            except
                                on E:Exception do
                                 sinfo1 := 'Failed to Save Uploaded File as ' + newfilename + ' - ' + E.Message ;
                            end;
                        end;
                        if FileExists(newfilename) then
                            Logit (RemoteClient.PostedDataLen) ;
                    end;
                end
                else
           // We don't accept any other request
                   sinfo1 := 'Unknown Post Data Content: ' + RemoteClient.RequestContentType ;
            except
                on E:Exception do
                    Display ('Exception Saving Posted Data - ' + E.Message) ;
            end;
            sinfo1 := sinfo1 + #13#10 +
                    'Upload FileName: ' + sFileName + #13#10 +
                    'FileTitle: ' + sfiletitle + #13#10 +
                    'Post URL: ' + sPageUrl + #13#10 +
                    'From IP Address: ' + RemoteClient.CPeerAddr ;
        end;
    end;
    Display(sinfo1);
    sinfo1 := StringReplace (sinfo1, #13#10, '<br>', [rfReplaceAll]);
    AnswerPage('', '', '\uploadfile.html', nil,
             ['sinfo1', sinfo1, 'sPageUrl', sPageUrl, 'sMaxFileSize', IntToKByte(MaxUploadSize, true),   { V9.1 }
              'sFileName', TextToHtmlText(sFileName), 'sFileTitle', TextToHtmlText(sfiletitle)
               ], {$IFDEF COMPILER12_UP} CP_ACP, CP_UTF8,{$ENDIF} Now);
    Finish;
end;

// V9.1 display posted data for diagnostic purposes

procedure TUrlHandlerPostInfo.Execute;
var
    sinfo1, rawdata, sPageUrl: string ;
    Decoder: TFormDataAnalyser;
    RemoteClient: THttpAppSrvConnection;
    ParamList: TStringList;
    ParamTot, I: Integer;
    ACodePage: longword;
begin
    RemoteClient := THttpAppSrvConnection(Client) ;
    sinfo1 := 'POST/PUT Content Size ' + IntToKbyte (RemoteClient.PostedDataLen) + IcsCRLF +
              'Request Content Type: ' + RemoteClient.RequestContentType + IcsCRLF;
    sPageUrl := RemoteClient.RequestProtocol + '://' + RemoteClient.RequestHost + Client.Path ;
    if Params <> '' then
    begin
        Sinfo1 := sinfo1 + 'Raw Params:' + IcsCRLF + IcsStrBeakup(Params, 132) + IcsCRLF + IcsCRLF;
        ParamList := TStringList.Create;
        try
            ParamTot := IcsExtractURLEncodedParamList(Params, ParamList, True) ;
            if ParamTot > 0 then begin
                for I := 0 to ParamTot - 1 do
                    Sinfo1 := sinfo1 + ParamList[I] + IcsCRLF;
            end;
        finally
            ParamList.Free;
        end;
    end;

// report POSTed content
    if (RemoteClient.Method = 'POST') or (RemoteClient.Method = 'PUT') then
    begin
        if RemoteClient.PostedDataLen < 9000 then begin
            rawdata := IcsStrRemCntlsTB(RemoteClient.PostedDataTB, True);
            rawdata := IcsStrBeakup(rawdata, 132);
            sinfo1 := sinfo1 + 'Raw Params:' + IcsCRLF + rawdata + IcsCRLF + IcsCRLF;
        end;

     // First we must tell the component that we've got all the data
        RemoteClient.PostedDataReceived;
        try
            // now what has been posted
            if Pos('multipart/form-data', RemoteClient.RequestContentType) > 0 then
            begin
                Decoder := TFormDataAnalyser.Create(nil);
                try
                    MimeCharsetToCodePage(RemoteClient.RequestContentType, ACodePage);
                    Decoder.FormCodePage := ACodePage;
                    RemoteClient.PostedDataStream.Position := 0;
                    Decoder.DecodeStream (RemoteClient.PostedDataStream) ;
                    sinfo1 := sinfo1 + Decoder.DecodeInfo;
                    // all done
                finally
                    FreeAndNil(Decoder);
                end;
            end
            else if (RemoteClient.PostedDataLen > 1000) or
               (SameText(RemoteClient.RequestContentType, 'application/binary') or
                          SameText(RemoteClient.RequestContentType, 'application/octet-stream') or
                                 SameText(RemoteClient.RequestContentType, 'application/zip')) then
            begin
                sinfo1 := sinfo1 + 'Unable to process POST/PUT content';
            end
            else
            begin
                Sinfo1 := sinfo1 + 'Raw Params:' + IcsCRLF +
                                IcsStrBeakup(IcsStrRemCntls(Params), 132) + IcsCRLF + IcsCRLF;
                ParamList := TStringList.Create;
                try
                    ParamTot := IcsExtractURLEncodedParamList(Client.PostedDataStr, ParamList, True) ;
                    if ParamTot > 0 then begin
                        for I := 0 to ParamTot - 1 do
                            Sinfo1 := sinfo1 + ParamList[I] + IcsCRLF;
                    end;
                finally
                    ParamList.Free;
                end;
            end;
        except
            Display ('Exception Saving Posted Data - ' + IcsGetExceptMess (ExceptObject)) ;
        end;
        sinfo1 := sinfo1 + IcsCRLF +
                'Post URL: ' + sPageUrl + IcsCRLF +
                'From IP Address: ' + RemoteClient.CPeerAddr ;
    end;
    Display (sinfo1);
    sinfo1 := TextToHtmlText (sinfo1);   // converts CRLF
    AnswerPage('', '', '\postinfo.html', nil, ['sinfo1', sinfo1, 'sPageUrl', sPageUrl],
                                                                {$IFDEF COMPILER12_UP} CP_ACP, CP_UTF8,{$ENDIF} Now);
    Finish;
end;

// V9.2 authentication test page for POST and GET

procedure TUrlHandlerDemoAuthAll.Execute;
var
    RemoteClient: THttpAppSrvConnection;
    sPageUrl: String;
begin
    RemoteClient := THttpAppSrvConnection(Client) ;
    sPageUrl := RemoteClient.Method + ' ' + RemoteClient.RequestProtocol + '://' + RemoteClient.RequestHost + Client.Path;
    if Client.PostedDataLen > 0 then
        sPageUrl := sPageUrl + ', PostParams: ' + Client.PostedDataStr;
    AnswerPage('', '', '\DemoAuthAll.html', nil, ['sPageUrl', sPageUrl, 'sAuthType', HttpAuthTypeNames[RemoteClient.AuthType]],
                                                                             {$IFDEF COMPILER12_UP} CP_ACP, CP_UTF8,{$ENDIF} Now);
    Finish;
end;


end.

