{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  ICS Internet Packet Monitoring Components - Display One Packet
Creation:     Octr 2005
Updated:      AprPr 2023
Version:      8.71
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2023 by Angus Robertson, Magenta Systems Ltd,
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

History:
20 Apr 2022 - V8.71 - baseline

}

unit OverbyteIcsNetMon2;

interface

uses
  Windows, Messages, Controls, Forms, ComCtrls, Classes,
  OverbyteIcsIniFiles;

type
  TFormOneRow = class(TForm)
    ListOneRow: TListView;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOneRow: TFormOneRow;

implementation

{$R *.dfm}

Uses OverbyteIcsNetMon1;

procedure TFormOneRow.FormClose(Sender: TObject; var Action: TCloseAction);
var
    IniFile: TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(GetIcsIniFileName);
    with IniFile do begin
    // form positions
        WriteInteger(SectionRow1Window, KeyWidth, Width);
        WriteInteger(SectionRow1Window, KeyHeight, Height);
        WriteInteger(SectionRow1Window, KeyTop, Top);
        WriteInteger(SectionRow1Window, KeyLeft, Left);
        UpdateFile;
    end;
    IniFile.Free;
end;

procedure TFormOneRow.FormShow(Sender: TObject);
var
    IniFile: TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(GetIcsIniFileName);
    with IniFile do begin
    // form positions
        Width := ReadInteger(SectionRow1Window, KeyWidth,  Width);
        Height := ReadInteger(SectionRow1Window, KeyHeight, Height);
        Top := ReadInteger(SectionRow1Window, KeyTop, MonForm.Top);      // default to right of main form
        Left := ReadInteger(SectionRow1Window, KeyLeft, MonForm.Left + MonForm.Width + 20);
    end;
    IniFile.Free;
end;

end.
