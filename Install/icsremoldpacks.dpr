{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson
Description:  ICS Pre-V91 Package Uninstaller V1
Creation:     June 2024
Version:      V1.0
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 1996-2024 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.

History:



}


program icsremoldpacks;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Registry,
  Classes,
  Windows;


const
    KnownBDSkKey = 'Software\Embarcadero\BDS\';
    KnownPackKey = '\Known Packages';
    KnownICSKey = '\Bpl\Ics';

var
    PackageList: TStringList;
    IDERegistry: TRegistry;
    FullPath, CmdArg: String;
    RestartFlag: Boolean;

procedure EmumPackages(const Dver: String);
begin
    PackageList.Clear;
    IDERegistry.RootKey := HKEY_CURRENT_USER;
    IDERegistry.Access := KEY_QUERY_VALUE;
 // any package for this version of Delphi
    Writeln('Checking for RAD Studio ' + Dver);
    FullPath := KnownBDSkKey + Dver + KnownPackKey;
    if NOT IDERegistry.KeyExists(FullPath) then begin
        Writeln('No IDE Packges Found: ' +  FullPath);
        IDERegistry.CloseKey;
        exit;
    end;

 // build list ofkey, package file names
    IDERegistry.OpenKeyReadOnly(FullPath);
    IDERegistry.GetValueNames(PackageList);
    IDERegistry.CloseKey;
end;

procedure CheckPackages;
var
    I, J: Integer;
    PName: String;
begin
    if PackageList.Count = 0 then Exit;
    for I := 0 to PackageList.Count - 1 do begin
        J := Pos (KnownICSKey, PackageList[I]);
        if J > 10 then begin
            PName := Copy(PackageList[I], J + 5, 999);
            if Pos('New', PName) = 0 then begin
                IDERegistry.Access := KEY_ALL_ACCESS;
                IDERegistry.OpenKey(FullPath, False);
                if IDERegistry.DeleteValue(PackageList[I]) then begin
                    Writeln('ICS Packge Uninstalled OK: ' +  PName);
                    RestartFlag := True;
                end
                else
                    Writeln('ICS Packge Uninstall Failed: ' +  PName);
                IDERegistry.CloseKey;
            end
            else
                 Writeln('New ICS Packge Found: ' +  PName);
        end;
    end;
end;

/////////////////////////////////////////////////////////////////////////////////////////////////////////

// supported argument, one only
// -D104, -D11, -D12, -DALL

begin
    PackageList := TStringList.Create;
    IDERegistry := TRegistry.Create(KEY_QUERY_VALUE);
    RestartFlag := False;
    ExitCode := 0;
    try
        try
            Writeln('ICS Pre-V91 Package Uninstaller V1 - ' + String(GetCommandLine)); // ParamStr(0) + ' ' + ParamStr(1));
            if ParamCount = 1 then begin
                CmdArg := ParamStr(1);
                if (CmdArg = '-D104') or (CmdArg = '-DALL') then begin
                    EmumPackages('21.0');
                    CheckPackages;
                end;
                if (CmdArg = '-D11') or (CmdArg = '-DALL') then begin
                    EmumPackages('22.0');
                    CheckPackages;
                end;
                if (CmdArg = '-D12') or (CmdArg = '-DALL') then begin
                    EmumPackages('23.0');
                    CheckPackages;
                end;
                if RestartFlag then begin
                    Writeln('Delphi IDE must be restarted to complete package removal');
                    ExitCode := 1;
                end;
            end
            else
                Writeln('Expected one argument: -D104, -D11, -D12, -DALL');
        except
            on E: Exception do
                Writeln(E.ClassName, ': ', E.Message);
        end;
    finally
       PackageList.Free;
       IDERegistry.Free;
    end;


end.
