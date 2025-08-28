rem batch file to create RES files

rem cd C:\DelphiComp\Ics\Source
brcc32 "OverbyteIcsXpManifest.RC"
brcc32 "OverbyteIcsCommonVersion.RC"
brcc32 "nmap-mac-prefixes.RC"
brcc32 "ICSPortList.RC"
brcc32 "ICSCerts.RC"
brcc32 "ICSMedia.RC"
pause

