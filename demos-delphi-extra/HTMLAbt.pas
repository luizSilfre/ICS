unit HTMLAbt;

interface

uses
    SysUtils, WinTypes, WinProcs, Messages, Classes, Graphics, Controls,
    Forms, Dialogs, StdCtrls, Buttons, HtmlGlobals, Htmlview, ExtCtrls, HTMLUn2,
    OverbyteIcsWSocket, OverbyteIcsUtils;

{$INCLUDE htmlcons.inc}

type
    TAboutBox = class(TForm)
        BitBtn1 : TBitBtn;
        Panel1 : TPanel;
        Viewer : THTMLViewer;
    private
        { Private declarations }
    public
        { Public declarations }
        constructor CreateIt(Owner: TComponent; const ProgName, CompName: string); overload;
        constructor CreateIt(Owner: TComponent; const Message: ThtString); overload;
    end;

var
    AboutBox : TAboutBox;

implementation

{$R *.DFM}

function ConfigInfo : String;
begin
    Result := '<ul><li>compiled with ' + IcsBuiltWithEx;   { V8.71 }
{$IFDEF UseTNT}
    Result := Result + '<li>Using TNT unicode controls.';
{$ELSE}
{$IFDEF UseElPack}
    Result := Result + '<li>Using ElPack unicode controls.';
{$ELSE}
{$IFDEF UNICODE}
{$IFDEF LCL}
    Result := Result + '<li>Using LCL unicode character controls.';
{$ELSE}
    Result := Result + '<li>Using VCL unicode character controls.';
{$ENDIF}
{$ELSE}
{$IFDEF LCL}
    Result := Result + '<li>Using LCL single byte character controls.';
{$ELSE}
    Result := Result + '<li>Using VCL single byte character controls.';
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}
    Result := Result + '<li>ICS  ' + Trim(OverbyteIcsWSocket.CopyRight);   // March 2018

    Result := Result + '</ul>';
end;

constructor TAboutBox.CreateIt(Owner: TComponent; const ProgName, CompName: string);
var
  S: String;
begin
  inherited Create(Owner);
  inherited Loaded;
  Viewer.DefFontName := 'MS Sans Serif';
  Viewer.DefFontSize := 9;
  Viewer.DefFontColor := clNavy;
  S :='<body text="000080">'+
    '<center>'+
    '<h1>'+ProgName+'</h1>'+
    '<font color="Maroon">A demo program for the '+CompName+' component</font>'+
    '<h3>Version '+ VersionNo +'</h3>'+
    '</center>'+
    ConfigInfo +
    '</body>';
  Viewer.LoadFromString(S);
end;


constructor TAboutBox.CreateIt(Owner: TComponent;
  const Message: ThtString);
begin
  inherited Create(Owner);
  inherited Loaded;
  if Owner is TCustomForm then
    Caption := TCustomForm(Owner).Caption;
  Viewer.DefFontName := 'Verdana';
  Viewer.DefFontSize := 12;
  Viewer.DefFontColor := clBlack;
  Viewer.LoadFromString('<body>' + Message + '</body>');
end;

end.
