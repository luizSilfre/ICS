program OverbyteIcsBasicWebServer;

uses
  Vcl.Forms,
  OverbyteIcsBasicWebServer1 in 'OverbyteIcsBasicWebServer1.pas' {WebServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TWebServerForm, WebServerForm);
  Application.Run;
end.
