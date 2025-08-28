program OverbyteIcsThrdSrvV2;

uses
  Forms,
  OverbyteIcsThrdSrvV2_1 in 'OverbyteIcsThrdSrvV2_1.pas' {TcpSrvForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TTcpSrvForm, TcpSrvForm);
  Application.Run;
end.
