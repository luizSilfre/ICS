program OverbyteIcsThrdSrv;

uses
  Forms,
  OverbyteIcsThrdSrv1 in 'OverbyteIcsThrdSrv1.pas' {TcpSrvForm};

{$R *.RES}

begin
  Application.CreateForm(TTcpSrvForm, TcpSrvForm);
  Application.Run;
end.
