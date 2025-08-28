program OverbyteIcsTcpSrvIPv6;

uses
  Forms,
  OverbyteIcsTcpSrv1IPv6 in 'OverbyteIcsTcpSrv1IPv6.pas' {TcpSrvForm};

{$R *.RES}

begin
  Application.CreateForm(TTcpSrvForm, TcpSrvForm);
  Application.Run;
end.
