program IcsTcpSrvIPv6;

uses
  FMX.Forms,
  FMX.Types,
  IcsTcpSrv1IPv6 in 'IcsTcpSrv1IPv6.pas' {TcpSrvForm};

{$R *.res}

begin
  Application.Initialize;
  GlobalDisableFocusEffect := True;
  Application.CreateForm(TTcpSrvForm, TcpSrvForm);
  Application.Run;
end.
