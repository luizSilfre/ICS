program OverbyteIcsSslSmtpServer;

uses
  Forms,
  OverbyteIcsSslSmtpServ1 in 'OverbyteIcsSslSmtpServ1.pas' {SmtpSslSrvForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Test SSL SMTP Server';
  Application.CreateForm(TSmtpSslSrvForm, SmtpSslSrvForm);
  Application.Run;
end.
