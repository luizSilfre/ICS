program IcsMailSnd;

uses
  FMX.Forms,
  FMX.Types,
  IcsMailSnd1 in 'IcsMailSnd1.pas' {SmtpTestForm};

{$R *.res}

begin
  Application.Initialize;
  GlobalDisableFocusEffect := TRUE;
  Application.CreateForm(TSmtpTestForm, SmtpTestForm);
  Application.Run;
end.
