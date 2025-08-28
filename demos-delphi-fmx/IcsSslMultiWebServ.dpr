program IcsSslMultiWebServ;

uses
//  FastMM4,
  FMX.Forms,
  IcsSslMultiWebServ1 in 'IcsSslMultiWebServ1.pas' {WeblServerForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TWeblServerForm, WeblServerForm);
  Application.Run;
end.
