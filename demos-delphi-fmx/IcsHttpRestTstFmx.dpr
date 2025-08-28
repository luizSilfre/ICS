program IcsHttpRestTstFmx;

uses
  FMX.Forms,
  IcsHttpRestTstFmx1 in 'IcsHttpRestTstFmx1.pas' {HttpRestForm},
  IcsHttpRestTstFmx2 in 'IcsHttpRestTstFmx2.pas' {JsonObjectWin},
  IcsLoginFmx in 'IcsLoginFmx.pas' {FormLogin};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ICS HTTPS REST and OAuth Demo';
  Application.CreateForm(THttpRestForm, HttpRestForm);
  Application.CreateForm(TJsonObjectWin, JsonObjectWin);
  Application.Run;
end.
