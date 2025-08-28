program OverbyteIcsMQTTst;

uses
  Forms,
  OverbyteIcsMQTTst1 in 'OverbyteIcsMQTTst1.pas' {MainForm},
  OverbyteIcsMQTTBroker in 'OverbyteIcsMQTTBroker.pas' {BrokerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TBrokerForm, BrokerForm);
  Application.Run;
end.
