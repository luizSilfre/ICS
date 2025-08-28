program OverbyteIcsSocksTst;

uses
  Forms,
  OverbyteIcsSocksTst1 in 'OverbyteIcsSocksTst1.pas' {SocksTestForm};

{$R *.RES}

begin
  Application.CreateForm(TSocksTestForm, SocksTestForm);
  Application.Run;
end.
