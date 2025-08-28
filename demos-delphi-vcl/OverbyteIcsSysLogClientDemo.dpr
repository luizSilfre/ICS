program OverbyteIcsSysLogClientDemo;

uses
  Forms,
  OverbyteIcsSysLogClientDemo1 in 'OverbyteIcsSysLogClientDemo1.pas' {SysLogClientForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSysLogClientForm, SysLogClientForm);
  Application.Run;
end.
