program OverbyteIcsSysLogServerDemo;

uses
  Forms,
  OverbyteIcsSysLogServerDemo1 in 'OverbyteIcsSysLogServerDemo1.pas' {SysLogServerForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSysLogServerForm, SysLogServerForm);
  Application.Run;
end.
