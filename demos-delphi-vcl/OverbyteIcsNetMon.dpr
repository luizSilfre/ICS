program OverbyteIcsNetMon;

uses
  Forms,
  OverbyteIcsNetMon1 in 'OverbyteIcsNetMon1.pas' {MonForm},
  OverbyteIcsNetMon2 in 'OverbyteIcsNetMon2.pas' {FormOneRow};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMonForm, MonForm);
  Application.CreateForm(TFormOneRow, FormOneRow);
  Application.Run;
end.
