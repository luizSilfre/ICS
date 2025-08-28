program IcsPemTest;

uses
  FMX.Forms,
  FMX.Types,
  IcsPemTest1 in 'IcsPemTest1.pas' {ClientForm};

{$R *.res}

begin
  Application.Initialize;
  GlobalDisableFocusEffect := TRUE;
  Application.CreateForm(TClientForm, ClientForm);
  Application.Run;
end.
