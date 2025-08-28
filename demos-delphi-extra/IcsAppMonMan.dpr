program IcsAppMonMan;

uses
  Vcl.Forms,
  IcsAppMonMan1 in 'IcsAppMonMan1.pas' {ManForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TManForm, ManForm);
  Application.Run;
end.
