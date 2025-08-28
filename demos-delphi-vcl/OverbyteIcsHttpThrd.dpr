program OverbyteIcsHttpThrd;

uses
  Forms, 
  OverbyteIcsHttpThr1 in 'OverbyteIcsHttpThr1.pas' {HttpThreadForm},
  OverbyteIcsHttpThr2 in 'OverbyteIcsHttpThr2.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(THttpThreadForm, HttpThreadForm);
  Application.Run;
end.
