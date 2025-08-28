program OverbyteIcsDllTst;

uses
  Forms, 
  OverbyteIcsDllTst1 in 'OverbyteIcsDllTst1.pas' {DllTestForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDllTestForm, DllTestForm);
  Application.Run;
end.
