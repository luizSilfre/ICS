program OverbyteIcsThrdSrvV3;

uses
  Forms,
  OverbyteIcsThrdSrvV3_1 in 'OverbyteIcsThrdSrvV3_1.pas' {ThrdSrvForm};

{$R *.RES}

begin
  Application.CreateForm(TThrdSrvForm, ThrdSrvForm);
  Application.Run;
end.
