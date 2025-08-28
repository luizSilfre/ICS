program OverbyteIcsNetTools;

uses
  Forms,
  OverbyteIcsNetTools1 in 'OverbyteIcsNetTools1.pas' {ToolsForm},
  OverbyteIcsNetTools2 in 'OverbyteIcsNetTools2.pas' {FormLog};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TToolsForm, ToolsForm);
  Application.CreateForm(TFormLog, FormLog);
  Application.Run;
end.
