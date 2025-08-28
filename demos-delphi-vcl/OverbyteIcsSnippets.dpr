program OverbyteIcsSnippets;

uses
  Forms,
  OverbyteIcsSnippets1 in 'OverbyteIcsSnippets1.pas' {Snippets};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TSnippets, Snippets);
  Application.Run;
end.
