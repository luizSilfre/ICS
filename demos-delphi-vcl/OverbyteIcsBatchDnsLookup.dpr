program OverbyteIcsBatchDnsLookup;

uses
  Forms,
  OverbyteIcsBatchDnsLookup1 in 'OverbyteIcsBatchDnsLookup1.pas' {BatchDnsLookupForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TBatchDnsLookupForm, BatchDnsLookupForm);
  Application.Run;
end.
