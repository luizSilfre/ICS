program OverbyteIcsSslFtpTst;

uses
  Forms,
  OverbyteIcsSslFtpTst1 in 'OverbyteIcsSslFtpTst1.pas' {FtpReceiveForm},
  OverbyteIcsSslFtpTst2 in 'OverbyteIcsSslFtpTst2.pas' {DirectoryForm};

{$R *.RES}

begin
  Application.CreateForm(TFtpReceiveForm, FtpReceiveForm);
  Application.CreateForm(TDirectoryForm, DirectoryForm);
  Application.Run;
end.
