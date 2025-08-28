unit OverbyteIcsPemtool2;

// Aug 2021 - never close window, only hide it
//            restore left and width
// June 2023 - save position in main unit

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmPemTool2 = class(TForm)
    Memo1: TMemo;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Memo1DblClick(Sender: TObject);
  private
  public
    { Public-Deklarationen }
  end;

var
  frmPemTool2: TfrmPemTool2;

implementation

{$R *.DFM}

uses
   OverbyteIcsPemTool1;

procedure TfrmPemTool2.FormShow(Sender: TObject);
begin
//
end;

procedure TfrmPemTool2.Memo1DblClick(Sender: TObject);
begin
    Memo1.Lines.Clear;   { V8.67 }
    Hide;                { V8.67 }
end;

procedure TfrmPemTool2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caHide;
end;

end.
