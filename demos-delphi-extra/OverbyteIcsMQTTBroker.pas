{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Description:  MQTT Broker Display.
Creation:     March 2009
Updated:      Feb 2023
Version:      8.71
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2008-2023 by pjde and Geoffrey Smith,
              https://github.com/pjde/delphi-mqtt
              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Feb 24th 2023 V8.71 Baseline incorporated into ICS samples.

WARNING - this sample needs the VirtualTree component installed from GetIt or
https://github.com/TurboPack/VirtualTreeView, otherwise remove the
OverbyteIcsMQTTBroker unit and all it's references.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMQTTBroker;

{$IF CompilerVersion < 15}
  {$MESSAGE FATAL 'This demo requires at least Delphi 7 or better'};
{$IFEND}

{$B-}                                 { Enable partial boolean evaluation   }
{$T-}                                 { Untyped pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long strings                    }
{$J+}                                 { Allow typed constant to be modified }
{ If you use Delphi 7, you may wants to disable warnings for unsage type,   }
{ unsafe code and unsafe typecast in the project options. Those warning are }
{ intended for .NET programs. You may also want to turn off deprecated      }
{ symbol and platform symbol warnings.                                      }

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Buttons, Types, StdCtrls,
  OverbyteIcsMQTT,
  VirtualTrees;

type
  TDataRec = record
    Broker : TIcsMQTTClient;
  end;
  PDataRec = ^TDataRec;

  TBrokerForm = class(TForm)
    MonTree: TVirtualStringTree;
    IPTxt: TEdit;
    PortTxt: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    DelBtn: TButton;
    AddBtn: TButton;
    BitBtn1: TBitBtn;
    StartBtn: TButton;
    StopBtn: TButton;
    procedure FormCreate(Sender: TObject);
    procedure MonTreeGetNodeDataSize(Sender: TBaseVirtualTree;
      var NodeDataSize: Integer);
    procedure FormShow(Sender: TObject);
    procedure AddBtnClick(Sender: TObject);
    procedure MonTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure BitBtn1Click(Sender: TObject);
    procedure DelBtnClick(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure IPTxtChange(Sender: TObject);
    procedure MonTreeChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
  private
    { Private declarations }
    procedure UpdateBtnStatus;

  public
    { Public declarations }
    FServer : TIcsMQTTServer;
    procedure Sync;
    procedure RefreshTree;
  end;

var
  BrokerForm: TBrokerForm;

procedure ShowBrokerForm (anOwner : TComponent; aServer : TIcsMQTTServer);

implementation


{$R *.dfm}

procedure ShowBrokerForm (anOwner : TComponent; aServer : TIcsMQTTServer);
var
  aForm : TBrokerForm;
begin
  aForm := TBrokerForm.Create (anOwner);
  aForm.FServer := aServer;
  aForm.Show;
end;

{ TForm2 }

procedure TBrokerForm.AddBtnClick(Sender: TObject);
var
  aBroker : TIcsMQTTClient;
  aNode : PVirtualNode;
  aData : PDataRec;
begin
  if FServer = nil then exit;
  aBroker := FServer.AddBroker (IPTxt.Text, StrToIntDef (PortTxt.Text, 1883));
  Sync;
  aNode := MonTree.GetFirst (false);
  while aNode <> nil do
    begin
      aData := MonTree.GetNodeData (aNode);
      if aData.Broker = aBroker then
        begin
          MonTree.Selected[aNode] := true;
          aNode := nil;
        end
      else
        aNode := MonTree.GetNext (aNode, false);
    end;
end;

procedure TBrokerForm.BitBtn1Click (Sender: TObject);
begin
  hide;
end;

procedure TBrokerForm.DelBtnClick(Sender: TObject);
var
  aNode : PVirtualNode;
  aData : PDataRec;
begin
  if FServer = nil then exit;
  aNode := MonTree.GetFirstSelected (false);
  if aNode = nil then exit;
  aData := MonTree.GetNodeData (aNode);
  FServer.Brokers.Remove (aData.Broker);
  aData.Broker.Free;
  Sync;
end;

procedure TBrokerForm.FormCreate (Sender: TObject);
begin
  FServer := nil;
end;

procedure TBrokerForm.FormShow (Sender: TObject);
begin
  Sync;
  UpdateBtnStatus;
end;

procedure TBrokerForm.IPTxtChange(Sender: TObject);
begin
  UpdateBtnStatus;
end;

procedure TBrokerForm.MonTreeChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  UpdateBtnStatus;
end;

procedure TBrokerForm.MonTreeGetNodeDataSize(Sender: TBaseVirtualTree;
  var NodeDataSize: Integer);
begin
   NodeDataSize := SizeOf (TDataRec);
end;

procedure TBrokerForm.MonTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
const
  ny : array [boolean] of string = ('NO', 'YES');
var
  aData : PDataRec;
begin
  aData := Sender.GetNodeData (Node);
  if FServer = nil then
    CellText := ''
  else if FServer.Brokers.IndexOf (aData.Broker) >= 0 then
    begin
      case Column of
        0 : CellText := aData.Broker.Host;
        1 : CellText := IntToStr (aData.Broker.Port);
        2 : Celltext := ny[aData.Broker.Enabled];
        3 : CellText := ny[aData.Broker.Online];
       end;
    end
  else
    CellText := '';
end;

procedure TBrokerForm.RefreshTree;
begin
  MonTree.Invalidate;
end;

procedure TBrokerForm.StartBtnClick(Sender: TObject);
var
  aNode : PVirtualNode;
  aData : PDataRec;
begin
  if FServer = nil then exit;
  aNode := MonTree.GetFirstSelected (false);
  if aNode = nil then exit;
  aData := MonTree.GetNodeData (aNode);
  aData.Broker.Activate (true);
  MonTree.Invalidate;
end;

procedure TBrokerForm.StopBtnClick(Sender: TObject);
var
  aNode : PVirtualNode;
  aData : PDataRec;
begin
  if FServer = nil then exit;
  aNode := MonTree.GetFirstSelected (false);
  if aNode = nil then exit;
  aData := MonTree.GetNodeData (aNode);
  aData.Broker.Activate (false);
  MonTree.Invalidate;
end;

procedure TBrokerForm.Sync;
var
  i, x : integer;
  aData : PDataRec;
  aNode, bNode : PVirtualNode;
begin
  if FServer = nil then
    begin
      MonTree.Clear;
      exit;
    end;
  MonTree.BeginUpdate;
  x := 0;
  aNode := MonTree.GetFirst (false);
  while (aNode <> nil) and (x < FServer.Brokers.Count)  do
    begin
      aData := MonTree.GetNodeData (aNode);
      aData.Broker := FServer.Brokers[x];
      x := x + 1;
      aNode := MonTree.GetNext (aNode, false);
    end;
  if aNode = nil then   // ran out of existing
    begin
      for i := x to FServer.Brokers.Count - 1 do
        begin
          aNode := MonTree.AddChild (nil);
          aData := MonTree.GetNodeData (aNode);
          aData.Broker := FServer.Brokers[x];
        end;
    end
  else     // delete any extra
    begin
      while aNode <> nil do
        begin
          bNode := MonTree.GetNext (aNode, false);
          MonTree.DeleteNode (aNode, false);
          aNode := bNode;
        end;
    end;
  MonTree.EndUpdate;
end;

procedure TBrokerForm.UpdateBtnStatus;
begin
  AddBtn.Enabled := (length (IPTxt.Text) > 0);
  DelBtn.Enabled := MonTree.GetFirstSelected (false) <> nil;
  StartBtn.Enabled := MonTree.GetFirstSelected (false) <> nil;
  StopBtn.Enabled := MonTree.GetFirstSelected (false) <> nil;
end;

end.
