{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  ICS HTTPS REST functions demo display form.
Creation:     Nov 2019
Updated:      Oct 2019
Version:      8.65
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      https://en.delphipraxis.net/forum/37-ics-internet-component-suite/
Legal issues: Copyright (C) 2022 by Angus Robertson, Magenta Systems Ltd,
              Croydon, England. delphi@magsys.co.uk, https://www.magsys.co.uk/delphi/

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
Nov 11, 2019 - V8.63 Basline
Dec 09, 2019 - V8.64 Allow clicking on nested arrays.
Nov 18, 2020 - V8.65 Don't try and display Json twice.
                     Allow clicking on Json array line to display only that record.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit IcsHttpRestTstFmx2;

{$I Include\OverbyteIcsDefs.inc}
{$DEFINE FMX}

interface

uses
  WinApi.Windows,Messages,System.SysUtils,System.Variants,System.Classes,
  FMX.Controls,FMX.Forms,FMX.Dialogs,System.Types,System.UITypes,FMX.Types,
  FMX.Layouts,FMX.Edit, FMX.ListBox, FMX.Controls.Presentation,
  FMX.StdCtrls, System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox,
  TypInfo,
  OverbyteIcsSuperObject;

type
  TJsonObjectWin = class(TForm)
    GridSubResp: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure GridSubRespCellDblClick(const Column: TColumn; const Row: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DispJson(const JsonStr: WideString);
  end;

var
  JsonObjectWin: TJsonObjectWin;
  JArrayTot: Integer;
  JArrayItems: Array of String;

implementation

{$R *.fmx}

Uses IcsHttpRestTstFmx1;

procedure TJsonObjectWin.FormClose(Sender: TObject; var Action: TCloseAction);
begin
//    Action := caHide;
    JArrayTot := 0;
    SetLength(JArrayItems, 0);
end;

procedure TJsonObjectWin.FormCreate(Sender: TObject);
begin
//
end;


procedure TJsonObjectWin.DispJson(const JsonStr: WideString);
var
    CVal: String;
    JsonObj, JsonRow: ISuperObject;
    JsonEnum: TSuperAvlIterator;
    JsonItem: TSuperAvlEntry;
    FirstRow: Boolean;
    I, J, CWid: Integer;
begin
    try
        JArrayTot := 0;
        SetLength(JArrayItems, 0);
        if (Pos ('{', JsonStr) <> 1) and (Pos ('[', JsonStr) <> 1) then Exit;
        JsonObj := TSuperObject.ParseString(PWideChar(JsonStr), True);
        GridSubResp.RowCount := 0;
        for J := 0 to GridSubResp.ColumnCount - 1 do
            GridSubResp.Columns[J].Header := '';
        if NOT Assigned(JsonObj) then begin
            HttpRestForm.AddLog('Failed to parse Json');
            Exit;
        end;
        Visible := True;
        BringToFront;
        if JsonObj.DataType = stArray then begin
            JArrayTot := JsonObj.AsArray.Length;
            if JArrayTot = 0 then Exit;
            SetLength(JArrayItems, JArrayTot);
            FirstRow := True;
            for I := 0 to JArrayTot - 1 do begin
                JsonRow := JsonObj.AsArray[I];
                JArrayItems[I] := JsonObj.AsArray[I].AsString;   { keep lines so we display them later }
                GridSubResp.RowCount := GridSubResp.RowCount + 1;
                if JsonRow.DataType = stObject then begin
                    JsonEnum := JsonRow.AsObject.GetEnumerator;
                    J := 0 ;
                    while JsonEnum.MoveNext do begin
                        JsonItem := JsonEnum.GetIter;
                        if NOT Assigned(JsonItem) then continue;
                        CVal := JsonItem.Value.AsString;
                        if FirstRow then begin
                          if (GridSubResp.ColumnCount <= J)  then   // do we need more blank columns?
                                GridSubResp.AddObject(GridSubResp.Columns[0].Clone(Self));
                            CWid := (Length(CVal) * 5) + 30;
                            if CWid > 400 then CWid := 400;
                            GridSubResp.Columns[J].Header := JsonItem.Name;
                            GridSubResp.Columns[J].Width := CWid;
                        end;
                        GridSubResp.Cells[J, I] := CVal;
                        J := J + 1;
                    end;
                end

             // not Json object, single column
                else begin
                    CVal := JsonRow.AsString;
                    if FirstRow then begin
                        GridSubResp.Columns[0].Header := 'Value';
                        GridSubResp.Columns[0].Width := 1000;
                    end;
                    GridSubResp.Cells[0, I] := CVal;
                end;
                FirstRow := False;
            end;
        end;

        if JsonObj.DataType = stObject then begin
         // note that values containing objects are displayed as raw Json
            GridSubResp.Columns[0].Header := 'Name';
            GridSubResp.Columns[0].Width := 100;
            GridSubResp.Columns[1].Header :=  'Type';
            GridSubResp.Columns[1].Width := 70;
            GridSubResp.Columns[2].Header := 'Value';
            GridSubResp.Columns[2].Width := 1000;
            GridSubResp.Columns[3].Header := '';
            GridSubResp.Columns[3].Width := 100;
            JsonEnum := JsonObj.AsObject.GetEnumerator;
            try
                I := 0;
                while JsonEnum.MoveNext do begin
                    JsonItem := JsonEnum.GetIter;
                    GridSubResp.RowCount := GridSubResp.RowCount + 1;
                    GridSubResp.Cells[0, I] := JsonItem.Name;
                    GridSubResp.Cells[1, I] := GetEnumName(TypeInfo(TSuperType), Ord(JsonItem.Value.DataType));
                    GridSubResp.Cells[2, I] := JsonItem.Value.AsString;
                    I := I + 1;
                end;
            finally
                JsonEnum.Free;
            end;
        end;
    except
        on E:Exception do
             HttpRestForm.AddLog('Error parsing Json: ' + E.Message);
    end;
end;

procedure TJsonObjectWin.GridSubRespCellDblClick(const Column: TColumn; const Row: Integer);
var
    MyName, MyType, MyValue: String;
begin
    try
        if Row < 0 then Exit;
        if (JArrayTot > 0) and (Length(JArrayItems) = JArrayTot) then begin
            DispJson(JArrayItems[Row]);
        end
        else begin
            MyName := GridSubResp.Cells[0, Row];
            MyType := GridSubResp.Cells[1, Row];
            MyValue := GridSubResp.Cells[2, Row];
            if (MyType = 'stArray') or (MyType = 'stObject') then { objects }
                DispJson(MyValue)
            else if (Pos ('{', MyName) = 1) or (Pos ('[', MyName) = 1) then
                DispJson(MyName);
        end;
    except
        on E:Exception do
             HttpRestForm.AddLog('Error finding Json: ' + E.Message);
    end;
end;


end.
