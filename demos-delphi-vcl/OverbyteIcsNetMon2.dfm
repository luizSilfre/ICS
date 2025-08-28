object FormOneRow: TFormOneRow
  Left = 0
  Top = 0
  Caption = 'One Capture Packet'
  ClientHeight = 662
  ClientWidth = 536
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 15
  object ListOneRow: TListView
    Left = 0
    Top = 0
    Width = 536
    Height = 662
    Align = alClient
    Columns = <
      item
        Caption = 'Title'
        Width = 120
      end
      item
        Caption = 'Value'
        Width = 600
      end>
    GridLines = True
    Items.ItemData = {
      058A0000000300000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
      000D5000610063006B006500740020004C0065006E0067007400680000000000
      FFFFFFFFFFFFFFFF00000000FFFFFFFF0000000011450074006800650072006E
      00650074002000500072006F0074006F0063006F006C0000000000FFFFFFFFFF
      FFFFFF00000000FFFFFFFF0000000000}
    ReadOnly = True
    TabOrder = 0
    ViewStyle = vsReport
    ExplicitWidth = 481
  end
end
