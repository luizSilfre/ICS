object FormLog: TFormLog
  Left = 0
  Top = 0
  Caption = 'Network Tools Log Window'
  ClientHeight = 851
  ClientWidth = 770
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnClose = FormClose
  OnShow = FormShow
  TextHeight = 15
  object Log: TMemo
    Left = 0
    Top = 0
    Width = 770
    Height = 851
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Lucida Console'
    Font.Style = []
    Lines.Strings = (
      'Log')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitLeft = -107
    ExplicitTop = -14
    ExplicitWidth = 588
    ExplicitHeight = 676
  end
end
