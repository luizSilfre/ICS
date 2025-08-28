object MimeDecodeForm: TMimeDecodeForm
  Left = 0
  Top = 41
  Caption = 'ICS MimeDecodeForm - https://www.overbyte.be - 6 July 2024'
  ClientHeight = 805
  ClientWidth = 810
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 810
    Height = 156
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 18
      Top = 105
      Width = 18
      Height = 13
      Caption = 'File'
    end
    object Label2: TLabel
      Left = 18
      Top = 10
      Width = 20
      Height = 13
      Caption = 'Text'
    end
    object FileEdit: TEdit
      Left = 49
      Top = 100
      Width = 537
      Height = 21
      TabOrder = 11
      Text = 'FileEdit'
    end
    object DecodeButton: TButton
      Left = 154
      Top = 128
      Width = 117
      Height = 21
      Caption = '&Decode File Simple'
      Default = True
      TabOrder = 14
      OnClick = DecodeButtonClick
    end
    object ClearButton: TButton
      Left = 720
      Top = 122
      Width = 73
      Height = 21
      Caption = '&Clear'
      TabOrder = 17
      OnClick = ClearButtonClick
    end
    object TextEdit: TEdit
      Left = 51
      Top = 4
      Width = 612
      Height = 21
      TabOrder = 0
      Text = 'TextEdit'
    end
    object Decode64Button: TButton
      Left = 16
      Top = 35
      Width = 75
      Height = 21
      Caption = 'Decode64'
      TabOrder = 1
      OnClick = Decode64ButtonClick
    end
    object Encode64Button: TButton
      Left = 100
      Top = 35
      Width = 75
      Height = 21
      Caption = 'Encode64'
      TabOrder = 2
      OnClick = Encode64ButtonClick
    end
    object DecAutoHeaderButton: TButton
      Left = 462
      Top = 35
      Width = 123
      Height = 21
      Caption = 'Decode &Auto Headers'
      TabOrder = 5
      OnClick = DecAutoHeaderButtonClick
    end
    object DecOneHeaderButton: TButton
      Left = 193
      Top = 35
      Width = 123
      Height = 21
      Caption = 'Decode &One Header'
      TabOrder = 3
      OnClick = DecOneHeaderButtonClick
    end
    object EncodeOneHdrButton: TButton
      Left = 325
      Top = 35
      Width = 123
      Height = 21
      Caption = '&Encode One Header'
      TabOrder = 4
      OnClick = EncodeOneHdrButtonClick
    end
    object DecodeFileExButton: TButton
      Left = 16
      Top = 129
      Width = 123
      Height = 21
      Caption = 'Decode &File Extended'
      TabOrder = 13
      OnClick = DecodeFileExButtonClick
    end
    object IgnoreBlankParts: TCheckBox
      Left = 299
      Top = 130
      Width = 109
      Height = 17
      Caption = 'Ignore Blank Parts'
      TabOrder = 15
    end
    object doCreateMimeList: TButton
      Left = 13
      Top = 68
      Width = 93
      Height = 25
      Caption = 'Mime from List'
      TabOrder = 6
      OnClick = doCreateMimeListClick
    end
    object doMimefromReg: TButton
      Left = 123
      Top = 68
      Width = 99
      Height = 25
      Caption = 'Mime from Registry'
      TabOrder = 7
      OnClick = doMimefromRegClick
    end
    object doMimeFromTypes: TButton
      Left = 238
      Top = 68
      Width = 106
      Height = 25
      Caption = 'Mime from Types File'
      TabOrder = 8
      OnClick = doMimeFromTypesClick
    end
    object doMimeTestExtn: TButton
      Left = 356
      Top = 68
      Width = 99
      Height = 25
      Caption = 'Test Extensions'
      TabOrder = 9
      OnClick = doMimeTestExtnClick
    end
    object doMimeTestContent: TButton
      Left = 466
      Top = 68
      Width = 99
      Height = 25
      Caption = 'Test Contents'
      TabOrder = 10
      OnClick = doMimeTestContentClick
    end
    object SelectFile: TBitBtn
      Left = 600
      Top = 100
      Width = 31
      Height = 25
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
        333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
        300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
        333337F373F773333333303330033333333337F3377333333333303333333333
        333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
        333337777F337F33333330330BB00333333337F373F773333333303330033333
        333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
        333377777F77377733330BBB0333333333337F337F33333333330BB003333333
        333373F773333333333330033333333333333773333333333333}
      NumGlyphs = 2
      TabOrder = 12
      OnClick = SelectFileClick
    end
    object LooseRFC: TCheckBox
      Left = 425
      Top = 130
      Width = 138
      Height = 17
      Caption = 'Loose RFC Rules'
      TabOrder = 16
    end
    object CloseButton: TButton
      Left = 720
      Top = 11
      Width = 73
      Height = 21
      Caption = '&Close'
      TabOrder = 18
      OnClick = CloseButtonClick
    end
    object doTestHttpHdrs: TButton
      Left = 573
      Top = 68
      Width = 110
      Height = 25
      Caption = 'Test HTTP Headers'
      TabOrder = 19
      OnClick = doTestHttpHdrsClick
    end
  end
  object MimeDecode1: TMimeDecodeW
    LooseRFC = True
    OnHeaderBegin = MimeDecode1HeaderBegin
    OnHeaderLine = MimeDecode1HeaderLine
    OnHeaderEnd = MimeDecode1HeaderEnd
    OnPartHeaderBegin = MimeDecode1PartHeaderBegin
    OnPartHeaderLine = MimeDecode1PartHeaderLine
    OnPartHeaderEnd = MimeDecode1PartHeaderEnd
    OnPartBegin = MimeDecode1PartBegin
    OnPartLine = MimeDecode1PartLine
    OnPartEnd = MimeDecode1PartEnd
    OnInlineDecodeBegin = MimeDecode1InlineDecodeBegin
    OnInlineDecodeLine = MimeDecode1InlineDecodeLine
    OnInlineDecodeEnd = MimeDecode1InlineDecodeEnd
    Left = 44
    Top = 211
  end
  object MimeDecodeEx1: TMimeDecodeEx
    MaxParts = 10
    SkipBlankParts = False
    LooseRFC = True
    Left = 148
    Top = 214
  end
  object MimeTypesList1: TMimeTypesList
    LoadOSonDemand = True
    MimeTypesFile = '/etc/mime.types'
    DefaultTypes.Strings = (
      '.htm=text/html'
      '.html=text/html'
      '.gif=image/gif'
      '.bmp=image/bmp'
      '.jpg=image/jpeg'
      '.jpeg=image/jpeg'
      '.tif=image/tiff'
      '.tiff=image/tiff'
      '.txt=text/plain'
      '.css=text/css'
      '.wav=audio/x-wav'
      '.ico=image/x-icon'
      '.wml=text/vnd.wap.wml'
      '.wbmp=image/vnd.wap.wbmp'
      '.wmlc=application/vnd.wap.wmlc'
      '.wmlscript=text/vnd.wap.wmlscript'
      '.wmlscriptc=application/vnd.wap.wmlscriptc'
      '.pdf=application/pdf'
      '.png=image/png'
      '.xml=application/xml'
      '.xhtml=application/xhtml+xml'
      '.zip=application/zip'
      '.exe=application/x-msdownload'
      '.msi=application/x-msdownload'
      '.bin=application/octet-stream'
      '.iso=application/octet-stream')
    MimeTypeSrc = MTypeList
    UnknownType = 'application/octet-stream'
    Left = 254
    Top = 216
  end
  object OpenDialog: TOpenDialog
    Left = 358
    Top = 226
  end
end
