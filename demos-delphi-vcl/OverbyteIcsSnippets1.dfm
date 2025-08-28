object Snippets: TSnippets
  Left = 0
  Top = 0
  Caption = 
    'ICS Snippets, small samples of codes for FTP, HTTP, sockets and ' +
    'email - https://www.overbyte.be - 5 Nov 2024'
  ClientHeight = 699
  ClientWidth = 944
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 944
    Height = 366
    Align = alTop
    Alignment = taLeftJustify
    TabOrder = 0
    object Label1: TLabel
      Left = 12
      Top = 277
      Width = 108
      Height = 15
      Caption = 'Temporary Directory'
    end
    object LabelProgress: TLabel
      Left = 17
      Top = 305
      Width = 557
      Height = 50
      AutoSize = False
      Caption = 'LabelProgress'
      Color = clYellow
      ParentColor = False
      Transparent = False
      WordWrap = True
    end
    object Label2: TLabel
      Left = 546
      Top = 11
      Width = 390
      Height = 239
      AutoSize = False
      Caption = 
        'The application contains snippets of code to illustrate many of ' +
        'the newer ICS components, mostly written self contained within a' +
        ' single method, with most of the parameters also hard coded in t' +
        'he method, file names, URLs, logins, etc. Most snippets support ' +
        'SSL/TLS automatically using OpenSSL which is usually linked into' +
        ' the sample '#13#10#13#10'Most of the snippets access Magenta Systems Ltd ' +
        'public ICS web and FTP servers and should just work without chan' +
        'ge, except for FTP uploading where you will need to request an a' +
        'ccount by emailing delphi@magsys.co.uk  '#13#10#13#10'Please change any of' +
        ' the hard coded settings or Temp Dir  to your own public or priv' +
        'ate servers and logins, but beware installing new versions of IC' +
        'S may overwrite this application. '
      WordWrap = True
    end
    object doFtpDownOneFile: TButton
      Left = 10
      Top = 15
      Width = 167
      Height = 25
      Caption = 'FTP Download One File'
      TabOrder = 0
      OnClick = doFtpDownOneFileClick
    end
    object doFileCopyOneFile: TButton
      Left = 10
      Top = 75
      Width = 167
      Height = 25
      Caption = 'File Copy One File'
      TabOrder = 1
      OnClick = doFileCopyOneFileClick
    end
    object DirTemp: TEdit
      Left = 126
      Top = 274
      Width = 260
      Height = 23
      TabOrder = 2
      Text = 'c:\temp-ics'
    end
    object ShowDiags: TCheckBox
      Left = 408
      Top = 278
      Width = 141
      Height = 17
      Caption = 'Show Diagnostics'
      TabOrder = 3
    end
    object doAbort: TButton
      Left = 610
      Top = 275
      Width = 75
      Height = 25
      Caption = 'Abort'
      TabOrder = 4
      OnClick = doAbortClick
    end
    object doClose: TButton
      Left = 781
      Top = 275
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 5
      OnClick = doCloseClick
    end
    object doFtpDownMultiFiles: TButton
      Left = 190
      Top = 15
      Width = 167
      Height = 25
      Caption = 'FTP Download Multiple Files'
      TabOrder = 6
      OnClick = doFtpDownMultiFilesClick
    end
    object doFileCopyMultiFiles: TButton
      Left = 190
      Top = 75
      Width = 167
      Height = 25
      Caption = 'File Copy Multiple Files'
      TabOrder = 7
      OnClick = doFileCopyMultiFilesClick
    end
    object doFtpUpOneFile: TButton
      Left = 10
      Top = 45
      Width = 167
      Height = 25
      Caption = 'FTP Upload One File'
      TabOrder = 8
      OnClick = doFtpUpOneFileClick
    end
    object doFtpUpMultiFiles: TButton
      Left = 190
      Top = 45
      Width = 167
      Height = 25
      Caption = 'FTP Upload Multiple Files'
      TabOrder = 9
      OnClick = doFtpUpMultiFilesClick
    end
    object doFtpViewDirs: TButton
      Left = 370
      Top = 15
      Width = 167
      Height = 25
      Caption = 'FTP View Directories'
      TabOrder = 10
      OnClick = doFtpViewDirsClick
    end
    object doHttpSimpleUpload: TButton
      Left = 11
      Top = 135
      Width = 167
      Height = 25
      Caption = 'HTTP Simple Upload File'
      TabOrder = 11
      OnClick = doHttpSimpleUploadClick
    end
    object doHttpDownList: TButton
      Left = 10
      Top = 105
      Width = 167
      Height = 25
      Caption = 'HTTP Download List of Files'
      TabOrder = 12
      OnClick = doHttpDownListClick
    end
    object doHttpDownLinked: TButton
      Left = 190
      Top = 105
      Width = 167
      Height = 25
      Caption = 'HTTP Download Linked Files'
      TabOrder = 13
      OnClick = doHttpDownLinkedClick
    end
    object doFileDirView: TButton
      Left = 370
      Top = 75
      Width = 167
      Height = 25
      Caption = 'View Local Directories'
      TabOrder = 14
      OnClick = doFileDirViewClick
    end
    object doHttpFormUpload: TButton
      Left = 190
      Top = 135
      Width = 167
      Height = 25
      Caption = 'HTTP Form Upload File'
      TabOrder = 15
      OnClick = doHttpFormUploadClick
    end
    object doHttpRestReq: TButton
      Left = 370
      Top = 105
      Width = 167
      Height = 25
      Caption = 'HTTP REST Json Request'
      TabOrder = 16
      OnClick = doHttpRestReqClick
    end
    object doEmailMailQu: TButton
      Left = 9
      Top = 165
      Width = 167
      Height = 25
      Caption = 'Send Email using MailQu'
      TabOrder = 17
      OnClick = doEmailMailQuClick
    end
    object doSocketLocal: TButton
      Left = 10
      Top = 195
      Width = 167
      Height = 25
      Caption = 'Local Socket Traffic'
      TabOrder = 18
      OnClick = doSocketLocalClick
    end
    object doClearLog: TButton
      Left = 695
      Top = 275
      Width = 75
      Height = 25
      Caption = 'Clear Log'
      TabOrder = 19
      OnClick = doClearLogClick
    end
    object FtpOnlyCheck: TCheckBox
      Left = 370
      Top = 44
      Width = 162
      Height = 25
      Caption = 'Only Check Multi File Xfers'
      Checked = True
      State = cbChecked
      TabOrder = 20
      WordWrap = True
    end
    object doSocketRemote: TButton
      Left = 190
      Top = 195
      Width = 167
      Height = 25
      Caption = 'Remote Socket Traffic'
      TabOrder = 21
      OnClick = doSocketRemoteClick
    end
    object doWebSocket: TButton
      Left = 190
      Top = 165
      Width = 167
      Height = 25
      Caption = 'WebSocket Client'
      TabOrder = 22
      OnClick = doWebSocketClick
    end
    object doHttpRestDown: TButton
      Left = 370
      Top = 135
      Width = 167
      Height = 25
      Caption = 'HTTP REST Download'
      TabOrder = 23
      OnClick = doHttpRestDownClick
    end
    object doHttpGetParams: TButton
      Left = 370
      Top = 165
      Width = 167
      Height = 25
      Caption = 'HTTP Get Params'
      TabOrder = 24
      OnClick = doHttpGetParamsClick
    end
    object doHttpPostParams: TButton
      Left = 370
      Top = 195
      Width = 167
      Height = 25
      Caption = 'HTTP Post Params'
      TabOrder = 25
      OnClick = doHttpPostParamsClick
    end
  end
  object LogWin: TMemo
    Left = 0
    Top = 366
    Width = 944
    Height = 333
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Lucida Console'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
  end
end
