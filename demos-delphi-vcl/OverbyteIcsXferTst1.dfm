object Form1: TForm1
  Left = 196
  Top = 51
  Caption = 
    'ICS Multi File Transfer Demo -  https://www.overbyte.eu  - 11 Fe' +
    'b 2025'
  ClientHeight = 806
  ClientWidth = 922
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 14
  object Label1: TLabel
    Left = 460
    Top = 2
    Width = 418
    Height = 42
    Caption = 
      'The demonstration application is designed to show how the three ' +
      'multi file rransfer components are used from code, there are num' +
      'erous properties at are set in code, not through the GUI.  '
    WordWrap = True
  end
  object LabelProgress: TLabel
    Left = 8
    Top = 346
    Width = 890
    Height = 46
    AutoSize = False
    Caption = 'Progress:'
    WordWrap = True
  end
  object LabelSslState: TLabel
    Left = 610
    Top = 197
    Width = 161
    Height = 34
    AutoSize = False
    Caption = 'LabelSslState'
    WordWrap = True
  end
  object Label26: TLabel
    Left = 460
    Top = 186
    Width = 119
    Height = 42
    Caption = 
      'This demo supports full Unicode when built with Delphi 2009 and ' +
      'later.  '
    WordWrap = True
  end
  object lbl1: TLabel
    Left = 713
    Top = 150
    Width = 92
    Height = 14
    Caption = 'SSL Client Security'
  end
  object Label29: TLabel
    Left = 460
    Top = 55
    Width = 65
    Height = 14
    Caption = 'Log Directory'
  end
  object LogText: TMemo
    Left = 0
    Top = 535
    Width = 922
    Height = 271
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 5
    Width = 440
    Height = 335
    ActivePage = TabSheet1
    MultiLine = True
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'TIcsFileCopy'
      object Label2: TLabel
        Left = 3
        Top = 0
        Width = 82
        Height = 14
        Caption = 'Source Directory'
      end
      object Label3: TLabel
        Left = 5
        Top = 50
        Width = 82
        Height = 14
        Caption = 'Source File Mask'
      end
      object Label4: TLabel
        Left = 5
        Top = 70
        Width = 77
        Height = 14
        Caption = 'Target Directory'
      end
      object Label19: TLabel
        Left = 5
        Top = 120
        Width = 30
        Height = 14
        Caption = 'Logon'
      end
      object Label20: TLabel
        Left = 170
        Top = 118
        Width = 50
        Height = 14
        Caption = 'Password'
      end
      object Label27: TLabel
        Left = 158
        Top = 52
        Width = 77
        Height = 14
        Caption = 'Ignore Directory'
      end
      object Label31: TLabel
        Left = 6
        Top = 150
        Width = 81
        Height = 14
        Caption = 'File Replacement'
      end
      object doCopyCheck: TButton
        Left = 30
        Top = 221
        Width = 101
        Height = 25
        Caption = 'Check Copy Files'
        TabOrder = 13
        OnClick = CopyFiles
      end
      object doCopyList: TButton
        Left = 30
        Top = 190
        Width = 101
        Height = 25
        Caption = 'List Source Files'
        TabOrder = 12
        OnClick = doCopyListClick
      end
      object CopySrcDir: TEdit
        Left = 3
        Top = 20
        Width = 416
        Height = 22
        TabOrder = 0
        Text = 'c:\windows\system32'
      end
      object CopySrcFile: TEdit
        Left = 93
        Top = 50
        Width = 52
        Height = 22
        TabOrder = 1
        Text = '*.txt'
      end
      object doCopyFiles: TButton
        Left = 30
        Top = 252
        Width = 101
        Height = 25
        Caption = 'Copy Files'
        TabOrder = 14
        OnClick = CopyFiles
      end
      object CopyTarDir: TEdit
        Left = 5
        Top = 85
        Width = 416
        Height = 22
        TabOrder = 3
        Text = 'c:\tempfiles'
      end
      object doCopyAbort: TButton
        Left = 150
        Top = 190
        Width = 101
        Height = 25
        Caption = 'Abort'
        Enabled = False
        TabOrder = 15
        OnClick = doAbortClick
      end
      object doDeleteCheck: TButton
        Left = 150
        Top = 221
        Width = 101
        Height = 25
        Caption = 'Check Delete Files'
        TabOrder = 16
        OnClick = DeleteFiles
      end
      object doDeleteFiles: TButton
        Left = 150
        Top = 252
        Width = 101
        Height = 25
        Caption = 'Delete Target Files'
        TabOrder = 17
        OnClick = DeleteFiles
      end
      object NetLogon: TEdit
        Left = 45
        Top = 115
        Width = 111
        Height = 22
        TabOrder = 4
      end
      object NetPassword: TEdit
        Left = 230
        Top = 115
        Width = 101
        Height = 22
        PasswordChar = '*'
        TabOrder = 5
      end
      object CopySubdirs: TCheckBox
        Left = 270
        Top = 150
        Width = 136
        Height = 17
        Caption = 'Include Sub Directories'
        Checked = True
        State = cbChecked
        TabOrder = 7
      end
      object CopyEmptyDirs: TCheckBox
        Left = 270
        Top = 170
        Width = 136
        Height = 17
        Caption = 'Copy Empty Directories'
        TabOrder = 8
      end
      object CopyWow64Disable: TCheckBox
        Left = 270
        Top = 190
        Width = 136
        Height = 17
        Caption = 'Disable Wow64 Redirect'
        TabOrder = 9
      end
      object CopyIgnorePath: TEdit
        Left = 242
        Top = 50
        Width = 174
        Height = 22
        TabOrder = 2
        Text = 'c:\temp\'
      end
      object CopyFileRepl: TComboBox
        Left = 107
        Top = 145
        Width = 113
        Height = 22
        TabOrder = 6
        Text = ' Never'
        Items.Strings = (
          'Never'
          'Always'
          'If Different'
          'If Newer')
      end
      object CopyZip: TCheckBox
        Left = 270
        Top = 210
        Width = 136
        Height = 17
        Caption = 'Zip Files During Copy'
        TabOrder = 10
      end
      object CopyUnzip: TCheckBox
        Left = 270
        Top = 230
        Width = 136
        Height = 17
        Caption = 'Unzip Copied Files'
        TabOrder = 11
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TIcsFtpMulti'
      ImageIndex = 1
      object Label9: TLabel
        Left = 5
        Top = 10
        Width = 43
        Height = 14
        Caption = 'FTP Host'
      end
      object Label10: TLabel
        Left = 5
        Top = 40
        Width = 30
        Height = 14
        Caption = 'Logon'
      end
      object Label11: TLabel
        Left = 199
        Top = 40
        Width = 50
        Height = 14
        Caption = 'Password'
      end
      object Label21: TLabel
        Left = 5
        Top = 70
        Width = 74
        Height = 14
        Caption = 'Secure Server:'
      end
      object Label22: TLabel
        Left = 110
        Top = 100
        Width = 40
        Height = 14
        Caption = 'FTP Port'
      end
      object Label23: TLabel
        Left = 220
        Top = 100
        Width = 42
        Height = 14
        Caption = 'SSL Port'
      end
      object Label24: TLabel
        Left = 5
        Top = 130
        Width = 183
        Height = 14
        Caption = 'Noop Keep Alive (minutes, 0 for none)'
      end
      object Label25: TLabel
        Left = 145
        Top = 151
        Width = 133
        Height = 14
        Caption = 'Bandwidth Limit (KBits/sec)'
      end
      object FtpUsername: TEdit
        Left = 41
        Top = 35
        Width = 143
        Height = 22
        TabOrder = 1
        Text = 'anonymous'
      end
      object FtpPassword: TEdit
        Left = 255
        Top = 35
        Width = 161
        Height = 22
        PasswordChar = '*'
        TabOrder = 2
        Text = 'test@'
      end
      object FtpServerType: TComboBox
        Left = 90
        Top = 65
        Width = 241
        Height = 20
        Style = csOwnerDrawFixed
        ItemHeight = 14
        TabOrder = 3
      end
      object FtpHost: TComboBox
        Left = 55
        Top = 5
        Width = 361
        Height = 22
        TabOrder = 0
        Text = 'www.magsys.co.uk'
        Items.Strings = (
          'www.magsys.co.uk'
          'ics.ftptest.org'
          'filezilla.ftptest.org'
          'msftp7.ftptest.org')
      end
      object FtpPassive: TCheckBox
        Left = 5
        Top = 100
        Width = 97
        Height = 17
        Caption = 'Passive Mode'
        TabOrder = 4
      end
      object FtpPort: TEdit
        Left = 165
        Top = 95
        Width = 46
        Height = 22
        TabOrder = 5
        Text = '21'
      end
      object FtpPortSsl: TEdit
        Left = 270
        Top = 95
        Width = 46
        Height = 22
        TabOrder = 6
        Text = '990'
      end
      object FtpKeepAlive: TEdit
        Left = 203
        Top = 125
        Width = 46
        Height = 22
        TabOrder = 7
        Text = '10'
      end
      object FtpNoFeatCmd: TCheckBox
        Left = 5
        Top = 155
        Width = 126
        Height = 17
        Caption = 'No FEAT Command'
        TabOrder = 8
      end
      object FtpBandWidth: TEdit
        Left = 315
        Top = 147
        Width = 46
        Height = 22
        TabOrder = 15
        Text = '0'
      end
      object FtpNoZlib: TCheckBox
        Left = 5
        Top = 175
        Width = 126
        Height = 17
        Caption = 'No Mode Z Compress'
        TabOrder = 9
      end
      object FtpNoTmpFile: TCheckBox
        Left = 145
        Top = 175
        Width = 134
        Height = 17
        Caption = 'No TMP File for Xfers'
        TabOrder = 12
      end
      object FtpNoUtf8: TCheckBox
        Left = 285
        Top = 174
        Width = 100
        Height = 17
        Caption = 'Turn UTF8 Off'
        TabOrder = 16
      end
      object ftpNoHost: TCheckBox
        Left = 145
        Top = 195
        Width = 117
        Height = 17
        Caption = 'No HOST Command'
        TabOrder = 13
      end
      object ftpIgnoreUtf8: TCheckBox
        Left = 285
        Top = 195
        Width = 100
        Height = 17
        Caption = 'Ignore UTF8 '
        TabOrder = 17
      end
      object FtpNoMd5: TCheckBox
        Left = 5
        Top = 195
        Width = 126
        Height = 17
        Caption = 'No MD5 Check'
        TabOrder = 10
      end
      object FtpNoCrc: TCheckBox
        Left = 5
        Top = 215
        Width = 126
        Height = 17
        Caption = 'No CRC Check'
        TabOrder = 11
      end
      object FtpFixPassiveLanIP: TCheckBox
        Left = 145
        Top = 215
        Width = 146
        Height = 17
        Caption = 'Fix Passive LAN IP Addr'
        TabOrder = 14
      end
    end
    object TabSheet5: TTabSheet
      Caption = 'Multi FTP'
      ImageIndex = 4
      object Label8: TLabel
        Left = 5
        Top = 10
        Width = 21
        Height = 14
        Caption = 'Path'
      end
      object Label13: TLabel
        Left = 125
        Top = 35
        Width = 82
        Height = 14
        Caption = 'Source File Mask'
      end
      object Label12: TLabel
        Left = 5
        Top = 140
        Width = 73
        Height = 14
        Caption = 'Local Directory'
      end
      object Label28: TLabel
        Left = 143
        Top = 55
        Width = 52
        Height = 28
        Caption = 'Ignore Directories'
        WordWrap = True
      end
      object Label30: TLabel
        Left = 143
        Top = 95
        Width = 81
        Height = 14
        Caption = 'File Replacement'
      end
      object FtpPath: TEdit
        Left = 60
        Top = 5
        Width = 361
        Height = 22
        TabOrder = 0
        Text = '/'
      end
      object FtpSrcFile: TEdit
        Left = 227
        Top = 30
        Width = 194
        Height = 22
        TabOrder = 6
        Text = 'd*.zip'
      end
      object FtpLocDir: TEdit
        Left = 3
        Top = 160
        Width = 416
        Height = 22
        TabOrder = 11
        Text = 'c:\tempfiles'
      end
      object doFtpDownCheck: TButton
        Left = 123
        Top = 195
        Width = 101
        Height = 25
        Caption = 'Check Download'
        TabOrder = 13
        OnClick = FtpDownload
      end
      object doFtpDownFiles: TButton
        Left = 120
        Top = 225
        Width = 101
        Height = 25
        Caption = 'Download Files'
        TabOrder = 16
        OnClick = FtpDownload
      end
      object doFtpAbort: TButton
        Left = 350
        Top = 195
        Width = 60
        Height = 25
        Caption = 'Abort'
        Enabled = False
        TabOrder = 15
        OnClick = doAbortClick
      end
      object doFtpUpCheck: TButton
        Left = 237
        Top = 195
        Width = 101
        Height = 25
        Caption = 'Check Upload'
        TabOrder = 14
        OnClick = FtpUpload
      end
      object doFtpUpFiles: TButton
        Left = 237
        Top = 225
        Width = 101
        Height = 25
        Caption = 'Upload Files'
        TabOrder = 17
        OnClick = FtpUpload
      end
      object doFtpList: TButton
        Left = 5
        Top = 195
        Width = 101
        Height = 25
        Caption = 'List Host Files'
        TabOrder = 12
        OnClick = doFtpListClick
      end
      object FtpSubdirs: TCheckBox
        Left = 5
        Top = 55
        Width = 136
        Height = 17
        Caption = 'Include Sub Directories'
        TabOrder = 2
      end
      object FtpDelDone: TCheckBox
        Left = 5
        Top = 95
        Width = 136
        Height = 17
        Caption = 'Delete After Transfer'
        TabOrder = 4
      end
      object FtpDelOldTar: TCheckBox
        Left = 5
        Top = 75
        Width = 136
        Height = 13
        Caption = 'Delete Old Target Files'
        TabOrder = 3
      end
      object FtpCopyAllDir: TCheckBox
        Left = 5
        Top = 35
        Width = 114
        Height = 17
        Caption = 'Copy All Directory '
        TabOrder = 1
      end
      object FtpEmptyDirs: TCheckBox
        Left = 5
        Top = 115
        Width = 136
        Height = 17
        Caption = 'Copy Empty Directories'
        TabOrder = 5
      end
      object doFtpUpThread: TButton
        Left = 237
        Top = 255
        Width = 101
        Height = 25
        Caption = 'Upload (Thread)'
        TabOrder = 20
        OnClick = doFtpUpThreadClick
      end
      object doFtpListThread: TButton
        Left = 5
        Top = 225
        Width = 101
        Height = 25
        Caption = 'List Files (Thread)'
        TabOrder = 18
        OnClick = doFtpListThreadClick
      end
      object doFtpDownThread: TButton
        Left = 120
        Top = 255
        Width = 101
        Height = 25
        Caption = 'Download  (Thread)'
        TabOrder = 19
        OnClick = doFtpDownThreadClick
      end
      object FtpIgnorePath: TEdit
        Left = 227
        Top = 60
        Width = 194
        Height = 22
        TabOrder = 7
        Text = '/work/'
      end
      object FtpMultiFileRepl: TComboBox
        Left = 246
        Top = 90
        Width = 113
        Height = 22
        TabOrder = 8
        Text = ' Never'
        Items.Strings = (
          ' Never'
          'Always'
          'If Different'
          'If Newer')
      end
      object FtpMultiUnzip: TCheckBox
        Left = 267
        Top = 137
        Width = 136
        Height = 17
        Caption = 'Unzip Downloaded Files'
        TabOrder = 10
      end
      object FtpMultiZip: TCheckBox
        Left = 120
        Top = 137
        Width = 136
        Height = 17
        Caption = 'Zip Files During Upload'
        TabOrder = 9
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Single FTP'
      ImageIndex = 3
      object Label15: TLabel
        Left = 5
        Top = 10
        Width = 60
        Height = 14
        Caption = 'Remote Path'
      end
      object Label16: TLabel
        Left = 5
        Top = 40
        Width = 98
        Height = 14
        Caption = 'Download File Name'
      end
      object Label17: TLabel
        Left = 5
        Top = 60
        Width = 96
        Height = 14
        Caption = 'Download Directory'
      end
      object Label18: TLabel
        Left = 5
        Top = 110
        Width = 127
        Height = 14
        Caption = 'Upload File Path and Name'
      end
      object Label32: TLabel
        Left = 5
        Top = 165
        Width = 81
        Height = 14
        Caption = 'File Replacement'
      end
      object Ftp1Path: TEdit
        Left = 75
        Top = 5
        Width = 346
        Height = 22
        TabOrder = 0
        Text = '/software'
      end
      object Ftp1SrcName: TEdit
        Left = 125
        Top = 35
        Width = 216
        Height = 22
        TabOrder = 1
        Text = 'dunman.zip'
      end
      object Ftp1LocDir: TEdit
        Left = 3
        Top = 80
        Width = 418
        Height = 22
        TabOrder = 2
        Text = 'c:\tempfiles'
      end
      object doFtpDown1: TButton
        Left = 18
        Top = 250
        Width = 96
        Height = 25
        Caption = 'Single Download'
        TabOrder = 4
        OnClick = doFtpDown1Click
      end
      object doFtpUp1: TButton
        Left = 127
        Top = 250
        Width = 96
        Height = 25
        Caption = 'Single Upload'
        TabOrder = 6
        OnClick = doFtpUp1Click
      end
      object Ftp1UpFile: TEdit
        Left = 3
        Top = 131
        Width = 413
        Height = 22
        TabOrder = 5
        Text = 'c:\tempfiles\dunman.zip'
      end
      object Ftp1DelDone: TCheckBox
        Left = 250
        Top = 164
        Width = 136
        Height = 17
        Caption = 'Delete After Transfer'
        TabOrder = 3
        Visible = False
      end
      object doFtpAbort1: TButton
        Left = 238
        Top = 250
        Width = 76
        Height = 25
        Caption = 'Abort'
        Enabled = False
        TabOrder = 7
        OnClick = doAbortClick
      end
      object Ftp1FileRepl: TComboBox
        Left = 110
        Top = 160
        Width = 113
        Height = 22
        TabOrder = 8
        Text = ' Never'
        Items.Strings = (
          ' Never'
          'Always'
          'If Different'
          'If Newer')
      end
      object Ftp1Zip: TCheckBox
        Left = 5
        Top = 190
        Width = 136
        Height = 17
        Caption = 'Zip Files During Copy'
        TabOrder = 9
      end
      object Ftp1Unzip: TCheckBox
        Left = 153
        Top = 190
        Width = 136
        Height = 17
        Caption = 'Zip Files During Copy'
        TabOrder = 10
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'TIcsHttpMulti'
      ImageIndex = 2
      object Label5: TLabel
        Left = 5
        Top = 0
        Width = 93
        Height = 14
        Caption = 'Source HTTP Paths'
      end
      object Label6: TLabel
        Left = 5
        Top = 145
        Width = 82
        Height = 14
        Caption = 'Source File Mask'
      end
      object Label7: TLabel
        Left = 5
        Top = 209
        Width = 77
        Height = 14
        Caption = 'Target Directory'
      end
      object Label14: TLabel
        Left = 284
        Top = 168
        Width = 79
        Height = 28
        Caption = 'Bandwidth Limit (KBits/sec)'
        WordWrap = True
      end
      object Label33: TLabel
        Left = 217
        Top = 145
        Width = 81
        Height = 14
        Caption = 'File Replacement'
      end
      object Label34: TLabel
        Left = 111
        Top = 170
        Width = 63
        Height = 14
        Caption = 'Parse Levels'
      end
      object HttpTarDir: TEdit
        Left = 5
        Top = 225
        Width = 416
        Height = 22
        TabOrder = 6
        Text = 'c:\tempfiles'
      end
      object doHttpDownCheck: TButton
        Left = 135
        Top = 255
        Width = 101
        Height = 25
        Caption = 'Check Down Files'
        TabOrder = 8
        OnClick = HttpDownload
      end
      object doHttpDownFiles: TButton
        Left = 3
        Top = 255
        Width = 101
        Height = 25
        Caption = 'Download Files'
        TabOrder = 7
        OnClick = HttpDownload
      end
      object doHttpAbort: TButton
        Left = 260
        Top = 255
        Width = 101
        Height = 25
        Caption = 'Abort'
        Enabled = False
        TabOrder = 9
        OnClick = doAbortClick
      end
      object HttpSrcFile: TComboBox
        Left = 100
        Top = 140
        Width = 111
        Height = 22
        TabOrder = 1
        Text = '*.zip'
        Items.Strings = (
          '*.ide'
          '*.zip'
          '*.htm'
          '*.*')
      end
      object HttpSrcDir: TMemo
        Left = 5
        Top = 20
        Width = 416
        Height = 115
        Lines.Strings = (
          'http://www.magsys.co.uk/dunman/default.asp')
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
      object HttpBandWidth: TEdit
        Left = 380
        Top = 170
        Width = 46
        Height = 22
        TabOrder = 5
        Text = '0'
      end
      object HttpFileRepl: TComboBox
        Left = 316
        Top = 140
        Width = 113
        Height = 22
        TabOrder = 2
        Text = ' Never'
        Items.Strings = (
          ' Never'
          'Always'
          'If Different'
          'If Newer')
      end
      object HttpParsePage: TCheckBox
        Left = 5
        Top = 165
        Width = 89
        Height = 17
        Caption = 'Parse HTML'
        TabOrder = 3
      end
      object HttpParseLevels: TEdit
        Left = 205
        Top = 170
        Width = 46
        Height = 22
        TabOrder = 4
        Text = '0'
      end
      object HttpUnzip: TCheckBox
        Left = 6
        Top = 185
        Width = 141
        Height = 17
        Caption = 'Unzip Downloaded Files'
        TabOrder = 10
      end
    end
    object TabSheet6: TTabSheet
      Caption = 'File Clean-up'
      ImageIndex = 5
      object Label35: TLabel
        Left = 5
        Top = 130
        Width = 124
        Height = 16
        AutoSize = False
        Caption = 'Clean-up Directory'
        WordWrap = True
      end
      object Label36: TLabel
        Left = 7
        Top = 183
        Width = 158
        Height = 14
        AutoSize = False
        Caption = 'Clean-up Mask with Wildcard'
      end
      object CleanupType: TRadioGroup
        Left = 5
        Top = 5
        Width = 191
        Height = 96
        Caption = 'File Clean-up Method'
        ItemIndex = 0
        Items.Strings = (
          'Delete Files by Age'
          'Delete Files by Dates'
          'Zip Files by Age, then Delete'
          'Zip Files by Dates, then Delete')
        TabOrder = 0
        OnClick = CleanupTypeClick
      end
      object BoxCleanupDays: TGroupBox
        Left = 210
        Top = 5
        Width = 211
        Height = 44
        Caption = 'Clean-up By Age'
        TabOrder = 1
        object Label115: TLabel
          Left = 10
          Top = 20
          Width = 78
          Height = 14
          Caption = 'Older than Days'
        end
        object CleanupDays: TEdit
          Left = 123
          Top = 16
          Width = 53
          Height = 22
          Hint = 'ie log-2021*.log'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = '90'
        end
      end
      object BoxCleanupDates: TGroupBox
        Left = 212
        Top = 51
        Width = 211
        Height = 85
        Caption = 'Clean-up Date Range'
        TabOrder = 2
        object Label109: TLabel
          Left = 10
          Top = 24
          Width = 49
          Height = 14
          Caption = 'From Date'
        end
        object Label114: TLabel
          Left = 10
          Top = 50
          Width = 36
          Height = 14
          Caption = 'To Date'
        end
        object CleanupDateFrom: TDateTimePicker
          Left = 93
          Top = 20
          Width = 99
          Height = 22
          Date = 36526.000000000000000000
          Time = 0.540688657398277400
          ParseInput = True
          TabOrder = 0
        end
        object CleanupDateTo: TDateTimePicker
          Left = 93
          Top = 50
          Width = 99
          Height = 22
          Date = 43466.000000000000000000
          Time = 0.540688657398277400
          TabOrder = 1
        end
      end
      object CleanupFileMask: TEdit
        Left = 170
        Top = 177
        Width = 96
        Height = 22
        Hint = 'ie log-2021*.log'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        Text = 'log*.log'
      end
      object CleanupSubDirs: TCheckBox
        Left = 280
        Top = 178
        Width = 174
        Height = 17
        Caption = 'Sub Directories'
        TabOrder = 5
      end
      object CleanupPath: TEdit
        Left = 5
        Top = 148
        Width = 418
        Height = 22
        TabOrder = 3
        Text = 'c:\tempfiles'
      end
      object doCleanupCheck: TButton
        Left = 135
        Top = 213
        Width = 101
        Height = 25
        Caption = 'Check Path'
        TabOrder = 6
        OnClick = doCleanupCheckClick
      end
      object doCleanupStart: TButton
        Left = 15
        Top = 213
        Width = 101
        Height = 25
        Caption = 'Start Clean-up'
        TabOrder = 7
        OnClick = doCleanupStartClick
      end
    end
    object TabSheet7: TTabSheet
      Caption = 'Single File Copy'
      ImageIndex = 6
      object Label37: TLabel
        Left = 5
        Top = 0
        Width = 54
        Height = 14
        Caption = 'Source File'
      end
      object Label38: TLabel
        Left = 5
        Top = 50
        Width = 52
        Height = 14
        Caption = 'Target File '
      end
      object Label39: TLabel
        Left = 5
        Top = 110
        Width = 81
        Height = 14
        Caption = 'File Replacement'
      end
      object Copy1FileTar: TEdit
        Left = 5
        Top = 70
        Width = 416
        Height = 22
        TabOrder = 0
        Text = 'c:\tempfiles\newname.txt'
      end
      object Copy1Replc: TComboBox
        Left = 110
        Top = 110
        Width = 113
        Height = 22
        TabOrder = 1
        Text = ' Never'
        Items.Strings = (
          'Never'
          'Always'
          'If Different'
          'If Newer')
      end
      object Copy1FileSrc: TEdit
        Left = 3
        Top = 20
        Width = 416
        Height = 22
        TabOrder = 2
        Text = 'c:\tempfiles\oldname.txt'
      end
      object doCopyOneFile: TButton
        Left = 5
        Top = 150
        Width = 101
        Height = 25
        Caption = 'Copy One File'
        TabOrder = 3
        OnClick = doCopyOneFileClick
      end
    end
    object TabZipping: TTabSheet
      Caption = 'Zip/Unzipping Files'
      ImageIndex = 7
      object LabelZipDir: TLabel
        Left = 5
        Top = 174
        Width = 116
        Height = 14
        Caption = 'Specific Unzip Directory'
      end
      object ZippingExtFmt: TRadioGroup
        Left = 207
        Top = 117
        Width = 186
        Height = 56
        Caption = 'FTP Upload Zip Name Format'
        ItemIndex = 0
        Items.Strings = (
          'Add Zip Extension'
          'Replace Extension with Zip')
        TabOrder = 3
      end
      object UnzipZipPath: TRadioGroup
        Left = 5
        Top = 35
        Width = 176
        Height = 123
        Caption = 'Unzipping Directory'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'New Directory '
          'Use Original Paths'
          'New Dir and Orig Paths'
          'Specific Directory'
          'Specific Dir and Orig Paths')
        TabOrder = 1
      end
      object UnzipDownDel: TCheckBox
        Left = 5
        Top = 12
        Width = 181
        Height = 17
        Caption = 'Delete After Successful Unzip'
        TabOrder = 0
      end
      object ZippingType: TRadioGroup
        Left = 207
        Top = 38
        Width = 199
        Height = 73
        Caption = 'File Copy Zip Type'
        ItemIndex = 0
        Items.Strings = (
          'Unzip Target Files'
          'Zip Source Files, Add Zip Exnt'
          'Zip Source Files, Replace Extn')
        TabOrder = 2
      end
      object UnzipDir: TEdit
        Left = 5
        Top = 197
        Width = 416
        Height = 22
        TabOrder = 4
        Text = 'c:\tempfiles'
      end
    end
  end
  object LogDelim: TMemo
    Left = 0
    Top = 398
    Width = 922
    Height = 137
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 14
  end
  object ShowDiagsHigh: TCheckBox
    Left = 460
    Top = 120
    Width = 231
    Height = 17
    Caption = 'Show High Level Diagnostic Information'
    Checked = True
    State = cbChecked
    TabOrder = 7
  end
  object doExit: TButton
    Left = 474
    Top = 255
    Width = 75
    Height = 25
    Caption = 'Exit'
    TabOrder = 12
    OnClick = doExitClick
  end
  object ShowDiagsLow: TCheckBox
    Left = 460
    Top = 140
    Width = 231
    Height = 17
    Caption = 'Show Low Level Diagnostic Information'
    TabOrder = 8
  end
  object ShowDiagsSSL: TCheckBox
    Left = 460
    Top = 160
    Width = 231
    Height = 17
    Caption = 'Show SSL Dump Diagnostic Information'
    TabOrder = 9
  end
  object doClear: TButton
    Left = 570
    Top = 255
    Width = 75
    Height = 25
    Caption = 'Clear'
    TabOrder = 13
    OnClick = doClearClick
  end
  object ShowXProgesss: TCheckBox
    Left = 460
    Top = 100
    Width = 140
    Height = 17
    Caption = 'Show Extended Progress'
    Checked = True
    State = cbChecked
    TabOrder = 6
  end
  object RevokeCheck: TCheckBox
    Left = 615
    Top = 80
    Width = 121
    Height = 17
    Caption = 'SSL Revoke Check'
    TabOrder = 4
  end
  object VerifyCertMode: TRadioGroup
    Left = 749
    Top = 80
    Width = 151
    Height = 68
    Caption = 'Verify Certificate Mode'
    ItemIndex = 0
    Items.Strings = (
      'None'
      'PEM Bundle File'
      'Windows Cert Store')
    TabOrder = 5
  end
  object ReportChain: TCheckBox
    Left = 460
    Top = 80
    Width = 141
    Height = 17
    Caption = 'Report SSL Certificates'
    TabOrder = 3
  end
  object SslSecurity: TComboBox
    Left = 713
    Top = 165
    Width = 182
    Height = 22
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 10
    Text = 'Ignore'
    Items.Strings = (
      'Ignore'
      'None'
      'SSLv3 Only'
      'TLSv1 Only'
      'TLSv1.1 Only'
      'TLSv1.2 Only'
      'TLSv1.3 Only'
      'TLSv1 or Better'
      'TLSv1.1 or Better'
      'TLSv1.2 or Better'
      'Backward Ciphers'
      'Intermediate Ciphers'
      'High Ciphers, 2048 keys'
      'High Ciphers, 3072 keys'
      'High Ciphers, 7680 keys')
  end
  object DirLogs: TEdit
    Left = 537
    Top = 50
    Width = 341
    Height = 22
    TabOrder = 1
  end
  object XferSockFamily: TRadioGroup
    Left = 793
    Top = 193
    Width = 102
    Height = 96
    Caption = 'Socket Family'
    ItemIndex = 0
    Items.Strings = (
      'Any'
      'Prefer IPv4'
      'Prefer IPv6'
      'Only IPv4 '
      'Only IPv6')
    TabOrder = 11
  end
  object NoSSL: TCheckBox
    Left = 615
    Top = 100
    Width = 87
    Height = 17
    Caption = 'No SSL/HTTPS'
    TabOrder = 15
  end
  object TimerUpdates: TTimer
    OnTimer = TimerUpdatesTimer
    Left = 690
    Top = 265
  end
end
