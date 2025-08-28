object frmPemTool1: TfrmPemTool1
  Left = 212
  Top = 124
  ClientHeight = 798
  ClientWidth = 880
  Color = clBtnFace
  Constraints.MinHeight = 379
  Constraints.MinWidth = 527
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  Menu = MainMenu1
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    880
    798)
  TextHeight = 14
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 880
    Height = 759
    ActivePage = TabCertsList
    Align = alClient
    TabOrder = 0
    OnChange = PageControl1Change
    object TabCertsList: TTabSheet
      Caption = 'List Certificates'
      object LvCerts: TListView
        Left = 0
        Top = 0
        Width = 872
        Height = 640
        Align = alClient
        Columns = <
          item
            AutoSize = True
            Caption = 'Common Name'
          end
          item
            AutoSize = True
            Caption = 'Issued to'
          end
          item
            AutoSize = True
            Caption = 'Issuer'
          end
          item
            Caption = 'Expires at'
            Width = 70
          end
          item
            Caption = 'File Name'
            Width = 300
          end
          item
          end>
        ReadOnly = True
        RowSelect = True
        PopupMenu = pmLv
        SortType = stData
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = LvCertsColumnClick
        OnCompare = LvCertsCompare
        OnCustomDraw = LvCertsCustomDraw
        OnDblClick = LvCertsDblClick
      end
      object Panel1: TPanel
        Left = 0
        Top = 640
        Width = 872
        Height = 90
        Align = alBottom
        Alignment = taLeftJustify
        TabOrder = 1
        object Label4: TLabel
          Left = 4
          Top = 35
          Width = 47
          Height = 14
          Caption = 'Directory:'
        end
        object Label8: TLabel
          Left = 500
          Top = 10
          Width = 144
          Height = 14
          Caption = 'PKCS12 Certificate Password'
        end
        object Label34: TLabel
          Left = 4
          Top = 55
          Width = 52
          Height = 28
          Caption = 'CA Root Bundle File'
          WordWrap = True
        end
        object Label24: TLabel
          Left = 481
          Top = 55
          Width = 33
          Height = 28
          Caption = 'Inter Bundle'
          WordWrap = True
        end
        object btnRefresh: TButton
          Left = 424
          Top = 5
          Width = 71
          Height = 21
          Caption = '&Refresh'
          TabOrder = 4
          OnClick = btnRefreshClick
        end
        object CurrentCertDirEdit: TEdit
          Left = 62
          Top = 33
          Width = 604
          Height = 22
          TabOrder = 6
          Text = 'CurrentCertDirEdit'
          OnChange = CurrentCertDirEditChange
        end
        object btnDeleteCert: TButton
          Left = 343
          Top = 5
          Width = 75
          Height = 21
          Caption = '&Delete'
          TabOrder = 3
          OnClick = btnDeleteCertClick
        end
        object btnCopyCert: TButton
          Left = 256
          Top = 5
          Width = 75
          Height = 21
          Caption = '&Copy'
          TabOrder = 2
          OnClick = btnCopyCertClick
        end
        object SelCurrDir: TBitBtn
          Left = 672
          Top = 30
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
          TabOrder = 7
          OnClick = SelCurrDirClick
        end
        object btnShowBundleFile: TButton
          Left = 12
          Top = 5
          Width = 109
          Height = 21
          Caption = '&View Bundle File'
          TabOrder = 0
          OnClick = btnShowBundleFileClick
        end
        object btnShowOneFile: TButton
          Left = 132
          Top = 5
          Width = 109
          Height = 21
          Caption = '&View Single File'
          TabOrder = 1
          OnClick = btnShowOneFileClick
        end
        object CertPassword: TEdit
          Left = 650
          Top = 5
          Width = 156
          Height = 22
          TabOrder = 5
        end
        object CertCheckOCSP: TCheckBox
          Left = 722
          Top = 35
          Width = 139
          Height = 17
          Caption = 'Check OCSP Revoke'
          TabOrder = 8
        end
        object CAFilesDir: TComboBox
          Left = 62
          Top = 61
          Width = 376
          Height = 22
          TabOrder = 9
          Text = 'Internal'
          Items.Strings = (
            'Internal'
            'RootCaCertsBundle.pem'
            'TrustedCABundle.pem')
        end
        object SelCARoots: TBitBtn
          Left = 444
          Top = 61
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
          TabOrder = 10
          OnClick = CAFilesDirClick
        end
        object IntersCAFile: TEdit
          Left = 526
          Top = 61
          Width = 300
          Height = 22
          TabOrder = 11
          Text = 'InterCaCertsBundle.pem'
        end
        object SelIntersCA: TBitBtn
          Left = 832
          Top = 58
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
          OnClick = SelIntersCAClick
        end
      end
    end
    object TabWindowsStore: TTabSheet
      Caption = 'Certificate Tools'
      ImageIndex = 1
      object BoxStoreTools: TGroupBox
        Left = 6
        Top = 4
        Width = 862
        Height = 665
        Caption = 'Windows Certificate Store Tools '
        TabOrder = 2
        object MsStoreType: TRadioGroup
          Left = 10
          Top = 20
          Width = 175
          Height = 112
          Caption = 'Windows Store Type'
          ItemIndex = 0
          Items.Strings = (
            'Personal Store'
            'Trusted Root CAs'
            'Enterprise Trust'
            'Certifcation Authorities'
            'Trusted People'
            'Software Publishers')
          TabOrder = 0
        end
        object doMsDisplay: TButton
          Left = 205
          Top = 25
          Width = 128
          Height = 25
          Caption = 'Display Cert Store'
          TabOrder = 2
          OnClick = doMsDisplayClick
        end
        object MsLocationType: TRadioGroup
          Left = 10
          Top = 138
          Width = 175
          Height = 91
          Caption = 'Wndows Store Location'
          ItemIndex = 0
          Items.Strings = (
            'Current User'
            'Local Machine'
            'Services'
            'Current Service'
            'Users')
          TabOrder = 1
        end
        object MSBriefList: TCheckBox
          Left = 205
          Top = 56
          Width = 107
          Height = 17
          Caption = 'Brief List Only'
          TabOrder = 3
        end
        object BoxStoreImport: TGroupBox
          Left = 10
          Top = 440
          Width = 486
          Height = 214
          Caption = 'Import Windows Store'
          TabOrder = 4
          object Label1: TLabel
            Left = 10
            Top = 18
            Width = 441
            Height = 42
            Caption = 
              ' Specified Certificate-Store is opened.  Then the DER formated c' +
              'erts are read and translated to PEM format. Certs are stored to ' +
              'the specified folder in the form of Hash.0. The '#39'Crt. Store Type' +
              #39' box has static values. '
            WordWrap = True
          end
          object Label2: TLabel
            Left = 10
            Top = 66
            Width = 74
            Height = 14
            Caption = 'Destination Dir.:'
          end
          object DestDirEdit: TEdit
            Left = 10
            Top = 85
            Width = 362
            Height = 22
            Hint = 'Existing destination directory '
            TabOrder = 0
            Text = 'DestDirEdit'
            OnChange = DestDirEditChange
          end
          object CheckBoxWarnDestNotEmpty: TCheckBox
            Left = 14
            Top = 118
            Width = 243
            Height = 17
            Caption = 'Warn me if destination folder is not empty'
            Checked = True
            State = cbChecked
            TabOrder = 2
          end
          object CheckBoxOverwriteExisting: TCheckBox
            Left = 14
            Top = 137
            Width = 243
            Height = 17
            Hint = 
              'If enabled, existing certs with the same name are overwritten.'#13#10 +
              'If not enabled, file extensions are changed. '#13#10'(e.g. 9d66eef0.0,' +
              ' 9d66eef0.1 etc)'
            Caption = 'Overwrite existing files, don'#39't change file ext.'
            TabOrder = 3
          end
          object CheckBoxEmptyDestDir: TCheckBox
            Left = 14
            Top = 158
            Width = 243
            Height = 17
            Hint = 'Warning! - deletes any file in destination folder '
            Caption = 'Empty destination directory'
            TabOrder = 4
          end
          object btnImport: TButton
            Left = 12
            Top = 183
            Width = 162
            Height = 21
            Caption = 'Start import from Windows'
            TabOrder = 7
            OnClick = btnImportClick
          end
          object CheckBoxWriteToBundle: TCheckBox
            Left = 271
            Top = 118
            Width = 197
            Height = 17
            Caption = 'Create a CA bundle file'
            TabOrder = 5
          end
          object SelImpDir: TBitBtn
            Left = 398
            Top = 75
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
            TabOrder = 1
            OnClick = SelImpDirClick
          end
          object CheckBoxComment: TCheckBox
            Left = 271
            Top = 137
            Width = 191
            Height = 17
            Caption = 'Add Comments to file'
            TabOrder = 6
          end
        end
        object BoxStoreInstall: TGroupBox
          Left = 10
          Top = 314
          Width = 484
          Height = 105
          Caption = 'Install Bundle with Private Key and Inters into Personal Store'
          TabOrder = 5
          object Label3: TLabel
            Left = 10
            Top = 21
            Width = 82
            Height = 14
            Caption = 'Bundle File Name'
          end
          object LabelAdminRightsIns: TLabel
            Left = 150
            Top = 74
            Width = 296
            Height = 16
            Caption = 'Local Machine store has/needs administrator rights'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
          end
          object MsInstallFile: TEdit
            Left = 10
            Top = 39
            Width = 421
            Height = 22
            TabOrder = 0
            Text = 'MsInstallFile'
          end
          object SelInstallFile: TBitBtn
            Left = 445
            Top = 38
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
            TabOrder = 1
            OnClick = SelInstallFileClick
          end
          object doMsInstallFile: TButton
            Left = 11
            Top = 67
            Width = 120
            Height = 25
            Caption = 'Install File into Store'
            TabOrder = 2
            OnClick = doMsInstallFileClick
          end
        end
        object doLoadtoCreate: TButton
          Left = 205
          Top = 95
          Width = 128
          Height = 25
          Caption = 'Load to Create Certs'
          TabOrder = 6
          OnClick = doLoadtoCreateClick
        end
        object doListPkeys: TButton
          Left = 205
          Top = 169
          Width = 128
          Height = 25
          Caption = 'List Private Key Store'
          TabOrder = 7
          OnClick = doListPkeysClick
        end
        object doCertStoreDel: TButton
          Left = 205
          Top = 131
          Width = 128
          Height = 25
          Caption = 'Delete from Cert Store'
          TabOrder = 8
          OnClick = doCertStoreDelClick
        end
        object MSKeyStoreType: TRadioGroup
          Left = 10
          Top = 235
          Width = 175
          Height = 68
          Caption = 'Key Store Location'
          ItemIndex = 0
          Items.Strings = (
            'Software Key Provider'
            'Smartcard Key Provider'
            'Trusted Platform Module')
          TabOrder = 9
        end
        object BoxFindCert: TGroupBox
          Left = 505
          Top = 544
          Width = 350
          Height = 107
          Caption = 'Find Best Certification in Store, Load to Create Bundle with Key'
          TabOrder = 10
          object Label39: TLabel
            Left = 10
            Top = 20
            Width = 188
            Height = 14
            Caption = 'Search Common Name, Title, Alt Names'
          end
          object LabelAdminRightsSrch: TLabel
            Left = 123
            Top = 66
            Width = 197
            Height = 36
            AutoSize = False
            Caption = 'Exporting private key needs administrator rights'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -13
            Font.Name = 'Arial'
            Font.Style = []
            ParentFont = False
            WordWrap = True
          end
          object MsSearchCert: TEdit
            Left = 11
            Top = 38
            Width = 314
            Height = 22
            Hint = 'Existing destination directory '
            TabOrder = 0
            OnChange = DestDirEditChange
          end
          object doSearchMsStore: TButton
            Left = 13
            Top = 68
            Width = 100
            Height = 25
            Caption = 'Search and Load'
            TabOrder = 1
            OnClick = doSearchMsStoreClick
          end
        end
        object BoxPrivKeys: TGroupBox
          Left = 506
          Top = 316
          Width = 350
          Height = 223
          Caption = 'Windows Private Key Store'
          TabOrder = 11
          object Label41: TLabel
            Left = 149
            Top = 188
            Width = 167
            Height = 26
            AutoSize = False
            Caption = 'Warning, Private Keys may be used by more than one certificate. '
            WordWrap = True
          end
          object PkeyList: TListBox
            Left = 11
            Top = 20
            Width = 331
            Height = 162
            ItemHeight = 14
            ScrollWidth = 1200
            TabOrder = 0
          end
          object doKeyStoreDel: TButton
            Left = 10
            Top = 189
            Width = 128
            Height = 25
            Caption = 'Delete from Key Store'
            Enabled = False
            TabOrder = 1
            OnClick = doKeyStoreDelClick
          end
        end
        object CertStoreList: TListBox
          Left = 346
          Top = 16
          Width = 503
          Height = 290
          ItemHeight = 14
          ScrollWidth = 1200
          TabOrder = 12
        end
      end
      object BoxSigning: TGroupBox
        Left = 265
        Top = 675
        Width = 184
        Height = 48
        Caption = 'Program Signing (Authenticode)'
        TabOrder = 0
        DesignSize = (
          184
          48)
        object btnCheckSigned: TButton
          Left = 13
          Top = 16
          Width = 95
          Height = 21
          Anchors = []
          Caption = '&Check Signed'
          TabOrder = 0
          OnClick = btnCheckSignedClick
        end
      end
      object BoxMisc: TGroupBox
        Left = 5
        Top = 675
        Width = 251
        Height = 48
        Caption = 'Misc'
        TabOrder = 1
        DesignSize = (
          251
          48)
        object btnImportPemFile: TButton
          Left = 10
          Top = 17
          Width = 231
          Height = 22
          Anchors = []
          Caption = 'Import/Hash a PEM Cert File to Destination Dir.'
          TabOrder = 0
          OnClick = btnImportPemFileClick
        end
      end
    end
    object TabCreateCerts: TTabSheet
      Caption = 'Create Certificates'
      ImageIndex = 2
      object BoxLoadCert: TGroupBox
        Left = 3
        Top = 3
        Width = 860
        Height = 213
        Caption = 
          'Load Certificate and/or Private Key from File or Lines - PEM, DE' +
          'R, P12, PFX, CER, CRT, P7B, P7S, P7C '
        TabOrder = 0
        object Label9: TLabel
          Left = 5
          Top = 25
          Width = 44
          Height = 14
          Caption = 'Directory'
        end
        object Label10: TLabel
          Left = 5
          Top = 55
          Width = 39
          Height = 14
          Caption = 'Cert File'
        end
        object Label11: TLabel
          Left = 359
          Top = 85
          Width = 69
          Height = 14
          Caption = 'Inter Certs File'
        end
        object Label12: TLabel
          Left = 571
          Top = 55
          Width = 59
          Height = 14
          Caption = 'Request File'
        end
        object Label13: TLabel
          Left = 352
          Top = 110
          Width = 85
          Height = 28
          Caption = 'Base64 Encoded DER (PEM)'
          WordWrap = True
        end
        object Label14: TLabel
          Left = 650
          Top = 85
          Width = 50
          Height = 14
          Caption = 'Password'
        end
        object Label27: TLabel
          Left = 280
          Top = 55
          Width = 57
          Height = 14
          Caption = 'Prv Key File'
        end
        object Label33: TLabel
          Left = 5
          Top = 145
          Width = 69
          Height = 14
          Caption = 'CA Bundle File'
        end
        object LoadDirectory: TEdit
          Left = 55
          Top = 20
          Width = 668
          Height = 22
          TabOrder = 0
        end
        object SelLoadDir: TBitBtn
          Left = 758
          Top = 19
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
          TabOrder = 1
          OnClick = SelLoadDirClick
        end
        object CertLinesOld: TMemo
          Left = 454
          Top = 110
          Width = 387
          Height = 91
          ScrollBars = ssBoth
          TabOrder = 18
        end
        object doLoadCert: TButton
          Left = 5
          Top = 80
          Width = 92
          Height = 21
          Caption = 'Load Certificate '
          TabOrder = 8
          OnClick = doLoadCertClick
        end
        object LoadCertFile: TEdit
          Left = 57
          Top = 48
          Width = 163
          Height = 22
          TabOrder = 2
          Text = 'mycertificate.pem'
        end
        object LoadPrivatetKey: TEdit
          Left = 350
          Top = 50
          Width = 163
          Height = 22
          TabOrder = 4
          Text = 'myprivatekey.pem'
        end
        object LoadRequestFile: TEdit
          Left = 650
          Top = 50
          Width = 166
          Height = 22
          TabOrder = 6
          Text = 'mycertrequest.pem'
        end
        object SelCertFile: TBitBtn
          Left = 240
          Top = 45
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
          TabOrder = 3
          OnClick = SelCertFileClick
        end
        object SelPrvKeyFile: TBitBtn
          Left = 520
          Top = 45
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
          TabOrder = 5
          OnClick = SelPrvKeyFileClick
        end
        object SelReqFile: TBitBtn
          Left = 822
          Top = 45
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
          TabOrder = 7
          OnClick = SelReqFileClick
        end
        object doLoadPrvKey: TButton
          Left = 106
          Top = 80
          Width = 80
          Height = 21
          Caption = 'Load Key'
          TabOrder = 9
          OnClick = doLoadPrvKeyClick
        end
        object doLoadReq: TButton
          Left = 192
          Top = 80
          Width = 79
          Height = 21
          Caption = 'Load Request '
          TabOrder = 10
          OnClick = doLoadReqClick
        end
        object doLoadBase64: TButton
          Left = 332
          Top = 155
          Width = 109
          Height = 21
          Caption = 'Load Base64 Cert'
          TabOrder = 17
          OnClick = doLoadBase64Click
        end
        object LoadCertPW: TEdit
          Left = 720
          Top = 80
          Width = 133
          Height = 22
          TabOrder = 14
        end
        object LoadInterCerts: TEdit
          Left = 444
          Top = 82
          Width = 163
          Height = 22
          TabOrder = 12
          Text = 'intercerts.pem'
        end
        object doLoadInters: TButton
          Left = 277
          Top = 80
          Width = 69
          Height = 21
          Caption = 'Load Inters'
          TabOrder = 11
          OnClick = doLoadIntersClick
        end
        object SelIntersFile: TBitBtn
          Left = 613
          Top = 75
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
          TabOrder = 13
          OnClick = SelIntersFileClick
        end
        object LoadCertPrivKey: TCheckBox
          Left = 9
          Top = 111
          Width = 148
          Height = 25
          Caption = 'Load Private Key from Cert File'
          TabOrder = 15
          WordWrap = True
        end
        object LoadCertInters: TCheckBox
          Left = 178
          Top = 107
          Width = 168
          Height = 17
          Caption = 'Load Inters from Cert File'
          TabOrder = 16
        end
        object LoadCaBundleFile: TEdit
          Left = 97
          Top = 142
          Width = 163
          Height = 22
          TabOrder = 19
          Text = 'mycabundle.pem'
        end
        object SelCAFile: TBitBtn
          Left = 280
          Top = 139
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
          TabOrder = 20
          OnClick = SelCAFileClick
        end
        object doLoadCABundle: TButton
          Left = 5
          Top = 170
          Width = 99
          Height = 21
          Caption = 'Load CA Bundle'
          TabOrder = 21
          OnClick = doLoadCABundleClick
        end
      end
      object BoxCertProc: TGroupBox
        Left = 5
        Top = 227
        Width = 860
        Height = 287
        Caption = 
          'Certificate Processing - Create Key,  Create Request, Create Sel' +
          'f Signed, Create by Signing Request (properties on separate tab)'
        TabOrder = 1
        object LabelStateCert: TLabel
          Left = 10
          Top = 50
          Width = 296
          Height = 136
          AutoSize = False
          Caption = 'Certificate: None'
          Color = clYellow
          ParentColor = False
          Transparent = False
          WordWrap = True
        end
        object LabelStateReq: TLabel
          Left = 595
          Top = 143
          Width = 257
          Height = 138
          AutoSize = False
          Caption = 'Certificate Request: None'
          Color = clYellow
          ParentColor = False
          Transparent = False
          WordWrap = True
        end
        object LabelStatePrivKey: TLabel
          Left = 315
          Top = 50
          Width = 273
          Height = 41
          AutoSize = False
          Caption = 'Private Key: None'
          Color = clYellow
          ParentColor = False
          Transparent = False
          WordWrap = True
        end
        object LabelStateCACert: TLabel
          Left = 13
          Top = 192
          Width = 296
          Height = 90
          AutoSize = False
          Caption = 'CA Certificate: None'
          Color = clYellow
          ParentColor = False
          Transparent = False
          WordWrap = True
        end
        object LabelInters: TLabel
          Left = 315
          Top = 97
          Width = 273
          Height = 185
          AutoSize = False
          Caption = 'Intermediate Certificates: None'
          Color = clYellow
          ParentColor = False
          Transparent = False
          WordWrap = True
        end
        object doClearCerts: TButton
          Left = 10
          Top = 20
          Width = 133
          Height = 21
          Caption = 'Clear Certs and Keys'
          TabOrder = 0
          OnClick = doClearCertsClick
        end
        object doCreateReqProps: TButton
          Left = 149
          Top = 20
          Width = 152
          Height = 21
          Caption = 'Create Request from Props'
          TabOrder = 1
          OnClick = doCreateReqPropsClick
        end
        object doCreateReqCert: TButton
          Left = 307
          Top = 20
          Width = 144
          Height = 21
          Caption = 'Create Request from Cert'
          TabOrder = 2
          OnClick = doCreateReqCertClick
        end
        object doCreateSelfCert: TButton
          Left = 457
          Top = 19
          Width = 187
          Height = 21
          Caption = 'Create Self Signed Cert from Props'
          TabOrder = 3
          OnClick = doCreateSelfCertClick
        end
        object doCreateCACert: TButton
          Left = 650
          Top = 20
          Width = 191
          Height = 21
          Caption = 'Create Cert from Req, Sign with CA'
          TabOrder = 4
          OnClick = doCreateCACertClick
        end
        object doCreateBundle: TButton
          Left = 594
          Top = 51
          Width = 127
          Height = 21
          Caption = 'Create Cert Bundle'
          TabOrder = 5
          OnClick = doCreateBundleClick
        end
        object doCheckBundleWin: TButton
          Left = 594
          Top = 78
          Width = 127
          Height = 21
          Caption = 'Check Cert Bundle (Win)'
          TabOrder = 6
          OnClick = doCheckBundleWinClick
        end
        object doCheckBundleSelf: TButton
          Left = 594
          Top = 105
          Width = 127
          Height = 21
          Caption = 'Check Cert Bundle (Self)'
          TabOrder = 7
          OnClick = doCheckBundleSelfClick
        end
        object doListRoots: TButton
          Left = 727
          Top = 51
          Width = 114
          Height = 21
          Caption = 'List Root Bundle'
          TabOrder = 8
          OnClick = doListRootsClick
        end
        object doListInters: TButton
          Left = 727
          Top = 78
          Width = 114
          Height = 21
          Caption = 'List Inters Bundle'
          TabOrder = 9
          OnClick = doListIntersClick
        end
      end
      object BoxCertSave: TGroupBox
        Left = 5
        Top = 525
        Width = 860
        Height = 201
        Caption = 
          'Save New Certificate, Public and Private Key and Certificate Req' +
          'uest to File'
        TabOrder = 2
        object Label17: TLabel
          Left = 5
          Top = 25
          Width = 44
          Height = 14
          Caption = 'Directory'
        end
        object Label18: TLabel
          Left = 5
          Top = 55
          Width = 62
          Height = 14
          Caption = 'PEM Cert File'
        end
        object Label19: TLabel
          Left = 5
          Top = 115
          Width = 97
          Height = 14
          Caption = 'PEM Private Key File'
        end
        object Label20: TLabel
          Left = 569
          Top = 55
          Width = 61
          Height = 14
          Caption = 'PEM Req File'
        end
        object Label22: TLabel
          Left = 570
          Top = 85
          Width = 75
          Height = 14
          Caption = 'PKCS7 Cert File'
        end
        object Label21: TLabel
          Left = 290
          Top = 55
          Width = 62
          Height = 14
          Caption = 'DER Cert File'
        end
        object Label23: TLabel
          Left = 290
          Top = 85
          Width = 81
          Height = 14
          Caption = 'PKCS12 Cert File'
        end
        object Label25: TLabel
          Left = 290
          Top = 115
          Width = 92
          Height = 14
          Caption = 'PEM Public Key File'
        end
        object Label26: TLabel
          Left = 570
          Top = 115
          Width = 50
          Height = 14
          Caption = 'Password'
        end
        object SaveDirectory: TEdit
          Left = 55
          Top = 20
          Width = 668
          Height = 22
          TabOrder = 0
        end
        object SelSaveDir: TBitBtn
          Left = 752
          Top = 15
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
          TabOrder = 1
          OnClick = SelSaveDirClick
        end
        object doSaveCertPem: TButton
          Left = 5
          Top = 165
          Width = 97
          Height = 21
          Caption = 'Save Pem Cert'
          TabOrder = 16
          OnClick = doSaveCertPemClick
        end
        object SaveCertPem: TEdit
          Left = 78
          Top = 50
          Width = 200
          Height = 22
          TabOrder = 2
          Text = 'newpemcert.pem'
        end
        object SavePrvFileFile: TEdit
          Left = 115
          Top = 110
          Width = 163
          Height = 22
          TabOrder = 9
          Text = 'newprivatekey.pem'
        end
        object SaveReqCertFile: TEdit
          Left = 656
          Top = 50
          Width = 166
          Height = 22
          TabOrder = 4
          Text = 'newrequest.pem'
        end
        object doSaveCertDer: TButton
          Left = 108
          Top = 165
          Width = 96
          Height = 21
          Caption = 'Save DER Cert'
          TabOrder = 17
          OnClick = doSaveCertDerClick
        end
        object doSaveReqCert: TButton
          Left = 692
          Top = 165
          Width = 109
          Height = 21
          Caption = 'Save Cert Request '
          TabOrder = 22
          OnClick = doSaveReqCertClick
        end
        object SavePkcs7File: TEdit
          Left = 656
          Top = 80
          Width = 163
          Height = 22
          TabOrder = 8
          Text = 'newcert.p7b'
        end
        object SavePrivateKey: TCheckBox
          Left = 5
          Top = 77
          Width = 166
          Height = 26
          Caption = 'Save Private Key in PEM/PKCS12 Cert File'
          TabOrder = 5
          WordWrap = True
        end
        object SaveCertDer: TEdit
          Left = 390
          Top = 52
          Width = 163
          Height = 22
          TabOrder = 3
          Text = 'newdercert.der'
        end
        object SavePkcs12File: TEdit
          Left = 391
          Top = 80
          Width = 163
          Height = 22
          TabOrder = 7
          Text = 'newcert.p12'
        end
        object SavePubKeyFile: TEdit
          Left = 390
          Top = 110
          Width = 163
          Height = 22
          TabOrder = 10
          Text = 'newpublickey.pem'
        end
        object SaveInterCerts: TCheckBox
          Left = 175
          Top = 80
          Width = 109
          Height = 17
          Caption = 'Save Inter Certs'
          TabOrder = 6
        end
        object SaveCertPW: TEdit
          Left = 656
          Top = 110
          Width = 163
          Height = 22
          TabOrder = 11
        end
        object SaveAutoReplace: TCheckBox
          Left = 5
          Top = 140
          Width = 210
          Height = 17
          Caption = 'Automatically Replace Existing File'
          TabOrder = 12
        end
        object doSavePkcs12: TButton
          Left = 212
          Top = 165
          Width = 109
          Height = 21
          Caption = 'Save PKCS12 Cert'
          TabOrder = 18
          OnClick = doSavePkcs12Click
        end
        object doSavePkcs7Cert: TButton
          Left = 330
          Top = 165
          Width = 109
          Height = 21
          Caption = 'Save PKCS7 Cert '
          TabOrder = 19
          OnClick = doSavePkcs7CertClick
        end
        object doSavePrivKey: TButton
          Left = 452
          Top = 165
          Width = 109
          Height = 21
          Caption = 'Save Private Key'
          TabOrder = 20
          OnClick = doSavePrivKeyClick
        end
        object doSavePubKey: TButton
          Left = 572
          Top = 165
          Width = 109
          Height = 21
          Caption = 'Save Public Key'
          TabOrder = 21
          OnClick = doSavePubKeyClick
        end
        object CertAddComment: TCheckBox
          Left = 221
          Top = 140
          Width = 170
          Height = 17
          Caption = 'Add Comments to PEM File:'
          Checked = True
          State = cbChecked
          TabOrder = 13
        end
        object CertPwPemKeys: TCheckBox
          Left = 407
          Top = 140
          Width = 146
          Height = 17
          Caption = 'Password PEM Keys'
          TabOrder = 14
        end
        object CertPwPkcs12: TCheckBox
          Left = 587
          Top = 140
          Width = 154
          Height = 17
          Caption = 'Password PKCS12 Keys'
          Checked = True
          State = cbChecked
          TabOrder = 15
        end
      end
    end
    object TabNewCertProps: TTabSheet
      Caption = 'New Certificate Properties'
      ImageIndex = 3
      object GroupBoxCertCreate: TGroupBox
        Left = 5
        Top = 330
        Width = 858
        Height = 404
        Caption = 'New Certificate or Request Properties '
        TabOrder = 0
        object lbCountry: TLabel
          Left = 5
          Top = 44
          Width = 69
          Height = 14
          Caption = 'Country Code:'
        end
        object lbState: TLabel
          Left = 5
          Top = 70
          Width = 28
          Height = 14
          Caption = 'State:'
        end
        object lbLocality: TLabel
          Left = 5
          Top = 95
          Width = 40
          Height = 14
          Caption = 'Locality:'
        end
        object lbOrganization: TLabel
          Left = 5
          Top = 120
          Width = 64
          Height = 14
          Caption = 'Organization:'
        end
        object lbOrganizationalUnit: TLabel
          Left = 5
          Top = 145
          Width = 93
          Height = 14
          Caption = 'Organizational Unit:'
        end
        object lbCommonName: TLabel
          Left = 352
          Top = 25
          Width = 146
          Height = 41
          AutoSize = False
          Caption = 
            'Common Name (server domain name or company name or email or CA n' +
            'ame):'
          WordWrap = True
        end
        object lbEMail: TLabel
          Left = 5
          Top = 170
          Width = 74
          Height = 14
          Caption = 'E-Mail address:'
        end
        object lbDays: TLabel
          Left = 355
          Top = 160
          Width = 61
          Height = 14
          Caption = 'Expiry Days:'
        end
        object Label28: TLabel
          Left = 5
          Top = 20
          Width = 150
          Height = 14
          Caption = 'Issued to (Subject) Information:'
        end
        object Label29: TLabel
          Left = 5
          Top = 195
          Width = 51
          Height = 14
          Caption = 'Desription:'
        end
        object Label30: TLabel
          Left = 352
          Top = 80
          Width = 127
          Height = 14
          Caption = 'Alternate Names: Domains'
        end
        object Label31: TLabel
          Left = 352
          Top = 130
          Width = 150
          Height = 14
          Caption = 'Alternate Names: IP Addresses'
        end
        object CertCountry: TEdit
          Left = 126
          Top = 40
          Width = 31
          Height = 22
          TabOrder = 0
        end
        object CertState: TEdit
          Left = 126
          Top = 65
          Width = 190
          Height = 22
          TabOrder = 1
        end
        object CertLocality: TEdit
          Left = 126
          Top = 90
          Width = 190
          Height = 22
          TabOrder = 2
        end
        object CertOrganization: TEdit
          Left = 126
          Top = 115
          Width = 215
          Height = 22
          TabOrder = 3
        end
        object CertOrganizationalUnit: TEdit
          Left = 126
          Top = 140
          Width = 215
          Height = 22
          TabOrder = 4
        end
        object CertCommonName: TEdit
          Left = 518
          Top = 30
          Width = 290
          Height = 22
          TabOrder = 7
          Text = 'www.domain.com'
        end
        object CertEMail: TEdit
          Left = 126
          Top = 165
          Width = 215
          Height = 22
          TabOrder = 5
        end
        object CertDays: TEdit
          Left = 432
          Top = 155
          Width = 70
          Height = 22
          TabOrder = 10
          Text = '366'
        end
        object CertSignHash: TRadioGroup
          Left = 570
          Top = 175
          Width = 271
          Height = 71
          Caption = 'Sign Digest Hash'
          Columns = 3
          ItemIndex = 2
          Items.Strings = (
            'SHA1 (old)'
            'SHA224'
            'SHA256'
            'SHA384'
            'SHA512'
            'SHA3_224'
            'SHA3_256'
            'SHA3_384'
            'SHA3_512')
          TabOrder = 12
        end
        object CertDescr: TEdit
          Left = 126
          Top = 190
          Width = 215
          Height = 22
          TabOrder = 6
        end
        object GroupBox1: TGroupBox
          Left = 15
          Top = 220
          Width = 200
          Height = 69
          Caption = 'Basic Constraints'
          TabOrder = 13
          object CertIsCA: TCheckBox
            Left = 10
            Top = 20
            Width = 181
            Height = 17
            Caption = 'Self Signed or Intermediate Cert'
            TabOrder = 0
          end
          object CertsRootCA: TCheckBox
            Left = 10
            Top = 43
            Width = 181
            Height = 17
            Caption = 'Root Certificate Authority  '
            TabOrder = 1
          end
        end
        object GroupBox2: TGroupBox
          Left = 230
          Top = 215
          Width = 185
          Height = 166
          Caption = 'Key Usage'
          TabOrder = 14
          object CertUsageCertSign: TCheckBox
            Left = 10
            Top = 20
            Width = 126
            Height = 17
            Caption = 'Certificate Sign'
            TabOrder = 0
          end
          object CertUsageCRLSign: TCheckBox
            Left = 10
            Top = 40
            Width = 126
            Height = 17
            Caption = 'CRL Sign'
            TabOrder = 1
          end
          object CertUsageDigSign: TCheckBox
            Left = 10
            Top = 60
            Width = 126
            Height = 17
            Caption = 'Digital Signature'
            TabOrder = 2
          end
          object CertUsageDataEn: TCheckBox
            Left = 10
            Top = 80
            Width = 126
            Height = 17
            Caption = 'Data Encipherment'
            TabOrder = 3
          end
          object CertUsageKeyEn: TCheckBox
            Left = 10
            Top = 100
            Width = 126
            Height = 17
            Caption = 'Key Encipherment'
            TabOrder = 4
          end
          object CertUsageKeyAgree: TCheckBox
            Left = 10
            Top = 120
            Width = 126
            Height = 17
            Caption = 'Key Agreement'
            TabOrder = 5
          end
          object CertUsageNonRepud: TCheckBox
            Left = 10
            Top = 140
            Width = 126
            Height = 17
            Caption = 'Key Repudiation'
            TabOrder = 6
          end
        end
        object GroupBox3: TGroupBox
          Left = 15
          Top = 291
          Width = 200
          Height = 101
          Caption = 'Extended Key Usage'
          TabOrder = 15
          object CertExtServer: TCheckBox
            Left = 10
            Top = 20
            Width = 181
            Height = 17
            Caption = 'TLS Web Server Authentication'
            TabOrder = 0
          end
          object CertExtClient: TCheckBox
            Left = 10
            Top = 40
            Width = 181
            Height = 17
            Caption = 'TLS Web Client Authentication'
            TabOrder = 1
          end
          object CertExtEmail: TCheckBox
            Left = 10
            Top = 60
            Width = 126
            Height = 17
            Caption = 'E-mail Protection'
            TabOrder = 2
          end
          object CertExtCodeSign: TCheckBox
            Left = 10
            Top = 80
            Width = 126
            Height = 17
            Caption = 'Code Signing'
            TabOrder = 3
          end
        end
        object CertLinesNew: TMemo
          Left = 433
          Top = 257
          Width = 401
          Height = 124
          ScrollBars = ssBoth
          TabOrder = 16
        end
        object CertAltDomains: TMemo
          Left = 518
          Top = 60
          Width = 290
          Height = 54
          Lines.Strings = (
            'www.domain.com'
            'mail.domain.com'
            'domain.com')
          ScrollBars = ssVertical
          TabOrder = 8
        end
        object CertAltIPs: TMemo
          Left = 518
          Top = 120
          Width = 290
          Height = 45
          ScrollBars = ssVertical
          TabOrder = 9
        end
        object NewCertCopyExt: TCheckBox
          Left = 437
          Top = 193
          Width = 124
          Height = 53
          Caption = 'Copy Request Extensions for New Certificate'
          TabOrder = 11
          WordWrap = True
        end
      end
      object GroupKeys: TGroupBox
        Left = 3
        Top = 10
        Width = 553
        Height = 311
        Caption = 'New Private and Public Key Properties'
        TabOrder = 1
        object KeyType: TRadioGroup
          Left = 10
          Top = 15
          Width = 251
          Height = 274
          Caption = 'Key Type'
          ItemIndex = 1
          Items.Strings = (
            'RSA 1,024 bits (level 1 - 80 bits)'
            'RSA 2,048 bits (level 2 - 112 bits) '
            'RSA 3,072 bits (level 3 - 128 bits, NIST min)'
            'RSA 4,096 bits (level 3 - 128 bits)'
            'RSA 7,680 bits (level 4 - 192 bits)'
            'RSA 15,360 bits (level 5 - 256 bits)'
            'Elliptic Curve secp256  (level 3 - 128 bits) '
            'Elliptic Curve secp384  (level 4 - 192 bits) '
            'Elliptic Curve secp512  (level 5 - 256 bits) '
            'EdDSA ED25519 (level 3 - 128 bits)  '
            'RSA-PSS 2,048 bits (level 2 - 112 bits) '
            'RSA-PSS 3,072 bits (level 3 - 128 bits)'
            'RSA-PSS 4,096 bits (level 3 - 128 bits)'
            'RSA-PSS 7,680 bits (level 4 - 192 bits)'
            'RSA-PSS 15,360 bits (level 5 - 256 bits)')
          TabOrder = 0
        end
        object KeyEncrypt: TRadioGroup
          Left = 275
          Top = 15
          Width = 185
          Height = 111
          Caption = 'Key File Encryption'
          ItemIndex = 0
          Items.Strings = (
            'None'
            'Triple DES'
            'IDEA'
            'AES128'
            'AES192'
            'AES256')
          TabOrder = 1
        end
        object KeyPairLines: TMemo
          Left = 275
          Top = 187
          Width = 261
          Height = 114
          Lines.Strings = (
            '')
          ScrollBars = ssBoth
          TabOrder = 3
        end
        object doGenKey: TButton
          Left = 274
          Top = 158
          Width = 109
          Height = 21
          Caption = 'Generate Key Pair'
          TabOrder = 2
          OnClick = doGenKeyClick
        end
      end
      object GroupQuickCerts: TGroupBox
        Left = 574
        Top = 2
        Width = 291
        Height = 318
        Caption = 'Create Quick Certificates (single function)'
        TabOrder = 2
        object Label15: TLabel
          Left = 5
          Top = 17
          Width = 275
          Height = 42
          Caption = 
            'These buttons use a single function to create certificates with ' +
            'a single PEM bundle file. using the Common and Alt Domain Names ' +
            'below, an existing file is always replaced'
          WordWrap = True
        end
        object Label16: TLabel
          Left = 5
          Top = 70
          Width = 124
          Height = 14
          Caption = 'Intermediate CA File Name'
        end
        object Label38: TLabel
          Left = 5
          Top = 155
          Width = 118
          Height = 14
          Caption = 'New Certificate File Path'
        end
        object Label40: TLabel
          Left = 5
          Top = 130
          Width = 50
          Height = 14
          Caption = 'Password'
        end
        object LabelAdminRightsRoot: TLabel
          Left = 12
          Top = 295
          Width = 256
          Height = 16
          Caption = 'Local Machine store has/needs admin rights'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -13
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object QuickCAFile: TEdit
          Left = 5
          Top = 90
          Width = 270
          Height = 22
          TabOrder = 0
          Text = 'Internal'
        end
        object QuickCertPath: TEdit
          Left = 5
          Top = 175
          Width = 270
          Height = 22
          TabOrder = 1
          Text = 'C:\ProgramData\ICS-OpenSSL\ICS-Certs\'
        end
        object doQuickCASign: TButton
          Left = 55
          Top = 239
          Width = 155
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Quick CA Sigmned Certificate'
          TabOrder = 2
          OnClick = doQuickCASignClick
        end
        object doQuickSelfSign: TButton
          Left = 55
          Top = 208
          Width = 155
          Height = 21
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Quick Self Signed Certificate'
          TabOrder = 3
          OnClick = doQuickSelfSignClick
        end
        object QuickPassword: TEdit
          Left = 66
          Top = 125
          Width = 163
          Height = 22
          TabOrder = 4
          Text = 'password'
        end
        object doInstallIcsRoot: TButton
          Left = 42
          Top = 267
          Width = 179
          Height = 22
          Margins.Left = 4
          Margins.Top = 4
          Margins.Right = 4
          Margins.Bottom = 4
          Caption = 'Install ICS Root in Windows Store'
          TabOrder = 5
          OnClick = doInstallIcsRootClick
        end
      end
    end
    object TabTestHosts: TTabSheet
      Caption = 'Test Host Certificates'
      ImageIndex = 4
      object Label5: TLabel
        Left = 10
        Top = 10
        Width = 260
        Height = 14
        Caption = 'Multiple SSL/TLS Hosts to Test, host:port (default 443)'
      end
      object Label6: TLabel
        Left = 296
        Top = 10
        Width = 140
        Height = 14
        Caption = 'Single SSL/TLS Hosts to Test'
      end
      object Label7: TLabel
        Left = 432
        Top = 335
        Width = 180
        Height = 14
        Caption = 'Downloaded Intermediate Certificates'
      end
      object Label32: TLabel
        Left = 10
        Top = 335
        Width = 144
        Height = 14
        Caption = 'Downloaded Host Certificates'
      end
      object Label35: TLabel
        Left = 456
        Top = 69
        Width = 211
        Height = 14
        Caption = 'Path to Save Downloaded Host Certificates '
      end
      object Label36: TLabel
        Left = 456
        Top = 124
        Width = 247
        Height = 14
        Caption = 'Path to Save Downloaded Intermediate Certificates '
      end
      object Label37: TLabel
        Left = 215
        Top = 335
        Width = 167
        Height = 14
        Caption = 'Double Click Selected to View Cert'
      end
      object TestHostList: TMemo
        Left = 10
        Top = 30
        Width = 265
        Height = 295
        Lines.Strings = (
          'www.magsys.co.uk'
          'ics.ftptest.org:990'
          'mail.magsys.co.uk:465'
          'www.telecom-tariffs.co.uk'
          'svn.overbyte.be'
          'www.embarcadero.com'
          'tp.embarcadero.com'
          'www.microsoft.com'
          'portal.azure.com'
          'login.microsoftonline.com'
          'outlook.office.com'
          'pop-mail.outlook.com:995'
          'www.google.com'
          'accounts.google.com'
          'mail.google.com'
          'smtp.gmail.com:465'
          'pop.gmail.com:995'
          'sectigo.com'
          'dev.digicert.com'
          'www.godaddy.com'
          'www.entrust.net'
          'www.globalsign.com'
          'www.certum.eu'
          'www.comodoca.com'
          'www.lawtrust.co.za'
          'www.quovadisglobal.com'
          'certs.securetrust.com'
          'www.starfieldtech.com'
          'www.swisssign.com'
          'enroll.visaca.com'
          'www.amazon.co.uk'
          'aws.amazon.com'
          'developers.cloudflare.com'
          'www.openssl.org'
          'developers.cloudflare.com'
          'letsencrypt.org'
          'www.w3schools.com'
          'www.okta.com'
          'api.twitter.com'
          'developer.twitter.com'
          'app.sipgate.com'
          'www.twilio.com'
          'www.hsbc.co.uk'
          'www.sophos.com'
          'www.worldpay.com'
          'www.feistyduck.com'
          'www.adobe.com')
        ScrollBars = ssVertical
        TabOrder = 0
        WordWrap = False
      end
      object TestHostOne: TEdit
        Left = 296
        Top = 30
        Width = 265
        Height = 22
        TabOrder = 1
        Text = 'www1.magsys.co.uk'
      end
      object doTestOneCert: TButton
        Left = 296
        Top = 65
        Width = 140
        Height = 25
        Caption = 'Test One Certificate'
        TabOrder = 4
        OnClick = doTestOneCertClick
      end
      object doTestListCerts: TButton
        Left = 296
        Top = 100
        Width = 140
        Height = 25
        Caption = 'Test List of Certificates'
        TabOrder = 5
        OnClick = doTestListCertsClick
      end
      object DownloadCerts: TCheckListBox
        Left = 10
        Top = 353
        Width = 416
        Height = 372
        ScrollWidth = 1200
        TabOrder = 10
        OnDblClick = DownloadCertsDblClick
      end
      object DownloadInters: TCheckListBox
        Left = 432
        Top = 354
        Width = 433
        Height = 372
        ScrollWidth = 1200
        TabOrder = 11
        OnDblClick = DownloadIntersDblClick
      end
      object DownCertsPath: TEdit
        Left = 456
        Top = 91
        Width = 405
        Height = 22
        TabOrder = 2
        Text = 'c:\mycertificates'
      end
      object DownIntersPath: TEdit
        Left = 456
        Top = 144
        Width = 405
        Height = 22
        TabOrder = 3
        Text = 'c:\myintermedates'
      end
      object SelDownHostPath: TBitBtn
        Left = 830
        Top = 60
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
        OnClick = SelDownHostPathClick
      end
      object SelDownInters: TBitBtn
        Left = 830
        Top = 113
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
        TabOrder = 13
        OnClick = SelDownIntersClick
      end
      object doHostSave: TButton
        Left = 296
        Top = 135
        Width = 140
        Height = 25
        Caption = 'Save Host Certs to Files'
        TabOrder = 6
        OnClick = doHostSaveClick
      end
      object doSaveInters: TButton
        Left = 296
        Top = 170
        Width = 140
        Height = 25
        Caption = 'Save Inter Certs to Files'
        TabOrder = 7
        OnClick = doSaveIntersClick
      end
      object doAddInterBundle: TButton
        Left = 296
        Top = 205
        Width = 140
        Height = 25
        Caption = 'Add Inters to Bundle File'
        TabOrder = 8
        OnClick = doAddInterBundleClick
      end
      object doTestAbort: TButton
        Left = 296
        Top = 300
        Width = 140
        Height = 25
        Caption = 'Abort Testing'
        TabOrder = 9
        OnClick = doTestAbortClick
      end
    end
  end
  object ProgressBar1: TProgressBar
    Left = 801
    Top = 4
    Width = 73
    Height = 16
    Anchors = [akTop, akRight]
    TabOrder = 1
    Visible = False
  end
  object Panel2: TPanel
    Left = 0
    Top = 759
    Width = 880
    Height = 39
    Align = alBottom
    TabOrder = 2
    object Status: TLabel
      Left = 5
      Top = 5
      Width = 864
      Height = 26
      AutoSize = False
      Caption = 'Status'
      Color = clActiveBorder
      ParentColor = False
      WordWrap = True
    end
  end
  object pmLv: TPopupMenu
    Left = 380
    Top = 749
    object pmShowDetails: TMenuItem
      Caption = 'Show Details'
      OnClick = LvCertsDblClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object pmCopy: TMenuItem
      Caption = 'Copy Certificate'
      OnClick = btnCopyCertClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pmDelete: TMenuItem
      Caption = 'Delete Certificate'
      OnClick = btnDeleteCertClick
    end
  end
  object OpenDlg: TOpenDialog
    Filter = 'All Files *.*|*.*|PEM Files *.pem|*.pem'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 133
    Top = 744
  end
  object MainMenu1: TMainMenu
    Left = 304
    Top = 746
    object MMFile: TMenuItem
      Caption = '&File'
      object MMResaveKey: TMenuItem
        Caption = 'Resave Private Key'
        OnClick = MMResaveKeyClick
      end
      object MMFileExit: TMenuItem
        Caption = '&Exit'
        OnClick = MMFileExitClick
      end
    end
    object MMExtras: TMenuItem
      Caption = '&Extras'
      object MMExtrasCreateSelfSignedCert: TMenuItem
        Caption = 'Create a self-signed certificate..'
        OnClick = MMExtrasCreateSelfSignedCertClick
      end
      object MMExtrasCreateCertRequest: TMenuItem
        Caption = 'Create a certificate request..'
        OnClick = MMExtrasCreateCertRequestClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MMExtrasEncryptStringRSA: TMenuItem
        Caption = 'RSA encrypt/decrypt..'
        OnClick = MMExtrasEncryptStringRSAClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MMExtrasEncryptStringBlowfish: TMenuItem
        Caption = 'Blowfish encrypt/decrypt string'
        OnClick = MMExtrasEncryptStringBlowfishClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MMExtrasEncryptStreamBlowfish: TMenuItem
        Caption = 'Blowfish encrypt/decrypt stream'
        OnClick = MMExtrasEncryptStreamBlowfishClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MMExtrasEncryptFileBlowfish: TMenuItem
        Caption = 'Blowfish encrypt file..'
        OnClick = MMExtrasEncryptFileBlowfishClick
      end
      object MMExtrasDecryptFileBlowfish: TMenuItem
        Caption = 'Blowfish decrypt file..'
        OnClick = MMExtrasDecryptFileBlowfishClick
      end
    end
  end
  object OpenDirDiag: TOpenDialog
    Options = [ofHideReadOnly, ofNoValidate, ofPathMustExist, ofNoTestFileCreate, ofEnableSizing]
    Title = 'Select Certificate Directory'
    Left = 212
    Top = 750
  end
  object IcsIpClient: TIcsIpStrmLog
    MaxSockets = 1
    RemoteIpPort = '443'
    SocFamily = sfAny
    LocalIpAddr = '0.0.0.0'
    LocalIpPort = '0'
    SrvIcsHosts = <>
    ForceSsl = False
    PingWaitSecs = 5
    CheckPing = False
    RetryAttempts = -1
    RetryWaitSecs = 2
    RetryNoImm = True
    AutoReconnect = True
    LogProtocol = logprotTcpClient
    KeepAliveSecs = 120
    UdpNoCRLF = False
    AddCRLF = True
    LineEndType = lineendLF
    CustomLineEnd = '$03'
    MaxLineLen = 132
    StripControls = True
    RawData = False
    MaxSendBuffer = 65536
    SndBufSize = 65536
    RcvBufSize = 65536
    SrvTimeoutSecs = 0
    MaxRecvData = 0
    LogSslCliSecurity = sslCliSecTls12
    SslSessCache = False
    LogSslVerMethod = logSslVerBundle
    LogSslRevocation = True
    LogSslReportChain = True
    LogSslRootFile = 'RootCaCertsBundle.pem'
    SrvCertAutoOrder = False
    CertExpireDays = 30
    UseUtf8 = False
    onLogRecvEvent = IcsIpClientLogRecvEvent
    onLogChangeEvent = IcsIpClientLogChangeEvent
    onLogProgEvent = IcsIpClientLogProgEvent
    OnLogHandshakeDone = IcsIpClientLogHandshakeDone
    Left = 459
    Top = 749
  end
end
