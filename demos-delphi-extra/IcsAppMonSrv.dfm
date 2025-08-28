object IcsAppMonControl: TIcsAppMonControl
  OnCreate = DDServiceCreate
  OnDestroy = DDServiceDestroy
  DisplayName = 'ICS Application Monitor'
  ServiceName = 'IcsAppMon'
  Description = 'ICS Application Monitor'
  FailureActions = <
    item
      ActionType = faRestart
      Delay = 10
    end>
  BeforeInstall = DDServiceBeforeInstall
  AfterInstall = DDServiceAfterInstall
  AfterUninstall = DDServiceAfterUninstall
  OnContinue = DDServiceContinue
  OnExecute = DDServiceExecute
  OnPause = DDServicePause
  OnRunException = DDServiceRunException
  OnShutdown = DDServiceShutdown
  OnStart = DDServiceStart
  OnStop = DDServiceStop
  Height = 248
  Width = 306
  PixelsPerInch = 120
end
