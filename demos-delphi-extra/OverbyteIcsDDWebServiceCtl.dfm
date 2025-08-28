object DDWebServiceCtl: TDDWebServiceCtl
  OnCreate = DDServiceCreate
  AllowPause = False
  AllowedExControls = [alNetBindChange]
  DisplayName = 'Overbyte DDService Web Server'
  ServiceName = 'OverbyteDDWebService'
  Description = 'Overbyte DDService Web Server'
  BeforeInstall = DDServiceBeforeInstall
  AfterInstall = DDServiceAfterInstall
  BeforeUninstall = DDServiceBeforeUninstall
  OnExecute = DDServiceExecute
  OnNetBindChange = DDServiceNetBindChange
  OnPowerEvent = DDServicePowerEvent
  OnRunException = DDServiceRunException
  OnShutdown = DDServiceShutdown
  OnStart = DDServiceStart
  OnStop = DDServiceStop
  Height = 188
  Width = 269
  PixelsPerInch = 120
end
