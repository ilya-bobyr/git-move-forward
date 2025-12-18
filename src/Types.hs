module Types
  ( App (..),
    Config,
  )
where

import Config (Config)
import RIO (HasLogFunc, LogFunc, lens, logFuncL)
import RIO.Process (HasProcessContext, ProcessContext, processContextL)

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appConfig :: !Config
    -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
