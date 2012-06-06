module Distribution.ArchLinux.Libalpm.Wrapper.Options where

import Control.Monad.Reader

import Distribution.ArchLinux.Libalpm.Wrapper.Types
import Distribution.ArchLinux.Libalpm.Wrapper.Callbacks
import Distribution.ArchLinux.Libalpm.Wrapper.Alpm

type AlpmOptions = [AlpmOption]

data AlpmOption = AlpmOption

withAlpmOptions :: AlpmOptions -> Alpm a -> Alpm a
withAlpmOptions options a = loadOptions options >> a

loadOptions = undefined

-- | Execute 'Alpm' monadic action with specified event handlers.
withEventHandlers :: EventHandlers -> Alpm a -> Alpm a
withEventHandlers eh a = do
  cb <- liftIO $ makeEventCallback eh
  local (updater cb) a
  where
    updater cb env =
      env {
        envCallbacks = Just $
          (maybe defaultCallbacks id (envCallbacks env)) {
            callbackEvents = Just cb
          }
      }

