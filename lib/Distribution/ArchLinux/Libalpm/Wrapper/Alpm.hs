module Distribution.ArchLinux.Libalpm.Wrapper.Alpm (
    Alpm,
    withinAlpmSession
) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.IO.Class
import Control.Monad.RWS

import Foreign hiding (unsafeLocalState)
import Foreign.C.String
import Foreign.Marshal.Unsafe (unsafeLocalState)

import Distribution.ArchLinux.Libalpm.Raw
import Distribution.ArchLinux.Libalpm.Wrapper.Types
import Distribution.ArchLinux.Libalpm.Wrapper.Util

-- | Main ALPM execution monad, encapsulates all state needed to run ALPM transactions and
-- to perform other actions.
newtype Alpm a = Alpm { unAlpm :: ErrorT AlpmError (RWST AlpmEnv AlpmLog AlpmState IO) a }
               deriving (Applicative, Functor, Monad, MonadError AlpmError, MonadState AlpmState, 
                         MonadReader AlpmEnv, MonadWriter AlpmLog, MonadIO,
                         MonadRWS AlpmEnv AlpmLog AlpmState)

-- | Perform an 'Alpm' action with newly initialized 'AlpmHandle' using provided 'AlpmConfig'
-- value.
withinAlpmSession :: AlpmConfig   -- ^ configuration for the system
                  -> Alpm a       -- ^ an action to perform
                  -> IO ((Either AlpmError a), AlpmLog)
withinAlpmSession conf@(AlpmConfig { configRoot, configDbPath }) (Alpm act) = do
  ehandle <- openHandle configRoot configDbPath
  case ehandle of
    Left err -> return (Left err, mempty)
    Right handle -> do
      env <- initEnv handle
      state <- initState
      evalRWST (runErrorT act) env state
  where
    initState      = return AlpmState
    initEnv handle = return AlpmEnv { envConfig = conf
                                    , envHandle = handle
                                    , envEventCallback = Nothing }

