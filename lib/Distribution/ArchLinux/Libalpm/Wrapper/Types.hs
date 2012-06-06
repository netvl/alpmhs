module Distribution.ArchLinux.Libalpm.Wrapper.Types (
  AlpmConfig(..),
  AlpmError(..),
  AlpmLogEntry(..),
  AlpmEnv(..),
  AlpmLog(..),
  AlpmState(..),
  AlpmHandle(..),
  AlpmPkg(..),
  AlpmCallbacks(..),
  AlpmDepend(..),
  AlpmPgpkey(..),
  EventCallback,
  openHandle,
  defaultCallbacks
) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.IO.Class
import Control.Monad.RWS

import Foreign hiding (unsafeLocalState)
import Foreign.C.String
import Foreign.Marshal.Unsafe (unsafeLocalState)

import Distribution.ArchLinux.Libalpm.Raw
import Distribution.ArchLinux.Libalpm.Wrapper.List
import Distribution.ArchLinux.Libalpm.Wrapper.Util

-- | A configuration value used for ALPM library configuration.
data AlpmConfig = AlpmConfig {
    configRoot   :: String  -- ^ Path to root directory to be used by ALPM
                            -- (@/@ for Archlinux systems)
  , configDbPath :: String  -- ^ Path to directory with sync database 
                            -- (@/var/lib/pacman@ for Archlinux)
}

-- | An error value returned by ALPM functions.
data AlpmError = ErrorCode {
    errorMessage :: String  -- ^ Error message
  , errorCode    :: Int     -- ^ Error code
}

instance Error AlpmError where
  noMsg    = ErrorCode { errorMessage = "", errorCode = 0 }
  strMsg s = ErrorCode { errorMessage = s, errorCode = 0 }

-- | Converts ALPM error code to its user-readable representation.
fromAlpmErrno :: C'_alpm_errno_t  -- ^ Error code
              -> AlpmError
fromAlpmErrno errcode = unsafeLocalState $ do
  str <- peekCString =<< c'alpm_strerror errcode
  return $ ErrorCode { errorMessage = str, errorCode = (fromIntegral errcode) }

-- | Single log entry.
data AlpmLogEntry = AlpmLogEntry String

-- | An aggregation for all callbacks.
data AlpmCallbacks = AlpmCallbacks {
    callbackEvents :: Maybe EventCallback
}

-- | Default structure with no callbacks set.
defaultCallbacks :: AlpmCallbacks
defaultCallbacks = AlpmCallbacks { callbackEvents = Nothing }

-- | An environment for ALPM actions. Contains read-only data needed for ALPM to operate.
data AlpmEnv = AlpmEnv {
    envHandle :: AlpmHandle
  , envConfig :: AlpmConfig
  , envCallbacks :: Maybe AlpmCallbacks
}

-- | Contains log of ALPM operations.
data AlpmLog = AlpmLog { entries :: [AlpmLogEntry] }

instance Monoid AlpmLog where
  (AlpmLog entries1) `mappend` (AlpmLog entries2) = AlpmLog $ entries1 ++ entries2
  mempty = AlpmLog []

-- | Dynamic state of ALPM operations, not sure if needed.
data AlpmState = AlpmState

newtype AlpmHandle = AlpmHandle (ForeignPtr C'alpm_handle_t)
newtype AlpmDB = AlpmDB (ForeignPtr C'alpm_db_t)
newtype AlpmPkg = AlpmPkg (Ptr C'alpm_pkg_t)
newtype AlpmTrans = AlpmTrans (ForeignPtr C'alpm_trans_t)

-- | Initialize Alpm handle. This should be done once per usage of Libalpm.
openHandle :: String -> String -> IO (Either AlpmError AlpmHandle)
openHandle root dbPath = alloca $ \errBuf ->
  withCStrings [root, dbPath] $ \[rootPtr, dbPathPtr] -> do
    handlePtr <- c'alpm_initialize rootPtr dbPathPtr errBuf
    err <- peek errBuf
    if err > 0
      then return . Left $ fromAlpmErrno err
      else Right . AlpmHandle <$> newForeignPtr p'alpm_handle_finalizer handlePtr

newtype AlpmDepend = AlpmDepend (Ptr C'alpm_depend_t)
newtype AlpmDepmissing = AlpmDepmissing (Ptr C'alpm_depmissing_t)
newtype AlpmConflict = AlpmConflict (Ptr C'alpm_conflict_t)
newtype AlpmFileconflict = AlpmFileconflict (Ptr C'alpm_fileconflict_t)
newtype AlpmGroup = AlpmGroup (Ptr C'alpm_group_t)
newtype AlpmDelta = AlpmDelta (Ptr C'alpm_delta_t)
newtype AlpmFile = AlpmFile (Ptr C'alpm_file_t)
newtype AlpmFilelist = AlpmFilelist (Ptr C'alpm_filelist_t)
newtype AlpmBackup = AlpmBackup (Ptr C'alpm_backup_t)
newtype AlpmPgpkey = AlpmPgpkey (Ptr C'alpm_pgpkey_t)
newtype AlpmSigresult = AlpmSigresult (Ptr C'alpm_sigresult_t)
newtype AlpmSiglist = AlpmSiglist (Ptr C'alpm_siglist_t)

-- | Event callback type.
type EventCallback = C'alpm_cb_event


