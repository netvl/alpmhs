module Distribution.ArchLinux.Libalpm.Wrapper.Types where

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

data AlpmConfig = AlpmConfig { root :: String, dbPath :: String }

data AlpmError = ErrorCode { message :: String, code :: Int }

instance Error AlpmError where
  noMsg = ErrorCode { message = "", code = 0 }
  strMsg s = ErrorCode { message = s, code = 0 }

fromAlpmErrno :: C'_alpm_errno_t -> AlpmError
fromAlpmErrno errcode = unsafeLocalState $ do
  str <- peekCString =<< c'alpm_strerror errcode
  return $ ErrorCode str (fromIntegral errcode)

data AlpmLogEntry = AlpmLogEntry String

data AlpmEnv = AlpmEnv { handle :: AlpmHandle, config :: AlpmConfig }

data AlpmLog = AlpmLog { entries :: [AlpmLogEntry] }

instance Monoid AlpmLog where
  (AlpmLog entries1) `mappend` (AlpmLog entries2) = AlpmLog $ entries1 ++ entries2
  mempty = AlpmLog []

data AlpmState = AlpmState

-- | Main ALPM execution monad, encapsulates all state needed to run ALPM transactions and
-- to perform other actions.
newtype Alpm a = Alpm { unAlpm :: ErrorT AlpmError (RWST AlpmEnv AlpmLog AlpmState IO) a }
               deriving (Applicative, Functor, Monad, MonadError AlpmError, MonadState AlpmState, 
                         MonadReader AlpmEnv, MonadWriter AlpmLog, MonadIO,
                         MonadRWS AlpmEnv AlpmLog AlpmState)

openHandle :: String -> String -> IO (Either AlpmError AlpmHandle)
openHandle root dbPath = alloca $ \errBuf ->
  withCStrings [root, dbPath] $ \[rootPtr, dbPathPtr] -> do
    handlePtr <- c'alpm_initialize rootPtr dbPathPtr errBuf
    err <- peek errBuf
    if err > 0
      then return $ Left $ fromAlpmErrno err
      else Right <$> AlpmHandle <$> newForeignPtr p'alpm_handle_finalizer handlePtr

withinAlpmSession :: AlpmConfig -> Alpm a -> IO ((Either AlpmError a), AlpmLog)
withinAlpmSession conf@(AlpmConfig { root, dbPath }) (Alpm act) = do
  ehandle <- openHandle root dbPath
  case ehandle of
    Left err -> return (Left err, mempty)
    Right handle -> do
      env <- initEnv handle
      state <- initState
      evalRWST (runErrorT act) env state
  where
    initState = return AlpmState
    initEnv handle = return AlpmEnv { config = conf, handle = handle }

newtype AlpmHandle = AlpmHandle (ForeignPtr C'alpm_handle_t)
newtype AlpmDB = AlpmDB (ForeignPtr C'alpm_db_t)
newtype AlpmPkg = AlpmPkg (ForeignPtr C'alpm_pkg_t)
newtype AlpmTrans = AlpmTrans (ForeignPtr C'alpm_trans_t)

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

