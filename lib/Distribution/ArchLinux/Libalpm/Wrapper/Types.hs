module Distribution.ArchLinux.Libalpm.Wrapper.Types where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.IO.Class
import Control.Monad.RWS

import Foreign

import Distribution.ArchLinux.Libalpm.Raw.Types
import Distribution.ArchLinux.Libalpm.Wrapper.List

data AlpmConfig = AlpmConfig

data AlpmError = ErrorCode { message :: String, code :: Int }

instance Error AlpmError where
  noMsg = ErrorCode { message = "", code = 0 }
  strMsg s = ErrorCode { message = s, code = 0 }

data AlpmLogEntry = AlpmLogEntry String

data AlpmEnv = AlpmEnv { config :: AlpmConfig }

data AlpmLog = AlpmLog { entries :: [AlpmLogEntry] }

instance Monoid AlpmLog where
  (AlpmLog entries1) `mappend` (AlpmLog entries2) = AlpmLog $ entries1 ++ entries2
  mempty = AlpmLog []

data AlpmState = AlpmState

-- | Main ALPM execution monad, encapsulates all state needed to run ALPM transactions and
-- to perform other actions.
newtype Alpm a = Alpm { unAlpm :: RWST AlpmEnv AlpmLog AlpmState (ErrorT AlpmError IO) a }
               deriving (Applicative, Functor, Monad, MonadError AlpmError, MonadState AlpmState, 
                         MonadReader AlpmEnv, MonadWriter AlpmLog, MonadIO,
                         MonadRWS AlpmEnv AlpmLog AlpmState)

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

