module Distribution.ArchLinux.Libalpm.Wrapper.Types where

import Foreign

import Distribution.ArchLinux.Libalpm.Raw.Types
import Distribution.ArchLinux.Libalpm.Wrapper.List

newtype AlpmHandle = AlpmHandle (ForeignPtr C'alpm_handle_t)
newtype AlpmDB = AlpmDB (ForeignPtr C'alpm_db_t)
newtype AlpmPkg = AlpmPkg (ForeignPtr C'alpm_pkg_t)
newtype AlpmTrans = AlpmTrans (ForeignPtr C'alpm_trans_t)

newtype AlpmDepend = AlpmDepend (ForeignPtr C'alpm_depend_t)
newtype AlpmDepmissing = AlpmDepmissing (ForeignPtr C'alpm_depmissing_t)
newtype AlpmConflict = AlpmConflict (ForeignPtr C'alpm_conflict_t)
newtype AlpmFileconflict = AlpmFileconflict (ForeignPtr C'alpm_fileconflict_t)
newtype AlpmGroup = AlpmGroup (ForeignPtr C'alpm_group_t)
newtype AlpmDelta = AlpmDelta (ForeignPtr C'alpm_delta_t)
newtype AlpmFile = AlpmFile (ForeignPtr C'alpm_file_t)
newtype AlpmFilelist = AlpmFilelist (ForeignPtr C'alpm_filelist_t)
newtype AlpmBackup = AlpmBackup (ForeignPtr C'alpm_backup_t)
newtype AlpmPgpkey = AlpmPgpkey (ForeignPtr C'alpm_pgpkey_t)
newtype AlpmSigresult = AlpmSigresult (ForeignPtr C'alpm_sigresult_t)
newtype AlpmSiglist = AlpmSiglist (ForeignPtr C'alpm_siglist_t)

