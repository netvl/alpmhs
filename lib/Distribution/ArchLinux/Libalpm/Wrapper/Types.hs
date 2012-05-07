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

