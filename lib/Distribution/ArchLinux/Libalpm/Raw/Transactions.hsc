#include <bindings.dsl.h>
#include <alpm.h>

module Distribution.ArchLinux.Libalpm.Raw.Transactions where
#strict_import

import Distribution.ArchLinux.Libalpm.Raw.Types

#ccall alpm_trans_get_flags , Ptr <alpm_handle_t> -> IO <alpm_transflag_t>

#ccall alpm_trans_get_add , Ptr <alpm_handle_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_trans_get_remove , Ptr <alpm_handle_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_trans_init , Ptr <alpm_handle_t> -> <alpm_transflag_t> -> IO CInt

#ccall alpm_trans_prepare , Ptr <alpm_handle_t> -> Ptr (Ptr <alpm_list_t>) -> IO CInt

#ccall alpm_trans_commit , Ptr <alpm_handle_t> -> Ptr (Ptr <alpm_list_t>) -> IO CInt

#ccall alpm_trans_interrupt , Ptr <alpm_handle_t> -> IO CInt

#ccall alpm_trans_release , Ptr <alpm_handle_t> -> IO CInt


#ccall alpm_sync_sysupgrade , Ptr <alpm_handle_t> -> CInt -> IO CInt

#ccall alpm_add_pkg , Ptr <alpm_handle_t> -> Ptr <alpm_pkg_t> -> IO CInt

#ccall alpm_remove_pkg , Ptr <alpm_handle_t> -> Ptr <alpm_pkg_t> -> IO CInt


