#include <bindings.dsl.h>
#include <alpm.h>

module Distribution.ArchLinux.Libalpm.Raw.Database where
#strict_import

import Distribution.ArchLinux.Libalpm.Raw.Types

#ccall alpm_db_register_sync , Ptr <alpm_handle_t> -> CString -> <alpm_siglevel_t> -> IO (Ptr <alpm_db_t>)

#ccall alpm_db_unregister , Ptr <alpm_db_t> -> IO CInt

#ccall alpm_db_unregister_all , Ptr <alpm_handle_t> -> IO CInt

#ccall alpm_db_get_name , Ptr <alpm_db_t> -> IO CString

#ccall alpm_db_get_siglevel , Ptr <alpm_db_t> -> IO <alpm_siglevel_t>

#ccall alpm_db_get_valid , Ptr <alpm_db_t> -> IO CInt

#ccall alpm_db_get_servers , Ptr <alpm_db_t> -> IO (Ptr <alpm_list_t>)
#ccall alpm_db_set_servers , Ptr <alpm_db_t> -> Ptr <alpm_list_t> -> IO CInt
#ccall alpm_db_add_server , Ptr <alpm_db_t> -> CString -> IO CInt
#ccall alpm_db_remove_server , Ptr <alpm_db_t> -> CString -> IO CInt

#ccall alpm_db_update , CInt -> Ptr <alpm_db_t> -> IO CInt

#ccall alpm_db_get_pkg , Ptr <alpm_db_t> -> CString -> IO (Ptr <alpm_pkg_t>)

#ccall alpm_db_get_pkgcache , Ptr <alpm_db_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_db_readgroup , Ptr <alpm_db_t> -> CString -> IO (Ptr <alpm_group_t>)

#ccall alpm_db_get_groupcache , Ptr <alpm_db_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_db_search , Ptr <alpm_db_t> -> Ptr <alpm_list_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_db_set_pkgreason , Ptr <alpm_handle_t> -> Ptr <alpm_pkg_t> -> <alpm_pkgreason_t> -> IO CInt

#ccall alpm_db_check_pgp_signature , Ptr <alpm_db_t> -> Ptr <alpm_siglist_t> -> IO CInt


