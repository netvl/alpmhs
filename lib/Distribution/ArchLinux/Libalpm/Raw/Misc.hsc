#include <bindings.dsl.h>
#include <alpm.h>

module Distribution.ArchLinux.Libalpm.Raw.Misc where
#strict_import

import Distribution.ArchLinux.Libalpm.Raw.Types

#ccall alpm_fetch_pkgurl , Ptr <alpm_handle_t> -> CString -> IO ()

#ccall alpm_siglist_cleanup , Ptr <alpm_siglist_t> -> IO CInt

#ccall alpm_find_group_pkgs , Ptr <alpm_list_t> -> CString -> IO (Ptr <alpm_list_t>)

#ccall alpm_sync_newversion , Ptr <alpm_pkg_t> -> Ptr <alpm_list_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_checkdeps , Ptr <alpm_handle_t> -> Ptr <alpm_list_t> -> Ptr <alpm_list_t> -> Ptr <alpm_list_t> -> CInt -> IO (Ptr <alpm_list_t>)

#ccall alpm_find_satisfier , Ptr <alpm_list_t> -> CString -> IO (Ptr <alpm_pkg_t>)

#ccall alpm_find_dbs_satisfier , Ptr <alpm_handle_t> -> Ptr <alpm_list_t> -> CString -> IO (Ptr <alpm_pkg_t>)

#ccall alpm_checkconflicts , Ptr <alpm_handle_t> -> Ptr <alpm_list_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_dep_compute_string , Ptr <alpm_depend_t> -> IO CString


#ccall alpm_compute_md5sum , CString -> IO CString

#ccall alpm_compute_sha256sum , CString -> IO CString


#ccall alpm_errno , Ptr <alpm_handle_t> -> IO <_alpm_errno_t>

#ccall alpm_strerror , <_alpm_errno_t> -> IO CString


#ccall alpm_initialize , CString -> CString -> Ptr <_alpm_errno_t> -> IO (Ptr <alpm_handle_t>)

#ccall alpm_release , Ptr <alpm_handle_t> -> IO CInt

#ccall alpm_handle_finalizer , Ptr <alpm_handle_t> -> IO ()


#ccall alpm_version , IO CString

#ccall alpm_capabilities , IO <alpm_caps>
