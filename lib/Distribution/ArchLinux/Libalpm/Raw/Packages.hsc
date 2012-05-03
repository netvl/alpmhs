#include <bindings.dsl.h>
#include <alpm.h>

module Distribution.ArchLinux.Libalpm.Raw.Packages where
#strict_import

import System.Posix.Types

import Distribution.ArchLinux.Libalpm.Raw.Types

#ccall alpm_pkg_load , Ptr <alpm_handle_t> -> CString -> CInt -> <alpm_siglevel_t> -> Ptr (Ptr <alpm_pkg_t>) -> IO CInt

#ccall alpm_pkg_free , Ptr <alpm_pkg_t> -> IO CInt

#ccall alpm_pkg_checkmd5sum , Ptr <alpm_pkg_t> -> IO CInt

#ccall alpm_pkg_vercmp , CString -> CString -> IO CInt

#ccall alpm_pkg_compute_requiredby , Ptr <alpm_pkg_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_pkg_get_filename , Ptr <alpm_pkg_t> -> IO CString

#ccall alpm_pkg_get_name , Ptr <alpm_pkg_t> -> IO CString

#ccall alpm_pkg_get_version , Ptr <alpm_pkg_t> -> IO CString

#ccall alpm_pkg_get_origin , Ptr <alpm_pkg_t> -> IO <alpm_pkgfrom_t>

#ccall alpm_pkg_get_desc , Ptr <alpm_pkg_t> -> IO CString

#ccall alpm_pkg_get_url , Ptr <alpm_pkg_t> -> IO CString

#ccall alpm_pkg_get_builddate , Ptr <alpm_pkg_t> -> IO CTime

#ccall alpm_pkg_get_installdate , Ptr <alpm_pkg_t> -> IO CTime

#ccall alpm_pkg_get_packager , Ptr <alpm_pkg_t> -> IO CString

#ccall alpm_pkg_get_md5sum , Ptr <alpm_pkg_t> -> IO CString

#ccall alpm_pkg_get_sha256sum , Ptr <alpm_pkg_t> -> IO CString

#ccall alpm_pkg_get_arch , Ptr <alpm_pkg_t> -> IO CString

#ccall alpm_pkg_get_size , Ptr <alpm_pkg_t> -> IO COff

#ccall alpm_pkg_get_isize , Ptr <alpm_pkg_t> -> IO COff

#ccall alpm_pkg_get_reason , Ptr <alpm_pkg_t> -> IO <alpm_pkgreason_t>

#ccall alpm_pkg_get_licenses , Ptr <alpm_pkg_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_pkg_get_groups , Ptr <alpm_pkg_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_pkg_get_depends , Ptr <alpm_pkg_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_pkg_get_optdepends , Ptr <alpm_pkg_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_pkg_get_conflicts , Ptr <alpm_pkg_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_pkg_get_provides , Ptr <alpm_pkg_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_pkg_get_deltas , Ptr <alpm_pkg_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_pkg_get_replaces , Ptr <alpm_pkg_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_pkg_get_files , Ptr <alpm_pkg_t> -> IO (Ptr <alpm_filelist_t>)

#ccall alpm_pkg_get_backup , Ptr <alpm_pkg_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_pkg_get_db , Ptr <alpm_pkg_t> -> IO (Ptr <alpm_db_t>)

#ccall alpm_pkg_get_base64_sig , Ptr <alpm_pkg_t> -> IO CString

#ccall alpm_pkg_changelog_open , Ptr <alpm_pkg_t> -> IO (Ptr ())

#ccall alpm_pkg_changelog_read , Ptr () -> CSize -> Ptr <alpm_pkg_t> -> Ptr () -> IO CSize

#ccall alpm_pkg_changelog_close , Ptr <alpm_pkg_t> -> Ptr () -> IO CInt

#ccall alpm_pkg_has_scriptlet , Ptr <alpm_pkg_t> -> IO CInt

#ccall alpm_pkg_download_size , Ptr <alpm_pkg_t> -> IO COff

#ccall alpm_pkg_unused_deltas , Ptr <alpm_pkg_t> -> IO (Ptr <alpm_list_t>)

#ccall alpm_pkg_check_pgp_signature , Ptr <alpm_pkg_t> -> Ptr <alpm_siglist_t> -> IO CInt

