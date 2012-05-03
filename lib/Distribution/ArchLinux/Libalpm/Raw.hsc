#include <bindings.dsl.h>
#include <alpm.h>

module Distribution.ArchLinux.Libalpm.Raw where
#strict_import

import System.Posix.Types

-- ALPM List type
#starttype alpm_list_t
#field data , Ptr ()
#field prev , Ptr <alpm_list_t>
#field next , Ptr <alpm_list_t>
#stoptype

#integral_t alpm_pkgreason_t
#num ALPM_PKG_REASON_EXPLICIT
#num ALPM_PKG_REASON_DEPEND

#integral_t alpm_pkgfrom_t
#num PKG_FROM_FILE
#num PKG_FROM_LOCALDB
#num PKG_FROM_SYNCDB

#integral_t alpm_depmod_t
#num ALPM_DEP_MOD_ANY
#num ALPM_DEP_MOD_EQ
#num ALPM_DEP_MOD_GE
#num ALPM_DEP_MOD_LE
#num ALPM_DEP_MOD_GT
#num ALPM_DEP_MOD_LT

#integral_t alpm_fileconflicttype_t
#num ALPM_FILECONFLICT_TARGET
#num ALPM_FILECONFLICT_FILESYSTEM

#integral_t alpm_siglevel_t
#num ALPM_SIG_PACKAGE
#num ALPM_SIG_PACKAGE_OPTIONAL
#num ALPM_SIG_PACKAGE_MARGINAL_OK
#num ALPM_SIG_PACKAGE_UNKNOWN_OK
#num ALPM_SIG_DATABASE
#num ALPM_SIG_DATABASE_OPTIONAL
#num ALPM_SIG_DATABASE_MARGINAL_OK
#num ALPM_SIG_DATABASE_UNKNOWN_OK
#num ALPM_SIG_USE_DEFAULT

#integral_t alpm_sigstatus_t
#num ALPM_SIGSTATUS_VALID
#num ALPM_SIGSTATUS_KEY_EXPIRED
#num ALPM_SIGSTATUS_SIG_EXPIRED
#num ALPM_SIGSTATUS_KEY_UNKNOWN
#num ALPM_SIGSTATUS_KEY_DISABLED
#num ALPM_SIGSTATUS_INVALID

#integral_t alpm_sigvalidity_t
#num ALPM_SIGVALIDITY_FULL
#num ALPM_SIGVALIDITY_MARGINAL
#num ALPM_SIGVALIDITY_NEVER
#num ALPM_SIGVALIDITY_UNKNOWN

#integral_t alpm_transflag_t
#num ALPM_TRANS_FLAG_NODEPS
#num ALPM_TRANS_FLAG_FORCE
#num ALPM_TRANS_FLAG_NOSAVE
#num ALPM_TRANS_FLAG_NODEPVERSION
#num ALPM_TRANS_FLAG_CASCADE
#num ALPM_TRANS_FLAG_RECURSE
#num ALPM_TRANS_FLAG_DBONLY
#num ALPM_TRANS_FLAG_ALLDEPS
#num ALPM_TRANS_FLAG_DOWNLOADONLY
#num ALPM_TRANS_FLAG_NOSCRIPTLET
#num ALPM_TRANS_FLAG_NOCONFLICTS
#num ALPM_TRANS_FLAG_NEEDED
#num ALPM_TRANS_FLAG_ALLEXPLICIT
#num ALPM_TRANS_FLAG_UNNEEDED
#num ALPM_TRANS_FLAG_RECURSEALL
#num ALPM_TRANS_FLAG_NOLOCK

#integral_t enum _alpm_errno_t
#num ALPM_ERR_MEMORY
#num ALPM_ERR_SYSTEM
#num ALPM_ERR_BADPERMS
#num ALPM_ERR_NOT_A_FILE
#num ALPM_ERR_NOT_A_DIR
#num ALPM_ERR_WRONG_ARGS
#num ALPM_ERR_DISK_SPACE
#num ALPM_ERR_HANDLE_NULL
#num ALPM_ERR_HANDLE_NOT_NULL
#num ALPM_ERR_HANDLE_LOCK
#num ALPM_ERR_DB_OPEN
#num ALPM_ERR_DB_CREATE
#num ALPM_ERR_DB_NULL
#num ALPM_ERR_DB_NOT_NULL
#num ALPM_ERR_DB_NOT_FOUND
#num ALPM_ERR_DB_INVALID
#num ALPM_ERR_DB_INVALID_SIG
#num ALPM_ERR_DB_VERSION
#num ALPM_ERR_DB_WRITE
#num ALPM_ERR_DB_REMOVE
#num ALPM_ERR_SERVER_BAD_URL
#num ALPM_ERR_SERVER_NONE
#num ALPM_ERR_TRANS_NOT_NULL
#num ALPM_ERR_TRANS_NULL
#num ALPM_ERR_TRANS_DUP_TARGET
#num ALPM_ERR_TRANS_NOT_INITIALIZED
#num ALPM_ERR_TRANS_NOT_PREPARED
#num ALPM_ERR_TRANS_ABORT
#num ALPM_ERR_TRANS_TYPE
#num ALPM_ERR_TRANS_NOT_LOCKED
#num ALPM_ERR_PKG_NOT_FOUND
#num ALPM_ERR_PKG_IGNORED
#num ALPM_ERR_PKG_INVALID
#num ALPM_ERR_PKG_INVALID_CHECKSUM
#num ALPM_ERR_PKG_INVALID_SIG
#num ALPM_ERR_PKG_OPEN
#num ALPM_ERR_PKG_CANT_REMOVE
#num ALPM_ERR_PKG_INVALID_NAME
#num ALPM_ERR_PKG_INVALID_ARCH
#num ALPM_ERR_PKG_REPO_NOT_FOUND
#num ALPM_ERR_SIG_MISSING
#num ALPM_ERR_SIG_INVALID
#num ALPM_ERR_DLT_INVALID
#num ALPM_ERR_DLT_PATCHFAILED
#num ALPM_ERR_UNSATISFIED_DEPS
#num ALPM_ERR_CONFLICTING_DEPS
#num ALPM_ERR_FILE_CONFLICTS
#num ALPM_ERR_RETRIEVE
#num ALPM_ERR_INVALID_REGEX
#num ALPM_ERR_LIBARCHIVE
#num ALPM_ERR_LIBCURL
#num ALPM_ERR_EXTERNAL_DOWNLOAD
#num ALPM_ERR_GPGME

#integral_t enum alpm_caps
#num ALPM_CAPABILITY_NLS
#num ALPM_CAPABILITY_DOWNLOADER
#num ALPM_CAPABILITY_SIGNATURES

#opaque_t alpm_handle_t
#opaque_t alpm_db_t
#opaque_t alpm_pkg_t
#opaque_t alpm_trans_t

#starttype alpm_depend_t
#field name , CString
#field version , CString
#field name_hash , CULong
#field mod , <alpm_depmod_t>
#stoptype

#starttype alpm_depmissing_t
#field target , CString
#field depend , Ptr <alpm_depend_t>
#field causingpkg , CString
#stoptype

#starttype alpm_conflict_t
#field package1_hash , CULong
#field package2_hash , CULong
#field package1 , CString
#field package2 , CString
#field reason , Ptr <alpm_depend_t>
#stoptype

#starttype alpm_fileconflict_t
#field target , CString
#field type , <alpm_fileconflicttype_t>
#field file , CString
#field ctarget , CString
#stoptype

#starttype alpm_group_t
#field name , CString
#field packages , Ptr <alpm_list_t>
#stoptype

#starttype alpm_delta_t
#field delta , CString
#field delta_md5 , CString
#field from , CString
#field to , CString
#field delta_size , COff
#field download_size , COff
#stoptype

#starttype alpm_file_t
#field name , CString
#field size , COff
#field mode , CMode
#stoptype

#starttype alpm_filelist_t
#field count , CSize
#field files, Ptr <alpm_list_t>
#stoptype

#starttype alpm_backup_t
#field name , CString
#field hash , CString
#stoptype

#starttype alpm_pgpkey_t
#field data , Ptr ()
#field fingerprint , CString
#field uid , CString
#field name , CString
#field email , CString
#field created , CTime
#field expires , CTime
#stoptype

#starttype alpm_sigresult_t
#field key , <alpm_pgpkey_t>
#field status , <alpm_sigstatus_t>
#field validity , <alpm_sigvalidity_t>
#stoptype

#starttype alpm_siglist_t
#field count , CSize
#field results , Ptr <alpm_sigresult_t>
#stoptype

-- TODO: log callbacks
--
-- #integral_t alpm_loglevel_t
-- #num ALPM_LOG_ERROR
-- #num ALPM_LOG_WARNING
-- #num ALPM_LOG_DEBUG
-- #num ALPM_LOG_FUNCTION
--
--

#integral_t alpm_event_t 
#num ALPM_EVENT_CHECKDEPS_START
#num ALPM_EVENT_CHECKDEPS_DONE
#num ALPM_EVENT_FILECONFLICTS_START
#num ALPM_EVENT_FILECONFLICTS_DONE
#num ALPM_EVENT_RESOLVEDEPS_START
#num ALPM_EVENT_RESOLVEDEPS_DONE
#num ALPM_EVENT_INTERCONFLICTS_START
#num ALPM_EVENT_INTERCONFLICTS_DONE
#num ALPM_EVENT_ADD_START
#num ALPM_EVENT_ADD_DONE
#num ALPM_EVENT_REMOVE_START
#num ALPM_EVENT_REMOVE_DONE
#num ALPM_EVENT_UPGRADE_START
#num ALPM_EVENT_UPGRADE_DONE
#num ALPM_EVENT_INTEGRITY_START
#num ALPM_EVENT_INTEGRITY_DONE
#num ALPM_EVENT_LOAD_START
#num ALPM_EVENT_LOAD_DONE
#num ALPM_EVENT_DELTA_INTEGRITY_START
#num ALPM_EVENT_DELTA_INTEGRITY_DONE
#num ALPM_EVENT_DELTA_PATCHES_START
#num ALPM_EVENT_DELTA_PATCHES_DONE
#num ALPM_EVENT_DELTA_PATCH_START
#num ALPM_EVENT_DELTA_PATCH_DONE
#num ALPM_EVENT_DELTA_PATCH_FAILED
#num ALPM_EVENT_SCRIPTLET_INFO
#num ALPM_EVENT_RETRIEVE_START
#num ALPM_EVENT_DISKSPACE_START
#num ALPM_EVENT_DISKSPACE_DONE

#callback alpm_cb_event , <alpm_event_t> -> Ptr () -> Ptr () -> IO ()

#integral_t alpm_question_t
#num ALPM_QUESTION_INSTALL_IGNOREPKG
#num ALPM_QUESTION_REPLACE_PKG
#num ALPM_QUESTION_CONFLICT_PKG
#num ALPM_QUESTION_CORRUPTED_PKG
#num ALPM_QUESTION_LOCAL_NEWER
#num ALPM_QUESTION_REMOVE_PKGS
#num ALPM_QUESTION_SELECT_PROVIDER
#num ALPM_QUESTION_IMPORT_KEY

#callback alpm_cb_question , <alpm_question_t> -> Ptr () -> Ptr () -> Ptr () -> Ptr CInt -> IO ()

#integral_t alpm_progress_t
#num ALPM_PROGRESS_ADD_START
#num ALPM_PROGRESS_UPGRADE_START
#num ALPM_PROGRESS_REMOVE_START
#num ALPM_PROGRESS_CONFLICTS_START
#num ALPM_PROGRESS_DISKSPACE_START
#num ALPM_PROGRESS_INTEGRITY_START
#num ALPM_PROGRESS_LOAD_START

#callback alpm_cb_progress , <alpm_progress_t> -> CString -> CInt -> CSize -> CSize -> IO ()

#callback alpm_cb_download , CString -> COff -> COff -> IO ()

#callback alpm_cb_totaldl , COff -> IO ()

#callback alpm_cb_fetch , CString -> CString -> CInt -> IO ()


#ccall alpm_fetch_pkgurl , Ptr <alpm_handle_t> -> CString -> IO ()

-- TODO: make log callbacks working
-- #ccall alpm_option_get_logcb , Ptr <alpm_handle_t> -> IO <alpm_cb_log>
-- #ccall alpm_option_set_logcb , Ptr <alpm_handle_t> -> <alpm_log_cb> -> IO CInt


#ccall alpm_option_get_dlcb , Ptr <alpm_handle_t> -> IO <alpm_cb_download>
#ccall alpm_option_set_dlcb , Ptr <alpm_handle_t> -> <alpm_cb_download> -> IO CInt

#ccall alpm_option_get_fetchcb , Ptr <alpm_handle_t> -> IO <alpm_cb_fetch>
#ccall alpm_option_set_fetchcb , Ptr <alpm_handle_t> -> <alpm_cb_fetch> -> IO CInt

#ccall alpm_option_get_totaldlcb , Ptr <alpm_handle_t> -> IO <alpm_cb_totaldl>
#ccall alpm_option_set_totaldlcb , Ptr <alpm_handle_t> -> <alpm_cb_totaldl> -> IO CInt

#ccall alpm_option_get_eventcb , Ptr <alpm_handle_t> -> IO <alpm_cb_event>
#ccall alpm_option_set_eventcb , Ptr <alpm_handle_t> -> <alpm_cb_event> -> IO CInt

#ccall alpm_option_get_questioncb , Ptr <alpm_handle_t> -> IO <alpm_cb_question>
#ccall alpm_option_set_questioncb , Ptr <alpm_handle_t> -> <alpm_cb_question> -> IO CInt

#ccall alpm_option_get_progresscb , Ptr <alpm_handle_t> -> IO <alpm_cb_progress>
#ccall alpm_option_set_progresscb , Ptr <alpm_handle_t> -> <alpm_cb_progress> -> IO CInt


#ccall alpm_option_get_root , Ptr <alpm_handle_t> -> IO CString
#ccall alpm_option_get_dbpath , Ptr <alpm_handle_t> -> IO CString
#ccall alpm_option_get_lockfile , Ptr <alpm_handle_t> -> IO CString


#ccall alpm_option_get_cachedirs , Ptr <alpm_handle_t> -> IO (Ptr <alpm_list_t>)
#ccall alpm_option_set_cachedirs , Ptr <alpm_handle_t> -> Ptr <alpm_list_t> -> IO CInt
#ccall alpm_option_add_cachedir , Ptr <alpm_handle_t> -> CString -> IO CInt
#ccall alpm_option_remove_cachedir , Ptr <alpm_handle_t> -> CString -> IO CInt

#ccall alpm_option_get_logfile , Ptr <alpm_handle_t> -> IO CString
#ccall alpm_option_set_logfile , Ptr <alpm_handle_t> -> CString -> IO CInt 

#ccall alpm_option_get_gpgdir , Ptr <alpm_handle_t> -> IO CString
#ccall alpm_option_set_gpgdir , Ptr <alpm_handle_t> -> CString -> IO CInt


#ccall alpm_option_get_usesyslog , Ptr <alpm_handle_t> -> IO CInt
#ccall alpm_option_set_usesyslog , Ptr <alpm_handle_t> -> CInt -> IO CInt


#ccall alpm_option_get_noupgrades , Ptr <alpm_handle_t> -> IO (Ptr <alpm_list_t>)
#ccall alpm_option_add_noupgrade , Ptr <alpm_handle_t> -> CString -> IO CInt
#ccall alpm_option_set_noupgrades , Ptr <alpm_handle_t> -> Ptr <alpm_list_t> -> IO CInt
#ccall alpm_option_remove_noupgrade , Ptr <alpm_handle_t> -> CString -> IO CInt


#ccall alpm_option_get_noextracts , Ptr <alpm_handle_t> -> IO (Ptr <alpm_list_t>)
#ccall alpm_option_add_noextract , Ptr <alpm_handle_t> -> CString -> IO CInt
#ccall alpm_option_set_noextracts , Ptr <alpm_handle_t> -> Ptr <alpm_list_t> -> IO CInt
#ccall alpm_option_remove_noextract , Ptr <alpm_handle_t> -> CString -> IO CInt


#ccall alpm_option_get_ignorepkgs , Ptr <alpm_handle_t> -> IO (Ptr <alpm_list_t>)
#ccall alpm_option_add_ignorepkg , Ptr <alpm_handle_t> -> CString -> IO CInt
#ccall alpm_option_set_ignorepkgs , Ptr <alpm_handle_t> -> Ptr <alpm_list_t> -> IO CInt
#ccall alpm_option_remove_ignorepkg , Ptr <alpm_handle_t> -> CString -> IO CInt


#ccall alpm_option_get_ignoregroups , Ptr <alpm_handle_t> -> IO (Ptr <alpm_list_t>)
#ccall alpm_option_add_ignoregroup , Ptr <alpm_handle_t> -> CString -> IO CInt
#ccall alpm_option_set_ignoregroups , Ptr <alpm_handle_t> -> Ptr <alpm_list_t> -> IO CInt
#ccall alpm_option_remove_ignoregroup , Ptr <alpm_handle_t> -> CString -> IO CInt

#ccall alpm_option_get_arch , Ptr <alpm_handle_t> -> IO CString
#ccall alpm_option_set_arch , Ptr <alpm_handle_t> -> CString -> IO CInt

#ccall alpm_option_get_usedelta , Ptr <alpm_handle_t> -> IO CInt
#ccall alpm_option_set_usedelta , Ptr <alpm_handle_t> -> CInt -> IO CInt

#ccall alpm_option_get_checkspace , Ptr <alpm_handle_t> -> IO CInt
#ccall alpm_option_set_checkspace , Ptr <alpm_handle_t> -> CInt -> IO CInt

#ccall alpm_option_get_default_siglevel , Ptr <alpm_handle_t> -> IO <alpm_siglevel_t>
#ccall alpm_option_set_default_siglevel , Ptr <alpm_handle_t> -> <alpm_siglevel_t> -> IO CInt

#ccall alpm_option_get_localdb , Ptr <alpm_handle_t> -> IO (Ptr <alpm_db_t>)

#ccall alpm_option_get_syncdbs , Ptr <alpm_handle_t> -> IO (Ptr <alpm_list_t>)

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

#ccall alpm_db_check_pgp_signature , Ptr <alpm_db_t> -> Ptr <alpm_siglist_t> -> IO CInt

#ccall alpm_siglist_cleanup , Ptr <alpm_siglist_t> -> IO CInt

#ccall alpm_find_groups_pkg , Ptr <alpm_list_t> -> CString -> IO (Ptr <alpm_list_t>)

#ccall alpm_sync_newversion , Ptr <alpm_pkg_t> -> Ptr <alpm_list_t> -> IO (Ptr <alpm_list_t>)

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


#ccall alpm_version , IO CString

#ccall alpm_capabilities , IO <alpm_caps>


