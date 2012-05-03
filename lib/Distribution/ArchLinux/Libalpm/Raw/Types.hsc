#include <bindings.dsl.h>
#include <alpm.h>

module Distribution.ArchLinux.Libalpm.Raw.Types where
#strict_import

import System.Posix.Types

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

#integral_t alpm_question_t
#num ALPM_QUESTION_INSTALL_IGNOREPKG
#num ALPM_QUESTION_REPLACE_PKG
#num ALPM_QUESTION_CONFLICT_PKG
#num ALPM_QUESTION_CORRUPTED_PKG
#num ALPM_QUESTION_LOCAL_NEWER
#num ALPM_QUESTION_REMOVE_PKGS
#num ALPM_QUESTION_SELECT_PROVIDER
#num ALPM_QUESTION_IMPORT_KEY

#integral_t alpm_progress_t
#num ALPM_PROGRESS_ADD_START
#num ALPM_PROGRESS_UPGRADE_START
#num ALPM_PROGRESS_REMOVE_START
#num ALPM_PROGRESS_CONFLICTS_START
#num ALPM_PROGRESS_DISKSPACE_START
#num ALPM_PROGRESS_INTEGRITY_START
#num ALPM_PROGRESS_LOAD_START

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

#callback alpm_cb_event , <alpm_event_t> -> Ptr () -> Ptr () -> IO ()

#callback alpm_cb_question , <alpm_question_t> -> Ptr () -> Ptr () -> Ptr () -> Ptr CInt -> IO ()

#callback alpm_cb_progress , <alpm_progress_t> -> CString -> CInt -> CSize -> CSize -> IO ()

#callback alpm_cb_download , CString -> COff -> COff -> IO ()

#callback alpm_cb_totaldl , COff -> IO ()

#callback alpm_cb_fetch , CString -> CString -> CInt -> IO ()
