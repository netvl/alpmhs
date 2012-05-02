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



