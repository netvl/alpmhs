#include <bindings.dsl.h>
#include <alpm.h>

module Distribution.ArchLinux.Libalpm.Raw.Options where
#strict_import

import Distribution.ArchLinux.Libalpm.Raw.Types

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
