#include <bindings.dsl.h>
#include <alpm.h>

module Distribution.ArchLinux.Libalpm.Raw where
#strict_import

#integral_t alpm_pkgreason_t

#opaque_t alpm_handle_t
#opaque_t alpm_db_t
#opaque_t alpm_pkg_t
#opaque_t alpm_trans_t

#starttype alpm_depend_t
#field name , CString
#field version , CString
#field name_hash , CULong
#field 
#stoptype



