#include <bindings.dsl.h>
#include <alpm_list.h>

module Distribution.ArchLinux.Libalpm.Raw.List where
#strict_import

import Distribution.ArchLinux.Libalpm.Raw.Types

#ccall alpm_list_add , Ptr <alpm_list_t> -> Ptr () -> IO (Ptr <alpm_list_t>)
#ccall alpm_list_free , Ptr <alpm_list_t> -> IO ()
