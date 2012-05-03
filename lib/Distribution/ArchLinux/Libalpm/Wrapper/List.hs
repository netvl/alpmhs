module Distribution.ArchLinux.Libalpm.Wrapper.List where

import Foreign.Ptr
import Foreign.Storable

import Distribution.ArchLinux.Libalpm.Raw

data AlpmList a = AlpmList (Ptr C'alpm_list_t)

fromList :: (Storable a) => [a] -> AlpmList a
fromList = undefined
