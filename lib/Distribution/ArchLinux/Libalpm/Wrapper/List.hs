module Distribution.ArchLinux.Libalpm.Wrapper.List where

import Foreign.Ptr
import Foreign.Storable
import Foreign.C.String

import Distribution.ArchLinux.Libalpm.Raw

data AlpmList a = AlpmList (Ptr C'alpm_list_t)

fromList :: (Storable a) => [a] -> AlpmList a
fromList []     = AlpmList nullPtr

fromListStr :: [String] -> IO (AlpmList String)
fromListStr [] = return $ AlpmList nullPtr
fromListStr ss = go nullPtr ss
    where
        go list [] = return $ AlpmList list
        go list (x:xs) = do 
            cx <- newCString x
            nlist <- c'alpm_list_add list (castPtr cx)
            go nlist xs
