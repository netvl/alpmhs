module Distribution.ArchLinux.Libalpm.Wrapper.Util where

import Foreign
import Foreign.C.String

withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings ss act = go ss []
  where
    go []     cs = act $ reverse cs
    go (s:ss) cs = withCString s $ \c -> go ss (c:cs)
