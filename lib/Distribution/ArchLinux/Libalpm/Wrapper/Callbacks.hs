module Distribution.ArchLinux.Libalpm.Wrapper.Callbacks where

import Foreign
import Foreign.C.String

import Distribution.ArchLinux.Libalpm.Raw.Types
import Distribution.ArchLinux.Libalpm.Wrapper.Types

data AlpmEventHandlers = AlpmEventHandlers {
    eventCheckdepsStart :: Maybe (IO ())
  , eventCheckdepsDone :: Maybe (IO ())
}

(=:) :: (a -> Maybe b -> a) -> b -> [a -> a]
(=:) c v = [\s -> c s (Just v)]

(=*) :: [a -> Maybe b -> a] -> b -> [a -> a]
(=*) cs v = foldr (\c rs -> (c =: v) ++ rs) [] cs

handlers :: [[a -> a]] -> [a -> a]
handlers = concat

setHandlers :: [a -> a] -> a -> a
setHandlers = foldr (.) id

-- handlers $ [ eventCheckdepsStart =: return (), [ eventCheckdepsDone, eventAddStart] =* return () ]

class AlpmEventCallback a b | a -> b where
  wrap :: a -> b

instance AlpmEventCallback (IO ()) (IO ()) where
  wrap = id

instance AlpmEventCallback (AlpmPkg -> IO ()) (Ptr C'alpm_pkg_t -> IO ()) where
  wrap f = f . AlpmPkg

instance AlpmEventCallback (AlpmPkg -> AlpmPkg -> IO ())
                           (Ptr C'alpm_pkg_t -> Ptr C'alpm_pkg_t -> IO ()) where
  wrap f = \p1 p2 -> f (AlpmPkg p1) (AlpmPkg p2)

instance AlpmEventCallback (String -> IO ()) (CString -> IO ()) where
  wrap f = \cstr -> peekCString cstr >>= f

instance AlpmEventCallback (String -> String -> IO ()) (CString -> CString -> IO ()) where
  wrap f = \cstr1 cstr2 -> do
    str1 <- peekCString cstr1
    str2 <- peekCString cstr2
    f str1 str2


