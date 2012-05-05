module Distribution.ArchLinux.Libalpm.Wrapper.List where

import Control.Applicative
import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Distribution.ArchLinux.Libalpm.Raw

data AlpmList a = AlpmList (ForeignPtr C'alpm_list_t)

traverse :: (b -> Ptr a -> IO b) -> b -> AlpmList c -> IO b
traverse f v (AlpmList fptr) = withForeignPtr fptr (follow v)
  where
    follow v ptr =
      if ptr == nullPtr
      then return v
      else do
        C'alpm_list_t datum prev next <- peek ptr
        nv <- f v $ castPtr datum
        follow nv next

class AlpmListApplicable a b | a -> b, b -> a where
  toPtr :: a -> IO (Ptr b)
  fromPtr :: Ptr b -> IO a
  
  fromList :: [a] -> IO (AlpmList (Ptr b))
  fromList xs = AlpmList <$> (newForeignPtr p'alpm_list_free_full =<< nptr)
    where
      nptr = foldM reductor nullPtr xs
      reductor ptr x = (toPtr x :: IO (Ptr b)) >>= c'alpm_list_add ptr . castPtr

  toList :: AlpmList (Ptr b) -> IO [a]
  toList = traverse reductor []
    where
        reductor :: [a] -> Ptr b -> IO [a]
        reductor xs ptr = fromPtr ptr >>= return . (:xs)

instance AlpmListApplicable String CChar where
  toPtr = newCString
  fromPtr = peekCString
