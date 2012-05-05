module Distribution.ArchLinux.Libalpm.Wrapper.List where

import Control.Applicative
import Control.Monad
import Foreign
import Foreign.C.String

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

class AlpmListConvertible a b where
  convert :: a -> IO (Ptr b)
  
  fromList :: [a] -> IO (AlpmList b)
  fromList xs = AlpmList <$> newForeignPtr p'alpm_list_free_full <*> nptr
    where
      nptr = foldM reductor nullPtr xs
      reductor :: Ptr b -> a -> IO (Ptr b)
      reductor ptr x = convert x >>= c'alpm_list_add ptr . castPtr

  toList :: AlpmList b -> IO [a]

instance AlpmListConvertible String CString where
  fromList [] = newForeignPtr_ nullPtr >>= return . AlpmList
  fromList ss = go nullPtr ss
    where
      go list [] = AlpmList <$> newForeignPtr p'alpm_list_free_full list 
      go list (x:xs) = do
        cx <- newCString x
        nlist <- c'alpm_list_add list (castPtr cx)
        go nlist xs

  toList = traverse reductor [] 
    where
      reductor xs ptr = peek ptr >>= peekCString >>= return . (:xs)
