module Distribution.ArchLinux.Libalpm.Wrapper.List (
  AlpmList,
  AlpmListApplicable(..)
) where

import Control.Applicative
import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Distribution.ArchLinux.Libalpm.Raw

-- | ALPM List wrapper used by many library functions. It is backed by 'ForeignPtr', so it will
-- automatically free underlying values when needed. It is safe to do so since most of 
-- ALPM functions do not store the list internally, copying it instead if needed.
newtype AlpmList a = AlpmList (ForeignPtr C'alpm_list_t)

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

-- | This class represents those values lists of which can be converted
-- to 'AlpmList' object.
-- By default lists are converted to 'AlpmList' backed by 'ForeignPtr' with special kind of
-- finalizer defined in helper.c file. This finalizer applies 'free' to each list element
-- using ALPM List function.
class AlpmListApplicable a b | a -> b, b -> a where
  -- | Converts a value to its pointer representation
  toPtr :: a -> IO (Ptr b)
  -- | Converts a pointer to its value
  fromPtr :: Ptr b -> IO a
  
  -- | Converts a list of values to 'AlpmList'
  fromList :: [a] -> IO (AlpmList a)
  fromList xs = AlpmList <$> (newForeignPtr p'alpm_list_free_full =<< nptr)
    where
      nptr = foldM reductor nullPtr (reverse xs)
      reductor ptr x = (toPtr x :: IO (Ptr b)) >>= c'alpm_list_add ptr . castPtr

  -- | Traverses 'AlpmList' and builds Haskell list from it
  toList :: AlpmList a -> IO [a]
  toList = traverse reductor []
    where
        reductor :: [a] -> Ptr b -> IO [a]
        reductor xs ptr = fromPtr ptr >>= return . (:xs)

instance AlpmListApplicable String CChar where
  toPtr = newCString
  fromPtr = peekCString
