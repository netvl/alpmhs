module Distribution.ArchLinux.Libalpm.Wrapper.List (
  AlpmList,
  AlpmListApplicable(..),
  AlpmListFinalizer(..),
  withAlpmList
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Foreign
import Foreign.C.String
import Foreign.C.Types

import Distribution.ArchLinux.Libalpm.Raw

-- | ALPM List wrapper used by many library functions.
newtype AlpmList a = AlpmList (Ptr C'alpm_list_t)

-- | Type of list finalizing used in 'withAlpmList' function.
data AlpmListFinalizer = Full    -- ^ Apply 'free' to all elements before freeing the list
                                 -- cells
                       | Simple  -- ^ Free only list cells, do not free contents
                       | None    -- ^ Do not free anything

-- | This class represents those values lists of which can be converted
-- to 'AlpmList' object.
-- By default lists are converted to 'AlpmList' backed by plain 'Ptr'. Because of this
-- lists must be properly freed if they are created from Haskell code, e.g. with 'withAlpmList'
-- function.
class AlpmListApplicable a b | a -> b, b -> a where
  -- | Converts a value to its pointer representation.
  toPtr :: a -> IO (Ptr b)
  -- | Converts a pointer to its value.
  fromPtr :: Ptr b -> IO a

  -- | Converts a list of values to 'AlpmList'. Note that this function may cause memory leaks
  -- if not used correctly, because 'toPtr' function used internally may allocate arbitrary
  -- amount of unmanaged memory. Because of this consider using 'withAlpmList' function, which 
  -- allocates and frees 'AlpmList' correctly.
  fromList :: [a] -> IO (AlpmList a)
  fromList xs = AlpmList <$> nptr
    where
      nptr = foldM reductor nullPtr (reverse xs)
      reductor ptr x = (toPtr x :: IO (Ptr b)) >>= c'alpm_list_add ptr . castPtr

  -- | Traverses 'AlpmList' and builds Haskell list from it. This function should be safe to use
  -- because 'fromPtr' generally should not allocate unmanaged memory.
  toList :: AlpmList a -> IO [a]
  toList = traverse reductor []
    where
        reductor :: [a] -> Ptr b -> IO [a]
        reductor xs ptr = fromPtr ptr >>= return . (:xs)

instance AlpmListApplicable String CChar where
  toPtr = newCString
  fromPtr = peekCString

-- | Apply an 'IO' action to a list converted to 'AlpmList', freeing the latter after finishing
-- the action. Type of memory release can be specified.
withAlpmList :: AlpmListApplicable a b 
             => [a]                   -- ^ An input list
             -> AlpmListFinalizer     -- ^ Type of finalizer to use
             -> (AlpmList a -> IO c)  -- ^ An IO action to be applied to the list
             -> IO c            
withAlpmList lst fin f = bracket (fromList lst) (freeList fin) f
  where
    freeList Full   = freeListFull
    freeList Simple = freeListSimple
    freeList None   = const (return ())  -- Do nothing

traverse :: (b -> Ptr a -> IO b) -> b -> AlpmList c -> IO b
traverse f v (AlpmList ptr) = follow v ptr
  where
    follow v ptr =
      if ptr == nullPtr
      then return v
      else do
        C'alpm_list_t datum prev next <- peek ptr
        nv <- f v $ castPtr datum
        follow nv next

walk :: (Ptr C'alpm_list_t -> IO ()) -> Ptr C'alpm_list_t -> IO ()
walk f ptr = when (ptr /= nullPtr) $ do
  next <- peek $ p'alpm_list_t'next ptr
  f ptr
  walk f next

freeListFull :: AlpmList a -> IO ()
freeListFull (AlpmList ptr) = walk freeCellData ptr >> c'alpm_list_free ptr
  where
    freeCellData cptr = c'alpm_list_t'data <$> peek cptr >>= free

freeListSimple :: AlpmList a -> IO ()
freeListSimple (AlpmList ptr) = c'alpm_list_free ptr
