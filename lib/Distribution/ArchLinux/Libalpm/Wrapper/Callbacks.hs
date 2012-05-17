module Distribution.ArchLinux.Libalpm.Wrapper.Callbacks (
  AlpmEventHandlers(..),
  AlpmEventCallback(..),
  (=:), (=*), handlers, setHandlers
) where

import Foreign
import Foreign.C.String

import Distribution.ArchLinux.Libalpm.Raw.Types
import Distribution.ArchLinux.Libalpm.Wrapper.Types

-- | Contains event handlers to be called when corresponding event is emitted
-- from inside of Libalpm. 'Nothing' means that the event will be ignored, 'Just' value
-- sets the handler.
data AlpmEventHandlers = AlpmEventHandlers {
    -- | Dependency checks started.
    eventCheckdepsStart      :: Maybe (IO ())
    -- | Dependency checks finished.
  , eventCheckdepsDone       :: Maybe (IO ())
    -- | File conflicts checks started.
  , eventFileconflictsStart  :: Maybe (IO ())
    -- | File conflicts checks finished.
  , eventFileconflictsDone   :: Maybe (IO ())
    -- | Dependency resolving started.
  , eventResolvedepsStart    :: Maybe (IO ())
    -- | Dependency resolving finished.
  , eventResolvedepsDone     :: Maybe (IO ())
    -- | Internal conflicts checks started.
  , eventInterconflictsStart :: Maybe (IO ())
    -- | Internal conflicts checks finished.
  , eventInterconflictsEnd   :: Maybe (IO ())
    -- | Package add procedure started. 'AlpmPkg' argument means the package
    -- being processed.
  , eventAddStart            :: Maybe (AlpmPkg -> IO ())
    -- | Package add procedure finished. 'AlpmPkg' argument means the package
    -- that just has been added. Note: in fact Libalpm emits this event with two
    -- 'AlpmPkg' arguments, but it seems that second argument is always null so
    -- it does not represented here.
  , eventAddDone             :: Maybe (AlpmPkg -> IO ())
    -- | Package removal procedure started. 'AlpmPkg' argument means the package
    -- being removed.
  , eventRemoveStart         :: Maybe (AlpmPkg -> IO ())
    -- | Package removal procedure finished. 'AlpmPkg' argument means the package
    -- that just has been removed.
  , eventRemoveDone          :: Maybe (AlpmPkg -> IO ())
    -- | Package upgrade procedure started. First 'AlpmPkg' argument means new
    -- package that will be installed; second 'AlpmPkg' argument means old package
    -- that will be replaced.
  , eventUpgradeStart        :: Maybe (AlpmPkg -> AlpmPkg -> IO ())
    -- | Package upgrade procedure finished. First 'AlpmPkg' argument means new
    -- package that just has been installed; second 'AlpmPkg' argument means old
    -- package that just has been replaced.
  , eventUpgradeDone         :: Maybe (AlpmPkg -> AlpmPkg -> IO ())
    -- | Integrity checks started.
  , eventIntegrityStart      :: Maybe (IO ())
    -- | Integrity checks finished.
  , eventIntegrityDone       :: Maybe (IO ())
    -- | Package loading started.
  , eventLoadStart           :: Maybe (IO ())
    -- | Package loading finished.
  , eventLoadDone            :: Maybe (IO ())
    -- | Delta integrity checks started.
  , eventDeltaIntegrityStart :: Maybe (IO ())
    -- | Delta integrity checks finished.
  , eventDeltaIntegrityDone  :: Maybe (IO ())
    -- | Delta patches application started.
  , eventDeltaPatchesStart   :: Maybe (IO ())
    -- | Delta patches application finished.
  , eventDeltaPatchesDone    :: Maybe (IO ())
    -- | Single delta patch application started. First 'String' argument is a 
    -- patch destination version; second 'String' argument is delta name (???).
  , eventDeltaPatchStart     :: Maybe (String -> String -> IO ())
    -- | Single delta patch application finished successfully.
  , eventDeltaPatchDone      :: Maybe (IO ())
    -- | Single delta patch application failed.
  , eventDeltaPatchFailed    :: Maybe (IO ())
}

-- | Assigns a handler to the specific event. To be used in conjunction with 'handlers'
-- and 'setHandlers'.
(=:) :: (a -> Maybe b -> a)     -- ^ Event identifier
     -> b                       -- ^ Handler itself
     -> [a -> a]                -- ^ A singleton list with updater function
(=:) c v = [\s -> c s (Just v)]

-- | Assigns a handler to a number of events simultaneously. To be used in conjunction with
-- 'handlers' and 'setHandlers'.
(=*) :: [a -> Maybe b -> a]     -- ^ A number of event identifiers
     -> b                       -- ^ Handler itself
     -> [a -> a]                -- ^ A list with updater functions
(=*) cs v = foldr (\c rs -> (c =: v) ++ rs) [] cs

-- | Disables a handler for the specific event. To be used in conjunction with 'handlers'
-- and 'setHandlers'.
disable :: (a -> Maybe b -> a)  -- ^ Event identifier
        -> [a -> a]             -- ^ A singleton list with updater function
disable c = [\s -> c s Nothing]

-- | Disables handlers for a number of event simultaneously. To be used in conjunction with
-- 'handlers' and 'setHandlers'.
disableAll :: [a -> Maybe b -> a]  -- ^ A number of event identifiers
           -> [a -> a]             -- ^ A list with updater functions
disableAll = foldr (\c rs -> disable c ++ rs) []

-- | Flattens a list of handler assignments created by '(=:)' and '(=*)' operators.
handlers :: [[a -> a]] -> [a -> a]
handlers = concat

-- | Sets all provided handlers in the specified handlers aggregation structure.
setHandlers :: [a -> a]   -- ^ A list of updater functions
            -> a          -- ^ Handler aggregation structure
            -> a          -- ^ An aggregation structure with handler places updated
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


