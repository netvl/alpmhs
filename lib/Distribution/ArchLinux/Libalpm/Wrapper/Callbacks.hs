{-# LANGUAGE TemplateHaskell #-}
module Distribution.ArchLinux.Libalpm.Wrapper.Callbacks where

import Foreign
import Foreign.C.String

import Distribution.ArchLinux.Libalpm.Raw.Types
import Distribution.ArchLinux.Libalpm.Wrapper.Types
import Distribution.ArchLinux.Libalpm.Wrapper.TH


-- | Contains event handlers to be called when corresponding event is emitted
-- from inside of Libalpm. 'Nothing' means that the event will be ignored, 'Just' value
-- sets the handler.
data AlpmEventHandlers = AlpmEventHandlers {
    -- | Dependency checks started.
    _eventCheckdepsStart      :: Maybe (IO ())
    -- | Dependency checks finished.
  , _eventCheckdepsDone       :: Maybe (IO ())
    -- | File conflicts checks started.
  , _eventFileconflictsStart  :: Maybe (IO ())
    -- | File conflicts checks finished.
  , _eventFileconflictsDone   :: Maybe (IO ())
    -- | Dependency resolving started.
  , _eventResolvedepsStart    :: Maybe (IO ())
    -- | Dependency resolving finished.
  , _eventResolvedepsDone     :: Maybe (IO ())
    -- | Internal conflicts checks started.
  , _eventInterconflictsStart :: Maybe (IO ())
    -- | Internal conflicts checks finished.
  , _eventInterconflictsEnd   :: Maybe (IO ())
    -- | Package add procedure started. 'AlpmPkg' argument means the package
    -- being processed.
  , _eventAddStart            :: Maybe (AlpmPkg -> IO ())
    -- | Package add procedure finished. 'AlpmPkg' argument means the package
    -- that just has been added. Note: in fact Libalpm emits this event with two
    -- 'AlpmPkg' arguments, but it seems that second argument is always null so
    -- it does not represented here.
  , _eventAddDone             :: Maybe (AlpmPkg -> IO ())
    -- | Package removal procedure started. 'AlpmPkg' argument means the package
    -- being removed.
  , _eventRemoveStart         :: Maybe (AlpmPkg -> IO ())
    -- | Package removal procedure finished. 'AlpmPkg' argument means the package
    -- that just has been removed.
  , _eventRemoveDone          :: Maybe (AlpmPkg -> IO ())
    -- | Package upgrade procedure started. First 'AlpmPkg' argument means new
    -- package that will be installed; second 'AlpmPkg' argument means old package
    -- that will be replaced.
  , _eventUpgradeStart        :: Maybe (AlpmPkg -> AlpmPkg -> IO ())
    -- | Package upgrade procedure finished. First 'AlpmPkg' argument means new
    -- package that just has been installed; second 'AlpmPkg' argument means old
    -- package that just has been replaced.
  , _eventUpgradeDone         :: Maybe (AlpmPkg -> AlpmPkg -> IO ())
    -- | Integrity checks started.
  , _eventIntegrityStart      :: Maybe (IO ())
    -- | Integrity checks finished.
  , _eventIntegrityDone       :: Maybe (IO ())
    -- | Package loading started.
  , _eventLoadStart           :: Maybe (IO ())
    -- | Package loading finished.
  , _eventLoadDone            :: Maybe (IO ())
    -- | Delta integrity checks started.
  , _eventDeltaIntegrityStart :: Maybe (IO ())
    -- | Delta integrity checks finished.
  , _eventDeltaIntegrityDone  :: Maybe (IO ())
    -- | Delta patches application started.
  , _eventDeltaPatchesStart   :: Maybe (IO ())
    -- | Delta patches application finished.
  , _eventDeltaPatchesDone    :: Maybe (IO ())
    -- | Single delta patch application started. First 'String' argument is a
    -- patch destination version; second 'String' argument is delta name (???).
  , _eventDeltaPatchStart     :: Maybe (String -> String -> IO ())
    -- | Single delta patch application finished successfully.
  , _eventDeltaPatchDone      :: Maybe (IO ())
    -- | Single delta patch application failed.
  , _eventDeltaPatchFailed    :: Maybe (IO ())
}

generateUpdaters ''AlpmEventHandlers
generateEmptyRecord "emptyEventHandlers" ''AlpmEventHandlers 'AlpmEventHandlers

-- | Assigns a handler to the specific event. To be used in conjunction with 'handlers'
-- and 'setHandlers'.
(=:) :: (Maybe b -> a -> a)     -- ^ Event identifier
     -> b                       -- ^ Handler itself
     -> [a -> a]                -- ^ A singleton list with updater function
(=:) c v = [c (Just v)]

-- | Assigns a handler to a number of events simultaneously. To be used in conjunction with
-- 'handlers' and 'setHandlers'.
(=*) :: [Maybe b -> a -> a]     -- ^ A number of event identifiers
     -> b                       -- ^ Handler itself
     -> [a -> a]                -- ^ A list with updater functions
(=*) cs v = foldr (\c rs -> (c =: v) ++ rs) [] cs

-- | Disables a handler for the specific event. To be used in conjunction with 'handlers'
-- and 'setHandlers'.
disable :: (Maybe b -> a -> a)  -- ^ Event identifier
        -> [a -> a]             -- ^ A singleton list with updater function
disable c = [c Nothing]

-- | Disables handlers for a number of event simultaneously. To be used in conjunction with
-- 'handlers' and 'setHandlers'.
disableAll :: [Maybe b -> a -> a]  -- ^ A number of event identifiers
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

-- | Represents callback function type which can be converted from high-level type to underlying
-- low-level C type.
class AlpmCallback a b | a -> b where
  wrap :: a -> b

instance AlpmCallback (IO ()) (IO ()) where
  wrap = id

instance AlpmCallback (AlpmPkg -> IO ()) (Ptr C'alpm_pkg_t -> IO ()) where
  wrap f = f . AlpmPkg

instance AlpmCallback (AlpmPkg -> AlpmPkg -> IO ())
                           (Ptr C'alpm_pkg_t -> Ptr C'alpm_pkg_t -> IO ()) where
  wrap f = \p1 p2 -> f (AlpmPkg p1) (AlpmPkg p2)

instance AlpmCallback (String -> IO ()) (CString -> IO ()) where
  wrap f = \cstr -> peekCString cstr >>= f

instance AlpmCallback (String -> String -> IO ()) (CString -> CString -> IO ()) where
  wrap f = \cstr1 cstr2 -> do
    str1 <- peekCString cstr1
    str2 <- peekCString cstr2
    f str1 str2


