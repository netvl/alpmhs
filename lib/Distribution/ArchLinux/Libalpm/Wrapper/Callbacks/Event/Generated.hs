{-# LANGUAGE TemplateHaskell #-}
module Distribution.ArchLinux.Libalpm.Wrapper.Callbacks.Event.Generated where

import Distribution.ArchLinux.Libalpm.Wrapper.Types
import Distribution.ArchLinux.Libalpm.Wrapper.TH

-- | Contains handlers for event callback to be called when corresponding event is emitted
-- from inside of Libalpm. 'Nothing' means that the event will be ignored, 'Just' value
-- sets the handler. Use 'withEventHandlers' function to set event handlers inside
-- 'Alpm' monad.
data EventHandlers = EventHandlers {
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
  , _eventInterconflictsDone  :: Maybe (IO ())
    -- | Package add procedure started. 'AlpmPkg' argument means the package
    -- being processed.
  , _eventAddStart            :: Maybe (AlpmPkg -> IO ())
    -- | Package add procedure finished. 'AlpmPkg' argument means the package
    -- that just has been added. Note: in fact Libalpm emits this event with two
    -- 'AlpmPkg' arguments, but it seems that second argument is always null so
    -- it is not represented here.
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
    -- | TODO: find description
  , _eventScriptletInfo       :: Maybe (String -> IO ())
    -- | Package retrieval started. TODO: find out what first argument means.
  , _eventRetrieveStart       :: Maybe (String -> IO ())
    -- | Disk space availability check started.
  , _eventDiskspaceStart      :: Maybe (IO ())
    -- | Disk space availability check finished.
  , _eventDiskspaceDone       :: Maybe (IO ())
}

generateUpdaters ''EventHandlers

-- | Empty handlers aggregation structure with all handlers disabled.
generateEmptyRecord "emptyEventHandlers" ''EventHandlers 'EventHandlers
