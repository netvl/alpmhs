{-# LANGUAGE TemplateHaskell #-}
module Distribution.ArchLinux.Libalpm.Wrapper.Callbacks.Event where

import Foreign
import Foreign.C.String

import Control.Applicative
import qualified Data.Map as M

import Distribution.ArchLinux.Libalpm.Raw.Types
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
generateEmptyRecord "emptyEventHandlers" ''EventHandlers 'EventHandlers

class EventHandlerFunction a where
  wrap :: a -> (Ptr () -> Ptr () -> IO ())

instance EventHandlerFunction (IO ()) where
  wrap f = \_ _ -> f

instance EventHandlerFunction (AlpmPkg -> IO ()) where
  wrap f = \ptr _ -> f (AlpmPkg $ castPtr ptr)

instance EventHandlerFunction (AlpmPkg -> AlpmPkg -> IO ()) where
  wrap f = \ptr1 ptr2 -> f (AlpmPkg $ castPtr ptr1) (AlpmPkg $ castPtr ptr2)

instance EventHandlerFunction (String -> IO ()) where
  wrap f = \ptr _ -> do
    str <- peekCString (castPtr ptr)
    f str

instance EventHandlerFunction (String -> String -> IO ()) where
  wrap f = \ptr1 ptr2 -> do
    str1 <- peekCString (castPtr ptr1)
    str2 <- peekCString (castPtr ptr2)
    f str1 str2

data EventHandler = forall a . EventHandlerFunction a => EventHandler a

data EventHandlerProjector = forall a . EventHandlerFunction a => 
                               EventHandlerProjector (EventHandlers -> Maybe a)

type EventHandlersMapping = M.Map C'alpm_event_t EventHandler

eventHandlerProjectorsMapping :: [(C'alpm_event_t, EventHandlerProjector)]
eventHandlerProjectorsMapping = 
  [ (c'ALPM_EVENT_CHECKDEPS_START,       EventHandlerProjector _eventCheckdepsStart)
  , (c'ALPM_EVENT_CHECKDEPS_DONE,        EventHandlerProjector _eventCheckdepsDone)
  , (c'ALPM_EVENT_FILECONFLICTS_START,   EventHandlerProjector _eventFileconflictsStart)
  , (c'ALPM_EVENT_FILECONFLICTS_DONE,    EventHandlerProjector _eventFileconflictsDone)
  , (c'ALPM_EVENT_RESOLVEDEPS_START,     EventHandlerProjector _eventResolvedepsStart)
  , (c'ALPM_EVENT_RESOLVEDEPS_DONE,      EventHandlerProjector _eventResolvedepsDone)
  , (c'ALPM_EVENT_INTERCONFLICTS_START,  EventHandlerProjector _eventInterconflictsStart)
  , (c'ALPM_EVENT_INTERCONFLICTS_DONE,   EventHandlerProjector _eventInterconflictsDone)
  , (c'ALPM_EVENT_ADD_START,             EventHandlerProjector _eventAddStart)
  , (c'ALPM_EVENT_ADD_DONE,              EventHandlerProjector _eventAddDone)
  , (c'ALPM_EVENT_REMOVE_START,          EventHandlerProjector _eventRemoveStart)
  , (c'ALPM_EVENT_REMOVE_DONE,           EventHandlerProjector _eventRemoveDone)
  , (c'ALPM_EVENT_UPGRADE_START,         EventHandlerProjector _eventUpgradeStart)
  , (c'ALPM_EVENT_UPGRADE_DONE,          EventHandlerProjector _eventUpgradeDone)
  , (c'ALPM_EVENT_INTEGRITY_START,       EventHandlerProjector _eventIntegrityStart)
  , (c'ALPM_EVENT_INTEGRITY_DONE,        EventHandlerProjector _eventIntegrityDone)
  , (c'ALPM_EVENT_LOAD_START,            EventHandlerProjector _eventLoadStart)
  , (c'ALPM_EVENT_LOAD_DONE,             EventHandlerProjector _eventLoadDone)
  , (c'ALPM_EVENT_DELTA_INTEGRITY_START, EventHandlerProjector _eventDeltaIntegrityStart)
  , (c'ALPM_EVENT_DELTA_INTEGRITY_DONE,  EventHandlerProjector _eventDeltaIntegrityDone)
  , (c'ALPM_EVENT_DELTA_PATCHES_START,   EventHandlerProjector _eventDeltaPatchesStart)
  , (c'ALPM_EVENT_DELTA_PATCHES_DONE,    EventHandlerProjector _eventDeltaPatchesDone)
  , (c'ALPM_EVENT_DELTA_PATCH_START,     EventHandlerProjector _eventDeltaPatchStart)
  , (c'ALPM_EVENT_DELTA_PATCH_DONE,      EventHandlerProjector _eventDeltaPatchDone)
  , (c'ALPM_EVENT_DELTA_PATCH_FAILED,    EventHandlerProjector _eventDeltaPatchFailed)
  , (c'ALPM_EVENT_SCRIPTLET_INFO,        EventHandlerProjector _eventScriptletInfo)
  , (c'ALPM_EVENT_RETRIEVE_START,        EventHandlerProjector _eventRetrieveStart)
  , (c'ALPM_EVENT_DISKSPACE_START,       EventHandlerProjector _eventDiskspaceStart)
  , (c'ALPM_EVENT_DISKSPACE_DONE,        EventHandlerProjector _eventDiskspaceDone)
  ]

collectEventHandlers :: EventHandlers -> EventHandlersMapping
collectEventHandlers eh = foldr reductor M.empty eventHandlerProjectorsMapping
  where
    reductor (e, EventHandlerProjector p) m = M.alter (\_ -> EventHandler <$> p eh) e m

-- | Create callback function from a map of handlers.
makeEventCallback :: EventHandlersMapping  -- ^ A map from event numbers to handlers
                  -> IO EventCallback      -- ^ A callback for event processing created 
                                           -- from handlers
makeEventCallback m = mk'alpm_cb_event $ \evt p1 p2 -> do
  case M.lookup evt m of
    Nothing               -> return ()
    Just (EventHandler h) -> (wrap h) p1 p2

