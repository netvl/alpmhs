{-# LANGUAGE TemplateHaskell #-}
module Distribution.ArchLinux.Libalpm.Wrapper.Callbacks.Event (
  makeEventCallback,
  module Distribution.ArchLinux.Libalpm.Wrapper.Callbacks.Event.Generated
) where

import Foreign
import Foreign.C.String

import Control.Applicative
import qualified Data.Map as M

import Distribution.ArchLinux.Libalpm.Raw.Types
import Distribution.ArchLinux.Libalpm.Wrapper.Types
import Distribution.ArchLinux.Libalpm.Wrapper.Callbacks.Event.Generated

-- | A type class for converting high-level Haskell functions into theirs low-level C
-- equivalents. Specific to event callback.
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

-- | An existential type denoting a handler function, that is, a function which
-- can be called from C-level callback.
data EventHandler = forall a . EventHandlerFunction a => EventHandler a

-- | An existential type denoting a projector function, that is, a function which
-- maps 'EventHandlers' aggregation structure to a handler function, possibly
-- returning 'Nothing'.
data EventHandlerProjector = forall a . EventHandlerFunction a =>
                               EventHandlerProjector (EventHandlers -> Maybe a)

-- | A map from event identifiers (foreign 'C'alpm_event_t' integral type) to projector functions.
type EventHandlersMapping = M.Map C'alpm_event_t EventHandler

-- | An actual mapping from event identifiers to projector functions.
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

-- | Converts an event handlers aggregation structure to the map from event identifiers
-- to event handler functions. It uses 'eventHandlerProjectorsMapping' to extract
-- corresponding event handlers from the aggregation structure.
collectEventHandlers :: EventHandlers -> EventHandlersMapping
collectEventHandlers eh = foldr reductor M.empty eventHandlerProjectorsMapping
  where
    reductor (e, EventHandlerProjector p) m = M.alter (\_ -> EventHandler <$> p eh) e m

-- | Creates callback function from a map from event identifiers to handlers.
makeEventCallback' :: EventHandlersMapping  -- ^ A map from event numbers to handlers
                   -> IO EventCallback      -- ^ A callback for event processing created
                                            -- from handlers
makeEventCallback' m = mk'alpm_cb_event $ \evt p1 p2 -> do
  case M.lookup evt m of
    Nothing               -> return ()
    Just (EventHandler h) -> (wrap h) p1 p2

-- | Creates callback function from the event handlers aggregation structure.
makeEventCallback :: EventHandlers       -- ^ an event handlers aggregation structure
                  -> IO EventCallback    -- ^ a C-level callback
makeEventCallback = makeEventCallback' . collectEventHandlers

