{-# LANGUAGE TemplateHaskell #-}
module Distribution.ArchLinux.Libalpm.Wrapper.Callbacks.Event where

import Foreign
import Foreign.C.String

import qualified Data.Map as M

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
}

generateUpdaters ''AlpmEventHandlers
generateEmptyRecord "emptyEventHandlers" ''AlpmEventHandlers 'AlpmEventHandlers

class AlpmEventHandler a where
  wrap :: a -> (Ptr () -> Ptr () -> IO ())

instance AlpmEventHandler (IO ()) where
  wrap f = \_ _ -> f

instance AlpmEventHandler (AlpmPkg -> IO ()) where
  wrap f = \ptr _ -> f (AlpmPkg $ castPtr ptr)

instance AlpmEventHandler (AlpmPkg -> AlpmPkg -> IO ()) where
  wrap f = \ptr1 ptr2 -> f (AlpmPkg $ castPtr ptr1) (AlpmPkg $ castPtr ptr2)

instance AlpmEventHandler (String -> IO ()) where
  wrap f = \ptr _ -> do
    str <- peekCString (castPtr ptr)
    f str

instance AlpmEventHandler (String -> String -> IO ()) where
  wrap f = \ptr1 ptr2 -> do
    str1 <- peekCString (castPtr ptr1)
    str2 <- peekCString (castPtr ptr2)
    f str1 str2

data EventHandler = forall a . AlpmEventHandler a => EventHandler a

type EventHandlers = M.Map C'alpm_event_t EventHandler

-- | Create callback function from a map of handlers.
makeEventCallback :: EventHandlers         -- ^ A map from event numbers to handlers
                  -> IO (C'alpm_cb_event)  -- ^ A callback for event processing created 
                                           -- from handlers
makeEventCallback m = mk'alpm_cb_event $ \evt p1 p2 -> do
  case M.lookup evt m of
    Nothing               -> return ()
    Just (EventHandler h) -> (wrap h) p1 p2

