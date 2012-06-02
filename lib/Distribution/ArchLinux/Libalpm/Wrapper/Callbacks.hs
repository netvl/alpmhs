module Distribution.ArchLinux.Libalpm.Wrapper.Callbacks (
  (=:), (=*), disable, disableAll, handlers, setHandlers,
  module Distribution.ArchLinux.Libalpm.Wrapper.Callbacks.Event
) where

import Foreign
import Foreign.C.String

import Distribution.ArchLinux.Libalpm.Raw.Types
import Distribution.ArchLinux.Libalpm.Wrapper.Types
import Distribution.ArchLinux.Libalpm.Wrapper.TH

import Distribution.ArchLinux.Libalpm.Wrapper.Callbacks.Event

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
(=*) cs v = concatMap (=: v) cs

-- | Disables a handler for the specific event. To be used in conjunction with 'handlers'
-- and 'setHandlers'.
disable :: (Maybe b -> a -> a)  -- ^ Event identifier
        -> [a -> a]             -- ^ A singleton list with updater function
disable c = [c Nothing]

-- | Disables handlers for a number of event simultaneously. To be used in conjunction with
-- 'handlers' and 'setHandlers'.
disableAll :: [Maybe b -> a -> a]  -- ^ A number of event identifiers
           -> [a -> a]             -- ^ A list with updater functions
disableAll = concatMap disable

-- | Flattens a list of handler assignments created by '(=:)' and '(=*)' operators.
handlers :: [[a -> a]] -> [a -> a]
handlers = concat

-- | Sets all provided handlers in the specified handlers aggregation structure.
setHandlers :: [a -> a]   -- ^ A list of updater functions
            -> a          -- ^ Handler aggregation structure
            -> a          -- ^ An aggregation structure with handler places updated
setHandlers = foldr (.) id


