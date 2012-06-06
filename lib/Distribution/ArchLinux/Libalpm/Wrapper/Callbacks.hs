{-|
  This module exposes a system for configuration of callbacks.

  Callback in Libalpm terminology is a user-defined function which is used to handle signals
  which the library emits. For example, the library can notify us how the installation is going on
  or ask us whether we should accept package removal.

  There is seven types of callbacks of which six are implemented in the wrapper (supposedly):

    * event callback, which handles transaction state changes, e.g. start/completion of some
      installation step;

    * question callback, which is used to ask user whether he allows some operation like package
      removal on upgrade;

    * progress callback, which is used to track operation progress, e.g. a percentage of package
      integrity check;

    * download callback, which is used to notify about download progress of a single package;

    * total download callback, which is used to notify about total download size;

    * fetch callback, which is called when a download requested; can be used to implement
      custom downloading method or to delegate downloading to an external utility.

  The seventh one is log callback. It is called when Libalpm wants to log something.
  Unfortunately it uses C vararg mechanism which does not translate well to Haskell. Until I
  think something out about it, it will be unimplemented. The following description is relevant
  only to the six callback types described above.

  Some of the callbacks have well-defined signature with fixed number of arguments. But some
  have variable number of arguments with variable types modelled by a number of @void*@ parameters
  named like @data1@, @data2@ etc. These callbacks also receive an identifier of an \"event\"
  with which they are called, and this identifier uniquely determines which actual parameters
  are supplied to the callback. pacman simply @switch@es over event identifier to choose required
  action. This is unacceptable for high-level Haskell binding, so I had to analyze libalpm/pacman
  sources and establish a mapping from event identifiers to concrete parameters types and their
  semantics. This information is used as follows.

  For each type of callback we have separate Haskell record -- 'EventHandlers', (TODO: add more).
  Each of these has its accessors named in @_accessorName@ form (note the underscore); it also has
  a type @'Maybe' h@ (about @h@ type parameter later). From these functions a Template Haskell
  mechanism generates corresponding setter functions, which take a 'Maybe' value and a record and
  return an updated record. An @h@ type is constrained (logically, of course) to a set of
  predefined functional types. This set is specific to each record. Each type in such set
  corresponds to one collection of actual parameters provided to the specific callback.
  For each such set of types a type class is defined containing single method, @wrap@. This
  method takes a value of given type from the set and returns a value of C-level callback type,
  that is, a function which can be applied to corresponding amount of @void*@ arguments.

  Inside corresponding module there is a functions which convert described record into a 'Map'
  from corresponding event identifier type ('C'alpm_event_t', 'C'alpm_question_t' etc) to
  an existential type which is a container with single element of described type class.
  Another function uses this map to create actual C-level callback, which looks up a handler
  in the map by actual identifier supplied to it, wraps the handler using type class method
  and invokes it with supplied arguments of @void*@ type. A composition of these functions, i.e.
  a function of type @Record -> Callback@, is exposed from the module.

  All this machinery is needed for two reasons:

    * clean and sane internal implementation without much of copypaste;

    * usable and convenient interface.

  The implementation is described above. Interface to it basically consists of 3 elements:

    * a number of records (also described above);

    * corresponding number of functions from these records to callbacks;

    * convenience functions for constructing records.

  These convenience functions are defined in this module. They are very simple in implementation
  and could be applied to any record, not only to one of defined in callbacks subsystem.

  So, callbacks usage looks like this.

  @
    let hs = 'handlers' 'emptyEventHandlers' [
      'eventAddStart'     '=:' addStartHandler
    , 'eventUpgradeStart' '=:' upgradeStartHandler
    , [ 'eventIntegrityStart'
      , 'eventIntegrityDone'
      , 'eventLoadStart'
      , 'eventLoadDone'
      ...
      ] '=*' ignoreHandler
    ...
    ] in 'makeEventCallback' hs
  @

  This expression will return an 'IO' action which, when executed, yields a C-level callback which
  is a 'FunPtr' that can be set as a callback in Libalpm. Note that it is important that all
  setters in the inner list are of same type which corresponds to handler type.
  It is not possible to assign a handler which takes no arguments to an event with arguments, and
  vice versa.

  A few notes about terminology used in this documentation (some functions are not exposed but
  still have documentation comments).

    * event handler -- a part of callback, that is, a function which is called to process single
      event; a callback consists of a number of handlers;

    * event handlers aggregation structure -- basically, a record described above; it is a
      container for handlers;

    * event identifier -- an integral value of a certain type; each such type is specific to
      certain callback;

    * projector -- basically, a member of aggregation structure, that is, one of record accessors.
-}
module Distribution.ArchLinux.Libalpm.Wrapper.Callbacks (
  (=:), (=*), disable, disableAll, handlers,
  module Distribution.ArchLinux.Libalpm.Wrapper.Callbacks.Event,
  module Distribution.ArchLinux.Libalpm.Wrapper.Callbacks.Question
) where

import Foreign
import Foreign.C.String

import Distribution.ArchLinux.Libalpm.Wrapper.TH

import Distribution.ArchLinux.Libalpm.Wrapper.Callbacks.Event
import Distribution.ArchLinux.Libalpm.Wrapper.Callbacks.Question

-- | Adds a handler to the specific callback handlers aggregation structure.
-- To be used in conjunction with 'handlers'.
(=:) :: (Maybe b -> a -> a)     -- ^ Callback updater function
     -> b                       -- ^ Handler itself
     -> [a -> a]                -- ^ A singleton list with updater function
(=:) c v = [c (Just v)]

-- | Adds a handler to a number of callback events simultaneously. To be used in conjunction with
-- 'handlers'.
(=*) :: [Maybe b -> a -> a]     -- ^ A number of callback updater functions
     -> b                       -- ^ Handler itself
     -> [a -> a]                -- ^ A list with updater functions
(=*) cs v = concatMap (=: v) cs

-- | Disables a handler for the specific callback event. To be used in conjunction with 'handlers'.
disable :: (Maybe b -> a -> a)  -- ^ Callback event identifier
        -> [a -> a]             -- ^ A singleton list with updater function
disable c = [c Nothing]

-- | Disables handlers for a number of callback event simultaneously. To be used in conjunction with
-- 'handlers'.
disableAll :: [Maybe b -> a -> a]  -- ^ A number of callback event identifiers
           -> [a -> a]             -- ^ A list with updater functions
disableAll = concatMap disable

-- | Flattens a list of handler assignments created by '=:' and '=*' operators.
combineHandlers :: [[a -> a]] -> [a -> a]
combineHandlers = concat

-- | Sets all provided handlers in the specified handlers aggregation structure.
setHandlers :: [a -> a]   -- ^ A list of updater functions
            -> a          -- ^ Handler aggregation structure
            -> a          -- ^ An aggregation structure with handler places updated
setHandlers = foldr (.) id

-- | Merges specified handlers to the callback handlers aggregation structure.
handlers :: a           -- ^ A source aggregation structure
         -> [[a -> a]]  -- ^ An assignments list generated with '=:', '=*', 'disable'
                        -- and 'disableAll' functions
         -> a           -- ^ Source aggregation structure merged with specified handlers
handlers a m = setHandlers (combineHandlers m) a

