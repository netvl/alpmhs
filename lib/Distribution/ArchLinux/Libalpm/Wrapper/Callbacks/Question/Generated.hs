{-# LANGUAGE TemplateHaskell #-}
module Distribution.ArchLinux.Libalpm.Wrapper.Callbacks.Question.Generated where

import Distribution.ArchLinux.Libalpm.Wrapper.Types
import Distribution.ArchLinux.Libalpm.Wrapper.TH

-- | Contains handlers for questions which are asked by Libalpm when certain
-- user input is needed. 'Nothing' means that corresponding question will be
-- ignored. Please note that ignoring questions is a very bad thing, since
-- the result can be undefined. Use custom constant response handlers when
-- you need to ignore a question.
data QuestionHandlers = QuestionHandlers {
    -- | Asked when an installation of ignored package is requested.
    -- 'True' result means confirmation, 'False' means refusal.
    -- First argument is a package in question.
    _questionInstallIgnorepkg :: Maybe (AlpmPkg -> IO Bool)
    -- | Asked when some package will be replaced during the install.
    -- 'True' result means confirmation, 'False' means refusal.
    -- First argument is installed package, second argument is incoming package,
    -- not sure about the third one.
  , _questionReplacePkg       :: Maybe (AlpmPkg -> AlpmPkg -> String -> IO Bool)
    -- | Asked when there are package conflicts.
    -- 'True' result means remove installed package, 'False' means the opposite.
    -- First argument is incoming package, second one is installed package,
    -- third is some conflict string (TODO: find out more).
  , _questionConflictPkg      :: Maybe (AlpmPkg -> AlpmPkg -> String -> IO Bool)
    -- | Asked when there is a corrupted package found.
    -- 'True' result means delete offensive package, 'False' means keep it.
    -- First argument is a file name of the package, second one is 'AlpmError'
    -- structure with concrete error.
  , _questionCorruptedPkg     :: Maybe (String -> AlpmError -> IO Bool)
    -- | Asked when incoming package has an older version than the installed one.
    -- 'True' result means replace installed package, 'False' means the opposite.
  , _questionLocalNewer       :: Maybe (AlpmPkg -> IO Bool)
    -- | This is very strange event because pacman handler for it does not
    -- correspond its name. TODO: find out more
  , _questionRemovePkgs       :: Maybe ([AlpmPkg] -> IO Bool)
    -- | Asked when there are more than one source for the package.
    -- First argument is a list of packages-candidates for installation.
    -- The result is a zero-based (TODO: ascertain this) index of selected
    -- package in the list.
  , _questionSelectProvider   :: Maybe ([AlpmPkg] -> AlpmDepend -> IO Int)
    -- | Asked when user confirmation is needed to import PGP key.
    -- 'True' result means confirmation, 'False' means refusal.
    -- First argument is the PGP key in question.
  , _questionImportKey        :: Maybe (AlpmPgpkey -> IO Bool)
}

generateUpdaters ''QuestionHandlers

-- | Empty handlers aggregation structure with all handlers disabled.
generateEmptyRecord "emptyQuestionHandlers" ''QuestionHandlers 'QuestionHandlers

