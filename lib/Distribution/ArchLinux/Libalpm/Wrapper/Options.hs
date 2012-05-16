module Distribution.ArchLinux.Libalpm.Wrapper.Options where

import Distribution.ArchLinux.Libalpm.Wrapper.Types
import Distribution.ArchLinux.Libalpm.Wrapper.Alpm

type AlpmOptions = [AlpmOption]

data AlpmOption = AlpmOption

withAlpmOptions :: AlpmOptions -> Alpm a -> Alpm a
withAlpmOptions options act = loadOptions options >> act

loadOptions = undefined

