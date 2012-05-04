module Main where

import Distribution.ArchLinux.Libalpm.Wrapper

main :: IO ()
main = do
    alst <- fromList ["a", "b", "c"]
    hlst <- toList alst
    print hlst