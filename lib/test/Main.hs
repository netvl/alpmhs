module Main where

import Tests.AlpmList


main = do
    putStrLn "=== Testing AlpmList ==="
    runAlpmListTests

    putStrLn "=== Done ==="