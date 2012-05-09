module Tests.AlpmList where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.List
import System.IO
import System.Mem

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Distribution.ArchLinux.Libalpm.Wrapper.List

newtype StringWithNoNulls = StringWithNoNulls { getString :: String } deriving (Show)

instance Arbitrary StringWithNoNulls where
  arbitrary = StringWithNoNulls <$> listOf almostArbitraryChar
    where
      almostArbitraryChar = (arbitrary :: Gen Char) `suchThat` (/= '\NUL')

runAlpmListTests :: IO ()
runAlpmListTests = do
  putStrLn "Test with not null chars"
  quickCheck $ forAll (arbitrary :: Gen [StringWithNoNulls]) $ 
    prop_fromList_toList_strings . map getString
  --putStrLn "Performing memory test"
  --memoryTest

arbStrList :: IO [String]
arbStrList = map getString <$> take 10 <$> sample' (arbitrary :: Gen StringWithNoNulls)

simpleStrList :: IO [String]
simpleStrList = return $ replicate 10 ['0'..'9']

wait = do
  putStr "Press enter" >> hFlush stdout
  _ <- getLine  
  return ()

memoryTest :: IO ()
memoryTest = do
  lst <- concat <$> (replicateM 100000 $ simpleStrList)
  alpmList <- withAlpmList lst Full toList
  wait
  
  putStrLn $ show $ alpmList == lst
  performGC

  wait
  return ()

prop_fromList_toList_strings :: [String] -> Property
prop_fromList_toList_strings lst = 
  all ('\NUL' `notElem`) lst ==> 
    monadicIO $ do
      clst <- run $ withAlpmList lst Full toList 
      assert $ clst == lst
