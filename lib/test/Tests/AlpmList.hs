module Tests.AlpmList where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.List
import System.IO

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
  --putStrLn "Test with not null chars"
  --quickCheck $ forAll (arbitrary :: Gen [StringWithNoNulls]) $ 
  --  prop_fromList_toList_strings . map getString
  putStrLn "Performing memory test"
  memoryTest

arbStrList :: IO [String]
arbStrList = map getString <$> take 10 <$> sample' (arbitrary :: Gen StringWithNoNulls)

memoryTest :: IO ()
memoryTest = do
  lst <- concat <$> (replicateM 100000 $ arbStrList)
  alpmList <- withAlpmList lst Full toList
  putStrLn $ show $ alpmList == lst
  putStr "Press enter" >> hFlush stdout
  _ <- getLine
  putStrLn $ show $ alpmList == lst
  return ()

prop_fromList_toList_strings :: [String] -> Property
prop_fromList_toList_strings lst = 
  all ('\NUL' `notElem`) lst ==> 
    monadicIO $ do
      clst <- run $ withAlpmList lst Full toList 
      assert $ clst == lst
