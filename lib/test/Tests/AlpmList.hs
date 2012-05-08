module Tests.AlpmList where

import Control.Applicative
import Control.Concurrent
import Data.List

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
  putStrLn "Test with arbitrary chars"
  quickCheck $ prop_fromList_toList_strings
  putStrLn "Performing memory test"
  memoryTest

memoryTest :: IO ()
memoryTest = do
  lst <- concatMap (map getString) <$> 
    (replicate 100000 <$> sample' (arbitrary :: Gen StringWithNoNulls))
  alpmList <- withAlpmList lst Full toList
  putStrLn $ show $ length alpmList
  putStrLn "Press enter"
  _ <- getLine
  return ()

prop_fromList_toList_strings :: [String] -> Property
prop_fromList_toList_strings lst = 
  all ('\NUL' `notElem`) lst ==> 
    monadicIO $ do
      clst <- run $ withAlpmList lst Simple toList 
      assert $ clst == lst
