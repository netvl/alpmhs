module Tests.AlpmList where

import Control.Applicative
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

prop_fromList_toList_strings :: [String] -> Property
prop_fromList_toList_strings lst = 
  all ('\NUL' `notElem`) lst ==> 
    monadicIO $ do
      alpm <- run $ fromList lst
      clst <- run $ toList alpm
      assert $ clst == lst
