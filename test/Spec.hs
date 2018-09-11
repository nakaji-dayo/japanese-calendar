module Main where

import qualified Data.Time.Calendar          as C
import           Data.Time.Calendar.Japanese
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

instance Arbitrary Day where
  arbitrary = do
    g <- arbitraryBoundedEnum
    let f = startDayOf g
    (y,m,d) <- C.toGregorian . C.ModifiedJulianDay <$> choose (C.toModifiedJulianDay f, C.toModifiedJulianDay endOfSupport)
    return $ Day g y m d

main :: IO ()
main = hspec $ do
  describe "fromDay" $ do
    it "from 2018-09-11" $
      fromDay (read "2018-09-11") `shouldBe` Just (Day Heisei 30 9 11)
    it "my birthday" $
      fromDay (read "1989-01-08") `shouldBe` Just (Day Heisei 1 1 8)
    it "not supported" $
      fromDay (read "2019-05-01") `shouldBe` Nothing
  describe "toDay" $ do
      it "H16/2/9" $
        toDay (Day Heisei 16 2 29) `shouldBe` read "2004-02-29"
  prop "convert" $ forAll genSupportedDay $ \x ->
    toDay <$> fromDay x `shouldBe` Just x

genSupportedDay = C.ModifiedJulianDay <$>
  choose ( C.toModifiedJulianDay (startDayOf Meiji)
         , C.toModifiedJulianDay endOfSupport - 1)
