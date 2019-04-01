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
    (y,m,d) <- C.toGregorian . C.ModifiedJulianDay <$> choose (C.toModifiedJulianDay f, C.toModifiedJulianDay limitDate)
    return $ Day g y m d

main :: IO ()
main = hspec $ do
  describe "fromDay" $ do
    it "from 2018-09-11" $
      fromDay (read "2018-09-11") `shouldBe` Just (Day Heisei 30 9 11)
    it "my birthday" $
      fromDay (read "1989-01-08") `shouldBe` Just (Day Heisei 1 1 8)
    it "from 2019-05-01" $
      fromDay (read "2019-05-01") `shouldBe` Just (Day Reiwa 1 5 1)
  describe "toDay" $ do
      it "H16/2/9" $
        toDay (Day Heisei 16 2 29) `shouldBe` read "2004-02-29"
      it "R16/2/9" $
        toDay (Day Reiwa 16 2 28) `shouldBe` read "2034-02-28"
  prop "convert" $ forAll genSupportedDay $ \x ->
    toDay <$> fromDay x `shouldBe` Just x

genSupportedDay = C.ModifiedJulianDay <$>
  choose ( C.toModifiedJulianDay (startDayOf Meiji)
         , C.toModifiedJulianDay limitDate - 1)


limitDate :: C.Day
limitDate = C.fromGregorian 2100 1 1
