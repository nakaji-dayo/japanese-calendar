module Data.Time.Calendar.Japanese where

import           Data.List
import qualified Data.Time.Calendar as C

data Gengo = Meiji | Taisho | Showa | Heisei
  deriving (Eq, Show, Read, Bounded, Enum)

data Day = Day Gengo Integer Int Int
  deriving (Eq, Show, Read)

-- |
-- Japanese Calendar(Wareki) Day from Data.Time.Calendar.Day.
-- Supports Meiji,Taisho,Showa,Heisei.
-- It returns Nothing if out of support range.
fromDay :: C.Day -> Maybe Day
fromDay day
  | day >= endOfSupport = Nothing
  | otherwise =
    let ms = find (\(g, s) -> day >= s) $ (\g -> (g, startDayOf g))
             <$> reverse [minBound .. maxBound]
    in uncurry fromBaseDay <$> ms
  where
    (y, m, d) = C.toGregorian day
    fromBaseDay g s =
      let (y', _, _) = C.toGregorian s
      in Day g (y - y' + 1) m d

-- |
-- Wareki to Data.Time.Calendar.Day.
toDay :: Day -> C.Day
toDay (Day g y m d) = C.fromGregorian (y + y' - 1) m d
  where (y', _, _) = C.toGregorian (startDayOf g)

endOfSupport :: C.Day
endOfSupport = C.fromGregorian 2019 5 1

startDayOf :: Gengo -> C.Day
startDayOf Heisei = C.fromGregorian 1989 1 8
startDayOf Showa  = C.fromGregorian 1926 12 26
startDayOf Taisho = C.fromGregorian 1912 7 30
startDayOf Meiji  = C.fromGregorian 1868 9 4

format :: Gengo -> String
format Heisei = "平成"
format Showa  = "昭和"
format Taisho = "大正"
format Meiji  = "明治"
