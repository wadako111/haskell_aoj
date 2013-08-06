import Control.Monad
import Data.List
type Range = (Int, Int)
type Weight = [Int]
main :: IO ()
main = do
    ns <- fmap (map read . words :: String -> [Int]) getLine
    let n = head ns
        s = ns !! 1
    ranges <- fmap (map (arrayToTuple . map read . words :: String -> (Int, Int))) $ replicateM n getLine
    weights <- fmap (map (map read . words :: String -> [Int])) $ replicateM n getLine
    print "hoge"

arrayToTuple :: [Int] -> (Int, Int)
arrayToTuple [x, y] = (x, y)

calcFromRoute :: [Range] -> Int -> Int
calcFromRoute ((from, to):xs) year = to - plus year from + calcFromRoute ((from, to):xs) to
    where plus i j = if i < j then 0 else i-j

bestRoute :: [Range] -> Int -> [Range]
bestRoute ranges year = answer best
    where best = selectBest ranges year
          answer (Just x) = x : bestRoute ranges (snd x)
          answer Nothing = []

include :: Int -> Range -> Bool
include year (from, to) = from <= year && year < to

selectBest :: [Range] -> Int -> Maybe Range
selectBest ranges year = if snd kouho > year then Just kouho else Nothing
    where kouho = minimumBy (f year) ranges
          f year (from1, to1) (from2, to2)
              | include year (from1, to1) = LT
              | include year (from2, to2) = GT
              | year < from1 && year < from2 = from1 `compare` from2
              | year > to1 && year < from2 = GT
              | year > to2 && year < from1 = LT
              | otherwise = EQ
