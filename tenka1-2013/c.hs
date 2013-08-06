-- 121
-- 131
-- 123
-- 321
-- 213
-- 231
-- 312
-- 132
--
-- 123
-- 131

import Data.List
type Grid = [[Int]]

main :: IO ()
main = do
    [m, n] <- fmap (map read . words) getLine :: IO [Int]
    print "hoge"

generate :: Grid -> [Grid]
generate xs = [xs]

-- kouho :: Grid -> (Int, Int) -> [Int]
-- kouho grid (x, y) = 

isOK :: Grid -> (Int, Int) -> Int -> Bool
isOK grid (x, y) n = n `notElem` row
    where row = drop (x-n) $ take (x+n+1) $ grid !! y

firstZero :: Grid -> Maybe (Int, Int)
firstZero grid = justTuple i (row >>= elemIndex 0)
    where i = findIndex (elem 0) grid
          row = fmap (grid !!) i

justTuple :: Maybe a -> Maybe b -> Maybe (a, b)
justTuple (Just a) (Just b) = Just (a, b)
justTuple _ _ = Nothing
