import Control.Applicative
import Data.List
type Row = [Int]
type Board = [Row]
type Point = (Int, Int)

eatable :: Board -> [Point]
eatable board = hoge (transpose board) (map (nub . foldr f [] . (\ xs -> [findIndex notAte xs, (length xs - 1 -) <$> findIndex notAte (reverse xs)])) board) 0
    where f (Just x) acc = x:acc
          f Nothing acc = acc

notAte :: Int -> Bool
notAte (-1) = False
notAte _ = True

hoge :: [[Int]] -> [[Int]] -> Int -> [Point]
hoge _ [] _ = []
hoge transposed (t:target) step = concatMap (\ tt -> f ((step ==) <$> findIndex notAte (transposed !! tt)) tt step) t ++ hoge transposed target (step + 1)
    where f (Just True) x y = [(x, y)]
          f (Just False) _ _ = []
