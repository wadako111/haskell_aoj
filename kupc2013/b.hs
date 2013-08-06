import Data.List
import Control.Applicative

type Ori = Int
type MaxLion = Int

main :: IO ()
main = do
    input1 <- (map read . words :: String -> [Int]) <$> getLine
    contents <- getContents
    let a = answer (head input1) (input1 !! 1) (map ((\ [x1, x2, x3] -> ([x1..x2],x3)) . (map read . words :: String -> [Int])) $ take (input1 !! 2) $ lines contents)
    putStrLn $ if null a then "-1" else unwords $ map show a

answer :: Ori -> MaxLion -> [([Int], Int)] -> [Int]
answer ori maxLion inputs = foldr (\ x acc -> if x == -1 then maxLion:acc else x:acc) [] $ (\ xs -> if null xs then [] else head xs) $ foldl1 mix $ map (calcKouhoWith1 ori maxLion) inputs

splitInt :: MaxLion -> Int -> Int -> [[Int]]
splitInt maxLion sumInt 1 = if sumInt <= maxLion then [[sumInt]] else []
splitInt maxLion sumInt to = [0..sumInt] >>= 
    (\ i -> if i <= maxLion then map (i:) $ splitInt maxLion (sumInt - i) (to - 1) else [])

calcKouhoWith1 :: Ori -> MaxLion -> ([Int], Int) -> [[Int]]
calcKouhoWith1 ori maxLion (range, sumInt) = map (\ xs -> replicate (head range - 1) (-1) ++ xs ++ replicate (ori - last range) (-1)) $ splitInt maxLion sumInt $ length range

mix :: [[Int]] -> [[Int]] -> [[Int]]
mix r1 r2 =  r1 >>= (\ xs -> r2 >>= (\ ys -> result $ f xs ys))
    where f :: [Int] -> [Int] -> Maybe [Int]
          f [] [] = Just []
          f (x:xs) (y:ys)
              | x == -1 = f xs ys >>= (\ yy -> Just (y:yy))
              | y == -1 = f xs ys >>= (\ xx -> Just (x:xx))
              | x == y = f xs ys >>= (\ xy -> Just (x:xy))
              | otherwise = Nothing
          result (Just x) = [x]
          result Nothing = []

hoge 0 = [[]]
hoge i = [a:as | a <- [0..10], as <- hoge (i - 1)]
