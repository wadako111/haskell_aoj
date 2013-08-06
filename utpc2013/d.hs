{-# OPTIONS_GHC -O2 -optc-Ofast -fignore-asserts #-}
import Debug.Trace
main :: IO ()
main = do
    getLine
    xs <- fmap (map read . words) getLine :: IO [Int]
    print $ answer xs

answer :: [Int] -> Int
answer xs
    | all (== head xs) xs = 1
    | otherwise =
        let m = minimum1 xs
        in (+) 1 $ foldr ((+) . answer) 0 $ split0 $ map (flip (-) m) xs

split0 :: [Int] -> [[Int]]
split0 [] = []
split0 (0:xs) = split0 xs
split0 xs = (\ (a, b) -> a: split0 b) $ break (== 0) xs

minimum1 :: [Int] -> Int
minimum1 xs 
    | 1 `elem` xs = 1
    | otherwise = minimum xs
