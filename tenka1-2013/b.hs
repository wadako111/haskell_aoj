import Control.Monad

main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    ls <- replicateM n getLine
    print $ length $ filter' $ map (map read . words) ls

filter' :: [[Int]] -> [[Int]]
filter' [] = []
filter' (x:xs)
    | sum x < 20 = x:filter' xs
    | otherwise = filter' xs
