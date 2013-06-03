fx :: Int -> Int
fx x = x ^ 2

main :: IO ()
main = do
  contents <- getContents
  let inputs = map read $ lines contents :: [Int]
  mapM_ print $ map solve inputs

solve :: Int -> Int
solve 0 = 0
solve d = foldl (\acc x -> acc + (d * fx x)) 0 [x * d | x <- [1..(quot 600 d - 1)]]

