isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n
  | n `mod` 2 == 0 = False
  | length (take 1 [x | x <- [3..quot n 2], mod n x == 0]) > 0 = False
  | otherwise = True

main :: IO ()
main = do
  contents <- getContents
  mapM_ (\n -> print $ length [x | x <- [2..n], isPrime x]) $ (map read $ lines contents :: [Int])
