main :: IO ()
main = do
  line <- getLine
  let numbers = map read $ words line :: [Int]
  putStrLn $ unwords $ map show $ sort numbers

sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) = 
  let larger = [s | s <- xs, s > x]
      smaller = [s | s <- xs, s <= x]
  in (sort larger) ++ [x] ++ (sort smaller)
