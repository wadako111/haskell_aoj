import System.Environment

-- main = do
--   args <- getArgs
--   let sentence = head args
--   let result = solve sentence
--   putStrLn $ show result

solve :: String -> Int
solve s = do
  let items = words s
  let result = foldl calc [] items
  head result

calc :: [Int] -> String -> [Int]
calc (first:second:rests) "+" = do
  second + first : rests
calc (first:second:rests) "-" = second - first : rests
calc (first:second:rests) "*" = second * first : rests
calc acc digit = read digit : acc
