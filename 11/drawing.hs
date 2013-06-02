main :: IO ()
main = do
  contents <- getContents
  let w = read $ head $ lines contents ::Int
      n = read $ (lines contents) !! 1 ::Int
      hLines = map ((map read) . splitComma) $ tail . tail $ lines contents ::[[Int]]
  mapM_ print $ foldl (\acc (x:y:[]) -> swap (x - 1) (y - 1) acc) [1..w] hLines

splitComma :: String -> [String]
splitComma "" = []
splitComma str 
  | any (== ',') str = (\s -> fst s : (splitComma $ tail $ snd s)) $ break (== ',') str
  | otherwise = [str]

swap :: Int -> Int -> [a] -> [a]
swap i j xs = reverse $ fst $ foldl f ([], 0) xs
  where
    f (acc, idx) x
      | idx == i = (xs !! j : acc, idx + 1)
      | idx == j = (xs !! i : acc, idx + 1)
      | otherwise = (x : acc, idx + 1)
