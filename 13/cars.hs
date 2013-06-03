main :: IO ()
main = do
  contents <- getContents
  let cars = map read $ lines contents ::[Int]
  mapM_ print $ reverse $ snd $ foldl f ([], []) cars
    where
     f (s:stack, result) 0 = (stack, s:result)
     f (stack, result) x = (x:stack, result)

