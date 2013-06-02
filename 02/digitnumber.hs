main = digitnumber

digitnumber = do
  contents <- getContents
  mapM (print . (foldl (+) 0 ) . separateDigits) $ lines contents

separateDigits :: String -> [Int]
separateDigits str = map read $ words str :: [Int]

