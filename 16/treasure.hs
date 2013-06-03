main :: IO ()
main = do
  contents <- getContents
  let inputs = map ((map read) . splitComma) $ lines contents ::[[Int]]
  print inputs

splitComma :: String -> [String]
splitComma "" = []
splitComma str 
  | any (== ',') str = (\s -> fst s : (splitComma $ tail $ snd s)) $ break (== ',') str
  | otherwise = [str]
