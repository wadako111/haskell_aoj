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

move ::Fractional a => a -> a -> (a, a, a) -> (a, a, a)
move meter degree (x, y, direction) = (x + meter * (cos $ radians direction), y + meter * (sin $ radians direction), direction + degree)

radians :: Float a => a -> a
radians r = r / 180 * pi
