main :: IO ()
main = do
  line <- getLine
  print $ length [0 | a <- [0..9], b <- [0..9], c <- [0..9], d <- [0..9], a + b + c + d == read line]
