main = do
  line <- getLine
  print $ show ((*) 10000 $ ceiling $ 10 * (1.05) ^ (read line))
