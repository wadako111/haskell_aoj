reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

main :: IO ()
main = do
  line <- getLine
  print $ reverse' line
