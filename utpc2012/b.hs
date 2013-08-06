-- DEADBEEFCAFEBABEHAHAHA
-- D                    A
-- DG                   A
-- DG      C            A
-- DG      C F          A
-- DG      C F   B      A
-- DG      C F   BE     A
-- DG      C F   BE    HA

import Data.List

main :: IO ()
main = do
    line <- getLine
    putStrLn $ resolve line

resolve :: String -> String
resolve str = foldl f "" $ tails str
  where f acc s = if null set then acc else acc ++ [head set]
          where set = notExistChar s \\  acc

notExistChar :: String -> String
notExistChar str = [x | x <- ['A'..'H'], x `notElem` str]
