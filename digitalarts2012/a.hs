import Control.Monad

main :: IO ()
main = do
    ws <- fmap words getLine
    n <- fmap read getLine :: IO Int
    ts <- replicateM n getLine
    putStrLn $ unwords $ answer ws ts

answer :: [String] -> [String] -> [String]
answer str ts = map (filter' ts) str

filter' :: [String] -> String -> String
filter' [] str = str
filter' (t:ts) str
    | match str t = replicate (length str) '*'
    | otherwise = filter' ts str


match :: String -> String -> Bool
match [] [] = True
match [] _ = False
match _ [] = False
match (s:ss) (t:ts)
    | t == '*' = match ss ts
    | s /= t = False
    | otherwise = match ss ts
