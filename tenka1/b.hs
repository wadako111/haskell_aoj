import Data.List
main = do
    str <- getLine
    putStrLn $ answer str

answer :: String -> String
answer str = intercalate "," $ words str

