import Control.Applicative

main = do
    inputs1 <- words <$> getLine
    let year = read $ inputs1 !! 1 :: Int
    contents <- map words <$> lines <$> getContents
    putStrLn $ answer (map (\ c -> (read $ head c :: Int, c !! 1)) contents) year "kogakubu10gokan"

answer :: [(Int, String)] -> Int -> String -> String
answer ((i, str):[]) year previoutName
    | year >= i = str
    | otherwise = previoutName
answer ((i, str):xs) year previousName
    | year >= i = answer xs year str
    | otherwise = previousName

