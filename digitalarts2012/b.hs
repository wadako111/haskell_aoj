import Data.List
import Data.Maybe

main :: IO ()
main = do
    str <- getLine
    putStrLn $ answer str

charToInt :: Char -> Int
charToInt c = 1 + fromJust (c `elemIndex` ['a'..'z'])

calcHash :: String -> Int
calcHash [] = 0
calcHash (x:xs) = charToInt x + calcHash xs

answer :: String -> String
answer "zzzzzzzzzzzzzzzzzzzz" = "NO"
answer "a" = "NO"
answer str
    | str /= new = new
--     | isZ str = tail str ++ str
    | isA str = [['a'..'z'] !! (l - 1)]
    | l == 20 = before (str !! 18) : next (str !! 19) : take 18 str
    | otherwise = take (l - 1) str ++ ['a', before (str !! (l - 1))]
        where new = change str
              l = length str

change :: String -> String
change (x:y:ys)
    | x == y = x:change(y:ys)
    | otherwise = y:x:ys
change xs = xs

isZ :: String -> Bool
isZ = all (== 'z')

isA :: String -> Bool
isA = all (== 'a')

next :: Char -> Char
next c = ['a'..'z'] !! charToInt c

before :: Char -> Char
before c = ['a'..'z'] !! (charToInt c - 2)
