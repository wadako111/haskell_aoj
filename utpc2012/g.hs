import Data.List
import Control.Applicative

type N = Int
type K = Int

main :: IO ()
main = do
    input1 <- (map read . words :: String -> [Int]) <$> getLine
    str <- getLine
    putStrLn $ show $ answer (head input1) (input1 !! 1) str

answer :: N -> K -> String -> Int
answer n k str = foldr (\ b acc -> if isOK k str b then acc + 1 else acc) 0 $ allStringWithN str n

-- 並び替えられたkouhoのsubStringsのk番目はstrか
isOK :: K -> String -> String -> Bool
isOK k str kouho = sort (subStrings kouho) !! (k - 1) == str

-- strを含む長さnの文字列
allStringWithN :: String -> N -> [String]
allStringWithN str n = permutations (take n ['a'..'z'] `diff` str) >>=
    (\ kouho -> map (\ [x1, x2] -> x1 ++ str ++ x2) $ 
      zipWith (\ x y -> [x,y]) (inits kouho) (tails kouho))

diff :: Eq a => [a] -> [a] -> [a]
diff = foldr delete

subStrings :: String -> [String]
subStrings xs = map (splitAt 1) (init $ tails xs) >>=
    (\ (x1, x2) -> map (\ x -> x1 ++ x) $ inits x2)
