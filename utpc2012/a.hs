import Data.List
import Control.Applicative
import qualified Data.Map as Map
import Data.Maybe
main :: IO ()
main = do
  result <- check <$> splitSlash <$> getLine
  putStrLn $ if result then "yes" else "no"

check :: [String] -> Bool
check str = countElem (head str) == countElem (str !! 1 ++ str !! 2)

splitSlash :: String -> [String]
splitSlash str
  | '/' `elem` str = (\ (hit, rest) -> hit : splitSlash (tail rest)) $ break (== '/') str
  | otherwise  = [str]

countElem :: (Ord a) => [a] -> Map.Map a Int
countElem = foldl f Map.empty
  where f acc x = if isNothing hoge then Map.insert x 1 acc else Map.insert x (1 + fromJust hoge) acc
          where
              hoge = Map.lookup x acc
