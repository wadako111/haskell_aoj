import Control.Monad
import Data.Map as Map
import Data.Maybe

main :: IO ()
main = do
  contents <- getContents
  mapM_ putStrLn $ sale_result $ lines contents

sale_result :: [String] -> [String]
sale_result ["0"] = []
sale_result str =
  let number_of_data = read $ head str ::Int
      datasets = Prelude.map parse_three $ take number_of_data $ tail str
      over = over_1_000_000 datasets
  in (if length over > 0 then Prelude.map show over else ["NA"]) ++ (sale_result $ drop (number_of_data + 1) str)

over_1_000_000 :: [(Int, Int, Int)] -> [Int]
over_1_000_000 xs = keys $ Map.filter (>= 1000000) $ Prelude.foldl f empty xs
  where f map (eid, price, amout) = insert eid (just_or_zero (Map.lookup eid map) + price * amout) map
        just_or_zero x = if x == Nothing then 0 else fromJust x

parse_three :: String -> (Int, Int, Int)
parse_three str = let nums = Prelude.map read $ words str ::[Int]
                  in (nums !! 0, nums !! 1, nums !! 2)
