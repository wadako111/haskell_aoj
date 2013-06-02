import Data.List
import Control.Monad

top3 :: [Int] -> [Int]
top3 xs = take 3 $ reverse $ sort xs

hilltop = do
  inputs <- replicateM 10 getLine
  mapM_ print $ top3 $ map read inputs

main = hilltop
