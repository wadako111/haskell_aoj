import Control.Monad
import Data.List

main = rightTriangle
rightTriangle = do
  count <- getLine
  inputs <- replicateM (read count) getLine
  mapM_ (print . isRightTriangle) $ inputs

isRightTriangle :: String -> String
isRightTriangle str = (\xs -> if ((xs!!0)^2 + (xs!!1)^2 == (xs!!2)^2) then "YES" else "NO") (sort $ map read $ words str :: [Int])
