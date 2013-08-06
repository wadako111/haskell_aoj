import Control.Monad
main = do
    [n, m] <- fmap (map read . words :: String -> [Int]) getLine
    aij <- replicateM m getLine
    str <- getLine
    print n
