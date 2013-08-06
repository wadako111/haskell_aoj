main :: IO ()
main = do
    n <- fmap read getLine :: IO Int
    print $ answer n

answer :: Int -> Int
answer n = fib !! (n + 1)

fib = 0:1:zipWith (+) fib (tail fib)
