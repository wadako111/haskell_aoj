import Data.List

main = do
  contents <- getContents
  mapM_ printGCDandLCM $ lines contents

printGCDandLCM line = do
  let numbers = (map read $ words line :: [Int])
  putStr $ show $ gcd' (numbers!!0) (numbers!!1)
  putStr " "
  putStr $ show $ lcm' (numbers!!0) (numbers!!1)
  putStrLn ""

factorization :: Int -> [Int]
factorization 1 = [1]
factorization n
  | isPrime n = [n]
  | otherwise = (\i -> i : (factorization $ quot n i)) $ head [x | x <- [2..], n `mod` x == 0]

isPrime :: Int -> Bool
isPrime 1 = False
isPrime 2 = True
isPrime n
  | n `mod` 2 == 0 = False
  | length (take 1 [x | x <- [3..quot n 2], mod n x == 0]) > 0 = False
  | otherwise = True

lcm' :: Int -> Int -> Int
lcm' n1 n2 = (\n2facts -> (foldl (*) 1 n2facts) * (foldl (*) 1 $ foldl (flip delete) (factorization n1) n2facts)) (factorization n2)

gcd' :: Int -> Int -> Int
gcd' n1 n2 = foldl (*) 1 $ moreIntersect (factorization n1) (factorization n2)

moreIntersect :: (Eq a) => [a] -> [a] -> [a]
moreIntersect list1 list2 = fst $ foldl (\(result, ys) x -> if any ((==) x) ys then (x:result, delete x ys) else (result, ys)) ([], list1) list2
