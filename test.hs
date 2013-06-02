import Data.List
import Data.Char

multThree :: Int -> Int -> Int -> Int
multThree x y z = x * y * z

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter f xs
    | otherwise = filter f xs

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' f = foldr (\x acc -> if f x then x:acc else acc) []

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

findKey :: (Eq k) => k -> [(k, v)] -> v
findKey key xs = snd $ head $ filter (\(k, v) -> k == key) xs

data CMaybe a = CNothing | CJust Int a deriving (Show)
instance Functor CMaybe where
  fmap f (CJust i a) = CJust (i + 1) (f a)
  fmap f CNothing = CNothing
