import Data.Maybe
import Data.List
import Control.Applicative
main :: IO ()
main = do
  fizz <- fizzbuzz 0 <$> lines <$> getContents
  mapM_ print fizz

fizzbuzz :: Int -> [String] -> [Int]
fizzbuzz i ["fizz"] = map (\y -> size15 i y) [3]
fizzbuzz i ["buzz"] = map (size15 i) [5]
fizzbuzz i ["fizz", "buzz"] = map (size15 i) [9, 10]
fizzbuzz i ["buzz", "fizz"] = map (size15 i) [5, 6]
fizzbuzz i ["fizz", "fizz"] = map (size15 i) [6..9]
fizzbuzz i ["fizz", "buzz", "fizz"] = map (size15 i) [3..6]
fizzbuzz i ["buzz", "fizz", "fizz"] = map (size15 i) [5..9]
fizzbuzz i ["fizz", "fizz", "buzz"] = map (size15 i) [6..10]
fizzbuzz i ["fizz", "buzz", "fizz", "fizz"] = map (size15 i) [3..9]
fizzbuzz i ["buzz", "fizz", "fizz", "buzz"] = map (size15 i) [5..10]
fizzbuzz i ["fizz", "fizz", "buzz", "fizz"] = map (size15 i) [6..12]
fizzbuzz i ["fizz", "buzz", "fizz", "fizz", "buzz"] = map (size15 i) [3..10]
fizzbuzz i ["buzz", "fizz", "fizz", "buzz", "fizz"] = map (size15 i) [5..12]
fizzbuzz i ["fizz", "buzz", "fizz", "fizz", "buzz", "fizz"] = map (size15 i) [3..12]
fizzbuzz i ["fizzbuzz"] = map (size15 i) [15]
fizzbuzz i ["fizz", "fizzbuzz"] = map (size15 i) [12..15]
fizzbuzz i ["buzz", "fizz", "fizzbuzz"] = map (size15 i) [10..15]
fizzbuzz i ["fizz", "buzz", "fizz", "fizzbuzz"] = map (size15 i) [9..15]
fizzbuzz i ["fizz", "fizz", "buzz", "fizz", "fizzbuzz"] = map (size15 i) [6..15]
fizzbuzz i ["buzz", "fizz", "fizz", "buzz", "fizz", "fizzbuzz"] = map (size15 i) [5..15]
fizzbuzz i ["fizz", "buzz", "fizz", "fizz", "buzz", "fizz", "fizzbuzz"] = map (size15 i) [3..15]
fizzbuzz i xs =
  let index = (fromJust $ elemIndex "fizzbuzz" xs) + 1
      tuple = splitAt index xs
  in (fizzbuzz i $ fst tuple) ++ (fizzbuzz (i + 1) $ snd tuple)

size15 :: Int -> Int -> Int
size15 i = (+) $ 15 * i





result = map fi [1..15]
fi i
  | i `mod` 15 == 0 = "FizzBuzz"
  | i `mod` 5 == 0 = "Buzz"
  | i `mod` 3 == 0 = "Fizz"
  | otherwise = ""

isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' _ [] = False
isPrefixOf' (x:xs) (y:ys) = if x == y then isPrefixOf' xs ys else False

-- search "hogeo" "ge" = 2
search :: Eq a => [a] -> [a] -> Maybe Int
search [] _ = Nothing
search list word
  | isPrefixOf' word list = Just 0
  | otherwise = fmap (+1) $ search (tail list) word

search' :: ([a] -> [b] -> Bool) -> [a] -> [b] -> Maybe Int
search' _ [] _ = Nothing
search' f list word
  | f list word = Just 0
  | otherwise = fmap (+1) $ search' f (tail list) word
