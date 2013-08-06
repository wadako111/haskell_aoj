import Data.List
-- import Control.Monad
-- import Control.Monad.Writer
import Control.Applicative

type Seito = (Int, Int)

main :: IO ()
main = do
    getLine
    inputs <- map (map read . words :: String -> [Int]) <$> lines <$> getContents
    print $ answer $ map (\ [x,y] -> (x,y)) inputs

answer :: [Seito] -> Int
answer xs = head $ filter (isOK xs) [(sum $ map fst xs)..]

isOK :: [Seito] -> Int -> Bool
isOK seitos number
  | or $ zipWith (>) (map snd seitos) $ ninzu (map fst seitos) number = False
  | otherwise = True

ninzu :: [Int] -> Int -> [Int]
ninzu votes maxNinzu = zipWith (+) seisu $ reverse $ fst $ foldl f ([], 0) votes
      where sumVote = sum votes
            seisu = map (\ v -> v * maxNinzu `div` sumVote) votes
            shosu = take (maxNinzu - sum seisu) $ sortBy (flip compare) $  map (getShosuBubun maxNinzu sumVote) votes
            f (acc, count) v
              | count >= maxNinzu - sum seisu = (0:acc, count)
              | getShosuBubun maxNinzu sumVote v `elem` shosu = (1:acc, count + 1)
              | otherwise = (0:acc, count)

getShosuBubun :: Int -> Int -> Int -> Float
getShosuBubun maxNinzu sumVote vote = fromIntegral (maxNinzu * vote) / fromIntegral sumVote - fromIntegral (maxNinzu * vote `div` sumVote) ::Float
