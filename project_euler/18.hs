import Control.Monad.Writer
import Control.Applicative
import Data.List
import qualified Data.Map as M

type Row = Int
type Number = Int
type MyWriter = Writer [Int] (Number, Row)

main :: IO ()
main = do
    l <- lines <$> getContents
    putStrLn $ unwords $ map show $ answer $ map (map read . words :: String -> [Int]) l

answer :: [[Int]] -> [Int]
answer xs = snd $ maximumBy (\ ((x,_),_) ((y,_),_) -> if x > y then GT else LT) $ fmap runWriter $ calcWriter $ zipWith zip xs $ repeat [0..]

-- calc :: [[(Number, Row)]] -> [(Number, Row)]
-- calc = foldl1 calc1

calcWriter :: [[(Number, Row)]] -> [Writer [Int] (Number, Row)]
calcWriter xs = foldl calc1Writer (return $ return (0,0)) xs

-- calc1 :: [(Number, Row)] -> [(Number, Row)] -> [(Number, Row)]
-- calc1 xs ys = xs >>= (\ (num, row) -> map (\ (x,y) -> (x + num, y) ) $ searchWithRow ys row)

calc1Writer :: [Writer [Int] (Number, Row)] -> [(Number, Row)] -> [Writer [Int] (Number, Row)]
calc1Writer xs ys =  deleteSmaller result
    where result = xs >>= (\ w -> map (\ (x,y) -> w >>= write x y) $ searchWithRow ys $ getRow w)
          write x y (num, _) = writer ((x + num, y), [x])

searchWithRow :: [(Number, Row)] -> Row -> [(Number, Row)]
searchWithRow xs row = filter (\ (_, y) -> y == row || y == row + 1) xs

deleteSmaller :: [Writer [Int] (Number, Row)] -> [Writer [Int] (Number, Row)]
deleteSmaller xs = map snd $ M.toList $ foldl f M.empty xs
    where f tree w = case (M.lookup (getRow w) tree) of
                           Just x -> if getNum w > getNum x then M.insert (getRow w) w tree else tree
                           Nothing -> M.insert (getRow w) w tree

getRow = snd . fst . runWriter
getNum = fst . fst . runWriter
getLog = execWriter

hoge :: Writer [Int] (Int, String) -> String
hoge (WriterT (_, (_,s))) = s
