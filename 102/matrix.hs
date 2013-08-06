import Control.Applicative
main :: IO ()
main = do
    datasets <- parseDataSet <$> lines <$> getContents
    mapM_ (mapM_ (putStrLn . join) . calc) datasets

type DataSet = (Int, Row)
type Row = [[Int]]

parseDataSet :: [String] -> [DataSet]
parseDataSet ["0"] = []
parseDataSet xs = fst $ foldl f ([], (0, [[]])) xs
  where
    f :: ([DataSet], DataSet) -> String -> ([DataSet], DataSet)
    f (sets, (num, rows)) str
      | length items == 1 && num == 0 = (sets, (head items, []))
      | length items == 1 = (sets ++ [(num, rows)], (head items, []))
      | otherwise = (sets, (num, rows ++ [items]))
          where items = map read $ words str :: [Int]

calc :: DataSet -> [[Int]]
calc (_, rows) = tmpResult ++ [foldl (zipWith (+)) (replicateInf 0) tmpResult]
  where tmpResult = map (\ xs -> xs ++ [sum xs]) rows

replicateInf :: a -> [a]
replicateInf a = a:replicateInf a

join :: Show a => [a] -> String
join (x:[]) = show x
join (x:xs) = show x ++ " " ++ join xs
