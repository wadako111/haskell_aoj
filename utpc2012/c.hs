import Data.List
import Control.Monad
import Control.Monad.Writer
import Control.Applicative

type Graph = (Int, [Edge])
type Edge = (Node, Node)
type Node = Int

main :: IO ()
main = do
    input1 <- words <$> getLine
    contents <- getContents
    let size = read $ head input1 :: Int
    mapM_ (\ graph -> putStrLn $ if isClose graph then "no" else "yes") $ tail $ scanl (\ graph line -> changeEdge graph (head line, line !! 1)) (initGraph size) $ map (map read . words :: String -> [Int]) $ lines contents
--     putStrLn $ if isClose graph then "yes" else "no"

listMonad2 :: [a] -> (a -> [b]) -> [b]
listMonad2 [] _ = []
listMonad2 xs f = concat $ transpose (map f xs)

changeEdge :: Graph -> Edge -> Graph
changeEdge (size, edges) (x, y) = 
    if  idEdge (x, y) `elem` edges then (size, Data.List.delete (idEdge (x, y)) edges) 
                                  else (size, idEdge (x, y):edges)

initGraph :: Int -> Graph
initGraph size = (size, [(x, y) | x <- [1..size], y <- [1..size], x < y])

idEdge :: Edge -> Edge
idEdge (x, y) = if x > y then (y, x) else (x, y)

isCloseFromNode :: Graph -> Node -> Bool
isCloseFromNode (size, edges) node = elem node $ snd $ runWriter $
    (foldr (<=<) return $ replicate size newGraphWriter) [((size, edges), node)]

newGraphWriter :: [(Graph, Node)] -> Writer [Node] [(Graph, Node)]
newGraphWriter xs = writer (xs `listMonad2` newGraphs, xs `listMonad2` (map snd . newGraphs))

newGraphs :: (Graph, Node) -> [(Graph, Node)]
newGraphs ((size, edges), node) =
    let kouhos = searchTuple edges node :: [Node]
        newGraph newNode = changeEdge (size, edges) (newNode, node)
    in  map (\ kouho -> (newGraph kouho, kouho)) kouhos

isClose :: Graph -> Bool
isClose (size, edges)
  | length edges >= size = True
  | otherwise = any (isCloseFromNode (size, edges)) [1..size]

searchTuple :: (Eq a) => [(a, a)] -> a -> [a]
searchTuple list a = foldl f [] list
  where f acc (x, y)
          | a == x = y:acc
          | a == y = x:acc
          | otherwise = acc
