import Control.Monad
import Data.List
data Judge = JLeft | JRight deriving (Show)
type Route = [Judge]
type Meiro = [String]
data Direction = DUp | DRight | DDown | DLeft deriving Show
type Point = (Int, Int)
-- data Status = SNormal | SWall | SBranch deriving Show
type Ari = (Point, Direction)
type IsGoal = Bool
type JudgeCount = Int
type AriCount = Int

main :: IO ()
main = do
    print "hoge"
meiro1 = [
    "S....",
    "##.##",
    ".....",
    ".#.#.",
    ".#.#G"] :: Meiro
meiro2 = [
    "S........",
    "####.####",
    ".........",
    ".#####.##",
    "...#.#...",
    ".#.#.####",
    ".#...#...",
    ".#.#.#.#.",
    ".#.#...#G"
    ]

answer :: Meiro -> Float
answer meiro = foldl (\ acc (_, _, a, j, _) -> acc + (realToFrac a) * (1/2)^j) 0 $ allRoute meiro

allRoute :: Meiro -> [(Meiro, Ari, AriCount, JudgeCount, IsGoal)]
allRoute meiro = _allRoute [(meiro, ((0,0), DRight), 1, 0, False)]

_allRoute :: [(Meiro, Ari, AriCount, JudgeCount, IsGoal)] -> [(Meiro, Ari, AriCount, JudgeCount, IsGoal)]
_allRoute set = if isEnd n then n else _allRoute n
    where n = set >>= next

next :: (Meiro, Ari, AriCount, JudgeCount, IsGoal) -> [(Meiro, Ari, AriCount, JudgeCount, IsGoal)]
next (m, a, i, j, True) = return (m, a, i, j, True)
next (meiro, ((x, y), direction), a, j, _)
    | isGoal meiro (x, y) = return (meiro, ((x, y), direction), a, j, True)
    | isWall meiro n = 
        if isWall meiro r && isWall meiro l
        then return (newMeiro, ((0, 0), DRight), a+1, j, False)
        else if (not $ isWall meiro r) && (not $ isWall meiro l)
             then [(meiro, (l, leftDirection direction), a, j+1, False ), 
                  (meiro, (r, rightDirection direction), a, j+1, False)]
             else if isWall meiro r
                  then return (meiro, (l, leftDirection direction), a, j, False)
                  else return (meiro, (r, rightDirection direction), a, j, False)
    | otherwise = return (meiro, (n, direction), a, j, False)
        where n = nextPoint (x, y) direction
              l = leftPoint (x, y) direction
              r = rightPoint (x, y) direction
              newMeiro = change y meiro $ change x (meiro !! y) '#'

nextPoint :: Point -> Direction -> Point
nextPoint (x, y) DUp = (x, y -1)
nextPoint (x, y) DRight = (x + 1, y)
nextPoint (x, y) DDown = (x, y + 1)
nextPoint (x, y) DLeft = (x - 1, y)

leftDirection :: Direction -> Direction
leftDirection DUp = DLeft
leftDirection DRight = DUp
leftDirection DDown = DRight
leftDirection DLeft = DDown

rightDirection :: Direction -> Direction
rightDirection DUp = DRight
rightDirection DRight = DDown
rightDirection DDown = DLeft
rightDirection DLeft = DUp

leftPoint :: Point -> Direction -> Point
leftPoint p = nextPoint p . leftDirection

rightPoint :: Point -> Direction -> Point
rightPoint p = nextPoint p . rightDirection


isWall :: Meiro -> Point -> Bool
isWall meiro (x, y) 
    | x < 0 || y < 0 = True
    | length meiro <= y = True
    | length (meiro !! y) <= x = True
    | otherwise = '#' == meiro !! y !! x

isGoal :: Meiro -> Point -> Bool
isGoal meiro (x, y) = meiro !! y !! x == 'G'

change :: Int -> [a] -> a -> [a]
change i xs x = take i xs ++ x:drop (i+1) xs

test :: Int -> [(Meiro, Ari, AriCount, JudgeCount, IsGoal)]
test n = (foldl (<=<) return $ replicate n next) (meiro2, ((0,0), DRight), 1, 0, False)

isEnd :: [(Meiro, Ari, AriCount, JudgeCount, IsGoal)] -> Bool
isEnd xs = all (\ (_, _, _, _, isGoal) -> isGoal) xs

seikei :: (Meiro, Ari, AriCount, JudgeCount, IsGoal) -> IO ()
seikei (meiro, ((x, y), direction), _, _, _) = putStrLn $ intercalate "\n" $ change y meiro $ change x (meiro !! y) (yajirushi direction)

yajirushi :: Direction -> Char
yajirushi DUp = 'u'
yajirushi DDown = 'd'
yajirushi DRight = 'r'
yajirushi DLeft = 'l'
