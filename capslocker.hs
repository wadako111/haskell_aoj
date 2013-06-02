import Control.Monad
import Data.Char
import System.IO

main = do
  withFile "haiku.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStr $ map toUpper contents
