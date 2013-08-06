{-# OPTIONS_GHC -O2 -optc-Ofast -fignore-asserts #-}
import Data.Maybe
import Control.Monad
import Data.Bits
import qualified Data.ByteString.Char8 as C

readInt :: C.ByteString -> Int
readInt = fst . fromJust . C.readInt

gao :: [Int] -> Int
gao a = sum b + if 0 `notElem` b then - 1 else 1
  where
    n = length a
    b = [1 `xor` e `xor` (if i == 1 then 1 else 0) `xor` (if i == n then 1 else 0) |
        (i, e) <- zip [1..n] a]

main :: IO ()
main = do
    m <- fmap (head . map readInt . C.words) C.getLine
    a <- replicateM m $ fmap (map readInt . C.words) C.getLine
    let b = zipWith (zipWith xor) (repeat 0: repeat (repeat 1)) a
        c = map gao b
    print $ sum c
