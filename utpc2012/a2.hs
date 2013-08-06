import Control.Applicative
import Data.List

ymd str = [year, month, day] where
  year = take 4 str
  month = take 2 $ drop 5 str
  day = take 2 $ drop 8 str

main = do
    [y, m, d] <- ymd <$> getLine
    putStrLn $ answer $ check y (m ++ d)

answer True = "yes"
answer False = "no"

check y md = sort y == sort md
