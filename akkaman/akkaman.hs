
akka ::  [[Int]]
akka = map (map _akka) [[(x,y) | x <- [0..]] | y <- [0..]]

_akka :: (Int, Int) -> Int
_akka (0, n) = n + 1
_akka (m, 0) = akka !! 1 !! (m-1)
_akka (m, n) = akka !! (akka !! (n-1) !! m) !! (m-1)
