import Control.Monad
import Control.Monad.State

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9.")

applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)


type Stack = [Int]

-- pop :: Stack -> (Int, Stack)
-- pop (x:xs) = (x, xs)
pop :: State Stack Int
pop = state $ \ (x:xs) -> (x, xs)

-- push :: Int -> Stack -> ((), Stack)
-- push x xs = ((), x:xs)
push :: Int -> State Stack ()
push a = state $ \ xs -> ((), a:xs)

-- stackManip :: Stack -> (Int, Stack)
-- stackManip stack = let
--   ((), newStack1) = push 3 stack
--   (a, newStack2) = pop newStack1
--   in pop newStack2
stackManip :: State Stack Int
stackManip = 
    push 3 >>= (\ x -> pop >>= (\ y -> pop))
--     _ <- push 3
--     a <- pop
--     pop

