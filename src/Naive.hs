{-# LANGUAGE DeriveFunctor #-}
module Main where
import           Control.Monad.Free


data Instruction next = Push Int next
                      | Add      next
                      | Sub      next
                      | Mul      next
                      | Dup      next
                      | End
                      deriving (Show, Functor)

type Mirth = Free Instruction

push :: Int -> Mirth ()
push n = liftF $ Push n ()

add, sub, mul, dup, end :: Mirth ()
add = liftF $ Add ()
sub = liftF $ Sub ()
mul = liftF $ Mul ()
dup = liftF $ Dup ()
end = liftF End

eval :: Mirth n -> Either String Int
eval = eval' []

eval' :: [Int] -> Free Instruction t -> Either String Int
eval' stack (Free (Push v cont))         = eval' (v : stack) cont
eval' (v : v' : stack) (Free (Add cont)) = eval' (v + v' : stack) cont
eval' (v : v' : stack) (Free (Sub cont)) = eval' (v - v' : stack) cont
eval' (v : v' : stack) (Free (Mul cont)) = eval' (v * v' : stack) cont
eval' (v : stack) (Free (Dup cont))      = eval' (v : v  : stack) cont
eval' _ (Free Add {})                    = Left "Not enough items on stack"
eval' _ (Free Sub {})                    = Left "Not enough items on stack"
eval' _ (Free Mul {})                    = Left "Not enough items on stack"
eval' _ (Free Dup {})                    = Left "Not enough items on stack"
eval' [] (Free End)                      = Left "Not enough items on stack"
eval' [r] (Free End)                     = Right r
eval' _ (Free End)                       = Left "Not Empty at End"
eval' _ Pure {}                          = Left "Missing End"

main :: IO ()
main = print $ eval $ do {push 2; push 3; add; end}
