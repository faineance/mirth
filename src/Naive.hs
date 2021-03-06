{-# LANGUAGE DeriveFunctor #-}
module Main where
import           Control.Monad.Free


data Instruction next = Push Int next
                      | Add      next
                      | Sub      next
                      | Mul      next
                      | Dup      next
                      deriving (Show, Functor)

type Mirth = Free Instruction

push :: Int -> Mirth ()
push n = liftF $ Push n ()

add, sub, mul, dup :: Mirth ()
add = liftF $ Add ()
sub = liftF $ Sub ()
mul = liftF $ Mul ()
dup = liftF $ Dup ()


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

eval' [] Pure {}                          = Left "Not enough items on stack"
eval' [r] Pure {}                          = Right r

main :: IO ()
main = print $ eval $ do {push 2; push 3; add}
