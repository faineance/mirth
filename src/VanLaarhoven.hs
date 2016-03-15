-- http://r6.ca/blog/20140210T181244Z.html
{-# LANGUAGE FlexibleContexts #-}
module Main where

import           Control.Exception
import           Control.Monad.Free.VanLaarhoven


import           Control.Monad.State             hiding (state)
import           Control.Monad.Trans.Except

data Instruction m = Instruction { pushv :: Int -> m (), popv :: m Int }

type Mirth = Free Instruction

data MirthException = NotEnoughItems
  deriving (Show)

instance Exception MirthException

push :: Int -> Mirth ()
push v = Free (`pushv` v)

pop :: Mirth Int
pop = Free popv

dup :: Mirth ()
dup = do
        v <- pop
        push v
        push v

bop :: (Int -> Int -> Int) -> Mirth ()
bop f = do
        x <- pop
        y <- pop
        push (x `f` y)

add, sub, mul, pow, square, cube :: Mirth ()
add = bop (+)
sub = bop (-)
mul = bop (*)
pow = bop (^)

square = do dup; mul
cube = do square; mul

state :: Instruction (ExceptT MirthException (State [Int]))
state = Instruction { pushv=pushv, popv=popv }
  where pushv v = modify (v :)
        popv = do
                stack <- get
                case stack of
                    (x:xs) -> do
                                put xs
                                return x
                    [] -> throwE NotEnoughItems

eval :: Mirth n -> Either MirthException n
eval p = runExceptT (runFree p state) `evalState` []

main :: IO ()
main = print $ eval $ do {push 2; push 3; add; pop}
