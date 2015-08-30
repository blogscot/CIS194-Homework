{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import ExprT as E
import Parser
import StackVM
import Control.Monad

-- Exercise 1

eval :: ExprT -> Integer
eval (E.Add x y) = eval x + eval y
eval (E.Mul x y) = eval x * eval y
eval (Lit x) = x


-- Exercise 2

evalStr :: String -> Maybe Integer
evalStr = liftM eval . parseExp Lit E.Add E.Mul

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = E.Add
  mul = E.Mul

-- Exercise 4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit x
    | x <= 0 = False
    | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Ord, Show)
newtype Mod7 = Mod7 {runMod7 :: Integer} deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add x y = x `max` y
  mul x y = x `min` y

instance Expr Mod7 where
  lit x = Mod7 $ x `mod` 7
  add x y = lit $ runMod7 x + runMod7 y
  mul x y = lit $ runMod7 x * runMod7 y

-- Exercise 5

instance Expr Program where
  lit x = [PushI x]
  add x y = x ++ y ++ [StackVM.Add]
  mul x y = x ++ y ++ [StackVM.Mul]


compile :: String -> Maybe Program
compile = parseExp lit add mul


main = do
  print $ compile "8+7"
  let prog = compile "8+7"
  print $ liftM stackVM prog
  -- print $ stackVM program101

-- Tests

program101 :: Program
program101 = [PushI 8, PushI 7, StackVM.Add]

testExprs = do
  print testInteger
  print testBool
  print testMM
  print testSat

testExprBool = do
  print (lit 5 :: Bool)
  print (lit (-5) :: Bool)
  print (add (lit (-5)) (lit 7) :: Bool)
  print (mul (lit (-5)) (lit 7) :: Bool)


testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

textExprInteger =
  print (mul (lit 3) (add (lit 5) (lit 10)) :: Integer)

textExprClass = do
  print $ eval $ lit 5
  print $ eval $ add (lit 5) (lit 3)
  print $ eval $ mul (add (lit 5) (lit 3)) (lit 4)

testEvalStr = do
  print $ evalStr "(2+13)*4"
  print $ evalStr "12+3*4"
  print $ evalStr "2+3*4+"

testEval = do
  print $ eval $ E.Add (Lit 2) (Lit 3)
  print $ eval $ E.Mul (Lit 2) (Lit 3)
  print $ eval (E.Mul (E.Add (Lit 2) (Lit 3)) (Lit 4))
  print $ eval (E.Mul (E.Mul (Lit 2) (Lit 3)) (Lit 4))