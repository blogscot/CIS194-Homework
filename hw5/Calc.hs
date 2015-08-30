module Calc where

import ExprT
import Parser

eval :: ExprT -> Int
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (Lit x) = fromInteger x :: Int


evalStr :: String -> Maybe Integer
evalStr str = case parseExp Lit Add Mul str of
              Just x -> Just . toInteger $ eval x
              Nothing -> Nothing

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit x = x
  add x y = x + y
  mul x y = x * y

instance Expr Bool where
  lit x
    | x <= 0 = False
    | otherwise = True
  add x y = x || y
  mul x y = x && y

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


main = testExprs

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
  print $ eval $ Add (Lit 2) (Lit 3)
  print $ eval $ Mul (Lit 2) (Lit 3)
  print $ eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
  print $ eval (Mul (Mul (Lit 2) (Lit 3)) (Lit 4))