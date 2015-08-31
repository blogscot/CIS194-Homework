module Fibonacci where

-- Exercise 1

fib :: Integer -> Integer
fib n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = [fib x | x <- [0..]]

-- Exercise 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs


instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons (f x) (streamFromSeed f (f x))

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+1) (-1)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) (Cons y ys) = Cons x (Cons y (interleaveStreams xs ys))

-- ruler :: Stream Integer

-- 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17
-- 0 1 0 2 0 1 0 3 0 1  0  2  0  1  0  4  0

main = print $ interleaveStreams nats $ streamRepeat 0

testStreamFromSeed = do
  print $ streamFromSeed (*4) 4
  print $ streamFromSeed (+1.3) 1.2

testStreamMap = print $ streamMap (*3) $ streamRepeat 8

testStreamRepeat = print $ streamRepeat 7

testStreamToList =
  print . take 20 . streamToList $ streamRepeat 8
