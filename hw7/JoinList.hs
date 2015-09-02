{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

import Data.Monoid

import Sized

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- Exercise 1

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty jl2 = jl2
(+++) jl1 Empty = jl1
(+++) jl1 jl2 = Append p jl1 jl2
  where p = tag jl1 `mappend` tag jl2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single x _) = x
tag (Append x _ _) = x

-- Exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n (Single _ jl)
  | n == 0 = Just jl
  | otherwise = Nothing
indexJ n (Append s jl1 jl2)
  | n < 0 || n >= s' = Nothing
  | n < size1 = indexJ n jl1
  | otherwise = indexJ (n - size1) jl2
    where s' = getSize $ size s
          size1 = getSize . size $ tag jl1

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl@(Single _ _)
  | n > 0 = Empty
  | otherwise = jl
dropJ n jl@(Append s jl1 jl2)
  | n < 0 = jl
  | n < size1 = dropJ n jl1 +++ jl2  -- note, this recomputes size
  | n < s' = dropJ (n - size1) jl2
  | otherwise = Empty
  where s' = getSize $ size s
        size1 = getSize . size $ tag jl1

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl@(Single _ _)
  | n > 0 = jl
  | otherwise = Empty
takeJ n jl@(Append s jl1 jl2)
  | n <= 0 = jl
  | n <= size1 = takeJ n jl1
  | n <= s' = jl1 +++ takeJ (n - size1) jl2
  | otherwise = Empty
  where s' = getSize $ size s
        size1 = getSize . size $ tag jl1


main = do
  testDropJ
  testTakeJ

l1, l2 :: JoinList Size Char
l1 = Append (Size 2) (Single (Size 1) 'a') (Single (Size 1) 'b')
l2 = Append (Size 3) (Append (Size 2) (Single (Size 1) 'c') (Single (Size 1) 'd')) (Single (Size 1) 'e')

testTakeJ = do
  print $ l1 +++ l2
  print $ takeJ 1 $ l1 +++ l2
  print $ takeJ 2 $ l1 +++ l2
  print $ takeJ 3 $ l1 +++ l2
  print $ takeJ 4 $ l1 +++ l2
  print $ takeJ 5 $ l1 +++ l2
  print $ takeJ 6 $ l1 +++ l2

testDropJ = do
  print $ l1 +++ l2
  print $ dropJ 1 $ l1 +++ l2
  print $ dropJ 2 $ l1 +++ l2
  print $ dropJ 3 $ l1 +++ l2
  print $ dropJ 4 $ l1 +++ l2
  print $ dropJ 5 $ l1 +++ l2

testIndexJ = do
  let l1 = Single (Size 1) 'c'
      l2 = Append (Size 2) (Single (Size 1) 'a') (Single (Size 1) 'b')
      l3 = Append (Size 3) (Append (Size 2) (Single (Size 1) 'e') (Single (Size 1) 'f')) (Single (Size 1) 'g')
  print $ indexJ 0 l1
  print $ indexJ 1 l2
  print $ indexJ 3 $ l2 +++ l3
  print $ getSize . tag $ l2 +++ l3
  print $ indexJ 1 (Empty :: JoinList Size Int)

testAppend2 = do
  let l1 = Append (Product 3) (Single (Product 4) 'a') (Single (Product 5) 'b')
      l2 = Single (Product 6) 'c'
  print $ jlToList $ l1 +++ l2

testAppend = do
  let l1 = Append (Product 3) (Single (Product 4) 'a') (Single (Product 5) 'b')
      l2 = Single (Product 6) 'c'
      l3 = Empty
  print $ l1 +++ l2
  print $ l3 +++ l2

testTag = do
  let l1 = Append (Product 3) (Single (Product 4) 'a') (Single (Product 5) 'b')
      l2 = Single (Product 6) 'a'
  print $ tag l1
  print $ tag l2

testJoinList = do
  let l = Append (Product 3) (Single (Product 4) 'a') (Single (Product 5) 'b')
  print l