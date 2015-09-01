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
(+++) l1 l2 = Append p l1 l2
  where p = tag l1 `mappend` tag l2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single x _) = x
tag (Append x _ _) = x

-- Exercise 2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n l
  | n < 0 || n >= len = Nothing
  | otherwise = Just $ lst !! n
  where lst = jlToList l
        len = length lst

-- Exercise 3

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl
  | n <= 0 = jl
dropJ _ (Single _ _) = Empty
dropJ n (Append s jl1 jl2)
  | n >= 2 = dropJ (n-2) jl2
  | otherwise = dropOne (Append s jl1 jl2)

dropOne :: (Sized b, Monoid b) => JoinList b a -> JoinList b a
dropOne Empty = Empty
dropOne (Single _ _ ) = Empty
dropOne (Append _ (Single size l1) jl2) = jl2
dropOne (Append _ (Append _ _ j2) jl2) = Append (newSize j2) j2 jl2
    where newSize j = tag j `mappend` tag jl2

main = testDropJ

testDropJ = do
  let l1 = Append (Size 3) (Single (Size 4) 'a') (Single (Size 5) 'b')
  let l2 = Append (Size 6) (Single (Size 7) 'c') (Single (Size 8) 'd')
  print $ l1 +++ l2
  print $ dropJ 1 $ l1 +++ l2
  print $ dropJ 2 $ l1 +++ l2
  print $ dropJ 3 $ l1 +++ l2
  print $ dropJ 1 l2


testIndexJ = do
  let l1 = Single (Size 6) 'c'
  let l2 = Append (Size 3) (Single (Size 4) 'a') (Single (Size 5) 'b')
  print $ indexJ 0 l1
  print $ indexJ 1 l2
  print $ indexJ 1 (Empty :: JoinList Size Int)

testAppend2 = do
  let l1 = Append (Product 3) (Single (Product 4) 'a') (Single (Product 5) 'b')
      l2 = Single (Product 6) 'c'
  print $ jlToList $ l1 +++ l2

testAppend = do
  let l1 = Append (Product 3) (Single (Product 4) 'a') (Single (Product 5) 'b')
      l2 = Single (Product 6) 'c'
  print $ l1 +++ l2

testTag = do
  let l1 = Append (Product 3) (Single (Product 4) 'a') (Single (Product 5) 'b')
      l2 = Single (Product 6) 'a'
  print $ tag l1
  print $ tag l2

testJoinList = do
  let l = Append (Product 3) (Single (Product 4) 'a') (Single (Product 5) 'b')
  print l