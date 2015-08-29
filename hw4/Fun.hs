import Test.QuickCheck

fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' (x:xs)
  | even x = (x - 2) * fun1' xs
  | otherwise = fun1' xs

fun1 :: [Integer] -> Integer
fun1 = product . map (+(-2)) . filter even

prop_fun :: [Integer] -> Bool
prop_fun lst = fun1 lst == fun1' lst

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n
  | even n = n + fun2' (n `div` 2)
  | otherwise = fun2' (3 * n + 1)

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (>1) . iterate (\x -> if even x then x `div` 2 else 3*x+1)

-- impractical to use quickCheck as this function is extremely recursive and quickCheck uses
-- very large integer values
-- prop_fun2 :: Integer -> Bool
-- prop_fun2 n = fun2 n == fun2' n

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- My first attemp led me up a blind alley.

-- insert :: (Ord a) => a -> Tree a -> Tree a
-- insert item Leaf = Node 0 Leaf item Leaf
-- insert item (Node x left item' right)
--   | item < item' = Node (x+1) (insert item left) item' right
--   | item > item' = Node (x+1) left item' (insert item right)
  -- | otherwise = Node x left item right

-- This solution from evansb@github is copied here for study purposes.
-- foldTree stores the list into a balanced tree, however note, it does
-- not produce a Binary Search Tree (the tree is ordered according to input
-- order).
foldTree :: (Ord a) => [a] -> Tree a
foldTree [] = Leaf
foldTree xs = Node height (foldTree $ take half xs) (xs !! half) (foldTree $ drop (half+1) xs)
  where len = length xs
        half = len `div` 2
        height = floor (logBase 2 (fromIntegral len) :: Double)

showTree :: (Show a) => Tree a -> String
showTree Leaf = ""
showTree (Node height left val right) = showTree left ++ " " ++ show (height, val) ++ " " ++ showTree right


xor :: [Bool] -> Bool
xor = odd . sum . map fromEnum

xor' :: [Bool] -> Bool
xor' = odd . foldr (\x y -> if x then y+1 else y) 0

prop_xor :: [Bool] -> Bool
prop_xor xs = xor xs == xor' xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []



main = quickCheck prop_map

testFoldTree = do
  -- putStrLn . showTree $ foldTree ['A'..'O'] -- a perfect binary tree (1,3,7,15... entries)
  putStrLn . showTree $ foldTree ['O','N'..'A']