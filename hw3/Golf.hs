module Golf where

import Data.List hiding (group)

skips :: [a] -> [[a]]
skips [] = []
skips xs = xs : skips (drop 1 xs)

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima (x:y:z:zs)
  | y > x && y > z = y : localMaxima (y:z:zs)
  | otherwise = localMaxima (y:z:zs)

histogram :: [Integer] -> String
histogram lst = concatMap displayLine lst' ++ "0123456789"
  where lst' = reverse $ build lst

build :: [Integer] -> [[Integer]]
build [] = []
build xs = ys : build zs
  where ys = group xs
        zs = xs \\ ys -- the difference

group :: [Integer] -> [Integer]
group lst = [ x | x <- [0..9], countElems x lst > 0]

countElems :: Integer -> [Integer] -> Integer
countElems n lst = toInteger . sum . map fromEnum $ map (==n) lst

displayLine :: [Integer] -> String
displayLine lst = concat [ if x `elem` lst then "*" else " " | x <-[0..9]] ++ "\n"

main :: IO ()
main = do
  putStrLn $ histogram [1,1,1,1,5]
  putStrLn "\n"
  putStrLn $ histogram [1,4,5,4,6,6,3,4,2,4,9]

testSkips :: IO ()
testSkips = do
  print $ skips "ABCD"
  print $ skips "hello!"

testLocalMaxima :: IO ()
testLocalMaxima = do
  print $ localMaxima [2,3,4,1,5]
  print $ localMaxima [2,9,5,6,1]

testGroup :: IO ()
testGroup = do
  print $ group [1,1,1,1,5]
  print $ group [1,4,5,4,6,6,3,4,2,4,9]
  print $ group [4,6,4,4]
  print $ group [4,4]

testBuild :: IO ()
testBuild = do
  print $ build [1,1,1,1,5]
  print $ build [1,4,5,4,6,6,3,4,2,4,9]
  print $ build []

testCountElems :: IO ()
testCountElems = do
  print $ countElems 5 [1,1,1,1,5]
  print $ countElems 1 [1,1,1,1,5]

testDisplayLine :: IO ()
testDisplayLine = do
  putStrLn $ concatMap displayLine [[1],[1],[1],[1,5]] ++ "0123456789"
  putStrLn "\n"
  putStrLn $ concatMap displayLine [[4],[4],[4,6],[1,2,3,4,5,6,9]] ++ "0123456789"