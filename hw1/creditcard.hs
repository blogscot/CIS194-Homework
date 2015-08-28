import Debug.Trace

double :: [Integer] -> [Integer]
double xs = reverse [if odd pos then x * 2 else x | (x, pos) <- zip (reverse xs) [0..]]

toDigits :: Integer -> [Integer]
toDigits num
  | num > 0 = [ read [n] | n <- show num]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse $ zipWith (*) (reverse xs) oneTwo
  where oneTwo = 1 : 2 : oneTwo

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther $ toDigits n) `mod` 10 == 0

main = do
  -- print $ double [1,3,9,6]
  -- print $ double [1,3,9,6,7]
  -- print $ toDigits 181
  -- print $ toDigits 0
  -- print $ toDigits (-17)
  -- print $ toDigitsRev 1234
  print $ doubleEveryOther [8,7,6,5]
  print $ doubleEveryOther [1,2,3]
  -- print $ sumDigits [16,7,12,5]
  -- print $ toDigits 4012888888881881
  -- print $ double $ toDigits 4012888888881881
  -- print $ sumDigits . double $ toDigits 4012888888881881
  print $ validate 4012888888881881
  print $ validate 4012888888881882