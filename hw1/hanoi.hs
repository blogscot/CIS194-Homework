type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi num a b c = hanoi (num-1) a c b ++ [(a, b)] ++ hanoi (num-1) c b a


main = print $ hanoi 3 "a" "b" "c"



-- Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
