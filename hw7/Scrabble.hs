{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Char
import Data.Map.Strict as M hiding (foldr, map)
import Data.Maybe

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

score :: Char -> Score
score c = Score $ fromMaybe 0 $ M.lookup [toUpper c] $ fromList scrabble

scoreString :: String -> Score
scoreString = Score . sum . map (getScore . score)


scrabble :: [(String, Int)]
scrabble =
  [ ("A", 1), ("B", 3), ("C", 3), ("D", 2), ("E", 1)
  , ("F", 4), ("G", 2), ("H", 4), ("I", 1), ("J", 8)
  , ("K", 5), ("L", 1), ("M", 3), ("N", 1), ("O", 1)
  , ("P", 3), ("Q", 10), ("R", 1), ("S", 1), ("T", 1)
  , ("U", 1), ("V", 4), ("W", 4), ("X", 8), ("Y", 4)
  , ("Z", 10) ]

tests :: IO ()
tests = do
  print $ score 'h'
  print $ scoreString "yay "
  print $ scoreString "haskell!"
