import Data.Char
import Data.Maybe
import Control.Applicative

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
        where (ns, rest) = span isDigit xs

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)


  -- 1.
  -- f ::     (a -> b)
  -- first :: (a -> b) -> (a, c) -> (b, c)
  -- first f :: (a, c) -> (b, c)
  -- 2.
  -- Parser is essentially wrapping a function
  -- Parser a :: String -> Maybe (a, String)
  -- a String :: Maybe (a, b)
  -- 3.
  -- Maybe is a functor which we can use fmap to manipulate
  -- fmap (a -> b) -> f a -> f b
  -- fmap (first f) Maybe (x, y) = Maybe (f x, y)
  -- which is wrapped again in Parser.
instance Functor Parser where
  fmap f (Parser a) = Parser $ \xs -> fmap (first f) (a xs)

-- class Functor f => Applicative (f :: * -> *) where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- instance Applicative Maybe where
--     pure = Just
--     Nothing <*> _ = Nothing
--     (Just f) <*> something = fmap f something

instance Applicative Parser where
-- f and g are functions, String -> (a, String)
-- The first parser gets applied to a string input, the results,
-- a function and a string, are applied to the second parser g, in
-- the same manner as with the Functor Parser above.
  pure a = Parser $ \xs -> Just (a, xs)
  Parser f <*> Parser g = Parser $ \xs ->
    case f xs of
    Nothing -> Nothing
    Just (f', rest) -> fmap (first f') (g rest)


abParser :: Parser (Char, Char)
abParser = (\x y -> (x,y)) <$> char 'a' <*> char 'b'
-- abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> char 'a' <*> char 'b'

intPair :: Parser [Integer]
intPair = (\x _ y -> x : [y]) <$> posInt <*> char ' ' <*> posInt

instance Alternative Parser where
  empty = Parser (const Nothing)
  l <|> r = Parser f
    where f str = runParser l str <|> runParser r str

intOrUppercase :: Parser ()
intOrUppercase = const () <$> posInt <|> const () <$> satisfy isUpper

main = do
  print $ runParser (satisfy isUpper) "ABC"
  print $ runParser (satisfy isUpper) "abc"
  print $ runParser (char 'x') "xyz"
  print $ runParser (char 'x') "abc"
  print $ runParser posInt "1234abcd"
  print $ runParser abParser "abcdef"
  print $ runParser abParser "aecdef"
  print $ runParser abParser_ "abcdef"
  print $ runParser intPair "12 34"
  print $ runParser (intPair <|> intPair) "12 56"
