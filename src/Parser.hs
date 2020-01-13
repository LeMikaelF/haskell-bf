module Parser (
    char
  , digit
  , integer
  , item
  , parse
  , satisfy
  , sepBy
  , spaces
  , string
  , Parser (..)
  )where

import Control.Applicative
import Data.Bifunctor (first)
import Data.Char (isDigit)
import Data.List

newtype Parser a = Parser { runParser :: String -> [(a, String)] }

instance Functor Parser where
  fmap f p = Parser $ \s ->
    fmap (first f) $ runParser p s

instance Applicative Parser where
  pure x = Parser $ \s -> [(x, s)]
  p1 <*> p2 = Parser $ \s1 -> do
    (f, s2) <- runParser p1 s1
    (x, s3) <- runParser p2 s2
    return (f x, s3)

instance Monad Parser where
  return = pure
  k >>= f = Parser $ \s1 -> do
    (x, s2) <- runParser k s1
    return =<< runParser (f x) s2

instance Alternative Parser where
  p1 <|> p2 = Parser $ \s ->
    case runParser p1 s of
      [] -> runParser p2 s
      x  -> x
  empty = Parser $ const mempty


--Exported functions

char :: Char -> Parser Char
char = satisfy . (==)

digit :: Parser Char
digit = satisfy isDigit

item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> empty
    (x:xs) -> pure (x, xs)

integer :: Parser Int
integer = do
  s <- pure <$> oneOf "--" <|> nothing
  ns <- some digit
  return $ read (s ++ ns)

nothing :: Parser String
nothing = return []

oneOf :: String -> Parser Char
oneOf = satisfy . flip elem

parse :: Parser a -> String -> Either String a
parse p s = case runParser p s of
  [(a, _)]  -> Right a
  _         -> Left "parser error"

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = item >>= \x ->
  if f x then pure x else empty

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p d = do
  x <- p
  xs <- many $ d >> p    
  return (x:xs)

spaces :: Parser String
spaces = (some $ oneOf " \n\r") <|> return [] 

string :: String -> Parser String
string = traverse char
