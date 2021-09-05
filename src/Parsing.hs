module Parsing
  ( module Parsing,
    module Control.Applicative,
  )
where

import Control.Applicative (Alternative (empty, (<|>)), many, optional, some)
import Data.Char (isAlpha, isAlphaNum, isDigit, isLower, isSpace, isUpper)

-- | A parser from `String` to values of type `a`.
newtype Parser a = Parser
  { parse :: String -> Maybe (a, String)
  }

-- impl Functor for Parser
instance Functor Parser where
  fmap ab ap =
    Parser
      ( \s -> case parse ap s of
          Nothing -> Nothing
          (Just (a, rest)) -> Just (ab a, rest)
      )

-- impl Applicative for Parser
instance Applicative Parser where
  pure v = Parser (\s -> Just (v, s))
  abp <*> ap =
    Parser
      ( \s -> case parse abp s of
          Nothing -> Nothing
          (Just (ab, rest)) -> parse (ab <$> ap) rest
      )

-- impl Monad for Parser
-- gives us >>= and >> to chain parsers
-- we also get to use return and do
instance Monad Parser where
  ap >>= bp =
    Parser
      ( \s -> case parse ap s of
          Nothing -> Nothing
          (Just (a, rest)) -> parse (bp a) rest
      )

-- impl Alternative for Parser
-- this is used mainly for <|> to choose between two parsers
instance Alternative Parser where
  empty = Parser (const Nothing)
  a <|> b =
    Parser
      ( \s -> case parse a s of
          Nothing -> parse b s
          res -> res
      )

-- primitive parsers

-- | Parse a single character.
getc :: Parser Char
getc =
  Parser
    ( \s -> case s of
        (x : xs) -> Just (x, xs)
        "" -> Nothing
    )

-- | Parse a single character satisfying the given predicate.
match :: (Char -> Bool) -> Parser Char
match f = do
  x <- getc
  if f x
    then return x
    else empty

-- | Parse a series of `a` separated by `b`s (without a leading `b`).
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy w s = do
  first <- w
  rest <- many (s >> w)
  return $ first : rest

-- | Parse a unicode whitespace character (space, tab, newline, etc.).
space :: Parser Char
space = match isSpace

-- | Parse a decimal digit.
digit :: Parser Char
digit = match isDigit

-- | Parse a lowercase letter.
lower :: Parser Char
lower = match isLower

-- | Parse an uppercase letter.
upper :: Parser Char
upper = match isUpper

-- | Parse any letter.
alpha :: Parser Char
alpha = match isAlpha

-- | Parse any letter or digit.
alphanum :: Parser Char
alphanum = match isAlphaNum

-- | Parse a specific character.
char :: Char -> Parser Char
char c = match (== c)

-- | Parse exactly the given string.
literal :: String -> Parser String
literal = foldr ((>>) . char) (return [])

-- | Make a parser consume any trailing whitespace.
lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  _ <- many space
  return x

-- | Parse exactly the given string, plus zero or more trailing whitespace.
symbol :: String -> Parser String
symbol = lexeme . literal
