module Grammar
  ( Expr (..),
    expr,
  )
where

import Control.Applicative
import Parsing

data Expr
  = Const Double
  | Sum Expr Expr
  | Diff Expr Expr
  | Prod Expr Expr
  | Quot Expr Expr
  | Pow Expr Expr
  | Neg Expr
  deriving (Eq, Show)

expr :: Parser Expr
expr = do
  lhs <- term
  rhs <- optional (symbol "+" >> expr)
  case rhs of
    Nothing -> return lhs
    Just r -> return $ Sum lhs r

term :: Parser Expr
term = do
  lhs <- factor
  rhs <- optional (symbol "*" >> term)
  case rhs of
    Nothing -> return lhs
    Just r -> return $ Prod lhs r

factor :: Parser Expr
factor = do
  lhs <- base
  rhs <- optional (symbol "^" >> factor)
  case rhs of
    Nothing -> return lhs
    Just r -> return $ Pow lhs r

base :: Parser Expr
base = do
  sign <- optional (symbol "+" <|> symbol "-")
  value <- (symbol "(" *> expr <* symbol ")") <|> constant
  return $ if sign == Just "-" then Neg value else value

constant :: Parser Expr
constant = Grammar.Const <$> lexeme number

number :: Parser Double
number = do
  n <- integer
  d <- fraction <|> return 0.0
  e <- tenExp <|> return 0
  return $ (n + d) * 10 ^^ e

integer :: Parser Double
integer = read <$> some digit

fraction :: Parser Double
fraction =
  do
    d <- char '.' *> some digit
    return $ read $ '0' : '.' : d

tenExp :: Parser Int
tenExp = do
  _ <- char 'e' <|> char 'E'
  s <- optional (symbol "+" <|> symbol "-")
  n <- read <$> some digit
  return $ if s == Just "-" then - n else n
