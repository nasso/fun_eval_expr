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
expr = term `chainl1` sumop

sumop :: Parser (Expr -> Expr -> Expr)
sumop = Sum <$ symbol "+" <|> Diff <$ symbol "-"

term :: Parser Expr
term = factor `chainl1` prodop

prodop :: Parser (Expr -> Expr -> Expr)
prodop = Prod <$ symbol "*" <|> Quot <$ symbol "/"

factor :: Parser Expr
factor = base `chainr1` powop

powop :: Parser (Expr -> Expr -> Expr)
powop = Pow <$ symbol "^"

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
