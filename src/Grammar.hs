module Grammar
  ( Expr (..),
    expr,
    number,
  )
where

import Parsing

data Expr
  = Const Double
  | Sum Expr Expr
  | Diff Expr Expr
  | Prod Expr Expr
  | Quot Expr Expr
  | Pow Expr Expr

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
  lhs <- pow
  rhs <- optional (symbol "^" >> factor)
  case rhs of
    Nothing -> return lhs
    Just r -> return $ Pow lhs r

pow :: Parser Expr
pow =
  ( do
      _ <- symbol "("
      e <- expr
      _ <- symbol ")"
      return e
  )
    <|> constant

constant :: Parser Expr
constant = Const <$> lexeme number

-- | Parse a number.
number :: Parser Double
number = do
  s <- char '-' <|> char '+' <|> return '+'
  n <- some digit
  _ <- char '.'
  d <- some digit
  let num = read $ n ++ ('.' : d)
   in return $ if s == '-' then - num else num
