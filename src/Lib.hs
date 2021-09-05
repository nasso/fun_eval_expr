module Lib
  ( eval,
  )
where

import Parsing (Parser, parse, real)

type ParseError = String

data Expr
  = Const Double
  | Sum Expr Expr
  | Diff Expr Expr
  | Prod Expr Expr
  | Quot Expr Expr
  | Pow Expr Expr

expr :: Parser Expr
expr = Const <$> real

calc :: Expr -> Double
calc (Const x) = x
calc (Sum x y) = calc x + calc y
calc (Diff x y) = calc x - calc y
calc (Prod x y) = calc x * calc y
calc (Quot x y) = calc x / calc y
calc (Pow x y) = calc x ** calc y

eval :: String -> Either ParseError Double
eval e = case parse expr e of
  Just (e, []) -> Right $ calc e
  Just (_, extra) -> Left $ "Unexpected extra input: " ++ extra
  _ -> Left $ "Couldn't parse expression: " ++ e
