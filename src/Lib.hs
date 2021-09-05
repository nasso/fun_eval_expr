module Lib
  ( eval,
  )
where

import Grammar (Expr (..), expr)
import Parsing (parse)

type ParseError = String

calc :: Expr -> Double
calc (Const x) = x
calc (Sum x y) = calc x + calc y
calc (Diff x y) = calc x - calc y
calc (Prod x y) = calc x * calc y
calc (Quot x y) = calc x / calc y
calc (Pow x y) = calc x ** calc y

eval :: String -> Either ParseError Double
eval s = case parse expr s of
  Just (e, []) -> Right $ calc e
  Just (_, extra) -> Left $ "Unexpected extra input: " ++ extra
  _ -> Left $ "Couldn't parse expression: " ++ s
