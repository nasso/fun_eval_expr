{-# LANGUAGE TemplateHaskell #-}

import Grammar (Expr (..), expr)
import Numeric (showEFloat, showFFloatAlt)
import Parsing (parse)
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Args (maxSuccess),
    elements,
    forAllProperties,
    quickCheckWithResult,
    stdArgs,
  )

data NumberFormatter = NumberFormatter (Double -> String) String

instance Show NumberFormatter where
  show (NumberFormatter _ name) = name

instance Arbitrary NumberFormatter where
  arbitrary = do
    (fn, name) <-
      elements $
        map
          (\(f, name) -> (flip (f Nothing) "", name))
          [(showFFloatAlt, "showFFloatAlt"), (showEFloat, "showEFloat")]
    return $ NumberFormatter fn name

eps :: Double
eps = 0.00001

floatEq :: Double -> Double -> Double -> Bool
floatEq x y = (<) $ abs (x - y)

exprEq :: Expr -> Expr -> Double -> Bool
exprEq (Const x) (Const y) d = floatEq x y d
exprEq (Sum x y) (Sum x' y') d = exprEq x x' d && exprEq y y' d
exprEq (Diff x y) (Diff x' y') d = exprEq x x' d && exprEq y y' d
exprEq (Prod x y) (Prod x' y') d = exprEq x x' d && exprEq y y' d
exprEq (Quot x y) (Quot x' y') d = exprEq x x' d && exprEq y y' d
exprEq (Pow x y) (Pow x' y') d = exprEq x x' d && exprEq y y' d
exprEq (Neg x) (Neg x') d = exprEq x x' d
exprEq _ _ _ = False

sExprEq :: String -> Expr -> Double -> Bool
sExprEq s e d = case parse expr s of
  Just (ex, "") -> exprEq ex e d
  _ -> False

prop_parseNatural :: Word -> Bool
prop_parseNatural x = parse expr (show x) == Just (Const $ fromIntegral x, "")

prop_parseInt :: Int -> Bool
prop_parseInt x
  | x >= 0 = parse expr (show x) == Just (Const $ fromIntegral x, "")
prop_parseInt x =
  parse expr (show x) == Just (Neg $ Const $ fromIntegral $ abs x, "")

prop_parseReal :: NumberFormatter -> Double -> Bool
prop_parseReal (NumberFormatter f _) x | x >= 0 = sExprEq (f x) (Const x) eps
prop_parseReal (NumberFormatter f _) x = sExprEq (f x) (Neg $ Const $ abs x) eps

--------------------------------------------------------------------------------

return []

runTests :: IO Bool
runTests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100})

main :: IO ()
main = do
  good <- runTests
  if good
    then exitSuccess
    else exitFailure
