{-# LANGUAGE StandaloneDeriving #-}
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

prop_parseNatural :: Word -> Bool
prop_parseNatural x = parse expr (show x) == Just (Const $ fromIntegral x, "")

prop_parseInt :: Int -> Bool
prop_parseInt x
  | x >= 0 = parse expr (show x) == Just (Const $ fromIntegral x, "")
prop_parseInt x =
  parse expr (show x) == Just (Neg $ Const $ fromIntegral $ abs x, "")

prop_parseReal :: NumberFormatter -> Double -> Bool
prop_parseReal (NumberFormatter f _) x
  | x >= 0 = parse expr (f x) == Just (Const x, "")
prop_parseReal (NumberFormatter f _) x =
  parse expr (f x) == Just (Neg $ Const $ abs x, "")

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
