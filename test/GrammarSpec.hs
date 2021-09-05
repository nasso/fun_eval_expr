{-# LANGUAGE TemplateHaskell #-}

module GrammarSpec where

import Grammar (number)
import Numeric (showFFloatAlt)
import Parsing (parse)
import Test.QuickCheck
  ( Args (maxSuccess),
    forAllProperties,
    quickCheckWithResult,
    stdArgs,
  )

showReal :: Double -> String
showReal x = showFFloatAlt Nothing x ""

prop_parseNatural :: Word -> Bool
prop_parseNatural x = parse number (show x) == Just (fromIntegral x, "")

prop_parseInt :: Int -> Bool
prop_parseInt x = parse number (show x) == Just (fromIntegral x, "")

prop_parseReal :: Double -> Bool
prop_parseReal x = parse number (showReal x) == Just (x, "")

--------------------------------------------------------------------------------

return []

runTests :: IO Bool
runTests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100})
