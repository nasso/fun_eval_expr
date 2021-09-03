{-# LANGUAGE TemplateHaskell #-}

module ParserSpec where

import Numeric (showFFloatAlt)
import Parsing (integer, natural, parse, real)
import Test.QuickCheck
  ( Args (maxSuccess),
    forAllProperties,
    quickCheckWithResult,
    stdArgs,
  )

showReal :: Double -> String
showReal x = showFFloatAlt Nothing x ""

prop_parseNatural :: Word -> Bool
prop_parseNatural x = parse natural (show x) == Just (x, "")

prop_parseInt :: Int -> Bool
prop_parseInt x = parse integer (show x) == Just (x, "")

prop_parseReal :: Double -> Bool
prop_parseReal x = parse real (showReal x) == Just (x, "")

--------------------------------------------------------------------------------

return []

runTests :: IO Bool
runTests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 100})
