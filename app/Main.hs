module Main where

import Grammar (expr)
import Lib (eval)
import Numeric (showFFloatAlt)
import Parsing (parse)
import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.IO (Handle, hPutStr, hPutStrLn, stderr, stdout)

hUsage :: Handle -> IO ()
hUsage h =
  hPutStr h "Usage: "
    >> getProgName >>= hPutStr h
    >> hPutStrLn h " <expression>"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [e] -> case eval e of
      Left err -> hPutStrLn stderr err >> exitWith (ExitFailure 84)
      Right val -> display val 2
    ["-p", e] -> print (parse expr e)
    [] -> hUsage stdout
    _ -> hUsage stderr >> exitWith (ExitFailure 84)

-- | Display a double precision floating point number with n decimal places.
display :: Double -> Int -> IO ()
display x n = putStrLn $ showFFloatAlt (Just n) r ""
  where
    r = fromIntegral (floor (x * t) :: Integer) / t
    t = 10 ^ n
