module Main where

import Lib (eval)
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
    [expr] -> case eval expr of
      Left err -> hPutStrLn stderr err >> exitWith (ExitFailure 84)
      Right val -> print val
    [] -> hUsage stdout
    _ -> hUsage stderr >> exitWith (ExitFailure 84)
