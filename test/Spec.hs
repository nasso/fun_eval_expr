import ParserSpec
import System.Exit

main :: IO ()
main = do
  good <- and <$> sequence [ParserSpec.runTests]
  if good
    then exitSuccess
    else exitFailure
