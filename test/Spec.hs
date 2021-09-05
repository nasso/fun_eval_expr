import GrammarSpec
import System.Exit

main :: IO ()
main = do
  good <- and <$> sequence [GrammarSpec.runTests]
  if good
    then exitSuccess
    else exitFailure
