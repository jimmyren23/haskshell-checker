import Test.HUnit
import Test.QuickCheck
import CheckerHUnitTests (runShellTests)

main :: IO ()
main = do
  putStrLn "Run unit tests:"
  runShellTests
  putStrLn "Done testing"
