import Test.HUnit
import QuickCheck
import CheckerHUnitTests (runShellTests)

main :: IO ()
main = do
  putStrLn "Run unit tests:"
  runShellTests
  putStrLn "Run quick check:"
  qc
  putStrLn "Done testing"
