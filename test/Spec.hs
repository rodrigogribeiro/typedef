import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = TestGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
               [ testCase "Solver stage 1 test for T0" $ undefined]

               
