import Data.Either (isRight)

import Test.Tasty
import Test.Tasty.HUnit

import Solver
    
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
               [ testCase "Solver test for T0" $ execATest "./test/cases/T0.c"
               , testCase "Solver test for T1" $ execATest "./test/cases/T1.c"
               , testCase "Solver test for T2" $ execATest "./test/cases/T2.c"
               , testCase "Solver test for T3" $ execATest "./test/cases/T3.c"
               , testCase "Solver test for T4" $ execATest "./test/cases/T4.c"
               , testCase "Solver test for T5" $ execATest "./test/cases/T5.c"
               , testCase "Solver test for T6" $ execATest "./test/cases/T6.c" ]


execATest file = do               
                  c <- readFile file     
                  r <- solver c file
                  assertBool (show r) (isRight r)


                          
