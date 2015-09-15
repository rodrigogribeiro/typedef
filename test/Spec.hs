import Test.Tasty
import Test.Tasty.HUnit

import Solver
    
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests]

unitTests = testGroup "Unit tests"
               [ testCase "Solver test for T0" $ execATest "./test/cases/T0.ctr",
                 testCase "Solver test for T1" $ execATest "./test/cases/T1.ctr",
                 testCase "Solver test for T2" $ execATest "./test/cases/T2.ctr"]


execATest file = do               
                  c <- readFile file
                  ts <- solver c
                  either putStrLn (mapM_ putStrLn) ts

                          
