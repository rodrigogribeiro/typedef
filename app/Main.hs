module Main where    
    
import Parser.ConstrParser    
import Solver.ConstrSolver
import Utils.SolverMonad
 

main :: IO ()
main = do
        f <- readFile "./test/cases/T0.ctr"
        either error 
               (\c -> print $ solver c emptyConf)
               (parser f)
