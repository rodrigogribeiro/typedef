module Main where    
    
import Parser.ConstrParser    
import Solver.ConstrSolver
import Utils.SolverMonad
import Utils.Pretty
 

main :: IO ()
main = do
        f <- readFile "./test/cases/T0.ctr"
        either error 
               (\c -> either putStrLn
                             (mapM_ (putStrLn . show . pprint))
                             (solver c emptyConf))
               (parser f)
