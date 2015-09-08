module Main where    

import Control.Monad (unless)   
import System.Environment (getArgs)
import System.Exit (exitSuccess)
    
import Parser.ConstrParser    
import Solver.ConstrSolver
import Utils.SolverMonad
import Utils.Pretty
 

main :: IO ()
main = do
        args <- getArgs
        unless (single args) exitSuccess
        f <- readFile (head args)
        either error 
               (\c -> either putStrLn
                             (mapM_ (putStrLn . show . pprint))
                             (solver c emptyConf))
               (parser f)


single :: [a] -> Bool
single [ _ ]  = True
single _ = False
