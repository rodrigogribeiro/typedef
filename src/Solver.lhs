Solver top level interface module
=====================

> module Solver where
  
> import Parser.ConstrParser
> import qualified Solver.ConstrSolver as S
> import Utils.Pretty    
> import Utils.SolverMonad

Solver top level function  
     
> solver :: String -> Either String [String]
> solver s = do
>             r <- parser s
>             r' <- S.solver r initialConf      
>             return (map ((flip (++) ";\n") . show . pprint) r')      

> initialConf :: Conf
> initialConf = emptyConf               
