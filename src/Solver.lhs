Solver top level interface module
=====================

> module Solver where

> import Data.List (intersperse)
  
> import Parser.ConstrParser
> import qualified Solver.ConstrSolver as S
> import Utils.Pretty    
> import Utils.SolverMonad

Solver top level function  
     
> solver :: String -> Either String [String]
> solver s = do
>             r <- parser s
>             r' <- S.solver r emptyConf      
>             return (intersperse ";\n" $ map (show . pprint) r')      
