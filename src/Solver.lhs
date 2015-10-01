Solver top level interface module
=====================

> module Solver where
  
> import Data.Map(Map)
> import qualified Data.Map as Map

> import Gen.ConstrGen
> import Parser.CoreCParser    
> import qualified Solver.ConstrSolver as S
> import Syntax.Type    
> import Utils.Pretty    
> import Utils.SolverMonad

Solver top level function       
     
> solver :: String -> IO (Either String [String])
> solver s = do
>             let r = parser s        
>             case r of
>               Left err -> return (Left err)
>               Right p  ->
>                   do
>                     t <- generator p
>                     print (pprint t)
>                     r' <- S.solver t initialConf
>                     case r' of
>                       Left err' -> return (Left err')
>                       Right t'  -> return $ Right $ (map ((flip (++) "\n") . show . pprint) t')


> initialConf :: Conf
> initialConf = emptyConf {
>                 ctx = Map.fromList operators
>               }

Operators                
                
> operators :: [(Name, Type)]
> operators = map (\(n,t) -> (Name n,Simple t)) ops
>             where
>               ops = [("*", typ "*"), ("/", typ "/"), ("+",typ "+"),
>                      ("-", typ "-"), ("<", typ' "<"), ("<=", typ' "<="),
>                      (">", typ' ">"), (">=", typ' ">="), ("==", typ' "=="),
>                      ("!=", typ' "!="), ("&", typ "&"), ("|", typ "|"),
>                      ("^", typ "^")]
>               typ n = Function (Name n) i [i, i]
>               typ' n = Function (Name n) b [b, b]
>               b = Simple CBool
>               i = Simple (Int None)    
