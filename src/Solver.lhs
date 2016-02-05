Solver top level interface module
=====================

> module Solver where
  
> import Data.Map(Map)
> import qualified Data.Map as Map
> import System.FilePath    

> import Gen.ConstrGen
> import Parser.CoreCParser
> import qualified Parser.ConstrParser as C    
> import qualified Solver.ConstrSolver as S
> import Syntax.Type    
> import Utils.Pretty    
> import Utils.SolverMonad

Solver top level function       
     
> solver :: String -> String -> IO (Either String [String])
> solver s f = do
>                let ext = (takeExtension f) == ".ctr"
>                if ext then justSolve s f
>                  else solver' s f

> justSolve :: String -> String -> IO (Either String [String])
> justSolve s f = do
>                   let r = C.parser s
>                   case r of
>                     Left err -> return (Left err)
>                     Right c  -> do
>                                  r' <- S.solver c initialConf
>                                  case r' of
>                                    Left err -> return (Left err)
>                                    Right t' -> do
>                                      let ss = map ((flip (++) "\n") . show . pprint) t'
>                                      writeFile (f -<.> "tdef") (concat ss)         
>                                      return $ Right $ ss             
>                   

> solver' :: String -> String -> IO (Either String [String])          
> solver' s f = do
>              let r = parser s        
>              case r of
>                Left err -> return (Left err)
>                Right p  ->
>                   do
>                     t <- generator p
>                     writeFile (f -<.> "ctr") (show $ pprint t)
>                     r' <- S.solver t initialConf
>                     case r' of
>                       Left err' -> return (Left err')
>                       Right t'  -> do
>                                      let ss = map ((flip (++) "\n") . show . pprint) t'
>                                      writeFile (f -<.> "tdef") (concat ss)         
>                                      return $ Right $ ss


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
