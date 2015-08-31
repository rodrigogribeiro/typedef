A monad for solving constraints
===============================  

> module Utils.SolverMonad where

> import Data.Map (Map)
> import qualified Data.Map as Map
  
> import Control.Monad.Identity
> import Control.Monad.State

> import Syntax.Type     


Monad definition

> data Conf = Conf {
>                counter :: Int
>             ,  ctx     :: Map Name Type
>             }      

> type SolverM a = (StateT Conf Identity) a

Generating fresh variables

> fresh :: SolverM Type
> fresh = do
>           s <- get
>           put (s{counter = (counter s) + 1})
>           return (Var $ Name ("x_" ++ show (counter s)))

Inserting a new definition            
                  
> insertDef :: Name -> Type -> SolverM ()
> insertDef n t
>     = do
>         s <- get
>         put (s{ctx = Map.insert n t (ctx s)})     
