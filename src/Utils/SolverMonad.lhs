A monad for solving constraints
===============================  

> module Utils.SolverMonad where

> import Data.Map (Map)
> import qualified Data.Map as Map

> import Control.Monad.Except    
> import Control.Monad.Identity
> import Control.Monad.State

> import Syntax.Type     


Monad definition

> type Ctx = Map Name Type

> type FieldMap = Map Name Fields      
  
> data Conf = Conf {
>                counter   :: Int
>             ,  ctx       :: Ctx
>             ,  fieldMap  :: FieldMap              
>             }      

> type SolverM a = ExceptT String (StateT Conf Identity) a

> runSolverM :: Conf -> SolverM a -> (Either String a, Conf)
> runSolverM c m = runIdentity (runStateT (runExceptT m) c)              

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

Looking up a definition and generating a fresh variable         

> lookupDef :: Name -> SolverM Type
> lookupDef n
>     = do
>         m <- gets (Map.lookup n . ctx)
>         maybe (fresh >>= \t -> insertDef n t >> return t)
>               return 
>               m

Inserting a new field

> insertField :: Name -> Field -> SolverM ()
> insertField n f = do
>                     s <- get
>                     let m = fieldMap s     
>                     put $ case Map.lookup n (fieldMap s) of
>                              Nothing -> s{fieldMap = Map.insert n [f] m }
>                              Just fs -> s{fieldMap = Map.insert n (f:fs) m }           
>                                     
