Generating constraints for CoreC programs
=========================================           

> module Gen.ConstrGen where

> import Control.Monad.Trans
> import Control.Monad.Writer
> import Control.Monad.State
> import Control.Monad.Reader    

> import Data.Map (Map)
> import qualified Data.Map as Map
  
> import Syntax.Constraint
> import Syntax.CoreC
> import Syntax.Type    

> import Utils.Pretty  

Monad for generating constraints

> type Ctx = Map Name Type       
        
> type GenM a = (ReaderT Ctx (WriterT Constr (StateT Int IO))) a

> fresh :: GenM Name
> fresh = do
>           n <- get
>           put (n + 1)     
>           return (Name $ "x_" ++ show n)     

> newVar :: GenM Type
> newVar = Var <$> fresh
            
Constraint generation algorithm

> class Generate a where
>    generate :: a -> Type -> GenM ()

> instance Generate Literal where
>     generate (IInt _) t = equality t (Simple (Int None))
>     generate (IChar _) t = equality t (Simple (Char None))
>     generate (IFloat _) t = equality t (Simple Float)
>     generate (IString _) t = equality t (Simple (Pointer (Simple (Char None))))                        

> instance Generate Exp where
>     generate (EVar n) t
>         = do
>             n <- fresh
>             exists n (equality t (Var n))
>     generate (Lit l) t = equality l t
>     generate (BOp op e e') t
>         = do
>             v <- newVar
>             v' <- newVar
>             generate e v
>             generate e' v'
>             equality ((varOp op) :=: (Simple (Function nameOp t [v,v'])))
>     generate (ECall n es) t
>         = do
>             vs <- mapM (const newVar) es
>             zipWithM_ generate es vs
>             equality ((Var n) :=: (Simple (Function n t vs)))
>     generate (FAccess e n) t
>         = do
>             v  <- newVar
>             v' <- newVar     
>             generate e v
>             generate n v'
>             has v n v'
>             equality t v'
>     generate (AAccess e e') t
>         = do
>             v  <- newVar
>             generate e v      
>             generate e' (Simple Int None)
>             equality t (Simple $ Pointer v)
>     generate (Cast t' e) t
>          = do
>              generate e t'
>              equality t' t
>     generate (Addr e) t
>          = do
>              v <- newVar
>              generate e v
>              equality t (Simple $ Pointer v)
>     generate (PAccess e) t
>          = do
>              v <- newVar
>              generate e v
>              equality v (Simple $ Pointer t)
>     generate (PFieldAccess e n) t
>          = do
>              v   <- newVar
>              v'  <- newVar
>              v'' <- newVar
>              generate e v
>              generate n v''
>              equality v (Simple $ Pointer v')
>              has v n v''
>              equality t v''
                      
> equality :: Type -> Type -> GenM ()
> equality t t' = tell (t :=: t')

> has :: Type -> Name -> Type -> GenM ()
> has t n t' = tell (Has (nameOf t) (Field n t'))

> exists :: Name -> GenM () -> GenM ()
> exists n g = censor (Exists n) g          

Auxiliar functions

> varOp :: Op -> Type
> varOp = Var . nameOp

> nameOp :: Op -> Name
> nameOp = Name . show . pprint          

> nameOf :: Type -> Name
> nameOf (Var v) = v
> nameOf (TypeDef _ v) = v
> nameOf _ = error "Impossible!\nConstrGen.nameOf"                       
