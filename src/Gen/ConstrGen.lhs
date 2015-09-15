Generating constraints for CoreC programs
=========================================           

> module Gen.ConstrGen where

> import Control.Monad.Trans
> import Control.Monad.Writer
> import Control.Monad.State    

> import Syntax.Constr
> import Syntax.CoreC    


Monad for generating constraints
        
> type GenM a = (ReaderT Ctx (WriterT Constraint (StateT Int IO))) a

> fresh :: GenM Name
> fresh = do
>           n <- get
>           put (n + 1)     
>           return (Name $ "x_" ++ show n)     

> newVar :: Fresh Type
> newVar = Var <$> fresh
            
Constraint generation algorithm

> class Generate a where
>    generate :: a -> Type -> GenM ()

> instance Generate Literal where
>     generate (IInt _) t = equality t (Simple Int)
>     generate (IChar _) t = equality t (Simple Char)
>     generate (IFloat _) t = equality t (Simple Float)
>     generate (IString _) t = equality t (Simple (Pointer Char))                        

> instance Generate Exp where
>     generate (EVar n) t = newVar >>= equality t
>     generate (Lit l) t = equality l t
>     generate (Bop op e e') t
>         = do
>              v <- newVar
>              v' <- newVar
>              generate e v
>              generate e' v'

> equality :: Type -> Type -> GenM ()
> equality t t' = tell (t :=: t')             
