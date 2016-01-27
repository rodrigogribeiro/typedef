Generating constraints for CoreC programs
=========================================           

> module Gen.ConstrGen (generator) where

> import Control.Monad  
> import Control.Monad.Trans
> import Control.Monad.Writer
> import Control.Monad.State    

> import Data.Maybe  
> import Data.Map (Map)
> import qualified Data.Map as Map
  
> import Syntax.Constraint
> import Syntax.CoreC
> import Syntax.Type    

> import Utils.Pretty

Top level interface for constraint generator
  
> generator :: Program -> IO Constr
> generator p = liftM (snd . fst) (runGenM (newVar >>= generate p))   

Monad for generating constraints       
        
> type GenM a = (WriterT Constr (StateT Int IO)) a

> runGenM :: GenM a -> IO ((a, Constr), Int)  
> runGenM g = runStateT (runWriterT g) 0
  
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
>     generate Null t = return ()
>     generate (EVar n) t
>         = equality (Var n) t
>     generate (Lit l) t = generate l t
>     generate (BOp op e e') t
>         = do
>             v <- fresh
>             v' <- fresh
>             exists v $
>                 exists v' $
>                    do
>                      generate e (Var v)
>                      generate e' (Var v')
>                      equality (varOp op)
>                               (Simple (Function (nameOp op) t
>                                                 [Var v, Var v']))
>     generate (ECall n es) t
>         = do
>             vs <- mapM (const fresh) es
>             foldr (\v ac -> exists v ac)
>                   (do
>                      let vs' = map Var vs
>                      zipWithM_ generate es vs'
>                      equality (Var n) (Simple (Function n t vs')))
>                   vs
>     generate (FAccess e n) t
>         = do
>             v  <- fresh
>             exists v $
>                do
>                   v' <- fresh
>                   exists v' $
>                      do 
>                        generate e (Var v)
>                        equality (Var n) (Var v')
>                        has (Var v) n (Var v')
>                        equality t (Var v')
>     generate (AAccess e e') t
>         = do
>             v  <- fresh
>             exists v $
>                 do
>                   generate e (Var v)      
>                   generate e' (Simple (Int None))
>                   equality t (Simple $ Pointer $ Var v)
>     generate (Cast t' e) t
>          = do
>              generate e t'
>              equality t' t
>     generate (Addr e) t
>          = do
>              v <- fresh
>              exists v $
>                do
>                  generate e (Var v)
>                  equality t (Simple $ Pointer (Var v))
>     generate (PAccess e) t
>          = do
>              v <- fresh
>              exists v $
>                 do
>                   generate e (Var v)
>                   equality (Var v) (Simple $ Pointer t)
>     generate (PFieldAccess e n) t
>          = do
>              v   <- fresh
>              v'  <- fresh
>              v'' <- fresh
>              exists v $
>                exists v' $
>                  exists v'' $
>                     do
>                       generate e (Var v)
>                       equality (Var n) (Var v'')
>                       equality (Var v) (Simple $ Pointer $ Var v')
>                       has (Var v) n (Var v'')
>                       equality t (Var v'')

> instance Generate Cmd where
>     generate (VarDef t n e) _ = isDef t >> define n t (generate e t)
>     generate (PointerAssign n e) _
>               = do
>                   v <- fresh
>                   exists v $
>                       do
>                         generate e (Var v)
>                         equality (Var n) (Simple $ Pointer $ Var v)
>     generate (FieldAssign n n' e) _
>               = do
>                    v <- fresh
>                    exists v $
>                       do
>                         generate e (Var v)
>                         equality (Var n') (Var v)         
>                         has (Var n) n' (Var v)
>     generate (ArrayAssign n e e') _
>               = do
>                   v <- fresh
>                   exists v $
>                      do     
>                        generate e' (Var v)
>                        generate e (Simple (Int None))
>                        equality (Var n) (Simple $ Pointer $ (Var v))
>     generate (CCall n es) _
>         = do
>             v <- fresh
>             vs <- mapM (const fresh) es
>             foldr (\v ac -> exists v ac)
>                   (do
>                      let vs' = map Var vs
>                      zipWithM_ generate es vs'
>                      equality (Var n) (Simple (Function n (Var v) vs')))
>                   vs
>     generate (Return e) t
>          = generate e t

> instance Generate Decl where
>    generate (DTypeDef t n) t' = define n t (tell Truth)
>    generate (DFunction t n ps cs) t' = do
>                        isDef t
>                        mapM_ (isDef . fst) ps      
>                        define n (Simple $ Function n t (map fst ps)) $
>                                 foldr (uncurry (flip define))
>                                       (mapM_ (flip generate t) cs)
>                                       ps
                    
> instance Generate Program where
>    generate p t = mapM_ (flip generate t) (unProg p)
                                             
> equality :: Type -> Type -> GenM ()
> equality t t' = tell (t :=: t')

> has :: Type -> Name -> Type -> GenM ()
> has t n t'
>     | isJust (nameOf t) = tell (Has (fromJust $ nameOf t) (Field n t'))
>     | otherwise = error "Impossible! ConstrGen.has!"                      

> exists :: Name -> GenM () -> GenM ()
> exists n g =  censor (Exists n) g

> define :: Name -> Type -> GenM () -> GenM ()
> define n t g = censor (Def n t) g          

> isDef :: Type -> GenM ()
> isDef t
>     | isJust (nameOf t) = tell (IsDefined (fromJust $ nameOf t))
>     | otherwise = return ()
                              
Auxiliar functions

> varOp :: Op -> Type
> varOp = Var . nameOp

> nameOp :: Op -> Name
> nameOp = Name . show . pprint          

> nameOf :: Type -> Maybe Name
> nameOf (Var v) = Just v
> nameOf _ = Nothing
