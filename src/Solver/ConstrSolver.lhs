> {-# LANGUAGE FlexibleInstances #-}
        
The solver algorithm
=============

Basically the algorithm performs 3 stages:

1 - Create all fresh variables for existencial quantifiers, collects all definitions and field constraints
2 - Expand all definitions types  
3 - Solve all equality constraints and updates field constraints by applying the resulting unifier
4 - Build record definitions from field constraints

> module Solver.ConstrSolver(solver) where

> import Control.Monad(zipWithM)  
> import Control.Monad.Except(throwError)  
> import Control.Monad.State(gets, modify)
> import Control.Monad.Trans
  
> import Data.Generics (everywhere, everything, listify, mkT, mkQ)
> import Data.List (union,nubBy)
> import Data.Map(Map)
> import qualified Data.Map as Map
> import Data.Set(Set)
> import qualified Data.Set as Set    

> import Syntax.Type
> import Syntax.Constraint
> import Syntax.CoreC    
> import Solver.Subst
> import Utils.Pretty    
> import Utils.SolverMonad

Solver top-level interface    

> solver :: Constr -> Conf -> IO (Either String [Decl])
> solver c conf = do
>                  (e,c) <- runSolverM conf (solve c)
>                  return (either Left (Right . nubBy cmp . (map (apply (fix (defs c)))) . (++ (dx c))) (f e))
>                 where
>                   dx c = Map.foldrWithKey step [] (defs c)
>                   f = either Left (Right . g) 
>                   g = foldr go []
>                   go t@(Simple t') ac = DTypeDef t (sname t') : ac
>                   step k t ac = DTypeDef t k : ac
>                   fix = Map.foldrWithKey (\ k v ac -> if isVar v then Map.insert (outVar v) (Var k) ac else ac) Map.empty
>                   outVar (Var n) = n
>                   cmp (DTypeDef _ n) (DTypeDef _ n') = n == n'                 
>                   
       
> solve :: Constr -> SolverM [Type]
> solve c
>     = do
>         c1 <- solverStage1 c
>         c2 <- solverStage2 c1
>         s <- solverStage3 c2    
>         solverStage4 s

       

Stage 1: fresh variable creation and context building  

> subst :: Name -> Type -> Constr -> Constr
> subst n t c = everywhere (mkT (subs n t)) c
>               where
>                  subs n t (Var n')
>                       | n == n' = t
>                       | otherwise = Var n'
>                  subs n t t' = t'     
  

> solverStage1 :: Constr -> SolverM Constr
> solverStage1 (Exists n c)
>              = do
>                  t <- fresh
>                  solverStage1 (subst n t c)
> solverStage1 (c :&: c')
>              = do
>                c1 <- solverStage1 c
>                c1' <- solverStage1 c'
>                return (c1 :&: c1')
> solverStage1 (Has n f)
>              = insertField n f >> return Truth
> solverStage1 (Def n t c)
>              = insertDef n t >> solverStage1 c
> solverStage1 (IsDefined n)
>              = lookupTypeDef n >> return Truth               
> solverStage1 c = return c               

Stage 2: expand definitions

> expand :: Ctx -> Ctx -> Constr -> Constr
> expand ctx defs c = foldr ($) c (zipWith subst vs ts)
>                     where
>                       ctx' = ctx `Map.union` defs
>                       ss = (Set.fromList $ Map.keys ctx') 
>                       vs = Set.toList $ Set.intersection ss (fv c)
>                       ts = (map ((Map.!) ctx') vs)
  
> solverStage2 :: Constr -> SolverM Constr
> solverStage2 c = do
>                    cx <- gets ctx
>                    ds <- gets defs
>                    return $ everywhere (mkT (expand cx ds)) c

Stage 3: unification of equality constraints  
                             
> collect :: Constr -> [Constr]
> collect = everything (++) (mkQ [] (\c -> if isEq c then [c] else []))

> isEq :: Constr -> Bool
> isEq (_ :=: _) = True
> isEq _ = False

> class Unifiable a where
>    unify :: a -> SolverM Subst           

> instance (Substitutable a, Unifiable a) => Unifiable [a] where
>    unify [] = return nullSubst
>    unify (c:cs) = do
>                     s <- unify c
>                     s' <- unify (apply s cs)
>                     return (s' @@ s)
                      
> instance Unifiable Constr where
>     unify (t :=: t') = unify (t,t')
>     unify c = throwError ("Impossible:\n" ++ (show $ pprint c))

> instance Unifiable (Field,Field) where
>     unify (f,f') = unify ((fieldType f),(fieldType f'))

> instance Unifiable (Type,Type) where
>     unify (Simple t, Simple t') = unify (t, t')
>     unify (Var n, t)                               
>         | occurs n t = occursCheckError n t
>         | otherwise  = return (n +-> t)
>     unify (t, Var n)                       
>         | occurs n t = occursCheckError n t
>         | otherwise  = return (n +-> t)


> instance Unifiable (CType, CType) where
>     unify (Pointer t, Pointer t') = unify (t, t') 
>     unify (Struct fs n, Struct fs' n')
>         | n == n' = do
>                       ss <- mapM unify (zip fs fs')
>                       return (foldr (@@) nullSubst ss)
>         | otherwise = unificationError n n'
>     unify (t@(Function n r ts), t'@(Function n' r' ts'))
>                 | n == n' = do
>                              s <- unify (r, r')
>                              ss <- mapM unify (zip ts ts')
>                              return (foldr (@@) nullSubst (ss ++ [s]))
>                 | otherwise = unificationError t t'
>     unify (t, t')
>         | convertible t t' = return nullSubst
>         | otherwise = unificationError t t'

> convertible :: CType -> CType -> Bool
> convertible t t' = or [t == t', intConversion t t']

> intConversion :: CType -> CType -> Bool
> intConversion t t' = integerPromotion t == integerPromotion t'  
  
> solverStage3 :: Constr -> SolverM Subst
> solverStage3 c
>     = do
>          s <- unify (collect c)     
>          modify (\st -> st{fieldMap = Map.map (apply s) (fieldMap st),
>                            ctx = Map.map (apply s) (ctx st) ,
>                            defs = Map.map (apply s) (defs st) })
>          return s

Stage 4: solving record constraints
           
> solverStage4 :: Subst -> SolverM [Type]
> solverStage4 s = do
>                    fs <- gets fieldMap
>                    cx <- gets ctx      
>                    Map.foldrWithKey (step cx) (return []) fs
>                  where
>                     isVar (Var _) = True
>                     isVar _ = False
>                     outVar (Var n) = n
>                     step cx k v co = case Map.lookup k cx of
>                                         Just ty -> if isVar ty then co >>= \ac -> return (Simple (Struct v (outVar ty)) : ac)
>                                                      else error "Impossible! ConstrSolver.solverStage4!"
>                                         Nothing -> undefinedSymbol k
           
Error messages

> undefinedSymbol :: Name -> SolverM a
> undefinedSymbol n = throwError $ show $ text "Symbol\n" <> pprint n <> text "\nis undefined."
           
> occursCheckError :: PPrint a => Name -> a -> SolverM b
> occursCheckError n t = throwError $ show $ text "Variable\n"
>                        <> pprint n <>
>                        text "\noccurs in\n" <> pprint t               

> unificationError :: PPrint a => a -> a -> SolverM b
> unificationError t t' = throwError $ show $ text "Impossible to unify\n" <>
>                         pprint t  <> text "with\n"   <>
>                         pprint t' 
