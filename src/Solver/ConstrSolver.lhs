The solver algorithm
=============

Basically the algorithm performs 3 stages:

1 - Create all fresh variables for existencial quantifiers, collects all definitions and field constraints
2 - Expand all definitions types  
3 - Solve all equality constraints and updates types in field constraints
4 - Build all typedef's from the solution of unification

> module Solver.ConstrSolver where

> import Control.Monad.State(gets)
  
> import Data.Generics (everywhere, everything, mkT, mkQ)
> import Data.Map(Map)
> import qualified Data.Map as Map
> import Data.Set(Set)
> import qualified Data.Set as Set    

> import Syntax.Type
> import Syntax.Constraint
> import Solver.Subst    
> import Utils.SolverMonad    


Stage 1: fresh variable creation and context building  

> subst :: Name -> Type -> Constr -> Constr
> subst n t c = everywhere (mkT (subs n t)) c
>               where
>                  subs n t (Var n')
>                       | n == n' = t
>                       | otherwise = Var n'
>                  subst n t t' = t'     
  

> solverStage1 :: Constr -> SolverM Constr
> solverStage1 (Exists n c)
>              = do
>                  t <- fresh
>                  return (subst n t c)
> solverStage1 (c :&: c')
>              = do
>                c1 <- solverStage1 c
>                c1' <- solverStage1 c'
>                return (c1 :&: c1')
> solverStage1 (Has n f)
>              = insertField n f >> return Truth
> solverStage1 (Def n t)
>              = insertDef n t >> return Truth
> solverStage1 c = return c               

Stage 2: expand definitions

> expand :: Ctx -> Constr -> Constr
> expand ctx c = foldr ($) c (zipWith subst vs ts)
>               where
>                  ss = Set.fromList $ Map.keys ctx
>                  vs = Set.toList $ Set.intersection ss (fv c)
>                  ts = map ((Map.!) ctx) vs

> solverStage2 :: Constr -> SolverM Constr
> solverStage2 c = do
>                    cx <- gets ctx
>                    return (everywhere (mkT (expand cx)) c)

Stage 3: unification of equality constraints  
                             
> collect :: Constr -> [Constr]
> collect = everything (++) (mkQ [] (\c@(_ :=: _) -> [c]))

           
