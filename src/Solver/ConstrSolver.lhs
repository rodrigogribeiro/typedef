The solver algorithm
=============

Basically the algorithm performs 3 stages:

1 - Create all fresh variables for existencial quantifiers and collects all definitions
2 - Solve all equality constraints and updates types in field constraints
3 - Build all typedef's from the solution of unification

> module Solver.ConstrSolver where

> import Data.Generics (everywhere, mkT)                            

> import Syntax.Type
> import Syntax.Constraint    
> import Utils.SolverMonad    


> subst :: Name -> Type -> Constr -> Constr
> subst n t c = everywhere (mkT (subs n t)) c
>               where
>                  subs n t (Var n')
>                       | n == n' = t
>                       | otherwise = Var n'
  

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
> solverStage1 (Def n t)
>              = insertDef n t >> return Truth
> solverStage1 c = return c               

