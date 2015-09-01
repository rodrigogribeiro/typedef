Definition of substitutions and its operations
=============================


> module Solver.Subst where

> import Data.Generics hiding (Constr)
> import Data.Map(Map)
> import qualified Data.Map as Map
> import Data.Set(Set)
> import qualified Data.Set as Set
    
> import Syntax.Type
> import Syntax.Constraint    


Substitution definition

> type Subst = Map Name Type

> nullSubst :: Subst
> nullSubst = Map.empty

> (+->) :: Name -> Type -> Subst
> n +-> t = Map.singleton n t         

> class Substitutable a where
>     apply :: Subst -> a -> a
>     fv    :: a -> Set Name

> instance Substitutable a => Substitutable [a] where
>     apply s = map (apply s)
>     fv = foldr (Set.union . fv) Set.empty

> instance Substitutable CType where
>     apply s (Struct fs) = Struct (apply s fs)
>     apply s (Pointer t) = Pointer (apply s t)
>     apply s (Function n r ts) = Function n (apply s r) (apply s ts)
>     apply s t = t

>     fv = everything Set.union (mkQ Set.empty (\(Var n) -> Set.singleton n))                       

> instance Substitutable Field where
>     apply s (Field n t) = Field n (apply s t)
>     fv = everything Set.union (mkQ Set.empty (\(Var n) -> Set.singleton n))                      


> instance Substitutable Type where
>     apply s (Simple t) = Simple (apply s t)
>     apply s (TypeDef t n) = TypeDef (apply s t) n
>     apply s v@(Var n) = case Map.lookup n s of
>                           Nothing -> v
>                           Just ty -> ty
>     fv = everything Set.union (mkQ Set.empty (\(Var n) -> Set.singleton n))                      


> instance Substitutable Constr where
>     apply s = everywhere (mkT (\v@(Var n) -> maybe v id (Map.lookup n s)))
>     fv = everything Set.union (mkQ Set.empty (\(Var n) -> Set.singleton n))


Occurs check test

> occurs :: Substitutable a => Name -> a -> Bool
> occurs n t = n `Set.member` fv t          

Composition

> (@@) :: Subst -> Subst -> Subst
> s1 @@ s2 = (Map.map (\t -> apply s1 t) s2) `Map.union` s1        
