> {-# LANGUAGE DeriveDataTypeable #-}

Constraints syntax 
====================

> module Syntax.Constraint where

> import Data.Monoid (Monoid)
> import qualified Data.Monoid as M
> import Data.Generics (Data, Typeable, everything, mkQ)
> import Data.Set(Set)
> import qualified Data.Set as Set
     
> import Syntax.Type  
> import Utils.Pretty  
  
Definition of constraints

> data Constr = Exists Name Constr   -- fresh variable introduction
>             | Constr :&: Constr    -- conjunction
>             | Type :=: Type        -- type equality
>             | Name :<-: Type       -- type ascription 
>             | Has Name Field       -- field constraint
>             | Def Name Type Constr -- definition and its type
>             | TypeDef Name Type    -- type definition
>             | Truth                -- empty constraint  
>             deriving (Eq, Ord, Data, Typeable)

Constraints form a monoid

> instance Monoid Constr where
>     mempty = Truth
>     mappend = conj
>               where
>                 conj Truth c = c
>                 conj c Truth = c                          
>                 conj c c' = c :&: c'


Definition of a pretty printer

> instance Show Constr where
>     show = show . pprint

> instance PPrint Constr where
>     pprint (Exists n c) = hsep [text "exists", pprint n, dot, pprint c]
>     pprint (c :&: c')
>            | isGround c' = hsep [pprint c, comma, pprint c']
>            | otherwise =  hsep [pprint c, comma, parens $ pprint c']               
>     pprint (t :=: t') = pprint t <+> equals <+> pprint t'
>     pprint (n :<-: t) = text "typeof" <> (parens (pprint n)) <+>
>                         equals <+> pprint t
>     pprint (Has n f)  = text "has" <> parens (pprint n <+>
>                                               comma    <>
>                                               pprint f)
>     pprint (Def n t c) = text "def"    <+> pprint n
>                          <+> colon     <+> pprint t
>                          <+> text "in" <+> pprint c 
>     pprint (TypeDef n t) = text "typedef" <+> pprint n
>                                           <+> text "as"
>                                           <+> pprint t    
>     pprint Truth = text "True"                       
                         
Auxiliar functions

> isGround :: Constr -> Bool
> isGround (Has _ _) = True
> isGround (_ :=: _) = True
> isGround _         = False                     

> dot :: Doc
> dot = char '.'       
