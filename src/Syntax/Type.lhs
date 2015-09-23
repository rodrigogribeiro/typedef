> {-# LANGUAGE DeriveDataTypeable #-}
  
Definition of types
====================        
        
> module Syntax.Type where

> import Data.Generics hiding (empty)
  
> import Utils.Pretty

Data type for representing C types

> newtype Name = Name { unName :: String }
>                deriving (Eq, Ord, Show, Data, Typeable)

> data Field = Field {
>                fieldName :: Name
>              , fieldType :: Type
>              } deriving (Eq, Ord, Show, Data, Typeable)    
     
> type Fields = [Field]     

> data Signed = Signed | Unsigned | None
>               deriving (Eq, Ord, Show , Data, Typeable)
  
> data CType = CBool
>            | Char         Signed
>            | ShortInt     Signed
>            | Int          Signed
>            | LongInt      Signed
>            | LongLongInt  Signed
>            | Float
>            | Double
>            | LongDouble
>            | FloatComplex
>            | DoubleComplex
>            | LongDoubleComplex
>            | Void
>            | Struct   { fields    :: Fields
>                       , sname     :: Name   }
>            | Pointer  { unPointer :: Type   }
>            | Function { name      :: Name
>                       , retTy     :: Type               
>                       , params    :: [Type] }
>            deriving (Eq , Ord, Show, Data, Typeable)


> data Type = Simple CType         -- simple type
>           | Var Name             -- type variables 
>           deriving (Eq, Ord, Show, Data, Typeable)
                          
Instances for pretty printting types

> instance PPrint Name where
>     pprint = text . unName

> instance PPrint Field where
>     pprint (Field n t) = pprint t <+> pprint n

> instance PPrint Signed where
>     pprint Signed = text "signed"
>     pprint Unsigned = text "unsigned"
>     pprint None = empty                  

> instance PPrint CType where
>     pprint CBool = text "_Bool"
>     pprint (Char u) = pprint u <+> text "char"
>     pprint (ShortInt u) = pprint u <+> text "short int"
>     pprint (Int u) = pprint u <+> text "int"
>     pprint (LongInt u) = pprint u <+> text "long int"
>     pprint (LongLongInt u) = pprint u <+> text "long long int"
>     pprint Float = text "float"
>     pprint Double = text "double"
>     pprint LongDouble = text "long double"
>     pprint FloatComplex = text "float _Complex"
>     pprint DoubleComplex = text "double _Complex"                      
>     pprint LongDoubleComplex = text "long double _Complex"
>     pprint Void = text "void"
>     pprint (Struct fs n ) = text "struct" <+>
>                             braces (hcat $ punctuate semi (map pprint fs))
>     pprint (Pointer p) = pprint p <+> star
>     pprint (Function n r ps) = pprint n <+>
>                                pprint r <+>
>                                parens (hcat $ punctuate comma
>                                                  (map pprint ps)) 


> instance PPrint Type where
>     pprint (Simple t) = pprint t
>     pprint (Var n) = pprint n

Rank for conversion between types

> rank :: CType -> Int
> rank CBool = 1
> rank (Char _) = 2
> rank (ShortInt _) = 3
> rank (Int _) = 4
> rank (LongInt _) = 5
> rank (LongLongInt _) = 6
> rank Float = 7
> rank Double = 8
> rank LongDouble = 9
> rank FloatComplex = 8
> rank DoubleComplex = 9
> rank t = 100                     

Integer promotion

> integerPromotion :: CType -> CType
> integerPromotion c
>      | rank c <= rank (Int Unsigned) = Int Unsigned
>      | otherwise = c
                                   
Some auxiliar code

> star :: Doc
> star = char '*'
                      

             
