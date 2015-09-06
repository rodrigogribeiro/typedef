> {-# LANGUAGE DeriveDataTypeable #-}
  
Definition of types
====================        
        
> module Syntax.Type where

> import Data.Generics
  
> import Utils.Pretty

Data type for representing C types

> newtype Name = Name { unName :: String }
>                deriving (Eq, Ord, Show, Data, Typeable)

> data Field = Field {
>                fieldName :: Name
>              , fieldType :: Type
>              } deriving (Eq, Ord, Show, Data, Typeable)    
     
> type Fields = [Field]     

> data CType = CBool
>            | Char         Bool  -- True ==> signed char / False unsigned char
>            | ShortInt     Bool
>            | Int          Bool
>            | LongInt      Bool
>            | LongLongInt  Bool
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
>           | TypeDef Type Name    -- type def
>           | Var Name             -- type variables 
>           deriving (Eq, Ord, Show, Data, Typeable)
                          
Instances for pretty printting types

> instance PPrint Name where
>     pprint = text . unName

> instance PPrint Field where
>     pprint (Field n t) = pprint t <+> pprint n

> instance PPrint CType where
>     pprint CBool = text "_Bool"
>     pprint (Char u) = signed u "char"
>     pprint (ShortInt u) = signed u "short int"
>     pprint (Int u) = signed u "int"
>     pprint (LongInt u) = signed u "long int"
>     pprint (LongLongInt u) = signed u "long long int"
>     pprint Float = text "float"
>     pprint Double = text "double"
>     pprint LongDouble = text "long double"
>     pprint FloatComplex = text "float _Complex"
>     pprint DoubleComplex = text "double _Complex"                      
>     pprint LongDoubleComplex = text "long double _Complex"
>     pprint Void = text "void"
>     pprint (Struct fs n ) = text "struct" <+>
>                             braces (hcat $ punctuate semi (map pprint fs)) <+> pprint n
>     pprint (Pointer p) = pprint p <+> star
>     pprint (Function n r ps) = pprint n <+>
>                                pprint r <+>
>                                parens (hcat $ punctuate comma
>                                                  (map pprint ps)) 


> instance PPrint Type where
>     pprint (Simple t) = pprint t
>     pprint (Var n) = pprint n
>     pprint (TypeDef t n) = text "typedef" <+> pprint t <+> pprint n
                                                   
Some auxiliar code

> signed :: Bool -> String -> Doc
> signed True s  = hsep $ map text ["signed", s]
> signed False s = hsep $ map text ["unsigned", s]

> star :: Doc
> star = char '*'
                      

             
