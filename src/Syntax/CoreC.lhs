> {-# LANGUAGE DeriveDataTypeable #-}

Syntax for Core C programs
=================

> module Syntax.CoreC where

> import Data.Generics
  
> import Syntax.Type
> import Utils.Pretty
  
Expression Syntax

> data Literal = IInt Int | IChar Char | IFloat Float | IString String
>                deriving (Eq, Ord, Show, Data, Typeable)

> data Op = Plus | Times | Minus | Div
>         | Mod | ShiftL | ShiftR | Or
>         | And | BAnd | BOr | BXor 
>         | OEq | ONeq | Not | Lt | Gt
>         | Le  | Ge
>         deriving (Eq, Ord, Show, Enum, Data, Typeable)
                 
> data Exp = EVar Name
>          | Lit Literal
>          | BOp Op Exp Exp
>          | ECall Name [Exp]
>          | FAccess Exp Name
>          | AAccess Exp Exp
>          | Cast Type Exp
>          | Addr Exp
>          | PAccess Exp
>          | PFieldAccess Exp Name
>          | Null
>          deriving (Eq, Ord, Show, Data, Typeable)

Statement syntax

> data Cmd = VarDef Type Name Exp
>          | VarAssign Name Exp
>          | PointerAssign Name Exp
>          | FieldAssign Name Name Exp
>          | PFieldAssign Name Name Exp  
>          | ArrayAssign Name Exp Exp
>          | CCall Name [Exp]
>          | Return Exp  
>          deriving (Eq, Ord, Show, Data, Typeable)


Declarations

> data Decl = DTypeDef Type Name
>           | DFunction Type Name [(Type,Name)] [Cmd]
>           deriving (Eq, Ord, Show, Data, Typeable)

Program definition

> newtype Program = Program { unProg :: [Decl] }
>                   deriving (Eq, Ord, Show, Data, Typeable)


Pretty printer definition

> instance PPrint Literal where
>    pprint (IInt i) = int i
>    pprint (IChar c) = char c
>    pprint (IFloat f) = float f
>    pprint (IString s) = text s                    

> instance PPrint Op where
>    pprint Plus = char '+'
>    pprint Times = char '*'
>    pprint Minus = char '-'
>    pprint Div = char '/'
>    pprint Mod = char '%'
>    pprint ShiftL = text ">>"
>    pprint ShiftR = text "<<"
>    pprint Or = text "||"
>    pprint And = text "&&"
>    pprint BOr = text "|"
>    pprint BAnd = text "&"
>    pprint BXor = text "ˆ"
>    pprint OEq  = text "=="
>    pprint ONeq = text "!="
>    pprint Not = text "!"
>    pprint Lt = text "<"
>    pprint Gt = text ">"
>    pprint Le = text "<="
>    pprint Ge = text ">="            

> instance PPrint Exp where
>     pprint Null = text "NULL"
>     pprint (EVar n) = pprint n
>     pprint (Lit l) = pprint l
>     pprint (ECall n es) = pprint n <+> parens (hcat $ punctuate comma es')
>                           where
>                             es' = map pprint es
>     pprint (FAccess e n) = pprint e <> dot <> pprint n
>     pprint (AAccess e e') = pprint e <> brackets (pprint e')
>     pprint (Cast t e) = parens (pprint t) <+> pprint e                         
>     pprint (Addr e) = text "&"<> pprint e
>     pprint (PAccess e) = text "*" <> pprint e
>     pprint (PFieldAccess e n) = pprint e <> text "->" <> pprint n                     
     
> instance PPrint Cmd where
>     pprint (VarDef t n e) = pprint t <+> pprint n <+> prhs e
>     pprint (PointerAssign n e) = text "*" <> pprint n <+> prhs e
>     pprint (FieldAssign n n' e) = pprint n <> dot <> pprint n' <+> prhs e
>     pprint (PFieldAssign n n' e) = pprint n <> text "->" <> pprint n' <+>  prhs e                              
>     pprint (ArrayAssign n e e') = pprint n <> brackets (pprint e) <+> prhs e'
>     pprint (Return e) = text "return" <+> pprint e                               
>     pprint (CCall n es) = pprint n <+> parens (hcat $ punctuate comma es')
>                           where
>                             es' = map pprint es
                                   

> instance PPrint Decl where
>     pprint (DTypeDef t n) = text "typedef" <+> pprint t <+> pprint n <> semi <> text "\n"
>     pprint (DFunction t n ps cs) = pprint t <+> pprint n <+>
>                                    parens ps' <+> brackets cs' <> text "\n"
>                                    where
>                                      ps' = hcat $ punctuate comma pp
>                                      pp = map (\(t,n) -> pprint t <+> pprint n) ps
>                                      cs' = hcat $ punctuate semi (map pprint cs)     

> instance PPrint Program where
>     pprint = hcat . map pprint . unProg
      
Auxiliar functions

> prhs :: Exp -> Doc         
> prhs e = text "=" <+> pprint e              

> dot :: Doc
> dot = char '.'       
