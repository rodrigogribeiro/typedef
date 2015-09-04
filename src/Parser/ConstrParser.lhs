A parsec based parser for constraints
========================

> module Parser.ConstrParser where

> import Data.Functor
> import Data.Functor.Identity
  
> import Text.Parsec
> import Text.Parsec.Language
> import Text.Parsec.Token (TokenParser)    
> import qualified Text.Parsec.Token as Tk
> import qualified Text.Parsec.Expr as Ex

> import Syntax.Type
> import Syntax.Constraint    

A type for parsers
  
> type Parser a = ParsecT String () Identity a

Type parser  ParsecT s u m a

> typeParser :: Parser Type
> typeParser = Ex.buildExpressionParser typeOpTable typeTerm

> type Operator a = Ex.Operator String () Identity a
> type OperatorTable a = [[Operator a]]    

> typeOpTable :: OperatorTable Type
> typeOpTable = [[ ]]

> typeTerm :: Parser Type
> typeTerm = undefined
    
Lexer definition
  
> constrLexer :: TokenParser st
> constrLexer = Tk.makeTokenParser constrDef

> nameParser :: Parser Name
> nameParser = Name <$> Tk.identifier constrLexer             

> reserved :: String -> Parser ()
> reserved = Tk.reserved constrLexer           

> reservedOp :: String -> Parser ()
> reservedOp = Tk.reservedOp constrLexer
  
Constraint language def

> constrDef :: LanguageDef st
> constrDef = emptyDef {
>               Tk.reservedOpNames = [",", ".", ":", "=", "->"] 
>             , Tk.reservedNames = ["exists", "def", "isdef", "True"
>                                  , "short", "long", "int", "float"
>                                  , "double", "_Bool", "_Complex"]
>             }             

              
