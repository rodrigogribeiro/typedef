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

Type parser

> typeParser :: Parser Type
> typeParser = undefined              

> fieldParser :: Parser Field
> fieldParser = Field <$> nameParser <*> typeParser               

> boolParser :: Parser Type
> boolParser = const (Simple CBool) <$> reserved "_Bool"

> charParser :: Parser Type
> charParser =  Simple . Char <$> signedParser <* reserved "char"

> shortIntParser :: Parser Type
> shortIntParser = Simple . ShortInt <$> signedParser <* reserved "short"
>                                                     <* reserved "int"

> intParser :: Parser Type
> intParser = Simple . Int <$> signedParser <* reserved "int"

> longIntParser :: Parser Type
> longIntParser = Simple . LongInt <$> signedParser <* reserved "long"
>                                                   <* reserved "int"

> longLongIntParser :: Parser Type
> longLongIntParser = Simple . LongLongInt <$> signedParser <* reserved "long"
>                                                           <* reserved "long"
>                                                           <* reserved "int"

> floatParser :: Parser Type
> floatParser = Simple <$> (reserved "float" *>
>                     option Float (FloatComplex <$ reserved "_Complex"))

> doubleParser :: Parser Type
> doubleParser = Simple <$> (reserved "double" *>
>                     option Double (DoubleComplex <$ reserved "_Complex"))

> longDoubleParser :: Parser Type
> longDoubleParser =  Simple LongDouble <$ reserved "long"   <*
>                                          reserved "double"

> voidParser :: Parser Type
> voidParser = Simple Void <$ reserved "void"

> structParser :: Parser Type
> structParser = (Simple . Struct) <$> (reserved "struct" *>
>                      braces  (fieldParser `sepBy` comma))
                                                                                                              
> signedParser :: Parser Bool
> signedParser = (const True)  <$> reserved "signed" <|>
>                (const False) <$> reserved "unsigned"
                 
Lexer definition
  
> constrLexer :: TokenParser st
> constrLexer = Tk.makeTokenParser constrDef

> nameParser :: Parser Name
> nameParser = Name <$> Tk.identifier constrLexer             

> reserved :: String -> Parser ()
> reserved = Tk.reserved constrLexer           

> reservedOp :: String -> Parser ()
> reservedOp = Tk.reservedOp constrLexer

> braces :: Parser a -> Parser a
> braces = Tk.braces constrLexer

> comma :: Parser String
> comma = Tk.comma constrLexer
  
Constraint language def

> constrDef :: LanguageDef st
> constrDef = emptyDef {
>               Tk.reservedOpNames = [".", ":", "=", "->"] 
>             , Tk.reservedNames = ["exists", "def", "isdef", "True"
>                                  , "short", "long", "int", "float"
>                                  , "double", "_Bool", "_Complex"
>                                  , "char", "signed", "unsigned"
>                                  , "void", "struct"]
>             }             

              
