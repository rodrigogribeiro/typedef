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
> typeParser = (Simple <$> cTypeParser) <|> typeDefParser

type def parser

> typeDefParser :: Parser Type
> typeDefParser = reserved "typedef" *> braces (TypeDef <$> typeParser <*> nameParser)
               
CType parser
  
> cTypeParser :: Parser CType
> cTypeParser = choice [ boolParser, charParser, shortIntParser, intParser
>                      , longIntParser, longLongIntParser, floatParser
>                      , doubleParser, longDoubleParser, voidParser
>                      , structParser,  functionParser, pointerParser ]             

> fieldParser :: Parser Field
> fieldParser = Field <$> nameParser <*> typeParser               

> boolParser :: Parser CType
> boolParser = CBool <$ reserved "_Bool"

> charParser :: Parser CType
> charParser =  Char <$> signedParser <* reserved "char"

> shortIntParser :: Parser CType
> shortIntParser =  ShortInt <$> signedParser <* reserved "short"
>                                             <* reserved "int"

> intParser :: Parser CType
> intParser = Int <$> signedParser <* reserved "int"

> longIntParser :: Parser CType
> longIntParser = LongInt <$> signedParser <* reserved "long"
>                                          <* reserved "int"

> longLongIntParser :: Parser CType
> longLongIntParser = LongLongInt <$> signedParser <* reserved "long"
>                                                  <* reserved "long"
>                                                  <* reserved "int"

> floatParser :: Parser CType
> floatParser = reserved "float" *>
>                     option Float (FloatComplex <$ reserved "_Complex")

> doubleParser :: Parser CType
> doubleParser =  reserved "double" *>
>                     option Double (DoubleComplex <$ reserved "_Complex")

> longDoubleParser :: Parser CType
> longDoubleParser =  LongDouble <$ reserved "long"   <*
>                                   reserved "double"

> voidParser :: Parser CType
> voidParser = Void <$ reserved "void"

> structParser :: Parser CType
> structParser = Struct <$> (reserved "struct" *>
>                      braces  (fieldParser `sepBy` comma))

> functionParser :: Parser CType
> functionParser = f <$> nameParser <*> (colon *> (typeParser `sepBy1` reservedOp "->"))
>                  where
>                     f n [] = error "Impossible! Function Parser!"
>                     f n (t : ts) = Function n t ts 
                       
> pointerParser :: Parser CType
> pointerParser = Pointer <$> typeParser <* starParser
                                                                                                              
> signedParser :: Parser Bool
> signedParser = True  <$ reserved "signed" <|>
>                False <$ reserved "unsigned"
                                  
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

> starParser :: Parser ()
> starParser = () <$ Tk.symbol constrLexer "*"

> colon :: Parser ()
> colon = () <$ Tk.colon constrLexer

> dot :: Parser ()
> dot = () <$ Tk.dot constrLexer
  
Constraint language def

> constrDef :: LanguageDef st
> constrDef = emptyDef {
>               Tk.reservedOpNames = [":", "=", "->"] 
>             , Tk.reservedNames = ["exists", "def", "isdef", "True"
>                                  , "short", "long", "int", "float"
>                                  , "double", "_Bool", "_Complex"
>                                  , "char", "signed", "unsigned"
>                                  , "void", "struct"]
>             }             

              
