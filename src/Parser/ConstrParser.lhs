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
> import Syntax.Constraint hiding (dot)  

A type for parsers
  
> type Parser a = ParsecT String () Identity a

Top level parsing function

> parser :: String -> Either String Constr
> parser = either (Left . show) Right . parse constraintParser ""
  
Constraint parser

> constraintParser :: Parser Constr
> constraintParser = Ex.buildExpressionParser opTable ctrParser
>                    where
>                      opTable = [[ Ex.Infix conjParser Ex.AssocRight ]]
>                      conjParser = (:&:) <$ comma 

> ctrParser :: Parser Constr
> ctrParser = choice [ existsParser, eqParser, hasParser
>                    , defParser, isDefParser, truthParser ]

> existsParser :: Parser Constr
> existsParser = reserved "exists" *> (Exists <$> nameParser <*>
>                                                 (dot *> constraintParser))

> eqParser :: Parser Constr
> eqParser = (:=:) <$> typeParser <*> (reservedOp "=" *> typeParser)

> hasParser :: Parser Constr
> hasParser = reserved "has" *> parens (Has <$> nameParser <*>
>                                               (comma *> fieldParser))

> defParser :: Parser Constr
> defParser = reserved "def" *> (Def <$> nameParser <*> (colon *> typeParser))

> isDefParser :: Parser Constr
> isDefParser = reserved "isdef" *> (IsDefined <$> nameParser)

> truthParser :: Parser Constr
> truthParser = Truth <$ reserved "True"
  
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

> arrowTypeParser :: Parser [Type]
> arrowTypeParser = undefined

> functionParser :: Parser CType
> functionParser = f <$> nameParser <*> (colon *> arrowTypeParser)
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

> parens :: Parser a -> Parser a
> parens = Tk.parens constrLexer          

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
>                                  , "void", "struct", "has"]
>             }             

              
