A parsec based parser for constraints
========================

> module Parser.ConstrParser (parser,typeParser) where

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
>                      opTable = [[ Ex.Infix conjParser Ex.AssocLeft ]]
>                      conjParser = (:&:) <$ comma          

> ctrParser :: Parser Constr
> ctrParser = choice [ existsParser, hasParser , varAscriptionParser
>                    , eqParser 
>                    , defParser, typeDefParser, truthParser
>                    , parens ctrParser ]

> existsParser :: Parser Constr
> existsParser = reserved "exists" *> (Exists <$> nameParser <*>
>                                                 (dot *> constraintParser))

> hasParser :: Parser Constr
> hasParser = reserved "has" *> parens (Has <$> nameParser <*>
>                                               (comma *> fieldParser))


> varAscriptionParser :: Parser Constr
> varAscriptionParser = build <$> reserved "typeof" <*>
>                                 parens nameParser <*>
>                                 reservedOp "="    <*>
>                                 (normal <|> funP)
>                       where
>                         build _ n _ (Left t) =  n :<-: t
>                         build _ n _ (Right ts) = n :<-: (Simple $ Function n (last ts) (init ts))                            
>                         normal = Left <$> typeParser
>                         funP = Right <$> functionParser
                                                
> eqParser :: Parser Constr
> eqParser = build <$> typeParser <*> (reservedOp "=" *> (normal <|> funP))
>            where
>               normal = Left <$> typeParser
>               funP = Right <$> functionParser
>               build t (Left t')  = t :=: t'       
>               build t@(Var n) (Right ts) = t :=: (Simple $ Function n (last ts) (init ts))
                       
                                                
> defParser :: Parser Constr
> defParser = reserved "def" *> (f <$> nameParser <*> (reservedOp ":" *> ((Left <$> typeParser) <|>
>                                                                        (Right <$> functionParser)))
>                                                 <*> (reserved "in" *> constraintParser))
>              where
>                f n (Left t) c = Def n t c
>                f n (Right ts) c = Def n (Simple $ Function n (last ts) (init ts)) c               
  
> typeDefParser :: Parser Constr
> typeDefParser = reserved "typedef" *>
>                 ((\n _ t -> TypeDef n t) <$> nameParser <*>
>                                              reserved "as" <*>
>                                              typeParser)

> truthParser :: Parser Constr
> truthParser = Truth <$ reserved "True"
  
Type parser

> typeParser :: Parser Type
> typeParser = choice [ Simple <$> cTypeParser
>                     , typeVarParser ]

> typeVarParser :: Parser Type
> typeVarParser = Var <$> nameParser                 

               
CType parser
  
> cTypeParser :: Parser CType
> cTypeParser = choice [ boolParser, charParser, shortIntParser, intParser
>                      , longIntParser, longLongIntParser, floatParser
>                      , doubleParser, longDoubleParser, voidParser
>                      , structParser, pointerParser ]               

> fieldParser :: Parser Field
> fieldParser = (\t _ n -> Field n t) <$> typeParser <*> colon <*> nameParser

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
>                      braces  (fieldParser `sepBy` comma)) <*> nameParser

> functionParser :: Parser [Type]
> functionParser = parens (typeParser `sepBy1` comma)
                       
> pointerParser :: Parser CType
> pointerParser = flip f <$> (many1 starParser) <*> typeParser
>                 where
>                    f t [ _ ] = Pointer t
>                    f t (_ : xs) = Pointer (Simple (f t xs))
                        
> signedParser :: Parser Signed
> signedParser = option None ((Unsigned  <$ reserved "unsigned") <|>
>                             (Signed <$ reserved "signed")) 
                                  
Lexer definition
  
> constrLexer :: TokenParser st
> constrLexer = Tk.makeTokenParser constrDef

> nameParser :: Parser Name
> nameParser = Name <$> (Tk.identifier constrLexer <|> Tk.operator constrLexer)

> reserved :: String -> Parser ()
> reserved = Tk.reserved constrLexer           

> reservedOp :: String -> Parser ()
> reservedOp = Tk.reservedOp constrLexer

> braces :: Parser a -> Parser a
> braces = Tk.braces constrLexer

> parens :: Parser a -> Parser a
> parens = Tk.parens constrLexer          

> comma :: Parser ()
> comma = () <$ Tk.comma constrLexer

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
>             , Tk.reservedNames = ["exists", "def", "in", "typedef", "True"
>                                  , "short", "long", "int", "float"
>                                  , "double", "_Bool", "_Complex"
>                                  , "char", "signed", "unsigned", "as"
>                                  , "void", "struct", "has"]
>             }             

              
