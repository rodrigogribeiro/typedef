A parser for CoreC language
===========================  

>module Parser.CoreCParser where

> import Data.Functor
> import Data.Functor.Identity

> import GHC.Float
  
> import Text.Parsec
> import Text.Parsec.Language
> import Text.Parsec.Token (TokenParser)    
> import qualified Text.Parsec.Token as Tk
> import qualified Text.Parsec.Expr as Ex

> import Parser.ConstrParser (typeParser)
  
> import Syntax.Type
> import Syntax.Constraint hiding (dot)
> import Syntax.CoreC
> import Utils.Pretty
     

A type for parsers
  
> type Parser a = ParsecT String () Identity a

> pOp :: Parser Op
> pOp = choice (map mk ops)
>       where
>         ops = enumFromTo Plus Ge
>         mk o = o <$ reservedOp (show $ pprint o)

> pName :: Parser Name
> pName = Name <$> Tk.identifier coreCLexer        

> pLiteral :: Parser Literal
> pLiteral = choice [ IInt    <$> pInteger
>                   , IChar   <$> pChar
>                   , f       <$> pFloat
>                   , IString <$> pString ]
>            where
>               f = IFloat . double2Float
  
Lexer definition

> coreCLexer :: TokenParser st
> coreCLexer = Tk.makeTokenParser coreCDef

> reservedOp :: String -> Parser ()
> reservedOp = Tk.reservedOp coreCLexer
  
> parens :: Parser a -> Parser a
> parens = Tk.parens coreCLexer

> brackets :: Parser a -> Parser a
> brackets = Tk.brackets coreCLexer

> pInteger :: Parser Int
> pInteger = fromInteger <$> Tk.integer coreCLexer

> pChar :: Parser Char
> pChar = Tk.charLiteral coreCLexer        

> pString :: Parser String
> pString = Tk.stringLiteral coreCLexer

> pFloat :: Parser Double 
> pFloat = Tk.float coreCLexer
  
CoreC language def 

> coreCDef :: LanguageDef st
> coreCDef = emptyDef {
>              Tk.reservedOpNames = [ "+", "-", "*", "/", "%", "==", "!="
>                                   , ">=", ">", "<=", "<", "!", "&", "&&"
>                                   , "||", "|", "^", "->", "."]
>            , Tk.reservedNames = [ "int", "float", "double", "char"
>                                 , "short", "long", "signed", "unsigned"
>                                 , "void", "_Bool", "_Complex", "struct"
>                                 , "typedef"]
>            }            
