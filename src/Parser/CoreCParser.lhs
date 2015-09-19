A parser for CoreC language
===========================  

>module Parser.CoreCParser(parser) where

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
> import Utils.Pretty (PPrint(..))
     

A type for parsers
   
> type Parser a = ParsecT String () Identity a

> parser :: String -> Either String Program   
> parser = either (Left . show) Right . parse pProgram ""     

> pProgram :: Parser Program
> pProgram = Program <$> many pDecl
  
> pDecl :: Parser Decl
> pDecl = pTypeDecl <|> pFunctionDecl

> pTypeDecl :: Parser Decl
> pTypeDecl = DTypeDef <$> (reserved "typedef" *> typeParser) <*> pName

> pFunctionDecl :: Parser Decl
> pFunctionDecl = DFunction <$> typeParser <*> pName <*> parameters <*> cmds
>                 where
>                    parameters = parens (param `sepBy` comma)
>                    param = (,) <$> typeParser <*> pName
>                    cmds = braces (pCmd `sepBy` semi)
  
> pCmd :: Parser Cmd
> pCmd = choice [ pVarDef
>               , pVarAssign
>               , pPointerAssign
>               , pFieldAssign
>               , pArrayAssign
>               , pPFieldAssign
>               , pCall]
>        where
>           pVarDef = VarDef <$> typeParser <*> pName <*> pRhs
>           pVarAssign = VarAssign <$> pName <*> pRhs
>           pPointerAssign = PointerAssign <$> (reservedOp "*" *> pName) <*> pRhs
>           pArrayAssign = ArrayAssign <$> pName <*> brackets pExpr <*> pRhs
>           pFieldAssign = FieldAssign <$> pName <*> (reservedOp "." *> pName) <*> pRhs
>           pPFieldAssign = PFieldAssign <$> pName <*> (reservedOp "->" *> pName) <*> pRhs
>           pCall = CCall <$> pName <*> parens (pExp `sepBy` comma)               
>           pRhs = reservedOp "=" *> pExpr
  
> pExpr :: Parser Exp
> pExpr = f <$> pExp <*> option id (choice [ arrayP
>                                          , fieldP
>                                          , pFieldP ])
>         where
>            f e g = g e
>            arrayP = flip AAccess <$> brackets pExpr
>            fieldP = par FAccess "."
>            pFieldP = par PFieldAccess "->"         
>            par f s = flip f <$> (reservedOp s *> pName)
  
> pExp :: Parser Exp
> pExp = Ex.buildExpressionParser opTable pAtom
>        where
>           derefP = PAccess <$ reservedOp "*"
>           addrP = Addr <$ reservedOp "&"
>           multP = BOp <$> choice [ Times <$ reservedOp "*"
>                                  , Div   <$ reservedOp "/"
>                                  , Mod   <$ reservedOp "%"]
>           addP = choice [ (BOp Plus)  <$ reservedOp "+"
>                         , (BOp Minus) <$ reservedOp "-"]
>           shiftP = BOp <$> choice [ ShiftL <$ reservedOp "<<"
>                                   , ShiftR <$ reservedOp ">>"]
>           relP = BOp <$> choice [ Lt <$ reservedOp "<"
>                                 , Gt <$ reservedOp ">"
>                                 , Le <$ reservedOp "<="
>                                 , Ge <$ reservedOp ">="]
>           eqP = BOp <$> choice [ OEq <$  reservedOp "=="
>                                , ONeq <$ reservedOp "!="]
>           andBP = (BOp BAnd) <$ reservedOp "&"
>           xorBP = (BOp BXor) <$ reservedOp "^"
>           orBP = (BOp BOr) <$ reservedOp "|"
>           andP = (BOp And) <$  reservedOp "&&"
>           orP = (BOp Or) <$ reservedOp "||"       
>           unvar (EVar n) = n
>           unvar _        = error "Impossible! Parser.CoreCParser.pExp!"                 
>           opTable = [ [ Ex.Prefix derefP ]
>                     , [ Ex.Prefix addrP ]
>                     , [ Ex.Infix multP Ex.AssocLeft ]  
>                     , [ Ex.Infix addP Ex.AssocLeft ]
>                     , [ Ex.Infix shiftP Ex.AssocLeft ]
>                     , [ Ex.Infix relP Ex.AssocLeft ]
>                     , [ Ex.Infix eqP Ex.AssocLeft ]  
>                     , [ Ex.Infix andBP Ex.AssocLeft]
>                     , [ Ex.Infix xorBP Ex.AssocLeft]
>                     , [ Ex.Infix orBP Ex.AssocLeft]
>                     , [ Ex.Infix andP Ex.AssocLeft]
>                     , [ Ex.Infix orP Ex.AssocLeft] ]

> pAtom :: Parser Exp
> pAtom = choice [ EVar <$> pName
>                , Lit  <$> pLiteral
>                , Cast <$> (parens typeParser) <*> pExp
>                , Addr <$> (reservedOp "&" *> pExp)
>                , PAccess <$> (reservedOp "*" *> pExp)
>                , ECall <$> pName <*> parens (pExp `sepBy` comma)]         


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

> reserved :: String -> Parser ()
> reserved = Tk.reserved coreCLexer
  
> reservedOp :: String -> Parser ()
> reservedOp = Tk.reservedOp coreCLexer
  
> parens :: Parser a -> Parser a
> parens = Tk.parens coreCLexer

> semi :: Parser ()
> semi = () <$ Tk.semi coreCLexer
  
> brackets :: Parser a -> Parser a
> brackets = Tk.brackets coreCLexer

> braces :: Parser a -> Parser a
> braces = Tk.braces coreCLexer          

> comma :: Parser ()
> comma = () <$ Tk.comma coreCLexer
  
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
>                                   , "||", "|", "^", "->", ".", ">>", "<<"]
>            , Tk.reservedNames = [ "int", "float", "double", "char"
>                                 , "short", "long", "signed", "unsigned"
>                                 , "void", "_Bool", "_Complex", "struct"
>                                 , "typedef"]
>            }            
