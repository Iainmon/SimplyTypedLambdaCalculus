module Parser where



import Syntax
import Data.Function
import           Data.Char
import           Text.Parsec hiding (parse)
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Expr     as Ex
import qualified Text.Parsec.Token    as Tok
import qualified Text.Parsec          as Parsec


data Lit
  = LInt Int
  | LBool Bool
  deriving ( Eq, Show )

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops   = ["λ", "->", "lambda"]
        names = []
        style = emptyDef {Tok.reservedOpNames = ops,
                          Tok.reservedNames   = names,
                          Tok.commentLine     = "#"}

data Lambda = Lambda

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

identifier :: Parser String
identifier = Tok.identifier lexer

parens :: Parser a -> Parser a
parens = Tok.parens lexer

contents :: Parser a -> Parser a
contents p = do
  Tok.whiteSpace lexer
  r <- p
  eof
  return r

natural :: Parser Integer
natural = Tok.natural lexer

variable :: Parser Expr
variable = do
  iden <- identifier
  return $ lit $ symbolize iden

number :: Parser Expr
number = undefined -- natural >>= return . (\x -> (Lit (LInt (fromIntegral x))))

anyt :: Parser String
anyt = Tok.symbol lexer "*"

typeLiteral :: Parser Type
typeLiteral = do
  typ <- ((try anyt)<|> identifier )
  return $ Type $ symbolize typ

typeFun :: Parser Type
typeFun = do
  lt <- typeExpr
  reservedOp "->"
  rt <- typeExpr
  return $ Fun lt rt

typeExpr :: Parser Type
typeExpr = typeLiteral <|> parens typeFun

typing :: Parser Expr
typing = do
  let lambdaTerm = variable
  e <- lambdaTerm
  reservedOp ":"
  typ <- typeExpr
  return $ assertType e typ

untypedVar :: Parser Expr
untypedVar = do
  iden <- identifier
  return $ lit $ symbolize iden

optionalTyping :: Parser Expr
optionalTyping = try typing <|> variable

lambda :: Parser Expr
lambda = do
  reservedOp "λ" <|> reservedOp "\\" <|> reservedOp "lambda"
  args <- many1 identifier
  reservedOp "." <|> reservedOp "->"
  body <- expr
  return $ foldr lam body (fmap symbolize args)

term :: Parser Expr
term =  parens expr
    <|> lambda
    <|> optionalTyping

    


    -- <|> variable

    -- <|> number

expr :: Parser Expr
expr = do
  es <- many1 term
  return (foldl1 app es)


assignment :: Parser Assignment
assignment = do
  name <- identifier
  reservedOp ":="
  val <- expr
  return (symbolize name,val)

-- Utility function for testing a single parser
doParse :: Parser a -> String -> Either ParseError a
doParse = flip Parsec.parse "=>"

-- parseExpr "(λx.x)"
parseExpr :: String -> Either ParseError Expr
parseExpr = doParse (contents expr)

-- parseAssignment "I := (λx.x)"
parseAssignment :: String -> Either ParseError Assignment
parseAssignment = doParse (contents assignment)

simplifyAST (Right ast) = Just ast
simplifyAST _ = Nothing

parse' :: String -> Either ParseError Expr
parse' s = parseExpr s

unsafeParse = unjust' . parse

parse = simplifyAST . parse'